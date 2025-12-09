#' testthat Expectations for Image Comparison
#'
#' Assert that images match or differ using odiff. These expectations are
#' designed for visual regression testing in testthat test suites.
#'
#' @param actual Path to the actual/current image, or a magick-image object.
#' @param expected Path to the expected/baseline image, or a magick-image object.
#' @param img1,img2 Paths to images being compared (for `expect_images_differ`).
#' @inheritParams compare_images
#' @param fail_on_layout Logical; if `TRUE`, fail if images have different
#'   dimensions. Default is `TRUE` for tests (stricter than [compare_images()]).
#' @param info Extra information to be included in the failure message
#'   (useful for providing context about what was being tested).
#' @param label Optional custom label for the actual image in failure messages.
#'   If not provided, uses the deparsed expression.
#'
#' @details
#' `expect_images_match()` asserts that two images are visually identical
#' (within the specified threshold). On failure, a diff image is saved to
#' `tests/testthat/_odiffr/` by default, which can be controlled via
#' `options(odiffr.save_diff = FALSE)` or `options(odiffr.diff_dir = "path")`.
#'
#' `expect_images_differ()` asserts that two images are visually different.
#' No diff image is saved since there's nothing to debug when images match
#' unexpectedly.
#'
#' Both expectations will skip (not fail) if the odiff binary is not available,
#' making tests portable across environments.
#'
#' @section Comparison with vdiffr:
#' odiffr expectations are designed for **pixel-based** comparison of
#' screenshots, rendered images, and bitmap files. For **SVG-based** comparison
#' of ggplot2 and grid graphics, consider using the vdiffr package instead.
#' The two approaches are complementary.
#'
#' @return Invisibly returns the comparison result (a data.frame/tibble with
#'   match, reason, diff_count, diff_percentage, etc.), allowing further
#'   inspection if needed.
#'
#' @seealso [compare_images()] for the underlying comparison function,
#'   [ignore_region()] for excluding regions from comparison.
#'
#' @export
#' @rdname expect_images
#'
#' @examples
#' \dontrun{
#' # Basic visual regression test
#' test_that("login page renders correctly", {
#'   skip_if_no_odiff()
#'
#'   expect_images_match(
#'     "screenshots/login_current.png",
#'     "screenshots/login_baseline.png"
#'   )
#' })
#'
#' # With tolerance for minor differences
#' test_that("chart renders correctly", {
#'   skip_if_no_odiff()
#'
#'   expect_images_match(
#'     "actual_chart.png",
#'     "expected_chart.png",
#'     threshold = 0.2,
#'     antialiasing = TRUE,
#'     ignore_regions = list(
#'       ignore_region(0, 0, 100, 30)  # Ignore timestamp
#'     )
#'   )
#' })
#'
#' # Assert images are different
#' test_that("button changes on hover", {
#'   skip_if_no_odiff()
#'
#'   expect_images_differ(
#'     "button_normal.png",
#'     "button_hover.png"
#'   )
#' })
#' }
expect_images_match <- function(actual,
                                expected,
                                threshold = 0.1,
                                antialiasing = FALSE,
                                fail_on_layout = TRUE,
                                ignore_regions = NULL,
                                ...,
                                info = NULL,
                                label = NULL) {
  # Check testthat availability
  check_testthat()

  # Capture labels for error messages (no rlang dependency)
  act_label <- if (!is.null(label)) label else deparse(substitute(actual))
  exp_label <- deparse(substitute(expected))

  # Skip if odiff not available
  if (!odiff_available()) {
    testthat::skip("odiff binary not available")
  }

  # Determine diff output path
  diff_dir <- get_diff_dir()
  diff_output <- NULL
  if (!is.null(diff_dir)) {
    if (!dir.exists(diff_dir)) {
      dir.create(diff_dir, recursive = TRUE)
    }
    diff_output <- generate_diff_filename(actual, expected, diff_dir)
  }

  # Run comparison (expected as img1 for intuitive "baseline vs actual" diff)
  result <- compare_images(
    img1 = expected,
    img2 = actual,
    diff_output = diff_output,
    threshold = threshold,
    antialiasing = antialiasing,
    fail_on_layout = fail_on_layout,
    ignore_regions = ignore_regions,
    ...
  )

  # Build failure message (testthat::expect requires character, not NULL)
  msg <- sprintf(
    "`%s` does not match expected `%s`.\nReason: %s",
    act_label, exp_label, result$reason
  )

  if (!is.na(result$diff_count)) {
    msg <- paste0(msg, sprintf(
      "\nDiff: %d pixels (%.2f%%)",
      result$diff_count, result$diff_percentage
    ))
  }

  if (!is.null(diff_output) && file.exists(diff_output)) {
    msg <- paste0(msg, sprintf("\nDiff image: %s", diff_output))
  }

  # Use testthat::expect() - the modern pattern
  testthat::expect(result$match, msg, info = info)

  invisible(result)
}


#' @export
#' @rdname expect_images
expect_images_differ <- function(img1,
                                 img2,
                                 threshold = 0.1,
                                 antialiasing = FALSE,
                                 ...,
                                 info = NULL,
                                 label = NULL) {
  # Check testthat availability
  check_testthat()

  # Capture labels for error messages
  lab1 <- if (!is.null(label)) label else deparse(substitute(img1))
  lab2 <- deparse(substitute(img2))

  # Skip if odiff not available
  if (!odiff_available()) {
    testthat::skip("odiff binary not available")
  }

  # No diff output needed - if they match, there's nothing to debug
  result <- compare_images(
    img1 = img1,
    img2 = img2,
    diff_output = NULL,
    threshold = threshold,
    antialiasing = antialiasing,
    ...
  )

  # Build failure message (testthat::expect requires character, not NULL)
  msg <- sprintf("`%s` unexpectedly matches `%s`.", lab1, lab2)

  testthat::expect(!result$match, msg, info = info)

  invisible(result)
}


# Internal: Check testthat availability
check_testthat <- function() {
  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop(
      "testthat is required to use odiffr expectations. ",
      "Install it with install.packages('testthat').",
      call. = FALSE
    )
  }
}


# Internal: Get diff output directory
get_diff_dir <- function() {
  # Check if saving is disabled
  if (!getOption("odiffr.save_diff", TRUE)) {
    return(NULL)
  }

  # Use custom dir if set
  custom <- getOption("odiffr.diff_dir", NULL)
  if (!is.null(custom)) {
    return(custom)
  }

  # Auto-detect: tests/testthat/_odiffr/
  # Use testthat's test_path() if available and we're in a test context
  if (requireNamespace("testthat", quietly = TRUE)) {
    path <- tryCatch(
      testthat::test_path("_odiffr"),
      error = function(e) NULL
    )
    if (!is.null(path)) {
      return(path)
    }
  }

  # Fallback for non-testthat contexts
  file.path("tests", "testthat", "_odiffr")
}


# Internal: Generate unique diff filename
generate_diff_filename <- function(actual, expected, diff_dir) {
  # Build descriptive base name from input paths when available
  if (is.character(actual) && is.character(expected)) {
    a <- tools::file_path_sans_ext(basename(actual))
    e <- tools::file_path_sans_ext(basename(expected))
    base <- paste0(a, "_vs_", e)
  } else {
    base <- "odiffr_diff"
  }

  # tempfile() guarantees uniqueness
  tempfile(pattern = paste0(base, "_"), tmpdir = diff_dir, fileext = ".png")
}
