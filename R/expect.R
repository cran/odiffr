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
#' Diff file names are deterministic, so re-running a failing test overwrites
#' the previous diff rather than accumulating files: for file paths the name
#' is `<actual>_vs_<expected>.png` (basenames without extension); for
#' magick-image objects it is built from the `label`/expressions passed (e.g.
#' `img_new_vs_img_old.png`). If two different comparisons in the same
#' session would produce the same name (e.g. identical basenames in different
#' directories), the parent directory names, or else a numeric suffix, are
#' added. A stale diff image left by a previous failing run is removed when
#' the expectation is run again (odiff writes no diff image when the images
#' match).
#'
#' `expect_images_differ()` asserts that two images are visually different.
#' No diff image is saved since there's nothing to debug when images match
#' unexpectedly.
#'
#' If odiff cannot compare the images (`reason == "error"`, e.g. a file that
#' cannot be loaded or has an unsupported format), both expectations fail and
#' the failure message includes odiff's error message. In particular, an
#' error does not count as the images differing for `expect_images_differ()`.
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
#'   match, reason, diff_count, diff_percentage, error, etc.), allowing further
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
  act_label <- if (!is.null(label)) label else paste(deparse(substitute(actual)), collapse = " ")
  exp_label <- paste(deparse(substitute(expected)), collapse = " ")

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
    diff_output <- generate_diff_filename(
      actual, expected, diff_dir,
      act_label = act_label, exp_label = exp_label
    )
    # Remove a stale diff from a previous failing run: odiff writes no diff
    # image when the images match (or when the comparison errors)
    if (file.exists(diff_output)) {
      unlink(diff_output)
    }
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

  err <- .result_error(result)
  if (!is.na(err)) {
    msg <- paste0(msg, sprintf("\nError: %s", err))
  }

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
  lab1 <- if (!is.null(label)) label else paste(deparse(substitute(img1)), collapse = " ")
  lab2 <- paste(deparse(substitute(img2)), collapse = " ")

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
  # A comparison error is not evidence that the images differ: fail with
  # odiff's error message instead of passing.
  if (identical(result$reason, "error")) {
    err <- .result_error(result)
    msg <- sprintf(
      "Could not compare `%s` and `%s`.\nReason: error\nError: %s",
      lab1, lab2, if (is.na(err)) "unknown error" else err
    )
    testthat::expect(FALSE, msg, info = info)
    return(invisible(result))
  }

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

  # During tests: tests/testthat/_odiffr/
  if (requireNamespace("testthat", quietly = TRUE) && testthat::is_testing()) {
    return(testthat::test_path("_odiffr"))
  }

  # From a package root: tests/testthat/_odiffr/; anywhere else use the
  # session temp directory rather than writing into the working directory
  if (dir.exists(file.path("tests", "testthat"))) {
    return(file.path("tests", "testthat", "_odiffr"))
  }
  file.path(tempdir(), "odiffr-diffs")
}


# Internal: error text from a compare_images() result (NA if none). Read
# defensively so results without an `error` column still work.
.result_error <- function(result) {
  err <- result$error
  if (is.null(err) || length(err) == 0) {
    return(NA_character_)
  }
  as.character(err[[1]])
}


# Internal: registry of diff filenames handed out in this session, so two
# different expectations writing to the same diff_dir do not collide.
# Maps "<diff_dir>\r<filename>" -> key identifying the comparison.
.diff_registry <- new.env(parent = emptyenv())


# Internal: sanitize a string for use in a file name
.sanitize_filename <- function(x, max_len = 60L) {
  x <- gsub("[^A-Za-z0-9._-]+", "_", x)
  x <- gsub("^[_.]+|_+$", "", x)
  if (!nzchar(x)) x <- "img"
  substr(x, 1L, max_len)
}


# Internal: Generate a deterministic diff filename
#
# The name is derived from the inputs so that re-running a failing
# expectation overwrites the previous diff instead of accumulating files:
# - file paths: "<actual>_vs_<expected>.png" (basenames without extension)
# - magick objects / other inputs: built from the expression labels (e.g.
#   "img_actual_vs_img_expected.png"), or "odiffr_diff.png" if no labels.
# If that name was already used in this session for a *different*
# comparison in the same diff_dir (e.g. same basenames in different
# directories), the parent directory names are added, and failing that a
# numeric suffix ("_2", "_3", ...).
generate_diff_filename <- function(actual, expected, diff_dir,
                                   act_label = NULL, exp_label = NULL) {
  is_path <- function(x) is.character(x) && length(x) == 1 && !is.na(x)

  if (is_path(actual) && is_path(expected)) {
    a <- .sanitize_filename(tools::file_path_sans_ext(basename(actual)))
    e <- .sanitize_filename(tools::file_path_sans_ext(basename(expected)))
    candidates <- c(
      paste0(a, "_vs_", e),
      paste0(.sanitize_filename(basename(dirname(actual))), "_", a, "_vs_",
             .sanitize_filename(basename(dirname(expected))), "_", e)
    )
    key <- paste(
      "path",
      normalizePath(actual, winslash = "/", mustWork = FALSE),
      normalizePath(expected, winslash = "/", mustWork = FALSE),
      sep = "\r"
    )
  } else {
    lab_part <- function(x, lab) {
      if (is_path(x)) {
        tools::file_path_sans_ext(basename(x))
      } else if (!is.null(lab) && length(lab) > 0) {
        paste(lab, collapse = "")
      } else {
        NA_character_
      }
    }
    # Identity of each side: the full path for files, the label otherwise
    id_part <- function(x, part) {
      if (is_path(x)) normalizePath(x, winslash = "/", mustWork = FALSE) else part
    }
    # Name qualified by the parent directory, for files with the same basename
    dir_part <- function(x, part) {
      if (is_path(x)) {
        paste0(.sanitize_filename(basename(dirname(x))), "_",
               .sanitize_filename(part))
      } else {
        .sanitize_filename(part)
      }
    }
    a <- lab_part(actual, act_label)
    e <- lab_part(expected, exp_label)
    if (is.na(a) || is.na(e)) {
      candidates <- "odiffr_diff"
    } else {
      candidates <- unique(c(
        paste0(.sanitize_filename(a), "_vs_", .sanitize_filename(e)),
        paste0(dir_part(actual, a), "_vs_", dir_part(expected, e))
      ))
    }
    key <- paste("other", id_part(actual, a), id_part(expected, e),
                 sep = "\r")
  }

  # Stable identity of the output directory
  dir_id <- normalizePath(diff_dir, winslash = "/", mustWork = FALSE)
  claim <- function(name) {
    reg_key <- paste(dir_id, name, sep = "\r")
    owner <- .diff_registry[[reg_key]]
    if (is.null(owner) || identical(owner, key)) {
      assign(reg_key, key, envir = .diff_registry)
      TRUE
    } else {
      FALSE
    }
  }

  for (cand in unique(candidates)) {
    if (claim(cand)) {
      return(file.path(diff_dir, paste0(cand, ".png")))
    }
  }
  i <- 2L
  repeat {
    cand <- paste0(candidates[[1]], "_", i)
    if (claim(cand)) {
      return(file.path(diff_dir, paste0(cand, ".png")))
    }
    i <- i + 1L
  }
}

