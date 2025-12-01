#' Run odiff Command (Low-Level)
#'
#' Direct wrapper around the odiff CLI with zero external dependencies.
#' Returns a structured list with comparison results.
#'
#' @param img1 Character; path to the first (baseline) image file.
#' @param img2 Character; path to the second (comparison) image file.
#' @param diff_output Character or `NULL`; optional path for the diff output
#'   image. Must have `.png` extension. If `NULL`, no diff image is created.
#' @param threshold Numeric; color difference threshold between 0.0 and 1.0.
#'   Lower values are more precise. Default is 0.1.
#' @param antialiasing Logical; if `TRUE`, ignore antialiased pixels.
#'   Default is `FALSE`.
#' @param fail_on_layout Logical; if `TRUE`, fail immediately if images have
#'   different dimensions. Default is `FALSE`.
#' @param diff_mask Logical; if `TRUE`, output only the changed pixels in the
#'   diff image. Default is `FALSE`.
#' @param diff_overlay Logical or numeric; if `TRUE` or a number between 0 and
#'   1, add a white shaded overlay to the diff image for easier reading.
#'   Default is `NULL` (no overlay).
#' @param diff_color Character; hex color for highlighting differences
#'   (e.g., `"#FF0000"`). Default is `NULL` (uses odiff default, red).
#' @param diff_lines Logical; if `TRUE`, include line numbers containing
#'   different pixels in the output. Default is `FALSE`.
#' @param reduce_ram Logical; if `TRUE`, use less memory but run slower.
#'   Useful for very large images. Default is `FALSE`.
#' @param ignore_regions A list of regions to ignore during comparison. Each
#'   region should be a list with `x1`, `y1`, `x2`, `y2` components, or use
#'   [ignore_region()] to create them. Can also be a data.frame with these
#'   columns.
#' @param timeout Numeric; timeout in seconds for the odiff process.
#'   Default is 60.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{match}{Logical; `TRUE` if images match, `FALSE` otherwise.}
#'     \item{reason}{Character; one of `"match"`, `"pixel-diff"`,
#'       `"layout-diff"`, or `"error"`.}
#'     \item{diff_count}{Integer; number of different pixels, or `NA`.}
#'     \item{diff_percentage}{Numeric; percentage of different pixels, or `NA`.}
#'     \item{diff_lines}{Integer vector of line numbers with differences,
#'       or `NULL`.}
#'     \item{exit_code}{Integer; odiff exit code (0 = match, 21 = layout diff,
#'       22 = pixel diff).}
#'     \item{stdout}{Character; raw stdout output.}
#'     \item{stderr}{Character; raw stderr output.}
#'     \item{img1}{Character; path to first image.}
#'     \item{img2}{Character; path to second image.}
#'     \item{diff_output}{Character or `NULL`; path to diff image if created.}
#'     \item{duration}{Numeric; time elapsed in seconds.}
#'   }
#'
#' @seealso [compare_images()] for a higher-level interface,
#'   [ignore_region()] for creating ignore regions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic comparison
#' result <- odiff_run("baseline.png", "current.png")
#' result$match
#'
#' # With diff output
#' result <- odiff_run("baseline.png", "current.png", "diff.png")
#'
#' # With threshold and antialiasing
#' result <- odiff_run("baseline.png", "current.png",
#'                     threshold = 0.05, antialiasing = TRUE)
#'
#' # Ignoring specific regions
#' result <- odiff_run("baseline.png", "current.png",
#'                     ignore_regions = list(
#'                       ignore_region(10, 10, 100, 50),
#'                       ignore_region(200, 200, 300, 300)
#'                     ))
#' }
odiff_run <- function(img1, img2,
                      diff_output = NULL,
                      threshold = 0.1,
                      antialiasing = FALSE,
                      fail_on_layout = FALSE,
                      diff_mask = FALSE,
                      diff_overlay = NULL,
                      diff_color = NULL,
                      diff_lines = FALSE,
                      reduce_ram = FALSE,
                      ignore_regions = NULL,
                      timeout = 60) {
  # Find odiff binary
  odiff_path <- find_odiff()

  # Validate inputs
  img1 <- .validate_image_path(img1, "img1")
  img2 <- .validate_image_path(img2, "img2")
  diff_output <- .validate_diff_output(diff_output)

  # Validate threshold
  if (!is.null(threshold)) {
    if (!is.numeric(threshold) || length(threshold) != 1 ||
        threshold < 0 || threshold > 1) {
      stop("threshold must be a single number between 0 and 1.", call. = FALSE)
    }
  }

  # Build arguments
  args <- .build_args(
    img1 = img1,
    img2 = img2,
    diff_output = diff_output,
    threshold = threshold,
    antialiasing = antialiasing,
    fail_on_layout = fail_on_layout,
    diff_mask = diff_mask,
    diff_overlay = diff_overlay,
    diff_color = diff_color,
    diff_lines = diff_lines,
    reduce_ram = reduce_ram,
    ignore_regions = ignore_regions
  )

  # Run odiff
  start_time <- Sys.time()

  result <- tryCatch(
    {
      # suppressWarnings: odiff uses non-zero exit codes for expected outcomes
      # (21 = layout diff, 22 = pixel diff), which system2 warns about
      output <- suppressWarnings(system2(
        command = odiff_path,
        args = args,
        stdout = TRUE,
        stderr = TRUE,
        timeout = timeout
      ))

      # Get exit code from attribute
      exit_code <- attr(output, "status")
      if (is.null(exit_code)) {
        exit_code <- 0L
      }

      # Separate stdout and stderr
      stderr_attr <- attr(output, "errmsg")
      stdout <- output
      stderr <- if (!is.null(stderr_attr)) stderr_attr else character()

      list(
        stdout = stdout,
        stderr = stderr,
        exit_code = as.integer(exit_code)
      )
    },
    error = function(e) {
      list(
        stdout = character(),
        stderr = e$message,
        exit_code = 1L
      )
    }
  )

  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Parse output
  parsed <- .parse_output(
    stdout = result$stdout,
    stderr = result$stderr,
    exit_code = result$exit_code,
    diff_lines_requested = diff_lines
  )

  # Add additional info
  parsed$img1 <- img1
  parsed$img2 <- img2
  parsed$diff_output <- diff_output
  parsed$duration <- duration

  # Check if diff file was created
  if (!is.null(diff_output) && !file.exists(diff_output)) {
    parsed$diff_output <- NULL
  }

  structure(parsed, class = c("odiff_result", "list"))
}

#' @export
print.odiff_result <- function(x, ...) {
  cat("odiff comparison result\n")
  cat("-----------------------\n")
  cat("Match:     ", if (x$match) "YES" else "NO", "\n")
  cat("Reason:    ", x$reason, "\n")

  if (!is.na(x$diff_count)) {
    cat("Diff count:", x$diff_count, "pixels\n")
  }
  if (!is.na(x$diff_percentage)) {
    cat("Diff %:    ", sprintf("%.4f%%", x$diff_percentage), "\n")
  }
  if (!is.null(x$diff_output)) {
    cat("Diff file: ", x$diff_output, "\n")
  }
  cat("Duration:  ", sprintf("%.3f sec", x$duration), "\n")

  invisible(x)
}

#' Create an Ignore Region
#'
#' Helper function to create a region specification for use with
#' [odiff_run()] and [compare_images()].
#'
#' @param x1 Integer; x-coordinate of the top-left corner.
#' @param y1 Integer; y-coordinate of the top-left corner.
#' @param x2 Integer; x-coordinate of the bottom-right corner.
#' @param y2 Integer; y-coordinate of the bottom-right corner.
#'
#' @return A list with components `x1`, `y1`, `x2`, `y2`.
#'
#' @export
#'
#' @examples
#' # Create a region to ignore
#' region <- ignore_region(10, 10, 100, 50)
#'
#' # Use with odiff_run
#' \dontrun{
#' result <- odiff_run("img1.png", "img2.png",
#'                     ignore_regions = list(region))
#' }
ignore_region <- function(x1, y1, x2, y2) {
  stopifnot(
    is.numeric(x1), length(x1) == 1,
    is.numeric(y1), length(y1) == 1,
    is.numeric(x2), length(x2) == 1,
    is.numeric(y2), length(y2) == 1,
    x2 >= x1,
    y2 >= y1
  )

  structure(
    list(
      x1 = as.integer(x1),
      y1 = as.integer(y1),
      x2 = as.integer(x2),
      y2 = as.integer(y2)
    ),
    class = c("odiff_region", "list")
  )
}

#' @export
print.odiff_region <- function(x, ...) {
  cat(sprintf("odiff ignore region: (%d,%d) to (%d,%d)\n",
              x$x1, x$y1, x$x2, x$y2))
  invisible(x)
}
