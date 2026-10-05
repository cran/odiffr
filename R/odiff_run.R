#' Run odiff Command (Low-Level)
#'
#' Direct wrapper around the odiff CLI with zero external dependencies.
#' Returns a structured list with comparison results.
#'
#' @param img1 Character; path to the first (baseline) image file.
#' @param img2 Character; path to the second (comparison) image file.
#' @param diff_output Character or `NULL`; optional path for the diff output
#'   image. odiff only writes PNG: a path with a different extension has it
#'   replaced by `.png`, and a path with no extension gets `.png` appended
#'   (both with a warning). If `NULL`, no diff image is created. No diff image
#'   is written when the images match.
#' @param threshold Numeric; colour difference threshold between 0.0 and 1.0.
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
#' @param diff_color Character; hex color for highlighting differences, in the
#'   form `"#RRGGBB"` or `"RRGGBB"` (e.g., `"#FF0000"`). Default is `NULL`
#'   (uses odiff default, red).
#' @param diff_lines Logical; if `TRUE`, include line numbers containing
#'   different pixels in the output. Default is `FALSE`.
#' @param reduce_ram Logical; if `TRUE`, use less memory but run slower.
#'   Useful for very large images. Default is `FALSE`.
#' @param enable_asm Logical; if `TRUE`, pass `--enable-asm` to the underlying
#'   `odiff` binary to enable assembly-optimised code paths (e.g. AVX-512) on
#'   supported CPUs. Requires odiff >= 4.1.1. Default is `FALSE`.
#' @param ignore_regions A list of regions to ignore during comparison. Each
#'   region should be a list with `x1`, `y1`, `x2`, `y2` components, or use
#'   [ignore_region()] to create them. Can also be a data.frame with these
#'   columns.
#' @param timeout Numeric; timeout in seconds for the odiff process.
#'   Default is 60. Positive values below one second are rounded up to one
#'   second (the resolution of [system2()]); `0` or `Inf` means no timeout.
#'   If the timeout is reached, the result has `reason = "error"` and an
#'   `error` message.
#' @param diff_cols Logical; if `TRUE`, include column numbers containing
#'   different pixels in the output (`--output-diff-cols`). Requires
#'   odiff >= 4.5.0; ignored with a warning for older versions.
#'   Default is `FALSE`.
#'
#' @return A list with the following components:
#'   \describe{
#'     \item{match}{Logical; `TRUE` if images match, `FALSE` otherwise.}
#'     \item{reason}{Character; one of `"match"`, `"pixel-diff"`,
#'       `"layout-diff"`, or `"error"`.}
#'     \item{diff_count}{Integer; number of different pixels (`0` for a
#'       match), or `NA` if unknown (layout difference or error).}
#'     \item{diff_percentage}{Numeric; percentage of different pixels (`0` for
#'       a match), or `NA` if unknown.}
#'     \item{diff_lines}{Integer vector of line numbers with differences,
#'       or `NULL`.}
#'     \item{exit_code}{Integer; odiff exit code (0 = match, 21 = layout diff,
#'       22 = pixel diff, other values = error).}
#'     \item{stdout}{Character; raw stdout output (odiff's parsable output).}
#'     \item{stderr}{Character; raw stderr output.}
#'     \item{error}{Character; `NA` if no error occurred, otherwise the error
#'       message reported by odiff (or by odiffr, e.g. on timeout).}
#'     \item{img1}{Character; path to first image.}
#'     \item{img2}{Character; path to second image.}
#'     \item{diff_output}{Character or `NULL`; path to diff image if created.}
#'     \item{duration}{Numeric; time elapsed in seconds.}
#'     \item{diff_cols}{Integer vector of column numbers with differences, or
#'       `NULL`. Only present when `diff_cols = TRUE`.}
#'     \item{params}{Named list of the effective comparison parameters
#'       (after version guards): `threshold`, `antialiasing`,
#'       `fail_on_layout`, `ignore_regions` (formatted as
#'       `"x1:y1-x2:y2,..."`, or `NA`), `diff_mask`, `diff_overlay` (`NA` if
#'       unset), `diff_color` (`NA` if unset), `reduce_ram` and `enable_asm`.
#'       Used by [audit_record()].}
#'   }
#'
#' @details
#' The `enable_asm` option is an advanced, platform-specific optimisation flag.
#' For odiff < 4.1.1, odiffr ignores `enable_asm` with a warning. Behaviour on
#' unsupported CPUs is determined by odiff itself.
#'
#' odiff is always invoked with `--parsable-stdout`, and its machine-readable
#' output is parsed to fill `diff_count`, `diff_percentage`, `diff_lines` and
#' `diff_cols`.
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
                      enable_asm = FALSE,
                      ignore_regions = NULL,
                      timeout = 60,
                      diff_cols = FALSE) {
  # Find odiff binary
  odiff_path <- find_odiff()

  # Validate inputs
  img1 <- .validate_image_path(img1, "img1")
  img2 <- .validate_image_path(img2, "img2")
  .validate_threshold(threshold)
  .validate_diff_color(diff_color)
  .validate_diff_overlay(diff_overlay)
  timeout_secs <- .validate_timeout(timeout)
  diff_output <- .validate_diff_output(diff_output)

  # Version guard for enable_asm
  if (isTRUE(enable_asm)) {
    ver <- odiff_version()
    if (is.na(ver) || utils::compareVersion(ver, "4.1.1") < 0) {
      warning("enable_asm = TRUE requires odiff >= 4.1.1; ",
              "flag will be ignored for older versions.", call. = FALSE)
      enable_asm <- FALSE
    }
  }

  # Version guard for diff_cols
  if (isTRUE(diff_cols)) {
    ver <- odiff_version()
    if (is.na(ver) || utils::compareVersion(ver, "4.5.0") < 0) {
      warning("diff_cols = TRUE requires odiff >= 4.5.0; ",
              "flag will be ignored for older versions.", call. = FALSE)
      diff_cols <- FALSE
    }
  }

  # Build arguments (paths are shell-quoted)
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
    enable_asm = enable_asm,
    ignore_regions = ignore_regions,
    diff_cols = diff_cols
  )

  # odiff writes no diff image for matching images, layout differences or
  # errors, so remove one left over from a previous run at the same path
  if (!is.null(diff_output) && file.exists(diff_output)) {
    unlink(diff_output)
  }

  # Run odiff
  start_time <- Sys.time()
  result <- .run_odiff(odiff_path, args, timeout_secs)
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

  # Parse output
  parsed <- .parse_output(
    stdout = result$stdout,
    stderr = result$stderr,
    exit_code = result$exit_code,
    diff_lines_requested = diff_lines,
    diff_cols_requested = diff_cols
  )
  if (!is.na(result$error)) {
    parsed$error <- result$error
  }

  # odiff < 4.5.0 reports differently sized images as a match unless
  # --fail-on-layout is given; detect that case from the image headers
  if (identical(parsed$reason, "match") && !isTRUE(fail_on_layout)) {
    ver <- odiff_version()
    if (is.na(ver) || utils::compareVersion(ver, "4.5.0") < 0) {
      d1 <- .image_dimensions(img1)
      d2 <- .image_dimensions(img2)
      if (!is.null(d1) && !is.null(d2) && !identical(d1, d2)) {
        parsed$match <- FALSE
        parsed$reason <- "layout-diff"
        parsed$diff_count <- NA_integer_
        parsed$diff_percentage <- NA_real_
      }
    }
  }

  # Add additional info
  parsed$img1 <- img1
  parsed$img2 <- img2
  parsed$diff_output <- diff_output
  parsed$duration <- duration

  # Check if diff file was created
  if (!is.null(diff_output) && !file.exists(diff_output)) {
    parsed$diff_output <- NULL
  }

  # Effective comparison parameters (after version guards), for audit_record()
  parsed$params <- list(
    threshold = threshold,
    antialiasing = isTRUE(antialiasing),
    fail_on_layout = isTRUE(fail_on_layout),
    ignore_regions = if (is.null(ignore_regions) ||
                         length(ignore_regions) == 0) {
      NA_character_
    } else {
      .format_regions(ignore_regions)
    },
    diff_mask = isTRUE(diff_mask),
    diff_overlay = if (is.null(diff_overlay)) NA else diff_overlay,
    diff_color = if (is.null(diff_color)) NA_character_ else diff_color,
    reduce_ram = isTRUE(reduce_ram),
    enable_asm = isTRUE(enable_asm)
  )

  structure(parsed, class = c("odiff_result", "list"))
}

# Run the odiff binary, capturing stdout and stderr separately.
# Returns list(stdout, stderr, exit_code, error) where `error` is
# NA_character_ unless odiffr itself detected a failure (timeout, failure to
# launch).
.run_odiff <- function(odiff_path, args, timeout_secs = 0) {
  stderr_file <- tempfile("odiffr_stderr_")
  on.exit(unlink(stderr_file), add = TRUE)

  read_stderr <- function() {
    if (file.exists(stderr_file)) {
      readLines(stderr_file, warn = FALSE)
    } else {
      character()
    }
  }

  tryCatch(
    {
      # Muffle warnings: odiff uses non-zero exit codes for expected outcomes
      # (21 = layout diff, 22 = pixel diff), which system2 warns about
      output <- withCallingHandlers(
        system2(
          command = odiff_path,
          args = args,
          stdout = TRUE,
          stderr = stderr_file,
          timeout = timeout_secs
        ),
        warning = function(w) invokeRestart("muffleWarning")
      )

      exit_code <- attr(output, "status")
      exit_code <- if (is.null(exit_code)) 0L else as.integer(exit_code)
      stdout <- as.character(output)
      attributes(stdout) <- NULL

      error <- NA_character_
      if (timeout_secs > 0 && identical(exit_code, 124L)) {
        error <- sprintf("odiff timed out after %s second%s",
                         timeout_secs, if (timeout_secs == 1) "" else "s")
      }

      list(
        stdout = stdout,
        stderr = read_stderr(),
        exit_code = exit_code,
        error = error
      )
    },
    error = function(e) {
      stderr <- read_stderr()
      list(
        stdout = character(),
        stderr = if (length(stderr)) stderr else conditionMessage(e),
        exit_code = 1L,
        error = paste0("Failed to run odiff: ", conditionMessage(e))
      )
    }
  )
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
  if (!is.null(x$error) && !is.na(x$error)) {
    cat("Error:     ", x$error, "\n")
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
