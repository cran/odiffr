#' Snapshot Testing for Images
#'
#' `expect_snapshot_image()` is a testthat snapshot expectation for images
#' that compares the image with its stored snapshot using odiff, rather than
#' requiring the files to be byte-for-byte identical. Snapshots are managed
#' with testthat's usual tools ([testthat::snapshot_review()],
#' [testthat::snapshot_accept()]).
#'
#' @param x The image to snapshot: a path to an image file (PNG; other
#'   formats are converted to PNG with magick), a magick-image
#'   object, a ggplot object, a function of no arguments that draws a plot
#'   (base or grid graphics) or returns a ggplot, lattice or grid object when
#'   called, or a recorded plot
#'   ([grDevices::recordPlot()]). Plots are rendered to PNG using
#'   `plot_options`.
#' @param name Snapshot file name. A `.png` extension is added if missing.
#'   If `NULL` (the default), the name is the base name of `x` when `x` is a
#'   file path, or the variable name when `x` is a simple variable (e.g.
#'   `expect_snapshot_image(p)` uses `"p.png"`). For any other expression
#'   (e.g. an inline function), `name` must be supplied. Names must be unique
#'   within a test file.
#' @param threshold Numeric; colour difference threshold between 0.0 and 1.0.
#'   Default is 0.1 (or the value from `preset`).
#' @param antialiasing Logical; if `TRUE`, ignore antialiased pixels.
#'   Default is `FALSE` (or the value from `preset`).
#' @param ignore_regions List of regions to ignore during comparison.
#'   Use [ignore_region()] to create regions, or pass a data.frame with
#'   columns `x1`, `y1`, `x2`, `y2`.
#' @param fail_on_layout Logical; if `TRUE` (the default), images with
#'   different dimensions do not match.
#' @param plot_options Options for rendering plot inputs, created with
#'   [plot_options()]. `NULL` uses the defaults of [plot_options()].
#' @param variant If not `NULL`, the snapshot is stored in a
#'   variant-specific subdirectory (`_snaps/<variant>/<file>/`). See the
#'   section on platform differences.
#' @param ... Additional arguments passed to [odiff_run()] (via
#'   [compare_file_odiff()]).
#' @param preset Optional name of a comparison preset, see [odiff_preset()]:
#'   `"strict"`, `"default"`, `"screenshot"` or `"cross_platform"`. The
#'   preset supplies `threshold` and `antialiasing`; values given explicitly
#'   for those arguments take precedence. `NULL` (the default) uses the
#'   argument defaults.
#' @param diff_dir Where to write a diff image when the comparison fails.
#'   See the section "Diff images" in [compare_file_odiff()]. `NULL` (the
#'   default, unless the `odiffr.snapshot_diff_dir` option is set) uses
#'   `tests/testthat/_odiffr/`; `FALSE` disables diff images.
#'
#' @details
#' On the first run, the image is saved as the snapshot
#' `tests/testthat/_snaps/<test-file>/<name>.png` and testthat emits an
#' "Adding new file snapshot" warning. On subsequent runs the image is
#' compared with the snapshot using odiff and the expectation fails if they
#' differ (beyond `threshold`, after `ignore_regions` and, optionally,
#' antialiasing are taken into account). On failure, the new image is saved
#' next to the snapshot as `<name>.new.png`, and a diff image highlighting
#' the changed pixels is written to `tests/testthat/_odiffr/` (outside
#' `_snaps/`, see [compare_file_odiff()]); a message gives its path.
#'
#' Like [testthat::expect_snapshot_file()], on which it is built:
#' \itemize{
#'   \item It requires the third edition of testthat.
#'   \item It is skipped on CRAN (when `NOT_CRAN` is not `"true"`), because
#'     snapshots are not shipped reliably and image rendering differs
#'     between machines.
#'   \item Snapshots must be committed to version control: depending on
#'     the testthat version, a missing snapshot may be reported as a failure
#'     rather than created when running on CI (the `CI` environment variable
#'     is `"true"`).
#' }
#' The expectation is skipped if the odiff binary is not available.
#'
#' If odiff cannot compare the images (e.g. the stored snapshot is not a
#' valid image), a warning with odiff's error message is given and the
#' expectation fails, so the new image can still be reviewed and accepted.
#'
#' @section Reviewing changes:
#' When a snapshot changes, run [testthat::snapshot_review()] to compare the
#' old and new images side by side in an interactive viewer, then accept the
#' new image with [testthat::snapshot_accept()] (or from the viewer) if the
#' change is intended. Unwanted `.new.png` files are removed on the next
#' successful run.
#'
#' @section Platform differences:
#' Rendered plots can differ slightly between operating systems, graphics
#' devices and installed fonts. To reduce spurious failures:
#' \itemize{
#'   \item Install the ragg package: plots are then rendered with
#'     [ragg::agg_png()], which gives consistent output across platforms.
#'   \item Use a tolerant comparison (`preset = "screenshot"` or
#'     `preset = "cross_platform"`, or `threshold`, `antialiasing = TRUE`,
#'     `ignore_regions`).
#'   \item Store separate snapshots per platform with `variant`, e.g.
#'     `variant = Sys.info()[["sysname"]]`.
#' }
#'
#' @section Comparison with vdiffr:
#' vdiffr snapshots plots as SVG and compares the SVG text. odiffr compares
#' rendered pixels, which also works for images that are not plots (e.g.
#' screenshots or magick images) and tolerates small rendering differences.
#'
#' @return Invisibly returns `NULL`, like other testthat snapshot
#'   expectations.
#'
#' @seealso [compare_file_odiff()] for the comparison function,
#'   [expect_images_match()] for comparing against a baseline file that you
#'   manage yourself, [testthat::expect_snapshot_file()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # tests/testthat/test-plots.R
#' test_that("scatter plot is stable", {
#'   p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'     ggplot2::geom_point()
#'   expect_snapshot_image(p)  # snapshot: _snaps/plots/p.png
#' })
#'
#' test_that("base graphics histogram is stable", {
#'   expect_snapshot_image(
#'     function() hist(mtcars$mpg),
#'     name = "mpg-histogram",
#'     plot_options = plot_options(width = 5, height = 4),
#'     antialiasing = TRUE
#'   )
#' })
#'
#' test_that("screenshot is stable on each OS", {
#'   expect_snapshot_image(
#'     "output/screenshot.png",
#'     preset = "screenshot",
#'     ignore_regions = list(ignore_region(0, 0, 200, 40)),  # timestamp
#'     variant = Sys.info()[["sysname"]]
#'   )
#' })
#'
#' # After an intended change, review and accept the new snapshots:
#' testthat::snapshot_review("plots")
#' testthat::snapshot_accept("plots")
#' }
expect_snapshot_image <- function(x,
                                  name = NULL,
                                  threshold = 0.1,
                                  antialiasing = FALSE,
                                  ignore_regions = NULL,
                                  fail_on_layout = TRUE,
                                  plot_options = NULL,
                                  variant = NULL,
                                  ...,
                                  preset = NULL,
                                  diff_dir = getOption("odiffr.snapshot_diff_dir")) {
  expr_label <- deparse(substitute(x))
  check_testthat()

  name <- .snapshot_image_name(x, name, expr_label)

  # Only pass threshold/antialiasing when given, so a preset can fill them
  args <- list(
    ignore_regions = ignore_regions,
    fail_on_layout = fail_on_layout,
    preset = preset,
    diff_dir = diff_dir
  )
  if (!missing(threshold)) args$threshold <- threshold
  if (!missing(antialiasing)) args$antialiasing <- antialiasing
  compare <- do.call(compare_file_odiff, c(args, list(...)))

  # Skip if odiff not available
  if (!odiff_available()) {
    testthat::skip("odiff binary not available")
  }

  path <- .snapshot_image_file(x, plot_options = plot_options)
  on.exit(unlink(path), add = TRUE)

  testthat::expect_snapshot_file(
    path,
    name = name,
    compare = compare,
    variant = variant
  )

  invisible(NULL)
}


#' Compare Files with odiff (for testthat and shinytest2 Snapshots)
#'
#' A function factory that returns a comparison function suitable for the
#' `compare` argument of [testthat::expect_snapshot_file()] and of
#' `shinytest2::AppDriver$expect_screenshot()`. The returned function takes
#' the paths of the old (snapshot) and new image files and returns `TRUE` if
#' odiff considers them a match. When they do not match, it writes a diff
#' image highlighting the changed pixels and reports where it is.
#' [expect_snapshot_image()] uses it internally; use it directly when you
#' write image files yourself or take screenshots with shinytest2.
#'
#' @inheritParams expect_snapshot_image
#' @param ... Additional arguments passed to [odiff_run()].
#' @param preset Optional name of a comparison preset, see [odiff_preset()]:
#'   `"strict"`, `"default"`, `"screenshot"` or `"cross_platform"`. The
#'   preset supplies `threshold` and `antialiasing`; values given explicitly
#'   for those arguments take precedence. `NULL` (the default) uses the
#'   argument defaults.
#' @param diff_dir Directory for diff images of failed comparisons. `NULL`
#'   (the default, unless the `odiffr.snapshot_diff_dir` option is set)
#'   chooses a directory automatically, see "Diff images". `FALSE` disables
#'   diff images. The directory is resolved each time the comparison runs.
#'
#' @section Diff images:
#' testthat removes unrecognised files from `tests/testthat/_snaps/`, so diff
#' images are written elsewhere. With `diff_dir = NULL` the directory is,
#' in order of preference:
#' \enumerate{
#'   \item the `odiffr.diff_dir` option (shared with [expect_images_match()]);
#'   \item `tests/testthat/_odiffr/` when running tests (or when called from
#'     a package root that has a `tests/testthat` directory);
#'   \item a directory in [tempdir()] otherwise.
#' }
#' Setting `options(odiffr.save_diff = FALSE)` disables automatic diff images
#' (an explicit `diff_dir` still wins).
#'
#' Inside that directory, the layout below `_snaps/` is mirrored and the file
#' is named after the snapshot: the diff for
#' `_snaps/<variant>/<test-file>/<name>.png` is
#' `<diff_dir>/<variant>/<test-file>/<name>_diff.png`. Re-running a failing
#' test overwrites the diff; a passing comparison removes a stale one. Add
#' `tests/testthat/_odiffr/` to `.gitignore` and `.Rbuildignore`.
#'
#' When a comparison fails, a message (not a warning, so it does not add a
#' warning to the test results) such as
#' `odiff: 1.26% pixels differ (126 px) in 'plot.png'; diff image: <path>`
#' is shown.
#'
#' @return A function with arguments `old` and `new` (file paths) that
#'   returns a single `TRUE` or `FALSE`. If odiff cannot compare the files
#'   (`reason == "error"`), the function gives a warning with odiff's error
#'   message and returns `FALSE`.
#'
#' @seealso [expect_snapshot_image()], [odiff_preset()],
#'   [snapshot_report()] for reviewing failed snapshots on CI.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' test_that("exported chart is stable", {
#'   path <- tempfile(fileext = ".png")
#'   save_my_chart(path)
#'   testthat::expect_snapshot_file(
#'     path,
#'     name = "chart.png",
#'     compare = compare_file_odiff(threshold = 0.2, antialiasing = TRUE)
#'   )
#' })
#'
#' # shinytest2: tolerate anti-aliasing noise in browser screenshots
#' test_that("app looks right", {
#'   app <- shinytest2::AppDriver$new()
#'   app$expect_screenshot(compare = compare_file_odiff(preset = "screenshot"))
#' })
#' }
compare_file_odiff <- function(threshold = 0.1,
                               antialiasing = FALSE,
                               ignore_regions = NULL,
                               fail_on_layout = TRUE,
                               ...,
                               preset = NULL,
                               diff_dir = getOption("odiffr.snapshot_diff_dir")) {
  if (!is.null(preset)) {
    values <- odiff_preset(preset)
    if (missing(threshold)) threshold <- values$threshold
    if (missing(antialiasing)) antialiasing <- values$antialiasing
  }
  .check_snapshot_diff_dir(diff_dir)

  # Force arguments now so later changes in the caller do not leak in
  force(threshold)
  force(antialiasing)
  force(ignore_regions)
  force(fail_on_layout)
  force(diff_dir)
  dots <- list(...)

  function(old, new) {
    diff_output <- .snapshot_diff_path(old, diff_dir)
    if (!is.null(diff_output)) {
      # Never leave a diff from an earlier run behind
      unlink(diff_output)
      dir.create(dirname(diff_output), recursive = TRUE, showWarnings = FALSE)
    }

    result <- do.call(compare_images, c(
      list(
        img1 = old,
        img2 = new,
        diff_output = diff_output,
        threshold = threshold,
        antialiasing = antialiasing,
        fail_on_layout = fail_on_layout,
        ignore_regions = ignore_regions
      ),
      dots
    ))

    if (identical(result$reason, "error")) {
      err <- .result_error(result)
      warning(
        sprintf("odiff could not compare '%s' with '%s': %s",
                basename(old), basename(new),
                if (is.na(err)) "unknown error" else err),
        call. = FALSE
      )
      return(FALSE)
    }

    if (isTRUE(result$match)) {
      if (!is.null(diff_output)) {
        unlink(diff_output)
        .remove_empty_dirs(dirname(diff_output),
                           .resolve_snapshot_diff_dir(diff_dir))
      }
      return(TRUE)
    }

    message(.snapshot_failure_message(result, old, diff_output))
    FALSE
  }
}


#' Comparison Presets
#'
#' Named sets of comparison settings (`threshold` and `antialiasing`) for
#' common situations. Pass the name as `preset` to [compare_file_odiff()],
#' [expect_snapshot_image()] or [snapshot_report()], or splice the values
#' into other functions with [do.call()].
#'
#' @param name One of `"strict"`, `"default"`, `"screenshot"` or
#'   `"cross_platform"`.
#'
#' @details
#' \describe{
#'   \item{`"strict"`}{`threshold = 0`, `antialiasing = FALSE`. Any change
#'     to any pixel fails.}
#'   \item{`"default"`}{`threshold = 0.1`, `antialiasing = FALSE`. The
#'     defaults of [compare_images()] and [expect_snapshot_image()].}
#'   \item{`"screenshot"`}{`threshold = 0.1`, `antialiasing = TRUE`. For
#'     browser screenshots (shinytest2, webshot2, chromote) and plots
#'     compared on the same platform. Browsers render the edges of rounded
#'     corners, circles and thin borders with slightly different
#'     anti-aliasing from run to run; odiff's anti-aliasing detection
#'     ignores those pixels. The colour threshold stays at 0.1 so that real
#'     colour changes are still caught.}
#'   \item{`"cross_platform"`}{`threshold = 0.2`, `antialiasing = TRUE`. For
#'     baselines shared across machines or operating systems. Also tolerates
#'     edges that move by up to about half a pixel, thicker anti-aliased
#'     borders and small colour or gamma shifts. The price: changes between
#'     colours of similar brightness (e.g. a blue element turning green) and
#'     very faint elements (light grey on white) can go unnoticed. Different
#'     fonts or text rendering are not tolerated; use snapshot variants or
#'     `ignore_regions` for those.}
#' }
#'
#' The values were calibrated with images rendered by ragg: shapes with
#' rounded corners and 1px borders drawn at sub-pixel offsets (0.25 and 0.5
#' px), and with a different anti-aliasing rasteriser (cairo), should pass,
#' while a new 10 x 10 pixel element, or a 10 x 10 pixel patch recoloured
#' from blue to green, must fail. With the `"default"` settings the
#' anti-aliasing-only differences fail (5 to 439 differing pixels); with
#' `"screenshot"` they pass, except a 0.5 px shift of a bordered shape (2
#' pixels), which `"cross_platform"` passes too. A blue to green recolouring
#' is detected up to a threshold of 0.14, which is why `"screenshot"` keeps
#' 0.1. The calibration is part of the package's tests.
#'
#' @return A named list with elements `threshold` and `antialiasing`.
#'
#' @seealso [compare_file_odiff()], [expect_snapshot_image()]
#'
#' @export
#'
#' @examples
#' odiff_preset("screenshot")
#'
#' \dontrun{
#' # Use with any comparison function
#' do.call(compare_images, c(list("before.png", "after.png"),
#'                           odiff_preset("cross_platform")))
#' }
odiff_preset <- function(name = c("strict", "default", "screenshot",
                                  "cross_platform")) {
  choices <- c("strict", "default", "screenshot", "cross_platform")
  if (missing(name)) {
    name <- "strict"
  }
  if (!is.character(name) || length(name) != 1 || is.na(name) ||
      !name %in% choices) {
    stop("preset must be one of ",
         paste0("\"", choices, "\"", collapse = ", "), ".", call. = FALSE)
  }
  switch(
    name,
    strict = list(threshold = 0, antialiasing = FALSE),
    default = list(threshold = 0.1, antialiasing = FALSE),
    screenshot = list(threshold = 0.1, antialiasing = TRUE),
    cross_platform = list(threshold = 0.2, antialiasing = TRUE)
  )
}


# Internal: validate the diff_dir argument of compare_file_odiff()
.check_snapshot_diff_dir <- function(diff_dir) {
  ok <- is.null(diff_dir) || isFALSE(diff_dir) ||
    (is.character(diff_dir) && length(diff_dir) == 1 && !is.na(diff_dir) &&
       nzchar(diff_dir))
  if (!ok) {
    stop("diff_dir must be NULL, FALSE or a single directory path.",
         call. = FALSE)
  }
  invisible(diff_dir)
}


# Internal: directory for snapshot diff images (NULL = no diff images)
.resolve_snapshot_diff_dir <- function(diff_dir) {
  if (isFALSE(diff_dir)) {
    return(NULL)
  }
  if (!is.null(diff_dir)) {
    return(diff_dir)
  }
  if (!isTRUE(getOption("odiffr.save_diff", TRUE))) {
    return(NULL)
  }
  custom <- getOption("odiffr.diff_dir", NULL)
  if (!is.null(custom)) {
    return(custom)
  }
  if (requireNamespace("testthat", quietly = TRUE) && testthat::is_testing()) {
    return(testthat::test_path("_odiffr"))
  }
  if (dir.exists(file.path("tests", "testthat"))) {
    return(file.path("tests", "testthat", "_odiffr"))
  }
  file.path(tempdir(), "odiffr-snapshot-diffs")
}


# Internal: path of the diff image for snapshot `old`, mirroring the layout
# below `_snaps/` (variant and test file directories)
.snapshot_diff_path <- function(old, diff_dir) {
  dir <- .resolve_snapshot_diff_dir(diff_dir)
  if (is.null(dir)) {
    return(NULL)
  }
  parts <- strsplit(
    normalizePath(dirname(old), winslash = "/", mustWork = FALSE), "/"
  )[[1]]
  snaps <- which(parts == "_snaps")
  sub_dirs <- if (length(snaps) > 0 && max(snaps) < length(parts)) {
    parts[(max(snaps) + 1):length(parts)]
  } else {
    character(0)
  }
  name <- paste0(tools::file_path_sans_ext(basename(old)), "_diff.png")
  do.call(file.path, as.list(c(dir, sub_dirs, name)))
}


# Internal: message shown when a snapshot comparison fails
.snapshot_failure_message <- function(result, old, diff_output) {
  what <- if (identical(result$reason, "pixel-diff") &&
                !is.na(result$diff_percentage)) {
    sprintf("%s%% pixels differ (%s px)",
            format(round(result$diff_percentage, 2), nsmall = 2),
            format(result$diff_count, big.mark = ","))
  } else if (identical(result$reason, "layout-diff")) {
    "image dimensions differ"
  } else {
    paste0("images differ (", result$reason, ")")
  }
  where <- if (!is.null(diff_output) && file.exists(diff_output)) {
    paste0("diff image: ",
           normalizePath(diff_output, winslash = "/", mustWork = FALSE))
  } else {
    "no diff image"
  }
  sprintf("odiff: %s in '%s'; %s", what, basename(old), where)
}


# Internal: determine the snapshot file name for expect_snapshot_image()
.snapshot_image_name <- function(x, name, expr_label) {
  if (is.null(name)) {
    expr_label <- paste(expr_label, collapse = "")
    if (is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)) {
      # A temporary file gets a new random name on every run, so its
      # snapshot would never be compared
      in_tempdir <- startsWith(
        normalizePath(dirname(x), winslash = "/", mustWork = FALSE),
        normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
      )
      if (in_tempdir) {
        stop("`x` is a temporary file, whose name changes on every run. ",
             "Supply `name`, e.g. expect_snapshot_image(path, name = ",
             "\"my-plot.png\").", call. = FALSE)
      }
      name <- basename(x)
    } else if (grepl("^[A-Za-z][A-Za-z0-9._]*$", expr_label)) {
      name <- expr_label
    } else {
      stop(
        "Can't derive a snapshot name from `", expr_label, "`. ",
        "Supply `name`, e.g. expect_snapshot_image(..., name = \"my-plot\").",
        call. = FALSE
      )
    }
  }

  if (!is.character(name) || length(name) != 1 || is.na(name) ||
      !nzchar(trimws(name))) {
    stop("name must be a single non-empty character string.", call. = FALSE)
  }
  if (grepl("[/\\\\]", name)) {
    stop("name must be a file name, not a path: ", name, call. = FALSE)
  }

  ext <- tools::file_ext(name)
  if (!identical(tolower(ext), "png")) {
    name <- paste0(name, ".png")
  }
  name
}


# Internal: convert a snapshot input into a temporary PNG file (always a
# fresh temp file that the caller removes)
.snapshot_image_file <- function(x, plot_options = NULL) {
  resolved <- .resolve_image_input(x, "x", plot_options = plot_options)
  if (isTRUE(resolved$temp)) {
    return(resolved$path)
  }
  path <- tempfile(fileext = ".png")
  if (!identical(tolower(tools::file_ext(resolved$path)), "png")) {
    # Snapshots are stored as PNG: convert other formats with magick
    if (!.has_magick()) {
      stop("x must be a PNG file (converting other formats requires the ",
           "'magick' package).", call. = FALSE)
    }
    magick::image_write(magick::image_read(resolved$path), path = path,
                        format = "png")
    return(path)
  }
  # File input: copy, so the snapshot machinery never touches the original
  if (!file.copy(resolved$path, path)) {
    stop("Could not copy ", resolved$path, " to a temporary file.",
         call. = FALSE)
  }
  path
}


# Internal: remove `dir` and its parents while they are empty, stopping at
# (and never removing) `root`
.remove_empty_dirs <- function(dir, root) {
  if (is.null(root)) return(invisible(NULL))
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  while (startsWith(dir, paste0(root, "/")) && dir.exists(dir) &&
         length(list.files(dir, all.files = TRUE, no.. = TRUE)) == 0) {
    unlink(dir, recursive = TRUE)  # empty, checked above
    dir <- dirname(dir)
  }
  invisible(NULL)
}
