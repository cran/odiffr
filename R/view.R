#' Plot an odiff Comparison
#'
#' Display the baseline, current and diff images of an [odiff_run()] result
#' in the current graphics device, using base graphics.
#'
#' @param x An `odiff_result` object returned by [odiff_run()].
#' @param which Which image(s) to show: `"all"` (default) shows the
#'   baseline, current and diff images side by side; `"diff"`, `"baseline"`
#'   or `"current"` show a single image.
#' @param ... Currently unused.
#'
#' @return `x`, invisibly.
#'
#' @details
#' PNG images are read with [png::readPNG()] (the png package must be
#' installed). Other formats (JPEG, WEBP, TIFF, ...) are read with the
#' magick package if it is installed.
#'
#' A diff image is only available when the comparison was run with
#' `diff_output` and the images differ; otherwise the diff panel says
#' "No diff image".
#'
#' The graphical parameters (`par()`) are restored on exit.
#'
#' For data-frame results of [compare_images()] or a row of a batch, use
#' [diff_image()] to get the diff image.
#'
#' @seealso [diff_image()], [odiff_run()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- odiff_run("baseline.png", "current.png", diff_output = "diff.png")
#' plot(result)
#' plot(result, which = "diff")
#' }
plot.odiff_result <- function(x, which = c("all", "diff", "baseline", "current"),
                              ...) {
  which <- match.arg(which)
  panels <- if (which == "all") c("baseline", "current", "diff") else which

  op <- graphics::par(mfrow = c(1L, length(panels)), mar = c(0.5, 0.5, 2.5, 0.5))
  on.exit(graphics::par(op), add = TRUE)

  for (panel in panels) {
    path <- switch(panel,
                   baseline = x$img1,
                   current = x$img2,
                   diff = x$diff_output)
    title <- switch(panel,
                    baseline = "Baseline",
                    current = "Current",
                    diff = .diff_panel_title(x))
    if (.view_file_exists(path)) {
      .draw_raster_panel(.read_image_raster(path), title)
    } else {
      note <- if (panel == "diff") .no_diff_note(x) else "Image not found"
      .draw_empty_panel(title, note)
    }
  }

  invisible(x)
}

#' Get the Diff Image of a Comparison
#'
#' Read the diff image produced by a comparison.
#'
#' @param x An `odiff_result` from [odiff_run()], a one-row data.frame from
#'   [compare_images()], a single row of an `odiffr_batch` (e.g.
#'   `results[2, ]`), or the path to a diff image file.
#' @param as Output type: `"magick"` (default) returns a `magick-image`
#'   (requires the magick package); `"raster"` returns a `raster` object
#'   (see [grDevices::as.raster()]) read with [png::readPNG()], so magick is
#'   not required.
#'
#' @return A `magick-image` or a `raster` object.
#'
#' @details
#' A diff image only exists when the comparison was run with `diff_output`
#' (or `diff_dir` for batches) and the images differ. An error is raised
#' otherwise.
#'
#' @seealso [plot.odiff_result()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- compare_images("baseline.png", "current.png", diff_output = TRUE)
#' img <- diff_image(result)
#'
#' # Without magick
#' plot(diff_image(result, as = "raster"))
#'
#' # One row of a batch
#' results <- compare_image_dirs("baseline/", "current/", diff_dir = "diffs/")
#' diff_image(failed_pairs(results)[1, ])
#' }
diff_image <- function(x, as = c("magick", "raster")) {
  as <- match.arg(as)
  path <- .diff_image_path(x)

  if (as == "magick") {
    if (!.has_magick()) {
      stop(
        "The 'magick' package is required for as = \"magick\".\n",
        "Install it with: install.packages('magick'), ",
        "or use as = \"raster\".",
        call. = FALSE
      )
    }
    return(magick::image_read(path))
  }

  .require_png()
  .array_to_raster(png::readPNG(path))
}

# Internal: locate the diff image file of a comparison result
.diff_image_path <- function(x) {
  if (inherits(x, "odiff_result")) {
    path <- x$diff_output
    reason <- x$reason
  } else if (is.data.frame(x)) {
    if (!"diff_output" %in% names(x)) {
      stop("x has no 'diff_output' column.", call. = FALSE)
    }
    if (nrow(x) != 1) {
      stop(sprintf(
        "x must have exactly one row, not %d. Select one, e.g. x[1, ].",
        nrow(x)
      ), call. = FALSE)
    }
    path <- as.character(x$diff_output[[1]])
    reason <- if ("reason" %in% names(x)) as.character(x$reason[[1]]) else NA
  } else if (is.character(x) && length(x) == 1) {
    path <- x
    reason <- NA
  } else {
    stop("x must be an odiff_result, a one-row comparison result, ",
         "or a path to a diff image.", call. = FALSE)
  }

  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    hint <- if (identical(reason, "match")) {
      "the images match"
    } else if (identical(reason, "error") || identical(reason, "missing")) {
      sprintf("the comparison failed (%s)", reason)
    } else {
      "no diff_output was requested"
    }
    stop("No diff image available: ", hint, ".", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("Diff image file not found: ", path, call. = FALSE)
  }
  path
}

# Internal: title for the diff panel
.diff_panel_title <- function(x) {
  pct <- x$diff_percentage
  if (identical(x$reason, "layout-diff")) {
    "Diff (layout differs)"
  } else if (!is.null(pct) && length(pct) == 1 && !is.na(pct)) {
    sprintf("Diff (%s)", .fmt_pct(pct))
  } else {
    "Diff"
  }
}

# Internal: explanation shown when there is no diff image
.no_diff_note <- function(x) {
  if (isTRUE(x$match)) {
    "No diff image\n(images match)"
  } else if (identical(x$reason, "error")) {
    "No diff image\n(comparison failed)"
  } else if (is.null(x$diff_output)) {
    "No diff image\n(diff_output not requested)"
  } else {
    "No diff image"
  }
}

.view_file_exists <- function(path) {
  is.character(path) && length(path) == 1 && !is.na(path) &&
    nzchar(path) && file.exists(path)
}

.require_png <- function() {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop(
      "The 'png' package is required to read PNG images.\n",
      "Install it with: install.packages('png')",
      call. = FALSE
    )
  }
}

# Internal: is the file a PNG (by signature)?
.is_png_file <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, "raw", n = 8L)
  identical(sig, as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
}

# Internal: read an image file as a raster (png for PNG, magick otherwise)
.read_image_raster <- function(path) {
  if (.is_png_file(path)) {
    .require_png()
    return(.array_to_raster(png::readPNG(path)))
  }
  if (!.has_magick()) {
    stop(
      "The 'magick' package is required to display non-PNG images: ",
      basename(path), "\n",
      "Install it with: install.packages('magick')",
      call. = FALSE
    )
  }
  grDevices::as.raster(magick::image_read(path))
}

# Internal: convert a readPNG() array (grey, grey+alpha, RGB, RGBA) to raster
.array_to_raster <- function(a) {
  if (length(dim(a)) == 3 && dim(a)[3] == 2) {
    a <- array(c(a[, , 1], a[, , 1], a[, , 1], a[, , 2]),
               dim = c(dim(a)[1:2], 4))
  }
  grDevices::as.raster(a)
}

# Internal: draw a raster in its own panel with correct aspect ratio
.draw_raster_panel <- function(r, title) {
  h <- nrow(r)
  w <- ncol(r)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, w), ylim = c(0, h), asp = 1,
                        xaxs = "i", yaxs = "i")
  graphics::rasterImage(r, 0, 0, w, h, interpolate = FALSE)
  graphics::rect(0, 0, w, h, border = "grey60")
  graphics::title(main = title)
}

# Internal: draw a placeholder panel with a note
.draw_empty_panel <- function(title, note) {
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1))
  graphics::text(0.5, 0.5, note, col = "grey40")
  graphics::title(main = title)
}
