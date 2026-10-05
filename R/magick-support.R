# magick package integration for odiffr
#
# These functions provide optional support for magick-image objects.
# The magick package is in Suggests, so all functions must handle
# the case where magick is not installed.

# Check if an object is a magick-image
.is_magick_image <- function(x) {
  inherits(x, "magick-image")
}

# Check if magick package is available
.has_magick <- function() {
  requireNamespace("magick", quietly = TRUE)
}

# Write a magick-image to a temporary file
# Returns the path to the temp file
.write_temp_image <- function(img, format = "png") {
  if (!.has_magick()) {
    stop(
      "The 'magick' package is required to handle magick-image objects.\n",
      "Install it with: install.packages('magick')",
      call. = FALSE
    )
  }

  if (!.is_magick_image(img)) {
    stop("Expected a magick-image object.", call. = FALSE)
  }
  if (length(img) != 1) {
    stop("magick-image objects must contain a single frame (this one has ",
         length(img), "); select one with img[1].", call. = FALSE)
  }

  # Create temp file with appropriate extension
  temp_file <- tempfile(fileext = paste0(".", format))

  # Write image using magick
  magick::image_write(img, path = temp_file, format = format)

  temp_file
}

# Resolve image input to a file path
# Accepts a file path (character), a magick-image object, or a plot input
# (ggplot object, function that draws a plot, or recorded plot).
# Returns a list with `path` and a `temp` flag indicating if cleanup is
# needed. The kind of input ("file", "magick" or "plot") is attached as the
# "kind" attribute (see .input_kind()); it is an attribute rather than a list
# element so the established list(path, temp) shape stays unchanged.
.resolve_image_input <- function(img, arg_name = "img", plot_options = NULL) {
  if (is.character(img)) {
    # It's a file path
    path <- .validate_image_path(img, arg_name)
    return(.resolved_input(path, temp = FALSE, kind = "file"))
  }

  if (.is_magick_image(img)) {
    # It's a magick-image, write to temp file
    path <- .write_temp_image(img, format = "png")
    return(.resolved_input(path, temp = TRUE, kind = "magick"))
  }

  if (.is_plot_input(img)) {
    # It's a plot: render it to a temporary PNG
    path <- tryCatch(
      .render_plot_with_options(img, plot_options = plot_options),
      error = function(e) {
        stop("Failed to render ", arg_name, " as a plot: ",
             conditionMessage(e), call. = FALSE)
      }
    )
    return(.resolved_input(path, temp = TRUE, kind = "plot"))
  }

  stop(
    arg_name, " must be a file path (character), a magick-image object, ",
    "a ggplot object, a function that draws a plot, or a recorded plot ",
    "(from grDevices::recordPlot()).",
    call. = FALSE
  )
}

# Internal: construct a resolved image input
.resolved_input <- function(path, temp, kind) {
  structure(list(path = path, temp = temp), kind = kind)
}

# Internal: the kind of a resolved input ("file", "magick" or "plot")
.input_kind <- function(resolved) {
  kind <- attr(resolved, "kind", exact = TRUE)
  if (is.null(kind)) {
    if (isTRUE(resolved$temp)) "magick" else "file"
  } else {
    kind
  }
}

# Internal: label used for a resolved input in result rows
.input_label <- function(resolved, path) {
  switch(.input_kind(resolved),
         magick = "<magick-image>",
         plot = "<plot>",
         path)
}

# Clean up temp files if needed
.cleanup_temp_files <- function(...) {
  paths <- list(...)
  for (item in paths) {
    if (is.list(item) && isTRUE(item$temp) && file.exists(item$path)) {
      unlink(item$path)
    }
  }
}
