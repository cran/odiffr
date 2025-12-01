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

  # Create temp file with appropriate extension
  temp_file <- tempfile(fileext = paste0(".", format))

  # Write image using magick
  magick::image_write(img, path = temp_file, format = format)

  temp_file
}

# Resolve image input to a file path
# Accepts either a file path (character) or a magick-image object
# Returns a list with path and a flag indicating if cleanup is needed
.resolve_image_input <- function(img, arg_name = "img") {
  if (is.character(img)) {
    # It's a file path
    path <- .validate_image_path(img, arg_name)
    return(list(path = path, temp = FALSE))
  }

  if (.is_magick_image(img)) {
    # It's a magick-image, write to temp file
    path <- .write_temp_image(img, format = "png")
    return(list(path = path, temp = TRUE))
  }

  stop(
    arg_name, " must be a file path (character) or a magick-image object.",
    call. = FALSE
  )
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
