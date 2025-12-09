# Test helpers for odiffr

# Skip if odiff is not available
skip_if_no_odiff <- function() {
  if (!odiff_available()) {
    testthat::skip("odiff binary not available")
  }
}

# Create a simple test PNG image programmatically
# Returns path to temp file
create_test_image <- function(width = 100, height = 100, color = "red") {
  if (!requireNamespace("png", quietly = TRUE)) {
    testthat::skip("png package required for creating test images")
  }

  # Create color matrix based on color name
  colors <- switch(
    color,
    "red" = c(1, 0, 0),
    "green" = c(0, 1, 0),
    "blue" = c(0, 0, 1),
    "white" = c(1, 1, 1),
    "black" = c(0, 0, 0),
    c(0.5, 0.5, 0.5)  # gray default
  )

  # Create RGB array
  img <- array(0, dim = c(height, width, 3))
  img[, , 1] <- colors[1]  # R
  img[, , 2] <- colors[2]  # G
  img[, , 3] <- colors[3]  # B

  # Write to temp file
  temp_file <- tempfile(fileext = ".png")
  png::writePNG(img, temp_file)

  temp_file
}

# Create test image with a specific modification
# Returns path to temp file
create_modified_image <- function(base_image, modification = "pixel") {
  if (!requireNamespace("png", quietly = TRUE)) {
    testthat::skip("png package required for test images")
  }

  # Read base image
  img <- png::readPNG(base_image)

  # Apply modification
  if (modification == "pixel") {
    # Change a single pixel
    if (length(dim(img)) == 3) {
      img[50, 50, ] <- c(1, 1, 1)  # White pixel
    } else {
      img[50, 50] <- 1
    }
  } else if (modification == "region") {
    # Change a region
    if (length(dim(img)) == 3) {
      img[40:60, 40:60, ] <- 1  # White square
    }
  }

  # Write to temp file
  temp_file <- tempfile(fileext = ".png")
  png::writePNG(img, temp_file)

  temp_file
}

# Clean up diff directory created by expect_images_match()
# Call this in teardown or after tests that generate diffs
clean_diff_dir <- function(dir = NULL) {
  if (is.null(dir)) {
    dir <- tryCatch(
      testthat::test_path("_odiffr"),
      error = function(e) file.path("tests", "testthat", "_odiffr")
    )
  }
  if (dir.exists(dir)) {
    unlink(dir, recursive = TRUE)
  }
  invisible(NULL)
}
