# Tests for magick-support.R internal functions

test_that(".is_magick_image returns FALSE for non-magick objects", {
  expect_false(odiffr:::.is_magick_image("path/to/image.png"))
  expect_false(odiffr:::.is_magick_image(123))
  expect_false(odiffr:::.is_magick_image(NULL))
  expect_false(odiffr:::.is_magick_image(list(a = 1)))
})

test_that(".is_magick_image returns TRUE for magick objects", {
  skip_if_not_installed("magick")

  img_path <- create_test_image(10, 10, "red")
  on.exit(unlink(img_path), add = TRUE)

  img <- magick::image_read(img_path)
  expect_true(odiffr:::.is_magick_image(img))
})

test_that(".has_magick returns logical", {
  result <- odiffr:::.has_magick()
  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that(".write_temp_image writes magick image to temp file", {
  skip_if_not_installed("magick")

  img_path <- create_test_image(20, 20, "blue")
  on.exit(unlink(img_path), add = TRUE)

  img <- magick::image_read(img_path)
  temp_path <- odiffr:::.write_temp_image(img, format = "png")
  on.exit(unlink(temp_path), add = TRUE)

  expect_true(file.exists(temp_path))
  expect_match(temp_path, "\\.png$")

  # Verify it's a valid image
  img_check <- magick::image_read(temp_path)
  info <- magick::image_info(img_check)
  expect_equal(info$width, 20)
  expect_equal(info$height, 20)
})

test_that(".write_temp_image errors for non-magick objects", {
  skip_if_not_installed("magick")

  expect_error(
    odiffr:::.write_temp_image("not a magick image"),
    "Expected a magick-image object"
  )
})

test_that(".resolve_image_input handles file paths", {
  skip_if_no_odiff()

  img_path <- create_test_image(10, 10, "green")
  on.exit(unlink(img_path), add = TRUE)

  result <- odiffr:::.resolve_image_input(img_path, "test_img")

  expect_type(result, "list")
  expect_named(result, c("path", "temp"))
  # Use basename comparison to avoid macOS /var vs /private/var symlink issues
  expect_equal(basename(result$path), basename(img_path))
  expect_false(result$temp)
})

test_that(".resolve_image_input handles magick objects", {
  skip_if_no_odiff()
  skip_if_not_installed("magick")

  img_path <- create_test_image(15, 15, "blue")
  on.exit(unlink(img_path), add = TRUE)

  img <- magick::image_read(img_path)
  result <- odiffr:::.resolve_image_input(img, "test_img")

  expect_type(result, "list")
  expect_named(result, c("path", "temp"))
  expect_true(file.exists(result$path))
  expect_true(result$temp)

  # Cleanup temp file
  unlink(result$path)
})

test_that(".resolve_image_input errors for invalid input", {
  expect_error(
    odiffr:::.resolve_image_input(123, "test_img"),
    "test_img must be a file path"
  )

  expect_error(
    odiffr:::.resolve_image_input(list(a = 1), "my_image"),
    "my_image must be a file path"
  )
})

test_that(".resolve_image_input errors for non-existent file", {
  expect_error(
    odiffr:::.resolve_image_input("/nonexistent/image.png", "img"),
    "does not exist"
  )
})

test_that(".cleanup_temp_files removes temp files", {
  # Create a temp file to clean up
  temp_file <- tempfile(fileext = ".png")
  file.create(temp_file)
  expect_true(file.exists(temp_file))

  # Simulate resolved input with temp = TRUE
  resolved <- list(path = temp_file, temp = TRUE)

  odiffr:::.cleanup_temp_files(resolved)

  expect_false(file.exists(temp_file))
})

test_that(".cleanup_temp_files skips non-temp files", {
  img_path <- create_test_image(10, 10, "red")

  # Simulate resolved input with temp = FALSE
  resolved <- list(path = img_path, temp = FALSE)

  odiffr:::.cleanup_temp_files(resolved)

  # File should still exist

  expect_true(file.exists(img_path))

  # Cleanup
  unlink(img_path)
})

test_that(".cleanup_temp_files handles multiple inputs", {
  # Create two temp files
  temp1 <- tempfile(fileext = ".png")
  temp2 <- tempfile(fileext = ".png")
  file.create(temp1)
  file.create(temp2)

  resolved1 <- list(path = temp1, temp = TRUE)
  resolved2 <- list(path = temp2, temp = TRUE)

  odiffr:::.cleanup_temp_files(resolved1, resolved2)

  expect_false(file.exists(temp1))
  expect_false(file.exists(temp2))
})

test_that(".cleanup_temp_files handles non-existent files gracefully", {
  resolved <- list(path = "/nonexistent/file.png", temp = TRUE)

  # Should not error
  expect_no_error(odiffr:::.cleanup_temp_files(resolved))
})

test_that(".write_temp_image errors when magick is not available", {
  # Mock .has_magick to return FALSE
  testthat::local_mocked_bindings(
    .has_magick = function() FALSE,
    .package = "odiffr"
  )

  expect_error(
    odiffr:::.write_temp_image("anything"),
    "magick.*package is required"
  )
})
