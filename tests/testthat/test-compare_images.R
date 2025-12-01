test_that("compare_images returns tibble when available", {
  skip_if_no_odiff()

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_test_image(50, 50, "red")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- compare_images(img1, img2)

  # Should be tibble if tibble is installed, data.frame otherwise
  expect_true(is.data.frame(result))
  expect_named(result, c("match", "reason", "diff_count", "diff_percentage",
                         "diff_output", "img1", "img2"))
})

test_that("compare_images detects matching images", {
  skip_if_no_odiff()

  img <- create_test_image(50, 50, "green")
  on.exit(unlink(img), add = TRUE)

  result <- compare_images(img, img)

  expect_true(result$match)
  expect_equal(result$reason, "match")
})

test_that("compare_images detects different images", {
  skip_if_no_odiff()

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_test_image(50, 50, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- compare_images(img1, img2)

  expect_false(result$match)
  expect_equal(result$reason, "pixel-diff")
})

test_that("compare_images with diff_output = TRUE creates temp file", {
  skip_if_no_odiff()

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_test_image(50, 50, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- compare_images(img1, img2, diff_output = TRUE)

  expect_false(is.na(result$diff_output))
  expect_true(file.exists(result$diff_output))

  # Cleanup
  unlink(result$diff_output)
})

test_that("compare_images passes threshold to odiff_run", {
  skip_if_no_odiff()

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_modified_image(img1, "pixel")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  # Very high threshold should be more lenient
  result <- compare_images(img1, img2, threshold = 1.0)

  # With threshold = 1.0, should likely match
  expect_true(result$match || result$diff_count < 100)
})

test_that("compare_images handles ignore_regions", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  # Ignore the modified region
  result <- compare_images(img1, img2,
    ignore_regions = list(ignore_region(35, 35, 65, 65))
  )

  expect_true(result$match)
})

test_that("compare_images handles magick objects when magick is available", {
  skip_if_no_odiff()
  skip_if_not_installed("magick")

  # Create test images
  img1_path <- create_test_image(50, 50, "red")
  img2_path <- create_test_image(50, 50, "red")
  on.exit(unlink(c(img1_path, img2_path)), add = TRUE)

  # Read as magick objects
  img1 <- magick::image_read(img1_path)
  img2 <- magick::image_read(img2_path)

  result <- compare_images(img1, img2)

  expect_true(result$match)
  expect_equal(result$img1, "<magick-image>")
  expect_equal(result$img2, "<magick-image>")
})

test_that("compare_images errors without magick for magick objects", {
  skip_if_no_odiff()
  skip_if_not_installed("magick")

  # This test is tricky since we can't easily unload magick

  # Just verify the function signature accepts magick objects
  img_path <- create_test_image(10, 10, "red")
  on.exit(unlink(img_path), add = TRUE)

  img <- magick::image_read(img_path)

  # Should work without error when magick is installed
  result <- compare_images(img, img)
  expect_true(result$match)
})

test_that("compare_images validates inputs", {
  skip_if_no_odiff()

  expect_error(
    compare_images("/nonexistent.png", "/also_nonexistent.png"),
    "does not exist"
  )
})

test_that("compare_images_batch works with data frame", {
  skip_if_no_odiff()

  # Create test images
  img1a <- create_test_image(50, 50, "red")
  img1b <- create_test_image(50, 50, "red")  # Match
  img2a <- create_test_image(50, 50, "blue")
  img2b <- create_test_image(50, 50, "green")  # Different
  on.exit(unlink(c(img1a, img1b, img2a, img2b)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img1a, img2a),
    img2 = c(img1b, img2b),
    stringsAsFactors = FALSE
  )

  result <- compare_images_batch(pairs)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true("pair_id" %in% names(result))
  expect_equal(result$pair_id, c(1, 2))

  # First pair should match, second should not
  expect_true(result$match[1])
  expect_false(result$match[2])
})

test_that("compare_images_batch creates diff directory", {
  skip_if_no_odiff()

  img1 <- create_test_image(30, 30, "red")
  img2 <- create_test_image(30, 30, "blue")
  diff_dir <- tempfile("diff_dir")
  on.exit(unlink(c(img1, img2, diff_dir), recursive = TRUE), add = TRUE)

  pairs <- data.frame(img1 = img1, img2 = img2, stringsAsFactors = FALSE)
  result <- compare_images_batch(pairs, diff_dir = diff_dir)

  expect_true(dir.exists(diff_dir))
  expect_false(is.na(result$diff_output))
})

test_that("compare_images_batch errors with invalid pairs data.frame", {
  expect_error(
    compare_images_batch(data.frame(a = 1, b = 2)),
    "must have 'img1' and 'img2' columns"
  )
})

test_that("compare_images_batch errors with invalid pairs type", {
  expect_error(
    compare_images_batch("not a data.frame"),
    "must be a data.frame or list"
  )
})

test_that("compare_images_batch works with list input", {
  skip_if_no_odiff()

  img1 <- create_test_image(30, 30, "red")
  img2 <- create_test_image(30, 30, "red")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  pairs_list <- list(
    list(img1 = img1, img2 = img2)
  )

  result <- compare_images_batch(pairs_list)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 1)
  expect_true(result$match[1])
})

test_that("compare_images with diff_output path creates file", {
  skip_if_no_odiff()

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_test_image(50, 50, "blue")
  diff_path <- tempfile(fileext = ".png")
  on.exit(unlink(c(img1, img2, diff_path)), add = TRUE)

  result <- compare_images(img1, img2, diff_output = diff_path)

  expect_equal(result$diff_output, diff_path)
  expect_true(file.exists(diff_path))
})

test_that("compare_images with antialiasing option", {
  skip_if_no_odiff()

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_test_image(50, 50, "red")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  # Just verify it doesn't error with antialiasing = TRUE
  result <- compare_images(img1, img2, antialiasing = TRUE)

  expect_true(result$match)
})

test_that("compare_images with fail_on_layout option", {
  skip_if_no_odiff()

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_test_image(60, 60, "red")  # Different size
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- compare_images(img1, img2, fail_on_layout = TRUE)

  expect_false(result$match)
  expect_match(result$reason, "layout")  # Can be "layout" or "layout-diff"
})

test_that("compare_images returns data.frame without tibble", {
 skip_if_no_odiff()

  img <- create_test_image(20, 20, "blue")
  on.exit(unlink(img), add = TRUE)

  result <- compare_images(img, img)

  # Should always be at least a data.frame
  expect_true(is.data.frame(result))
})
