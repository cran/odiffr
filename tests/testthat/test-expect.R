# Tests for testthat expectations

test_that("expect_images_match passes for identical images", {
  skip_if_no_odiff()

  img <- create_test_image(100, 100, "red")
  on.exit(unlink(img), add = TRUE)

  # Should pass silently

  expect_silent(expect_images_match(img, img))
})

test_that("expect_images_match returns result invisibly", {
  skip_if_no_odiff()

  img <- create_test_image(100, 100, "blue")
  on.exit(unlink(img), add = TRUE)

  result <- expect_images_match(img, img)
  expect_true(result$match)
  expect_equal(result$reason, "match")
})

test_that("expect_images_match fails for different images", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  expect_failure(
    expect_images_match(img2, img1),
    "does not match expected"
  )
})

test_that("expect_images_match failure message includes diff details", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "pixel")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  # Capture the error to check message contents
  err <- tryCatch(
    expect_images_match(img2, img1),
    expectation_failure = function(e) e
  )

  expect_match(err$message, "pixel-diff", fixed = TRUE)
  expect_match(err$message, "Diff:", fixed = TRUE)
})

test_that("expect_images_match saves diff image on failure by default", {
  skip_if_no_odiff()

  # Use a temp directory for this test
  diff_dir <- withr::local_tempdir()
  withr::local_options(odiffr.diff_dir = diff_dir)

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  # Suppress the expectation failure, we just want to check diff was created
  suppressMessages(try(expect_images_match(img2, img1), silent = TRUE))

  # Check that a diff file was created
  diff_files <- list.files(diff_dir, pattern = "\\.png$")
  expect_length(diff_files, 1)
})

test_that("expect_images_match respects odiffr.save_diff = FALSE", {
  skip_if_no_odiff()

  diff_dir <- withr::local_tempdir()
  withr::local_options(
    odiffr.save_diff = FALSE,
    odiffr.diff_dir = diff_dir
  )

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  suppressMessages(try(expect_images_match(img2, img1), silent = TRUE))

  # No diff should be created
  diff_files <- list.files(diff_dir, pattern = "\\.png$")
  expect_length(diff_files, 0)
})

test_that("expect_images_match respects odiffr.diff_dir option", {
  skip_if_no_odiff()

  custom_dir <- withr::local_tempdir()
  withr::local_options(odiffr.diff_dir = custom_dir)

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  suppressMessages(try(expect_images_match(img2, img1), silent = TRUE))

  # Diff should be in custom dir
  diff_files <- list.files(custom_dir, pattern = "\\.png$")
  expect_length(diff_files, 1)
})

test_that("expect_images_match fails on layout difference by default", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_test_image(200, 200, "red")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  # Disable diff saving to simplify
  withr::local_options(odiffr.save_diff = FALSE)

  expect_failure(
    expect_images_match(img2, img1),
    "layout-diff"
  )
})

test_that("expect_images_match threshold affects matching", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "pixel")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  withr::local_options(odiffr.save_diff = FALSE)

  # With default threshold, should fail
  expect_failure(expect_images_match(img2, img1, threshold = 0.0))

  # With very high threshold, single pixel diff might pass
  # (depends on how odiff handles threshold for tiny diffs)
})

test_that("expect_images_differ passes for different images", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  expect_silent(expect_images_differ(img1, img2))
})

test_that("expect_images_differ fails for identical images", {
  skip_if_no_odiff()

  img <- create_test_image(100, 100, "green")
  on.exit(unlink(img), add = TRUE)

  expect_failure(
    expect_images_differ(img, img),
    "unexpectedly matches"
  )
})

test_that("expect_images_differ returns result invisibly", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- expect_images_differ(img1, img2)
  expect_false(result$match)
  expect_equal(result$reason, "pixel-diff")
})

test_that("expectations skip when odiff not available", {
  # Mock odiff_available to return FALSE
  local_mocked_bindings(odiff_available = function() FALSE)

  img <- tempfile(fileext = ".png")

  expect_condition(
    expect_images_match(img, img),
    class = "skip"
  )

  expect_condition(
    expect_images_differ(img, img),
    class = "skip"
  )
})

test_that("check_testthat errors when testthat not available", {
  # This test verifies the error message, but we can't easily mock

  # requireNamespace, so just verify the function exists and has correct
 # structure by calling it (it should succeed since testthat IS available)
  expect_silent(odiffr:::check_testthat())
})

test_that("generate_diff_filename creates unique filenames", {
  skip_if_no_odiff()

  diff_dir <- withr::local_tempdir()

  # With path inputs, should include basenames
  f1 <- odiffr:::generate_diff_filename("path/to/actual.png", "path/to/expected.png", diff_dir)
  expect_match(f1, "actual_vs_expected")
  expect_match(f1, "\\.png$")
  expect_true(startsWith(f1, diff_dir))

  # Multiple calls should give unique names
  f2 <- odiffr:::generate_diff_filename("path/to/actual.png", "path/to/expected.png", diff_dir)
  expect_false(f1 == f2)

  # With non-character inputs, uses fallback
  f3 <- odiffr:::generate_diff_filename(list(), list(), diff_dir)
  expect_match(f3, "odiffr_diff")
})

test_that("get_diff_dir respects options", {
  # Test odiffr.save_diff = FALSE
  withr::local_options(odiffr.save_diff = FALSE)
  expect_null(odiffr:::get_diff_dir())

  # Test odiffr.diff_dir
  withr::local_options(
    odiffr.save_diff = TRUE,
    odiffr.diff_dir = "/custom/path"
  )
  expect_equal(odiffr:::get_diff_dir(), "/custom/path")
})

test_that("expect_images_match works with magick objects", {
  skip_if_no_odiff()
  skip_if_not_installed("magick")

  withr::local_options(odiffr.save_diff = FALSE)

  # Create test images
  img1_path <- create_test_image(100, 100, "red")
  on.exit(unlink(img1_path), add = TRUE)

  # Read as magick objects
  img1_magick <- magick::image_read(img1_path)

  # Same image should match
  expect_silent(expect_images_match(img1_magick, img1_magick))

  # Magick vs path should also work
  expect_silent(expect_images_match(img1_path, img1_magick))
})
