test_that("odiff_run compares identical images correctly", {
  skip_if_no_odiff()

  # Create two identical test images
  img1 <- create_test_image(100, 100, "red")
  img2 <- create_test_image(100, 100, "red")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- odiff_run(img1, img2)

  expect_s3_class(result, "odiff_result")
  expect_true(result$match)
  expect_equal(result$reason, "match")
  expect_equal(result$exit_code, 0L)
})

test_that("odiff_run detects pixel differences", {
  skip_if_no_odiff()

  # Create two different test images
  img1 <- create_test_image(100, 100, "red")
  img2 <- create_test_image(100, 100, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- odiff_run(img1, img2)

  expect_false(result$match)
  expect_equal(result$reason, "pixel-diff")
  expect_equal(result$exit_code, 22L)
})

test_that("odiff_run creates diff output file", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_test_image(100, 100, "blue")
  diff_file <- tempfile(fileext = ".png")
  on.exit(unlink(c(img1, img2, diff_file)), add = TRUE)

  result <- odiff_run(img1, img2, diff_output = diff_file)

  expect_false(result$match)
  expect_true(file.exists(diff_file))
  # Compare basenames to avoid Windows 8.3 short path vs long path issues
  expect_equal(basename(result$diff_output), basename(diff_file))
})

test_that("odiff_run respects threshold parameter", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "pixel")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  # With default threshold, might detect difference
  result_default <- odiff_run(img1, img2, threshold = 0.1)

  # With very high threshold, should match
  result_high <- odiff_run(img1, img2, threshold = 1.0)

  # High threshold should be more permissive
  expect_true(result_high$match || result_high$diff_count <= result_default$diff_count)
})

test_that("odiff_run validates input paths", {
  skip_if_no_odiff()

  expect_error(
    odiff_run("/nonexistent/image1.png", "/nonexistent/image2.png"),
    "does not exist"
  )
})

test_that("odiff_run validates threshold range", {
  skip_if_no_odiff()

  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)

  expect_error(
    odiff_run(img, img, threshold = -0.5),
    "threshold must be a single number between 0 and 1"
  )

  expect_error(
    odiff_run(img, img, threshold = 1.5),
    "threshold must be a single number between 0 and 1"
  )
})

test_that("odiff_run handles ignore_regions", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")  # Modifies 40:60, 40:60
  on.exit(unlink(c(img1, img2)), add = TRUE)

  # Without ignoring the modified region, should detect differences
  result_no_ignore <- odiff_run(img1, img2)
  expect_false(result_no_ignore$match)

  # Ignoring the modified region should result in match
  result_ignore <- odiff_run(img1, img2,
    ignore_regions = list(ignore_region(35, 35, 65, 65))
  )
  expect_true(result_ignore$match)
})

test_that("ignore_region creates correct structure", {
  region <- ignore_region(10, 20, 100, 200)

  expect_s3_class(region, "odiff_region")
  expect_equal(region$x1, 10L)
  expect_equal(region$y1, 20L)
  expect_equal(region$x2, 100L)
  expect_equal(region$y2, 200L)
})

test_that("ignore_region validates coordinates", {
  # x2 < x1 should error
  expect_error(ignore_region(100, 10, 50, 100))

  # y2 < y1 should error
  expect_error(ignore_region(10, 100, 100, 50))
})

test_that("odiff_result prints correctly", {
  skip_if_no_odiff()

  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)

  result <- odiff_run(img, img)

  expect_output(print(result), "odiff comparison result")
  expect_output(print(result), "Match:")
  expect_output(print(result), "Reason:")
})

test_that(".build_args produces correct CLI arguments", {
  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    diff_output = "diff.png",
    threshold = 0.05,
    antialiasing = TRUE,
    fail_on_layout = TRUE
  )

  expect_true("--threshold=0.05" %in% args)
  expect_true("--antialiasing" %in% args)
  expect_true("--fail-on-layout" %in% args)
  expect_true("a.png" %in% args)
  expect_true("b.png" %in% args)
  expect_true("diff.png" %in% args)
})

test_that(".format_regions handles various inputs", {
  # Single region as list
  result1 <- odiffr:::.format_regions(list(x1 = 10, y1 = 20, x2 = 30, y2 = 40))
  expect_equal(result1, "10:20-30:40")

  # Multiple regions
  result2 <- odiffr:::.format_regions(list(
    list(x1 = 10, y1 = 20, x2 = 30, y2 = 40),
    list(x1 = 50, y1 = 60, x2 = 70, y2 = 80)
  ))
  expect_equal(result2, "10:20-30:40,50:60-70:80")

  # Data frame
  df <- data.frame(x1 = c(10, 50), y1 = c(20, 60), x2 = c(30, 70), y2 = c(40, 80))
  result3 <- odiffr:::.format_regions(df)
  expect_equal(result3, "10:20-30:40,50:60-70:80")
})
