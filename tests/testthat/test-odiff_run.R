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
  # Path arguments are shell-quoted for system2()
  expect_true(shQuote("a.png") %in% args)
  expect_true(shQuote("b.png") %in% args)
  expect_true(shQuote("diff.png") %in% args)
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

test_that("odiff_run works with enable_asm flag", {
  skip_if_no_odiff()
  ver <- odiff_version()
  skip_if(is.na(ver) || utils::compareVersion(ver, "4.1.1") < 0,
          "enable_asm requires odiff >= 4.1.1")

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_test_image(50, 50, "red")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- odiff_run(img1, img2, enable_asm = TRUE)

  expect_s3_class(result, "odiff_result")
  expect_true(result$match)
})

test_that("odiff_run warns and disables enable_asm on old odiff", {
  skip_if_no_odiff()

  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)

  # Mock odiff_version to return old version, triggering the guard

  testthat::local_mocked_bindings(
    odiff_version = function() "4.0.0",
    .package = "odiffr"
  )

  expect_warning(
    result <- odiff_run(img, img, enable_asm = TRUE),
    "enable_asm = TRUE requires odiff >= 4.1.1"
  )

  # Should still produce a valid result (flag was disabled)
  expect_s3_class(result, "odiff_result")
  expect_true(result$match)
})

# Regression tests ---------------------------------------------------------

test_that("odiff_run handles paths with spaces and apostrophes", {
  skip_if_no_odiff()

  base <- withr::local_tempdir()
  dir <- file.path(base, "dir with space", "it's here")
  dir.create(dir, recursive = TRUE)

  red <- create_test_image(100, 100, "red")
  mod <- create_modified_image(red, "region")
  on.exit(unlink(c(red, mod)), add = TRUE)
  img1 <- file.path(dir, "base image.png")
  img2 <- file.path(dir, "new image.png")
  file.copy(red, img1)
  file.copy(mod, img2)
  diff_file <- file.path(dir, "diff out.png")

  same <- odiff_run(img1, img1)
  expect_equal(same$reason, "match")
  expect_identical(same$error, NA_character_)

  result <- odiff_run(img1, img2, diff_output = diff_file)
  expect_equal(result$reason, "pixel-diff")
  expect_identical(result$error, NA_character_)
  expect_identical(result$diff_count, 441L)
  expect_true(file.exists(diff_file))
})

test_that("odiff_run returns 0 diff for identical images", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  on.exit(unlink(img), add = TRUE)

  result <- odiff_run(img, img)
  expect_identical(result$diff_count, 0L)
  expect_identical(result$diff_percentage, 0)
  expect_identical(result$error, NA_character_)
  expect_null(result$diff_lines)
})

test_that("odiff_run parses count and percentage", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")  # rows/cols 40:60 (1-based)
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- odiff_run(img1, img2)
  expect_identical(result$diff_count, 441L)
  expect_equal(result$diff_percentage, 4.41)
  expect_null(result$diff_lines)
  expect_type(result$stdout, "character")
  expect_type(result$stderr, "character")
})

test_that("odiff_run diff_lines = TRUE returns correct count and lines", {
  skip_if_no_odiff()

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- odiff_run(img1, img2, diff_lines = TRUE)
  expect_identical(result$diff_count, 441L)
  expect_equal(result$diff_percentage, 4.41)
  # odiff reports 0-based line numbers
  expect_identical(result$diff_lines, 39:59)
})

test_that("odiff_run diff_cols = TRUE returns columns (odiff >= 4.5.0)", {
  skip_if_no_odiff()
  ver <- odiff_version()
  skip_if(is.na(ver) || utils::compareVersion(ver, "4.5.0") < 0,
          "diff_cols requires odiff >= 4.5.0")

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- odiff_run(img1, img2, diff_cols = TRUE)
  expect_identical(result$diff_count, 441L)
  expect_null(result$diff_lines)
  expect_identical(result$diff_cols, 39:59)

  both <- odiff_run(img1, img2, diff_lines = TRUE, diff_cols = TRUE)
  expect_identical(both$diff_lines, 39:59)
  expect_identical(both$diff_cols, 39:59)

  same <- odiff_run(img1, img1, diff_cols = TRUE)
  expect_true("diff_cols" %in% names(same))
  expect_null(same$diff_cols)
})

test_that("odiff_run warns and ignores diff_cols on old odiff", {
  skip_if_no_odiff()

  img1 <- create_test_image(20, 20, "red")
  img2 <- create_test_image(20, 20, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  testthat::local_mocked_bindings(
    odiff_version = function() "4.4.0",
    .package = "odiffr"
  )

  expect_warning(
    result <- odiff_run(img1, img2, diff_cols = TRUE),
    "diff_cols = TRUE requires odiff >= 4.5.0"
  )
  expect_equal(result$reason, "pixel-diff")
  expect_identical(result$diff_count, 400L)
})

test_that("odiff_run reports odiff errors in `error` and `stderr`", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  bad <- tempfile(fileext = ".png")
  writeLines("not an image", bad)
  on.exit(unlink(c(img, bad)), add = TRUE)

  result <- odiff_run(img, bad)
  expect_false(result$match)
  expect_equal(result$reason, "error")
  expect_type(result$error, "character")
  expect_false(is.na(result$error))
  expect_false(grepl("^Error:", result$error))
  expect_true(length(result$stderr) > 0)
  expect_true(any(grepl("Error", result$stderr)))
  expect_output(print(result), "Error:")
})

test_that("print.odiff_result omits error line when there is no error", {
  skip_if_no_odiff()

  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)

  out <- capture.output(print(odiff_run(img, img)))
  expect_false(any(grepl("^Error", out)))
})

test_that("odiff_run appends .png to diff_output without extension", {
  skip_if_no_odiff()

  img1 <- create_test_image(20, 20, "red")
  img2 <- create_test_image(20, 20, "blue")
  dir <- withr::local_tempdir()
  on.exit(unlink(c(img1, img2)), add = TRUE)

  expect_warning(
    result <- odiff_run(img1, img2, diff_output = file.path(dir, "diff")),
    "Adding '.png' extension"
  )
  expect_equal(result$reason, "pixel-diff")
  expect_identical(result$error, NA_character_)
  expect_true(file.exists(file.path(dir, "diff.png")))
  expect_equal(basename(result$diff_output), "diff.png")
})

test_that("odiff_run validates threshold, diff_color, diff_overlay", {
  skip_if_no_odiff()

  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)

  expect_error(odiff_run(img, img, threshold = NA),
               "threshold must be a single number between 0 and 1")
  expect_error(odiff_run(img, img, threshold = c(0.1, 0.2)),
               "threshold must be a single number between 0 and 1")
  expect_error(odiff_run(img, img, diff_color = "red"), "diff_color")
  expect_error(odiff_run(img, img, diff_overlay = 2), "diff_overlay")
  expect_error(odiff_run(img, img, timeout = -1), "timeout")
})

test_that("odiff_run accepts valid diff_color forms and tiny thresholds", {
  skip_if_no_odiff()

  img1 <- create_test_image(20, 20, "red")
  img2 <- create_test_image(20, 20, "blue")
  diff_file <- tempfile(fileext = ".png")
  on.exit(unlink(c(img1, img2, diff_file)), add = TRUE)

  for (col in c("#00FF00", "00ff00")) {
    result <- odiff_run(img1, img2, diff_output = diff_file, diff_color = col,
                        diff_overlay = 0.5)
    expect_equal(result$reason, "pixel-diff")
    expect_identical(result$error, NA_character_)
  }

  result <- odiff_run(img1, img2, threshold = 1e-05)
  expect_equal(result$reason, "pixel-diff")
  expect_identical(result$error, NA_character_)
})

test_that("odiff_run reports timeouts as errors", {
  skip_on_os("windows")
  skip_if(!nzchar(Sys.which("sleep")), "sleep not available")

  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)

  fake <- file.path(withr::local_tempdir(), "slow odiff")
  writeLines(c("#!/bin/sh", "sleep 5"), fake)
  Sys.chmod(fake, "0755")
  withr::local_options(odiffr.path = fake)

  result <- odiff_run(img, img, timeout = 0.2)
  expect_false(result$match)
  expect_equal(result$reason, "error")
  expect_match(result$error, "timed out after 1 second")
  expect_true(result$duration < 4.5)
})

test_that("odiff_run never reports a stale diff image", {
  skip_if_no_odiff()

  red <- create_test_image(100, 100, "red")
  blue <- create_test_image(100, 100, "blue")
  big <- create_test_image(120, 100, "red")
  bad <- tempfile(fileext = ".png")
  writeLines("not a png", bad)
  on.exit(unlink(c(red, blue, big, bad)), add = TRUE)
  diff_file <- file.path(withr::local_tempdir(), "diff.png")

  # A differing run leaves a diff image ...
  result <- odiff_run(red, blue, diff_output = diff_file)
  expect_equal(result$reason, "pixel-diff")
  expect_true(file.exists(diff_file))

  # ... which a later match, layout diff or error must not report
  result <- odiff_run(red, red, diff_output = diff_file)
  expect_true(result$match)
  expect_null(result$diff_output)
  expect_false(file.exists(diff_file))

  odiff_run(red, blue, diff_output = diff_file)
  result <- odiff_run(red, big, diff_output = diff_file, fail_on_layout = TRUE)
  expect_equal(result$reason, "layout-diff")
  expect_null(result$diff_output)
  expect_false(file.exists(diff_file))

  odiff_run(red, blue, diff_output = diff_file)
  result <- odiff_run(bad, red, diff_output = diff_file)
  expect_equal(result$reason, "error")
  expect_null(result$diff_output)
  expect_false(file.exists(diff_file))
})

test_that("odiff_run reports a size mismatch as layout-diff on odiff < 4.5.0", {
  skip_if_no_odiff()

  small <- create_test_image(100, 100, "red")
  wide <- create_test_image(120, 100, "red")
  on.exit(unlink(c(small, wide)), add = TRUE)

  # Old odiff without --fail-on-layout reports such images as a match
  testthat::local_mocked_bindings(
    odiff_version = function() "4.1.1",
    .run_odiff = function(odiff_path, args, timeout_secs = 0) {
      list(stdout = "0", stderr = character(), exit_code = 0L,
           error = NA_character_)
    },
    .package = "odiffr"
  )
  result <- odiff_run(small, wide)
  expect_false(result$match)
  expect_equal(result$reason, "layout-diff")
  expect_identical(result$diff_count, NA_integer_)
  expect_identical(result$diff_percentage, NA_real_)

  # Same-sized images are still a match
  result <- odiff_run(small, small)
  expect_true(result$match)
  expect_equal(result$reason, "match")
  expect_equal(result$diff_count, 0L)
})

test_that("odiff_run leaves odiff >= 4.5.0 match results alone", {
  skip_if_no_odiff()

  small <- create_test_image(100, 100, "red")
  wide <- create_test_image(120, 100, "red")
  on.exit(unlink(c(small, wide)), add = TRUE)

  testthat::local_mocked_bindings(
    odiff_version = function() "4.5.0",
    .run_odiff = function(odiff_path, args, timeout_secs = 0) {
      list(stdout = "0", stderr = character(), exit_code = 0L,
           error = NA_character_)
    },
    .package = "odiffr"
  )
  result <- odiff_run(small, wide)
  expect_true(result$match)
  expect_equal(result$reason, "match")
})

test_that("odiff_run with a real odiff < 4.5.0 binary reports layout-diff", {
  # Point ODIFFR_TEST_OLD_ODIFF at an odiff 4.1.x binary to run this test
  old_bin <- Sys.getenv("ODIFFR_TEST_OLD_ODIFF")
  skip_if(!nzchar(old_bin) || !file.exists(old_bin),
          "ODIFFR_TEST_OLD_ODIFF not set to an odiff binary")
  withr::local_options(odiffr.path = old_bin)
  ver <- odiff_version()
  skip_if(is.na(ver) || utils::compareVersion(ver, "4.5.0") >= 0,
          "ODIFFR_TEST_OLD_ODIFF is not an odiff < 4.5.0")

  small <- create_test_image(100, 100, "red")
  wide <- create_test_image(120, 100, "red")
  on.exit(unlink(c(small, wide)), add = TRUE)

  result <- odiff_run(small, wide)
  expect_equal(result$exit_code, 0L)  # odiff itself reports a match
  expect_false(result$match)
  expect_equal(result$reason, "layout-diff")
  expect_identical(result$diff_count, NA_integer_)

  expect_true(odiff_run(small, small)$match)
  expect_equal(odiff_run(small, wide, fail_on_layout = TRUE)$reason,
               "layout-diff")
})

test_that("odiff_run accepts timeouts larger than an integer", {
  skip_if_no_odiff()
  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)

  result <- odiff_run(img, img, timeout = 1e10)
  expect_true(result$match)
  expect_identical(result$error, NA_character_)
})
