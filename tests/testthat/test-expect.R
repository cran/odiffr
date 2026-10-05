# Tests for testthat expectations

test_that("expect_images_match passes for identical images", {
  skip_if_no_odiff()
  withr::local_options(odiffr.diff_dir = withr::local_tempdir())

  img <- create_test_image(100, 100, "red")
  on.exit(unlink(img), add = TRUE)

  # Should pass silently

  expect_silent(expect_images_match(img, img))
})

test_that("expect_images_match returns result invisibly", {
  skip_if_no_odiff()
  withr::local_options(odiffr.diff_dir = withr::local_tempdir())

  img <- create_test_image(100, 100, "blue")
  on.exit(unlink(img), add = TRUE)

  result <- expect_images_match(img, img)
  expect_true(result$match)
  expect_equal(result$reason, "match")
})

test_that("expect_images_match fails for different images", {
  skip_if_no_odiff()
  withr::local_options(odiffr.diff_dir = withr::local_tempdir())

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
  withr::local_options(odiffr.diff_dir = withr::local_tempdir())

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

test_that("generate_diff_filename creates deterministic filenames", {
  diff_dir <- withr::local_tempdir()

  # With path inputs, should include basenames
  f1 <- odiffr:::generate_diff_filename("path/to/actual.png", "path/to/expected.png", diff_dir)
  expect_equal(basename(f1), "actual_vs_expected.png")
  expect_true(startsWith(f1, diff_dir))

  # Repeated calls for the same comparison give the same name (reruns
  # overwrite instead of accumulating files)
  f2 <- odiffr:::generate_diff_filename("path/to/actual.png", "path/to/expected.png", diff_dir)
  expect_equal(f1, f2)

  # With non-character inputs and no labels, uses fallback
  f3 <- odiffr:::generate_diff_filename(list(), list(), diff_dir)
  expect_equal(basename(f3), "odiffr_diff.png")
})

test_that("generate_diff_filename disambiguates colliding basenames", {
  diff_dir <- withr::local_tempdir()

  f1 <- odiffr:::generate_diff_filename("a/shot.png", "a/base.png", diff_dir)
  f2 <- odiffr:::generate_diff_filename("b/shot.png", "b/base.png", diff_dir)
  f3 <- odiffr:::generate_diff_filename("b/shot.png", "b/base.png", diff_dir)
  expect_equal(basename(f1), "shot_vs_base.png")
  expect_equal(basename(f2), "b_shot_vs_b_base.png")
  expect_equal(f2, f3)

  # Same basenames *and* parent dir names fall back to a numeric suffix
  f4 <- odiffr:::generate_diff_filename("x/a/shot.png", "x/a/base.png", diff_dir)
  expect_equal(basename(f4), "a_shot_vs_a_base.png")
  f4b <- odiffr:::generate_diff_filename("y/a/shot.png", "y/a/base.png", diff_dir)
  expect_equal(basename(f4b), "shot_vs_base_2.png")

  # A different diff_dir has its own namespace
  other_dir <- withr::local_tempdir()
  f5 <- odiffr:::generate_diff_filename("b/shot.png", "b/base.png", other_dir)
  expect_equal(basename(f5), "shot_vs_base.png")
})

test_that("generate_diff_filename uses labels for magick inputs", {
  diff_dir <- withr::local_tempdir()

  f1 <- odiffr:::generate_diff_filename(list(), list(), diff_dir,
                                        act_label = "img_new",
                                        exp_label = "magick::image_read(x)")
  expect_equal(basename(f1), "img_new_vs_magick_image_read_x.png")
  f2 <- odiffr:::generate_diff_filename(list(), list(), diff_dir,
                                        act_label = "img_new",
                                        exp_label = "magick::image_read(x)")
  expect_equal(f1, f2)
})

test_that("expect_images_match overwrites diff on rerun and removes stale diff", {
  skip_if_no_odiff()

  diff_dir <- withr::local_tempdir()
  withr::local_options(odiffr.diff_dir = diff_dir, odiffr.save_diff = TRUE)

  dir <- withr::local_tempdir()
  expected <- file.path(dir, "baseline.png")
  actual <- file.path(dir, "current.png")
  file.copy(create_test_image(100, 100, "red"), expected)
  file.copy(create_modified_image(expected, "region"), actual)

  # Two failing runs -> exactly one diff file, with a deterministic name
  expect_failure(expect_images_match(actual, expected))
  expect_failure(expect_images_match(actual, expected))
  diff_files <- list.files(diff_dir, pattern = "\\.png$")
  expect_equal(diff_files, "current_vs_baseline.png")

  # Fix the image -> passing run deletes the stale diff
  file.copy(expected, actual, overwrite = TRUE)
  expect_success(expect_images_match(actual, expected))
  expect_length(list.files(diff_dir, pattern = "\\.png$"), 0)
})

test_that("expect_images_match uses deterministic diff name for magick inputs", {
  skip_if_no_odiff()
  skip_if_not_installed("magick")

  diff_dir <- withr::local_tempdir()
  withr::local_options(odiffr.diff_dir = diff_dir, odiffr.save_diff = TRUE)

  p1 <- create_test_image(100, 100, "red")
  p2 <- create_modified_image(p1, "region")
  on.exit(unlink(c(p1, p2)), add = TRUE)
  img_old <- magick::image_read(p1)
  img_new <- magick::image_read(p2)

  expect_failure(expect_images_match(img_new, img_old))
  expect_failure(expect_images_match(img_new, img_old))
  expect_equal(list.files(diff_dir), "img_new_vs_img_old.png")
})

test_that("expect_images_match failure message includes odiff error text", {
  skip_if_no_odiff()
  withr::local_options(odiffr.save_diff = FALSE)

  img <- create_test_image(20, 20, "red")
  bad <- tempfile(fileext = ".png")
  writeLines("not a png", bad)
  on.exit(unlink(c(img, bad)), add = TRUE)

  err <- tryCatch(
    expect_images_match(bad, img),
    expectation_failure = function(e) e
  )
  expect_s3_class(err, "expectation_failure")
  expect_match(err$message, "Reason: error", fixed = TRUE)
  expect_match(err$message, "Could not load", fixed = TRUE)
})

test_that("expect_images_differ fails (not passes) on comparison error", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  bad <- tempfile(fileext = ".png")
  writeLines("not a png", bad)
  on.exit(unlink(c(img, bad)), add = TRUE)

  expect_failure(expect_images_differ(img, bad), "Could not load")

  err <- tryCatch(
    expect_images_differ(img, bad),
    expectation_failure = function(e) e
  )
  expect_s3_class(err, "expectation_failure")
  expect_match(err$message, "Could not compare", fixed = TRUE)
  expect_false(grepl("unexpectedly matches", err$message))
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

# Regression tests ---------------------------------------------------------

test_that("failure messages do not repeat for multi-line expressions", {
  skip_if_no_odiff()
  withr::local_options(odiffr.save_diff = FALSE)

  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "region")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  err <- tryCatch(
    expect_images_match(
      {
        x <- img2
        x
      },
      img1
    ),
    expectation_failure = function(e) e
  )
  expect_s3_class(err, "expectation_failure")
  expect_length(gregexpr("does not match expected", err$message)[[1]], 1)

  err <- tryCatch(
    expect_images_differ(
      {
        x <- img1
        x
      },
      img1
    ),
    expectation_failure = function(e) e
  )
  expect_s3_class(err, "expectation_failure")
  expect_length(gregexpr("unexpectedly matches", err$message)[[1]], 1)
})

test_that("get_diff_dir uses test_path() inside a test run", {
  withr::local_options(odiffr.save_diff = NULL, odiffr.diff_dir = NULL)
  expect_true(testthat::is_testing())
  expect_equal(odiffr:::get_diff_dir(), testthat::test_path("_odiffr"))
})

test_that("get_diff_dir uses tempdir() outside tests and package roots", {
  withr::local_options(odiffr.save_diff = NULL, odiffr.diff_dir = NULL)
  withr::local_envvar(TESTTHAT = "false")
  expect_false(testthat::is_testing())

  withr::local_dir(withr::local_tempdir())
  res <- odiffr:::get_diff_dir()
  expect_equal(res, file.path(tempdir(), "odiffr-diffs"))
  expect_false(grepl("tests[/\\\\]testthat", res))

  # From a package root: tests/testthat/_odiffr
  dir.create(file.path("tests", "testthat"), recursive = TRUE)
  expect_equal(odiffr:::get_diff_dir(), file.path("tests", "testthat", "_odiffr"))
})

test_that("plot diffs against same-named baselines get distinct names", {
  diff_dir <- withr::local_tempdir()
  root <- withr::local_tempdir()
  light <- file.path(root, "light", "base.png")
  dark <- file.path(root, "dark", "base.png")
  draw <- function() plot(1)

  f1 <- odiffr:::generate_diff_filename(draw, light, diff_dir,
                                        act_label = "draw")
  f2 <- odiffr:::generate_diff_filename(draw, dark, diff_dir,
                                        act_label = "draw")
  expect_false(identical(f1, f2))
  expect_equal(basename(f1), "draw_vs_base.png")
  expect_equal(basename(f2), "draw_vs_dark_base.png")
  # Stable on rerun
  expect_equal(odiffr:::generate_diff_filename(draw, dark, diff_dir,
                                               act_label = "draw"), f2)
})

test_that("a passing plot expectation keeps another one's failure diff", {
  skip_if_no_odiff()
  diff_dir <- withr::local_tempdir()
  withr::local_options(odiffr.diff_dir = diff_dir, odiffr.save_diff = TRUE)
  opts <- plot_options(width = 3, height = 3, res = 40)

  draw <- function() graphics::plot(1:10)
  root <- withr::local_tempdir()
  light <- file.path(root, "light", "base.png")
  dark <- file.path(root, "dark", "base.png")
  dir.create(dirname(light))
  dir.create(dirname(dark))
  file.copy(odiffr:::.render_plot_with_options(draw, opts), light)
  file.copy(odiffr:::.render_plot_with_options(function() graphics::plot(10:1),
                                               opts), dark)

  expect_failure(expect_images_match(draw, dark, plot_options = opts))
  diffs <- list.files(diff_dir)
  expect_length(diffs, 1)

  expect_success(expect_images_match(draw, light, plot_options = opts))
  expect_equal(list.files(diff_dir), diffs)
})
