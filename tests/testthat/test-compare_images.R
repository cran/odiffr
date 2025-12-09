test_that("compare_images returns tibble when tibble is installed", {
  skip_if_no_odiff()
  skip_if_not_installed("tibble")

  img1 <- create_test_image(50, 50, "red")
  img2 <- create_test_image(50, 50, "red")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  result <- compare_images(img1, img2)

  expect_true(is.data.frame(result))
  expect_s3_class(result, "tbl_df")
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

test_that("compare_images errors for magick objects when magick support unavailable", {
  skip_if_no_odiff()
  skip_if_not_installed("magick")  # Need magick to create the object

  img_path <- create_test_image(10, 10, "red")
  on.exit(unlink(img_path), add = TRUE)

  img <- magick::image_read(img_path)

  # Mock .has_magick to simulate magick being unavailable
  testthat::local_mocked_bindings(
    .has_magick = function() FALSE,
    .package = "odiffr"
  )

  expect_error(
    compare_images(img, img),
    "magick.*package is required"
  )
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

  # Verify the diff file was created and path points to it
  # Note: Can't directly compare paths because normalizePath behaves differently

  # before vs after file creation (especially on macOS with /var -> /private/var)
  expect_true(file.exists(result$diff_output))
  expect_true(file.exists(diff_path))
  # Verify they point to the same file by normalizing both AFTER file exists
  expect_equal(normalizePath(result$diff_output), normalizePath(diff_path))
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

test_that("compare_images returns plain data.frame when tibble is unavailable", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "blue")
  on.exit(unlink(img), add = TRUE)

  # Mock requireNamespace to simulate tibble being unavailable
  testthat::with_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tibble") return(FALSE)
      base::requireNamespace(pkg, ...)
    },
    .package = "base",
    {
      result <- compare_images(img, img)
      expect_true(is.data.frame(result))
      expect_false(inherits(result, "tbl_df"))
    }
  )
})

test_that("compare_images_batch returns plain data.frame when tibble is unavailable", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "blue")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(img1 = img, img2 = img, stringsAsFactors = FALSE)

  # Mock requireNamespace to simulate tibble being unavailable
  testthat::with_mocked_bindings(
    requireNamespace = function(pkg, ...) {
      if (pkg == "tibble") return(FALSE)
      base::requireNamespace(pkg, ...)
    },
    .package = "base",
    {
      result <- compare_images_batch(pairs)
      expect_true(is.data.frame(result))
      expect_false(inherits(result, "tbl_df"))
      expect_s3_class(result, "odiffr_batch")
    }
  )
})

# Tests for compare_image_dirs() -----------------------------------------

test_that("compare_image_dirs compares matching directories", {
  skip_if_no_odiff()

  # Create two directories with matching images
  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img1 <- create_test_image(30, 30, "red")
  img2 <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  file.copy(img1, file.path(baseline_dir, "image1.png"))
  file.copy(img2, file.path(baseline_dir, "image2.png"))
  file.copy(img1, file.path(current_dir, "image1.png"))
  file.copy(img2, file.path(current_dir, "image2.png"))

  result <- compare_image_dirs(baseline_dir, current_dir)

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(result$match))
})

test_that("compare_image_dirs detects differences", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  # Baseline: red, Current: blue (different)
  file.copy(img_red, file.path(baseline_dir, "test.png"))
  file.copy(img_blue, file.path(current_dir, "test.png"))

  result <- compare_image_dirs(baseline_dir, current_dir)

  expect_equal(nrow(result), 1)
  expect_false(result$match[1])
  expect_equal(result$reason[1], "pixel-diff")
})

test_that("compare_image_dirs warns and filters missing files in current_dir", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  # Baseline has 2 images, current only has 1

  file.copy(img, file.path(baseline_dir, "exists.png"))
  file.copy(img, file.path(baseline_dir, "missing.png"))
  file.copy(img, file.path(current_dir, "exists.png"))
  # missing.png is NOT in current_dir

  expect_warning(
    result <- compare_image_dirs(baseline_dir, current_dir),
    "1 file\\(s\\) missing from current_dir"
  )

  # Only the existing pair should be in results
  expect_equal(nrow(result), 1)
  expect_true(grepl("exists.png", result$img2[1]))
})

test_that("compare_image_dirs errors when no images match pattern", {
  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  # Create a text file, not an image

  writeLines("hello", file.path(baseline_dir, "file.txt"))
  writeLines("hello", file.path(current_dir, "file.txt"))

  expect_error(
    compare_image_dirs(baseline_dir, current_dir),
    "No images found in baseline_dir matching pattern"
  )
})

test_that("compare_image_dirs errors when no matching pairs exist", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  # Baseline has an image, current is empty
  file.copy(img, file.path(baseline_dir, "test.png"))

  expect_warning(
    expect_error(
      compare_image_dirs(baseline_dir, current_dir),
      "No matching image pairs found"
    ),
    "1 file\\(s\\) missing"
  )
})

test_that("compare_image_dirs respects custom pattern", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  # Create both png and jpeg (actually png but named .jpg)
  file.copy(img, file.path(baseline_dir, "image.png"))
  file.copy(img, file.path(baseline_dir, "image.jpg"))
  file.copy(img, file.path(current_dir, "image.png"))
  file.copy(img, file.path(current_dir, "image.jpg"))

  # Only match .png files
  result <- compare_image_dirs(baseline_dir, current_dir, pattern = "\\.png$")

  expect_equal(nrow(result), 1)
  expect_true(grepl("\\.png$", result$img1[1]))
})

test_that("compare_image_dirs works with recursive = TRUE", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  # Create subdirectory structure
  dir.create(file.path(baseline_dir, "subdir"))
  dir.create(file.path(current_dir, "subdir"))

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  file.copy(img, file.path(baseline_dir, "root.png"))
  file.copy(img, file.path(baseline_dir, "subdir", "nested.png"))
  file.copy(img, file.path(current_dir, "root.png"))
  file.copy(img, file.path(current_dir, "subdir", "nested.png"))

  # Without recursive, only root
  result_flat <- compare_image_dirs(baseline_dir, current_dir, recursive = FALSE)
  expect_equal(nrow(result_flat), 1)

  # With recursive, both
  result_recursive <- compare_image_dirs(baseline_dir, current_dir, recursive = TRUE)
  expect_equal(nrow(result_recursive), 2)
})

test_that("compare_image_dirs creates diff images when diff_dir specified", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()
  diff_dir <- withr::local_tempdir()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  file.copy(img_red, file.path(baseline_dir, "diff_test.png"))
  file.copy(img_blue, file.path(current_dir, "diff_test.png"))

  result <- compare_image_dirs(baseline_dir, current_dir, diff_dir = diff_dir)

  expect_false(is.na(result$diff_output[1]))
  expect_true(file.exists(result$diff_output[1]))
})

test_that("compare_image_dirs passes through threshold option", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  # Need larger image for create_modified_image (it modifies pixel at 50,50)
  img1 <- create_test_image(100, 100, "red")
  img2 <- create_modified_image(img1, "pixel")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  file.copy(img1, file.path(baseline_dir, "test.png"))
  file.copy(img2, file.path(current_dir, "test.png"))

  # High threshold should be more lenient
  result <- compare_image_dirs(baseline_dir, current_dir, threshold = 1.0)

  expect_true(result$match[1] || result$diff_count[1] < 100)
})

test_that("compare_image_dirs validates directory arguments", {
  expect_error(
    compare_image_dirs("/nonexistent/baseline", "/nonexistent/current"),
    "baseline_dir does not exist"
  )

  baseline_dir <- withr::local_tempdir()
  expect_error(
    compare_image_dirs(baseline_dir, "/nonexistent/current"),
    "current_dir does not exist"
  )

  expect_error(
    compare_image_dirs(c("a", "b"), withr::local_tempdir()),
    "baseline_dir must be a single directory path"
  )
})

test_that("compare_image_dirs messages about unmatched current files", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  # Baseline has 1, current has 2 (extra file triggers message)
  file.copy(img, file.path(baseline_dir, "common.png"))
  file.copy(img, file.path(current_dir, "common.png"))
  file.copy(img, file.path(current_dir, "extra.png"))

  # Should emit message about unmatched file
  expect_message(
    compare_image_dirs(baseline_dir, current_dir),
    "1 file.*in current_dir have no baseline.*extra"
  )
})

# Tests for parallel processing -------------------------------------------

test_that("compare_images_batch parallel=FALSE works (baseline)", {
  skip_if_no_odiff()

  img1 <- create_test_image(30, 30, "red")
  img2 <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img1, img1),
    img2 = c(img1, img2),
    stringsAsFactors = FALSE
  )

  result <- compare_images_batch(pairs, parallel = FALSE)

  expect_s3_class(result, "odiffr_batch")
  expect_equal(nrow(result), 2)
  expect_true(result$match[1])
  expect_false(result$match[2])
})

test_that("compare_images_batch parallel=TRUE produces same results as sequential", {
  skip_if_no_odiff()
  skip_on_os("windows")  # mclapply not available on Windows

  img1 <- create_test_image(30, 30, "red")
  img2 <- create_test_image(30, 30, "blue")
  img3 <- create_test_image(30, 30, "green")
  on.exit(unlink(c(img1, img2, img3)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img1, img1, img1),
    img2 = c(img1, img2, img3),
    stringsAsFactors = FALSE
  )

  result_seq <- compare_images_batch(pairs, parallel = FALSE)
  result_par <- compare_images_batch(pairs, parallel = TRUE)

  # Results should be identical

  expect_equal(result_seq$match, result_par$match)
  expect_equal(result_seq$reason, result_par$reason)
  expect_equal(result_seq$diff_count, result_par$diff_count)
  expect_equal(result_seq$pair_id, result_par$pair_id)
})

test_that("compare_images_batch parallel=TRUE on Windows falls back to sequential", {
  skip_if_no_odiff()
  # Skip on Windows - we're testing the fallback behavior for non-Windows platforms.
  # On actual Windows, the mock loses .Platform$file.sep and other properties,
  # breaking path handling. The real Windows behavior is tested implicitly by
  # other tests that run on Windows CI.
  skip_on_os("windows")

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(img1 = img, img2 = img, stringsAsFactors = FALSE)

  # Mock Windows platform to test fallback on non-Windows systems
  testthat::with_mocked_bindings(
    `.Platform` = list(OS.type = "windows"),
    .package = "base",
    {
      # Should not error, just fall back to sequential
      result <- compare_images_batch(pairs, parallel = TRUE)
      expect_s3_class(result, "odiffr_batch")
      expect_true(result$match[1])
    }
  )
})

test_that("compare_image_dirs passes parallel parameter through", {
  skip_if_no_odiff()
  skip_on_os("windows")

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img1 <- create_test_image(30, 30, "red")
  img2 <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  file.copy(img1, file.path(baseline_dir, "a.png"))
  file.copy(img2, file.path(baseline_dir, "b.png"))
  file.copy(img1, file.path(current_dir, "a.png"))
  file.copy(img2, file.path(current_dir, "b.png"))

  result_seq <- compare_image_dirs(baseline_dir, current_dir, parallel = FALSE)
  result_par <- compare_image_dirs(baseline_dir, current_dir, parallel = TRUE)

  expect_equal(result_seq$match, result_par$match)
  expect_equal(nrow(result_par), 2)
})

test_that("compare_images_batch parallel handles single pair", {
  skip_if_no_odiff()
  skip_on_os("windows")

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(img1 = img, img2 = img, stringsAsFactors = FALSE)

  # Single pair should work fine with parallel
  result <- compare_images_batch(pairs, parallel = TRUE)

  expect_s3_class(result, "odiffr_batch")
  expect_equal(nrow(result), 1)
  expect_true(result$match[1])
})

test_that("compare_images_batch parallel maintains pair_id order", {
  skip_if_no_odiff()
  skip_on_os("windows")

  img1 <- create_test_image(30, 30, "red")
  img2 <- create_test_image(30, 30, "blue")
  img3 <- create_test_image(30, 30, "green")
  img4 <- create_test_image(30, 30, "yellow")
  on.exit(unlink(c(img1, img2, img3, img4)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img1, img2, img3, img4),
    img2 = c(img1, img2, img3, img4),
    stringsAsFactors = FALSE
  )

  result <- compare_images_batch(pairs, parallel = TRUE)

  # pair_id should be in order 1, 2, 3, 4
  expect_equal(result$pair_id, 1:4)
})

test_that("compare_images_batch creates distinct diff filenames", {
  skip_if_no_odiff()

  # Create images with same basename to test collision prevention
  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  diff_dir <- withr::local_tempdir()
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  # Two pairs with same img2 basename
  pairs <- data.frame(
    img1 = c(img_red, img_red),
    img2 = c(img_blue, img_blue),
    stringsAsFactors = FALSE
  )

  result <- compare_images_batch(pairs, diff_dir = diff_dir)

  # Both diff files should exist and be distinct
  diff_files <- list.files(diff_dir)
  expect_equal(length(diff_files), 2)
  expect_equal(length(unique(diff_files)), 2)  # All distinct

  # Filenames should include index prefix
  expect_true(all(grepl("^\\d{3}_", diff_files)))
})

# Tests for compare_dirs_report() ------------------------------------------

test_that("compare_dirs_report creates diffs and report", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  file.copy(img_red, file.path(baseline_dir, "test.png"))
  file.copy(img_blue, file.path(current_dir, "test.png"))
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  diff_dir <- file.path(output_dir, "diffs")
  results <- compare_dirs_report(baseline_dir, current_dir, diff_dir = diff_dir)

  expect_s3_class(results, "odiffr_batch")
  expect_true(dir.exists(diff_dir))
  expect_true(file.exists(file.path(diff_dir, "report.html")))
})

test_that("compare_dirs_report respects parallel parameter", {
  skip_if_no_odiff()
  skip_on_os("windows")

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  file.copy(img, file.path(baseline_dir, "test.png"))
  file.copy(img, file.path(current_dir, "test.png"))
  on.exit(unlink(img), add = TRUE)

  diff_dir <- file.path(output_dir, "diffs")

  # Should not error with parallel = TRUE
  results <- compare_dirs_report(baseline_dir, current_dir,
                                  diff_dir = diff_dir, parallel = TRUE)
  expect_s3_class(results, "odiffr_batch")
})

test_that("compare_dirs_report allows custom output_file", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  file.copy(img, file.path(baseline_dir, "test.png"))
  file.copy(img, file.path(current_dir, "test.png"))
  on.exit(unlink(img), add = TRUE)

  diff_dir <- file.path(output_dir, "diffs")
  custom_report <- file.path(output_dir, "my-report.html")

  compare_dirs_report(baseline_dir, current_dir,
                      diff_dir = diff_dir, output_file = custom_report)

  expect_true(file.exists(custom_report))
})

test_that("compare_dirs_report passes report args correctly", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  file.copy(img_red, file.path(baseline_dir, "test.png"))
  file.copy(img_blue, file.path(current_dir, "test.png"))
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  diff_dir <- file.path(output_dir, "diffs")
  report_file <- file.path(diff_dir, "report.html")

  # Should not error - embed is a report arg, not passed to compare_image_dirs
  results <- compare_dirs_report(baseline_dir, current_dir,
                                  diff_dir = diff_dir,
                                  embed = TRUE,
                                  title = "Custom Title")

  expect_s3_class(results, "odiffr_batch")
  expect_true(file.exists(report_file))

  # Verify report contains embedded images and custom title
  html <- readLines(report_file)
  expect_true(any(grepl("data:image/png;base64,", html)))
  expect_true(any(grepl("Custom Title", html)))
})

test_that("compare_dirs_report creates parent directory for output_file", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()
  output_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  file.copy(img, file.path(baseline_dir, "test.png"))
  file.copy(img, file.path(current_dir, "test.png"))
  on.exit(unlink(img), add = TRUE)

  diff_dir <- file.path(output_dir, "diffs")
  # output_file in a NEW directory that doesn't exist yet
  nested_report <- file.path(output_dir, "reports", "nested", "report.html")

  expect_false(dir.exists(dirname(nested_report)))

  compare_dirs_report(baseline_dir, current_dir,
                      diff_dir = diff_dir, output_file = nested_report)

  expect_true(file.exists(nested_report))
})

test_that("compare_dirs_report passes relative_paths to batch_report", {
  skip_if_no_odiff()

  output_dir <- withr::local_tempdir()
  diff_dir <- file.path(output_dir, "diffs")
  report_file <- file.path(output_dir, "reports", "qa-report.html")

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  file.copy(img_red, file.path(baseline_dir, "test.png"))
  file.copy(img_blue, file.path(current_dir, "test.png"))
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  compare_dirs_report(baseline_dir, current_dir, diff_dir = diff_dir,
                      output_file = report_file, relative_paths = TRUE)

  html <- paste(readLines(report_file), collapse = "\n")

  # Should have relative path, not absolute
  # Check that absolute path prefix is NOT present (normalize for consistent comparison)
  abs_prefix <- gsub("\\\\", "/", normalizePath(output_dir, mustWork = FALSE))
  expect_false(grepl(abs_prefix, html, fixed = TRUE))
  # Check for relative path pattern (always forward slashes from .make_relative_path)
  # Should contain ".." (go up) followed by "/" and "diffs", or just "diffs/" at start
  expect_true(
    grepl('src="../diffs/', html, fixed = TRUE) ||
    grepl('src="diffs/', html, fixed = TRUE)
  )
})
