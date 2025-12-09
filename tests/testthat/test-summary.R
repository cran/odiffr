# Tests for summary.odiffr_batch() and print.odiffr_batch_summary()

test_that("compare_images_batch returns odiffr_batch class", {
  skip_if_no_odiff()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(img1 = img, img2 = img, stringsAsFactors = FALSE)
  result <- compare_images_batch(pairs)

  expect_s3_class(result, "odiffr_batch")
})

test_that("summary works for all-passing batch", {
  skip_if_no_odiff()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(
    img1 = c(img, img),
    img2 = c(img, img),
    stringsAsFactors = FALSE
  )

  result <- compare_images_batch(pairs)
  summ <- summary(result)

  expect_s3_class(summ, "odiffr_batch_summary")
  expect_equal(summ$total, 2)
  expect_equal(summ$passed, 2)
  expect_equal(summ$failed, 0)
  expect_equal(summ$pass_rate, 1.0)
  expect_null(summ$reason_counts)
  expect_null(summ$diff_stats)
  expect_null(summ$worst)
})

test_that("summary works for all-failing batch", {
  skip_if_no_odiff()

  img1 <- create_test_image(30, 30, "red")
  img2 <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img1, img1),
    img2 = c(img2, img2),
    stringsAsFactors = FALSE
  )
  result <- compare_images_batch(pairs)
  summ <- summary(result)

  expect_equal(summ$total, 2)
  expect_equal(summ$passed, 0)
  expect_equal(summ$failed, 2)
  expect_equal(summ$pass_rate, 0.0)
  expect_false(is.null(summ$reason_counts))
  expect_false(is.null(summ$diff_stats))
  expect_false(is.null(summ$worst))
})

test_that("summary works for mixed results", {
  skip_if_no_odiff()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img_red, img_red, img_red),
    img2 = c(img_red, img_blue, img_blue),  # 1 pass, 2 fail
    stringsAsFactors = FALSE
  )
  result <- compare_images_batch(pairs)
  summ <- summary(result)

  expect_equal(summ$total, 3)
  expect_equal(summ$passed, 1)
  expect_equal(summ$failed, 2)
  expect_equal(summ$pass_rate, 1/3)
})

test_that("summary reason_counts is accurate", {
  skip_if_no_odiff()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  img_small <- create_test_image(20, 20, "red")  # Different size
  on.exit(unlink(c(img_red, img_blue, img_small)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img_red, img_red),
    img2 = c(img_blue, img_small),
    stringsAsFactors = FALSE
  )
  result <- compare_images_batch(pairs, fail_on_layout = TRUE)
  summ <- summary(result)

  expect_false(is.null(summ$reason_counts))
  expect_true("pixel-diff" %in% names(summ$reason_counts))
  # Layout diff should appear with fail_on_layout = TRUE
  expect_true(any(grepl("layout", names(summ$reason_counts))))
})

test_that("summary diff_stats are calculated correctly", {
  skip_if_no_odiff()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img_red, img_red),
    img2 = c(img_blue, img_blue),
    stringsAsFactors = FALSE
  )
  result <- compare_images_batch(pairs)
  summ <- summary(result)

  expect_false(is.null(summ$diff_stats))
  expect_true(all(c("min", "median", "mean", "max") %in% names(summ$diff_stats)))
  # All same diff so min = max
  expect_equal(summ$diff_stats$min, summ$diff_stats$max)
})

test_that("summary worst offenders are ordered correctly", {
  skip_if_no_odiff()

  # Create images with different diff amounts
  img_base <- create_test_image(100, 100, "red")
  img_small_diff <- create_modified_image(img_base, "pixel")  # Small diff
  img_large_diff <- create_test_image(100, 100, "blue")  # Large diff
  on.exit(unlink(c(img_base, img_small_diff, img_large_diff)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img_base, img_base),
    img2 = c(img_small_diff, img_large_diff),
    stringsAsFactors = FALSE
  )
  result <- compare_images_batch(pairs)
  summ <- summary(result)

  # Worst should be ordered by diff_percentage descending
  expect_false(is.null(summ$worst))
  expect_true(nrow(summ$worst) <= 5)  # Default n_worst
  # First worst should have highest diff
  if (nrow(summ$worst) >= 2) {
    expect_true(summ$worst$diff_percentage[1] >= summ$worst$diff_percentage[2])
  }
})

test_that("summary n_worst parameter works", {
  skip_if_no_odiff()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  pairs <- data.frame(
    img1 = rep(img_red, 10),
    img2 = rep(img_blue, 10),
    stringsAsFactors = FALSE
  )
  result <- compare_images_batch(pairs)

  summ_default <- summary(result)
  summ_3 <- summary(result, n_worst = 3)
  summ_10 <- summary(result, n_worst = 10)

  expect_equal(nrow(summ_default$worst), 5)  # Default

  expect_equal(nrow(summ_3$worst), 3)
  expect_equal(nrow(summ_10$worst), 10)
})

test_that("print.odiffr_batch_summary produces output", {
  skip_if_no_odiff()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img_red, img_red),
    img2 = c(img_red, img_blue),
    stringsAsFactors = FALSE
  )
  result <- compare_images_batch(pairs)
  summ <- summary(result)

  output <- capture.output(print(summ))

  expect_true(any(grepl("odiffr batch comparison", output)))
  expect_true(any(grepl("Passed:", output)))
  expect_true(any(grepl("Failed:", output)))
  expect_true(any(grepl("Diff statistics", output)))
  expect_true(any(grepl("Worst offenders", output)))
})

test_that("print.odiffr_batch_summary returns invisibly", {
  skip_if_no_odiff()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(img1 = img, img2 = img, stringsAsFactors = FALSE)
  result <- compare_images_batch(pairs)
  summ <- summary(result)

  ret <- withVisible(print(summ))
  expect_false(ret$visible)
  expect_identical(ret$value, summ)
})

test_that("summary works with data.frame (non-tibble) batch results", {
  skip_if_no_odiff()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  # Manually construct an odiffr_batch data.frame (not tibble)
  # to test that summary works without tibble
  result <- data.frame(
    pair_id = 1L,
    match = TRUE,
    reason = "match",
    diff_count = 0L,
    diff_percentage = 0,
    diff_output = NA_character_,
    img1 = img,
    img2 = img,
    stringsAsFactors = FALSE
  )
  class(result) <- c("odiffr_batch", "data.frame")

  expect_false(inherits(result, "tbl_df"))
  expect_s3_class(result, "odiffr_batch")

  summ <- summary(result)
  expect_s3_class(summ, "odiffr_batch_summary")
  expect_equal(summ$total, 1)
  expect_equal(summ$passed, 1)
})

test_that("print handles all-passing batch gracefully", {
  skip_if_no_odiff()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(img1 = img, img2 = img, stringsAsFactors = FALSE)
  result <- compare_images_batch(pairs)
  summ <- summary(result)

  output <- capture.output(print(summ))

  expect_true(any(grepl("Passed: 1", output)))
  expect_true(any(grepl("Failed: 0", output)))
  # Should not have diff stats or worst offenders sections
  expect_false(any(grepl("Diff statistics", output)))
  expect_false(any(grepl("Worst offenders", output)))
})

test_that("summary validates object class", {
  # Should error on non-odiffr_batch objects
  expect_error(
    summary.odiffr_batch(data.frame(x = 1)),
    "inherits"
  )
})

test_that("summary validates n_worst parameter", {
  skip_if_no_odiff()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(img1 = img, img2 = img, stringsAsFactors = FALSE)
  result <- compare_images_batch(pairs)

  expect_error(
    summary(result, n_worst = -1),
    "n_worst must be a non-negative integer"
  )

  expect_error(
    summary(result, n_worst = "foo"),
    "n_worst must be a non-negative integer"
  )

  # Valid values should work
  expect_silent(summary(result, n_worst = 0))
  expect_silent(summary(result, n_worst = 10))
})

test_that("print shows magick-image label for magick inputs", {
  skip_if_no_odiff()
  skip_if_not_installed("magick")

  img_path1 <- create_test_image(30, 30, "red")
  img_path2 <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_path1, img_path2)), add = TRUE)

  img1 <- magick::image_read(img_path1)
  img2 <- magick::image_read(img_path2)

  pairs <- list(list(img1 = img1, img2 = img2))
  result <- compare_images_batch(pairs)
  summ <- summary(result)

  output <- capture.output(print(summ))
  # Should show "pair 1" instead of filename for magick images
  expect_true(any(grepl("pair 1", output)))
})


# Tests for failed_pairs() and passed_pairs() ------------------------------

test_that("failed_pairs returns only failures", {
  skip_if_no_odiff()

  img_match <- create_test_image(30, 30, "red")
  img_diff <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_match, img_diff)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img_match, img_match),
    img2 = c(img_match, img_diff),
    stringsAsFactors = FALSE
  )
  results <- compare_images_batch(pairs)

  failed <- failed_pairs(results)
  expect_equal(nrow(failed), 1)
  expect_false(failed$match[1])
})

test_that("passed_pairs returns only passes", {
  skip_if_no_odiff()

  img_match <- create_test_image(30, 30, "red")
  img_diff <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_match, img_diff)), add = TRUE)

  pairs <- data.frame(
    img1 = c(img_match, img_match),
    img2 = c(img_match, img_diff),
    stringsAsFactors = FALSE
  )
  results <- compare_images_batch(pairs)

  passed <- passed_pairs(results)
  expect_equal(nrow(passed), 1)
  expect_true(passed$match[1])
})

test_that("failed_pairs and passed_pairs validate input", {
  expect_error(failed_pairs(data.frame(x = 1)), "odiffr_batch")
  expect_error(passed_pairs(data.frame(x = 1)), "odiffr_batch")
})

test_that("failed_pairs returns empty for all-passing batch", {
  skip_if_no_odiff()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)

  pairs <- data.frame(img1 = img, img2 = img, stringsAsFactors = FALSE)
  results <- compare_images_batch(pairs)

  failed <- failed_pairs(results)
  expect_equal(nrow(failed), 0)
})

test_that("passed_pairs returns empty for all-failing batch", {
  skip_if_no_odiff()

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  pairs <- data.frame(img1 = img_red, img2 = img_blue, stringsAsFactors = FALSE)
  results <- compare_images_batch(pairs)

  passed <- passed_pairs(results)
  expect_equal(nrow(passed), 0)
})

test_that("failed_pairs preserves tibble class", {
  skip_if_no_odiff()
  skip_if_not_installed("tibble")

  img_red <- create_test_image(30, 30, "red")
  img_blue <- create_test_image(30, 30, "blue")
  on.exit(unlink(c(img_red, img_blue)), add = TRUE)

  pairs <- data.frame(img1 = img_red, img2 = img_blue, stringsAsFactors = FALSE)
  results <- compare_images_batch(pairs)

  # Results should be tibble (tibble is available)
  expect_true(inherits(results, "tbl_df"))

  # failed_pairs should preserve tibble
  failed <- failed_pairs(results)
  expect_true(inherits(failed, "tbl_df"))
})
