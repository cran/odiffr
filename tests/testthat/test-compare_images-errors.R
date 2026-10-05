# Regression tests for error surfacing, missing files, patterns, and batch
# robustness in compare_images(), compare_images_batch(), compare_image_dirs()

create_corrupt_png <- function() {
  path <- tempfile(fileext = ".png")
  writeLines("this is not a png", path)
  path
}

batch_cols <- c("pair_id", "match", "reason", "diff_count", "diff_percentage",
                "diff_output", "img1", "img2", "error")

# ---- Error surfacing -------------------------------------------------------

test_that("compare_images reports odiff error text in error column", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  bad <- create_corrupt_png()
  on.exit(unlink(c(img, bad)), add = TRUE)

  result <- compare_images(img, bad)

  expect_false(result$match)
  expect_equal(result$reason, "error")
  expect_equal(names(result)[ncol(result)], "error")
  expect_type(result$error, "character")
  expect_false(is.na(result$error))
  expect_match(result$error, "Could not load", fixed = TRUE)
})

test_that("compare_images error column is NA for successful comparisons", {
  skip_if_no_odiff()

  img1 <- create_test_image(20, 20, "red")
  img2 <- create_test_image(20, 20, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)

  expect_identical(compare_images(img1, img1)$error, NA_character_)
  expect_identical(compare_images(img1, img2)$error, NA_character_)
})

test_that("compare_images reports unsupported .tif extension as error", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  tif <- sub("\\.png$", ".tif", img)
  file.copy(img, tif)
  on.exit(unlink(c(img, tif)), add = TRUE)

  result <- compare_images(img, tif)
  expect_equal(result$reason, "error")
  expect_false(is.na(result$error))
  expect_true(nzchar(result$error))
})

test_that(".odiff_error_message reads error element defensively", {
  f <- odiffr:::.odiff_error_message
  # New-style odiff_run() result
  expect_identical(f(list(reason = "match", error = NA_character_)),
                   NA_character_)
  expect_identical(f(list(reason = "error", error = "Unsupported image format")),
                   "Unsupported image format")
  # Old-style result without `error`: falls back to odiff output
  expect_identical(f(list(reason = "match")), NA_character_)
  expect_identical(
    f(list(reason = "error", stdout = "Error: Could not load base image: x.png",
           stderr = character(), exit_code = 1L)),
    "Could not load base image: x.png"
  )
  expect_match(f(list(reason = "error", stdout = character(),
                      stderr = character(), exit_code = 1L)),
               "exit code 1")
})

test_that("compare_images_batch carries error column through", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  bad <- create_corrupt_png()
  on.exit(unlink(c(img, bad)), add = TRUE)

  pairs <- data.frame(img1 = c(img, img), img2 = c(img, bad),
                      stringsAsFactors = FALSE)
  result <- compare_images_batch(pairs)

  expect_named(result, batch_cols)
  expect_true(is.na(result$error[1]))
  expect_equal(result$reason[2], "error")
  expect_match(result$error[2], "Could not load", fixed = TRUE)
})

# ---- Batch robustness ------------------------------------------------------

test_that("compare_images_batch reports per-pair errors instead of aborting", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  on.exit(unlink(img), add = TRUE)
  nonexistent <- file.path(tempdir(), "definitely-not-here-odiffr.png")

  pairs <- data.frame(img1 = c(nonexistent, img), img2 = c(img, img),
                      stringsAsFactors = FALSE)

  modes <- if (.Platform$OS.type == "unix") c(FALSE, TRUE) else FALSE
  for (par in modes) {
    result <- compare_images_batch(pairs, parallel = par)
    expect_s3_class(result, "odiffr_batch")
    expect_equal(nrow(result), 2)
    expect_equal(result$pair_id, 1:2)
    expect_false(result$match[1])
    expect_equal(result$reason[1], "error")
    expect_match(result$error[1], "img1 does not exist")
    expect_equal(result$img1[1], nonexistent)
    expect_true(result$match[2])
    expect_true(is.na(result$error[2]))
  }
})

test_that("parallel worker failures are converted to error rows", {
  on_error <- function(msg) odiffr:::.batch_row(pair_id = 1L, error = msg)

  err <- try(stop("worker exploded"), silent = TRUE)
  row <- odiffr:::.parallel_result_or_error(err, on_error)
  expect_false(row$match)
  expect_equal(row$reason, "error")
  expect_match(row$error, "worker exploded")

  row <- odiffr:::.parallel_result_or_error(NULL, on_error)
  expect_equal(row$reason, "error")
  expect_match(row$error, "no result")

  ok <- odiffr:::.batch_row(pair_id = 1L, match = TRUE, reason = "match")
  expect_identical(odiffr:::.parallel_result_or_error(ok, on_error), ok)
})

test_that("compare_images_batch parallel does not crash when a worker fails", {
  skip_on_os("windows")
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  on.exit(unlink(img), add = TRUE)

  # Make mclapply return a try-error for one worker, as it does when a
  # worker process errors
  real_mclapply <- parallel::mclapply
  testthat::local_mocked_bindings(
    mclapply = function(X, FUN, ...) {
      res <- lapply(X, FUN)
      res[[2]] <- try(stop("simulated worker crash"), silent = TRUE)
      res
    },
    .package = "parallel"
  )
  pairs <- data.frame(img1 = c(img, img), img2 = c(img, img),
                      stringsAsFactors = FALSE)
  result <- compare_images_batch(pairs, parallel = TRUE)
  expect_s3_class(result, "odiffr_batch")
  expect_equal(nrow(result), 2)
  expect_equal(result$pair_id, 1:2)
  expect_true(result$match[1])
  expect_false(result$match[2])
  expect_equal(result$reason[2], "error")
  expect_match(result$error[2], "simulated worker crash")
  expect_equal(result$img1[2], img)
})

test_that("compare_images_batch returns empty batch for empty input", {
  expected_types <- c(pair_id = "integer", match = "logical",
                      reason = "character", diff_count = "integer",
                      diff_percentage = "double", diff_output = "character",
                      img1 = "character", img2 = "character",
                      error = "character")

  empty_df <- data.frame(img1 = character(), img2 = character(),
                         stringsAsFactors = FALSE)
  for (input in list(empty_df, list())) {
    result <- compare_images_batch(input)
    expect_s3_class(result, "odiffr_batch")
    expect_equal(nrow(result), 0)
    expect_named(result, names(expected_types))
    expect_equal(vapply(result, typeof, character(1)), expected_types)
  }
})

test_that("compare_images_batch empty result works without tibble", {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package == "tibble") FALSE else base::requireNamespace(package, ...)
    },
    .package = "base"
  )
  result <- compare_images_batch(list())
  expect_s3_class(result, "odiffr_batch")
  expect_false(inherits(result, "tbl_df"))
  expect_named(result, batch_cols)
})

test_that("compare_images_batch validates list input elements", {
  expect_error(
    compare_images_batch(list(list(img1 = "a.png", img2 = "b.png"),
                              list(img1 = "a.png"))),
    "pairs\\[\\[2\\]\\] must be a list with 'img1' and 'img2'"
  )
  expect_error(
    compare_images_batch(list("a.png")),
    "pairs\\[\\[1\\]\\]"
  )
})

test_that("compare_images_batch has consistent column types across rows", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  img2 <- create_test_image(20, 20, "blue")
  on.exit(unlink(c(img, img2)), add = TRUE)
  diff_dir <- withr::local_tempdir()

  pairs <- list(
    list(img1 = img, img2 = img),               # match, no diff file
    list(img1 = img, img2 = img2),              # diff, diff file
    list(img1 = "nope-odiffr.png", img2 = img)  # error row
  )
  result <- compare_images_batch(pairs, diff_dir = diff_dir)
  expect_named(result, batch_cols)
  expect_type(result$diff_output, "character")
  expect_type(result$error, "character")
  expect_type(result$diff_count, "integer")
  expect_type(result$pair_id, "integer")
  expect_equal(result$reason, c("match", "pixel-diff", "error"))
  expect_true(file.exists(result$diff_output[2]))
})

test_that(".batch_n_cores respects _R_CHECK_LIMIT_CORES_ and mc.cores", {
  withr::local_options(mc.cores = 8L)

  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = "false")
  expect_equal(odiffr:::.batch_n_cores(10), 8L)

  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = NA)
  expect_equal(odiffr:::.batch_n_cores(10), 8L)

  for (val in c("TRUE", "true", "warn")) {
    withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = val)
    expect_equal(odiffr:::.batch_n_cores(10), 2L)
  }

  withr::local_envvar(`_R_CHECK_LIMIT_CORES_` = "false")
  # Capped by number of tasks
  expect_equal(odiffr:::.batch_n_cores(3), 3L)
  withr::local_options(mc.cores = 1L)
  expect_equal(odiffr:::.batch_n_cores(10), 1L)
})

# ---- Directory comparison --------------------------------------------------

test_that(".validate_directory rejects NA and empty strings", {
  expect_error(odiffr:::.validate_directory(NA_character_, "baseline_dir"),
               "baseline_dir must be a non-empty directory path")
  expect_error(odiffr:::.validate_directory("", "current_dir"),
               "current_dir must be a non-empty directory path")
  expect_error(compare_image_dirs(NA_character_, tempdir()),
               "baseline_dir must be a non-empty directory path")
  expect_error(compare_image_dirs(tempdir(), ""),
               "current_dir must be a non-empty directory path")
})

test_that("compare_image_dirs default pattern matches odiff formats only", {
  pattern <- formals(compare_image_dirs)$pattern
  files <- c("a.png", "b.jpg", "c.JPEG", "d.webp", "e.tiff", "f.bmp", "g.tif",
             "h.gif")
  expect_equal(grepl(pattern, files, ignore.case = TRUE),
               c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
})

test_that("compare_image_dirs picks up .bmp and skips .tif by default", {
  skip_if_no_odiff()
  skip_if_not_installed("magick")

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img <- create_test_image(20, 20, "red")
  on.exit(unlink(img), add = TRUE)
  m <- magick::image_read(img)
  magick::image_write(m, file.path(baseline_dir, "shot.bmp"), format = "bmp")
  magick::image_write(m, file.path(current_dir, "shot.bmp"), format = "bmp")
  file.copy(img, file.path(baseline_dir, "other.tif"))
  file.copy(img, file.path(current_dir, "other.tif"))

  result <- compare_image_dirs(baseline_dir, current_dir)
  expect_equal(nrow(result), 1)
  expect_match(result$img1, "shot\\.bmp$")
  expect_true(result$match)

  # A user pattern that matches .tif surfaces odiff's error
  result <- compare_image_dirs(baseline_dir, current_dir, pattern = "\\.tif$")
  expect_equal(result$reason, "error")
  expect_false(is.na(result$error))
})

test_that("compare_dirs_report works with missing rows", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()
  diff_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)
  file.copy(img, file.path(baseline_dir, "a.png"))
  file.copy(img, file.path(baseline_dir, "gone.png"))
  file.copy(img, file.path(current_dir, "a.png"))

  out <- file.path(diff_dir, "report.html")
  expect_warning(
    res <- compare_dirs_report(baseline_dir, current_dir, diff_dir = diff_dir,
                               output_file = out, show_all = TRUE),
    "missing"
  )
  expect_true(file.exists(out))
  html <- paste(readLines(out), collapse = "\n")
  expect_match(html, "missing")
  expect_match(html, "gone.png")
  expect_equal(res$reason, c("match", "missing"))
})

test_that("compare_image_dirs missing rows work without tibble", {
  skip_if_no_odiff()

  baseline_dir <- withr::local_tempdir()
  current_dir <- withr::local_tempdir()

  img <- create_test_image(30, 30, "red")
  on.exit(unlink(img), add = TRUE)
  file.copy(img, file.path(baseline_dir, "a.png"))
  file.copy(img, file.path(baseline_dir, "b.png"))
  file.copy(img, file.path(current_dir, "b.png"))

  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (package == "tibble") FALSE else base::requireNamespace(package, ...)
    },
    .package = "base"
  )
  expect_warning(res <- compare_image_dirs(baseline_dir, current_dir),
                 "missing")
  expect_s3_class(res, "odiffr_batch")
  expect_false(inherits(res, "tbl_df"))
  expect_equal(res$reason, c("missing", "match"))
  expect_equal(res$pair_id, 1:2)
})
