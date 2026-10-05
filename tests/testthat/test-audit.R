# Write a small file with fixed contents and return its path
write_bytes <- function(dir, name, bytes) {
  path <- file.path(dir, name)
  writeBin(as.raw(bytes), path)
  path
}

sha256_of <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  paste(as.character(unclass(openssl::sha256(con))), collapse = "")
}

test_that("odiff_run() stores the effective parameters", {
  skip_if_no_odiff()

  img <- create_test_image(20, 20, "red")
  on.exit(unlink(img), add = TRUE)

  res <- odiff_run(img, img)
  expect_named(res$params, c("threshold", "antialiasing", "fail_on_layout",
                             "ignore_regions", "diff_mask", "diff_overlay",
                             "diff_color", "reduce_ram", "enable_asm"))
  expect_equal(res$params$threshold, 0.1)
  expect_false(res$params$antialiasing)
  expect_true(is.na(res$params$ignore_regions))
  expect_true(is.na(res$params$diff_overlay))
  expect_true(is.na(res$params$diff_color))

  res <- odiff_run(img, img, threshold = 0.05, antialiasing = TRUE,
                   fail_on_layout = TRUE, diff_mask = TRUE,
                   diff_overlay = 0.5, diff_color = "#00FF00",
                   reduce_ram = TRUE,
                   ignore_regions = list(ignore_region(1, 2, 3, 4),
                                         ignore_region(5, 6, 7, 8)))
  expect_equal(res$params$threshold, 0.05)
  expect_true(res$params$antialiasing)
  expect_true(res$params$fail_on_layout)
  expect_true(res$params$diff_mask)
  expect_equal(res$params$diff_overlay, 0.5)
  expect_equal(res$params$diff_color, "#00FF00")
  expect_true(res$params$reduce_ram)
  expect_equal(res$params$ignore_regions, "1:2-3:4,5:6-7:8")
  # Existing elements are unchanged
  expect_true(res$match)
  expect_equal(res$reason, "match")
})

test_that("audit_record() records a single odiff_run() result", {
  skip_if_no_odiff()
  skip_if_not_installed("openssl")

  img1 <- create_test_image(20, 20, "red")
  img2 <- create_test_image(20, 20, "blue")
  diff <- tempfile(fileext = ".png")
  on.exit(unlink(c(img1, img2, diff)), add = TRUE)

  res <- odiff_run(img1, img2, diff, threshold = 0.2)
  rec <- audit_record(res)

  expect_named(rec, c("header", "comparisons"))
  h <- rec$header
  expect_equal(h$schema, "odiffr-audit/1")
  expect_match(h$created, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z$")
  expect_equal(h$odiffr_version,
               as.character(utils::packageVersion("odiffr")))
  expect_equal(h$odiff_version, odiff_version())
  expect_equal(h$odiff_path, find_odiff())
  expect_equal(h$odiff_hash, sha256_of(find_odiff()))
  expect_equal(h$hash_algorithm, "sha256")
  expect_equal(h$platform, R.version$platform)
  expect_equal(h$user, Sys.info()[["user"]])
  expect_equal(h$n_comparisons, 1L)
  expect_equal(h$params, res$params)
  expect_equal(h$params$threshold, 0.2)

  cmp <- rec$comparisons
  expect_s3_class(cmp, "data.frame")
  expect_equal(nrow(cmp), 1L)
  # Results hold normalised paths (on macOS /var resolves to /private/var)
  expect_equal(cmp$img1, normalizePath(img1))
  expect_equal(cmp$img1_hash, sha256_of(img1))
  expect_equal(cmp$img2_hash, sha256_of(img2))
  expect_equal(cmp$img1_size, file.size(img1))
  expect_equal(cmp$diff_output, normalizePath(diff))
  expect_equal(cmp$diff_output_hash, sha256_of(diff))
  expect_false(cmp$match)
  expect_equal(cmp$reason, "pixel-diff")
  expect_equal(cmp$diff_count, res$diff_count)
  expect_true(is.na(cmp$error))
})

test_that("audit_record() hashes batch files, including missing ones", {
  skip_if_not_installed("openssl")
  dir <- withr::local_tempdir()
  a <- write_bytes(dir, "a.png", 1:10)
  b <- write_bytes(dir, "b.png", 11:30)
  d <- write_bytes(dir, "d.png", 31:35)
  missing <- file.path(dir, "missing.png")

  batch <- make_batch(
    match = c(TRUE, FALSE, FALSE),
    reason = c("match", "pixel-diff", "missing"),
    diff_count = c(0L, 7L, NA),
    diff_percentage = c(0, 1.5, NA),
    diff_output = c(NA, d, NA),
    img1 = c(a, a, a),
    img2 = c(a, b, missing),
    error = c(NA, NA, "img2 not found")
  )
  rec <- audit_record(batch)
  cmp <- rec$comparisons

  expect_equal(rec$header$n_comparisons, 3L)
  expect_null(rec$header$params)
  expect_equal(cmp$pair_id, 1:3)
  expect_equal(cmp$img1_hash, rep(sha256_of(a), 3))
  expect_equal(cmp$img2_hash, c(sha256_of(a), sha256_of(b), NA))
  expect_equal(cmp$img2_size, c(10, 20, NA))
  expect_equal(cmp$diff_output_hash, c(NA, sha256_of(d), NA))
  expect_equal(cmp$reason, c("match", "pixel-diff", "missing"))
  expect_equal(cmp$diff_count, c(0L, 7L, NA))
  expect_equal(cmp$error, c(NA, NA, "img2 not found"))

  # tibble batches work the same
  tb <- make_batch(match = TRUE, reason = "match", img1 = a, img2 = b,
                   tibble = TRUE)
  expect_equal(audit_record(tb)$comparisons$img2_hash, sha256_of(b))
})

test_that("audit_record() accepts compare_images()-style data frames", {
  dir <- withr::local_tempdir()
  a <- write_bytes(dir, "a.png", 1:10)
  df <- data.frame(match = TRUE, reason = "match", diff_count = 0L,
                   diff_percentage = 0, diff_output = NA_character_,
                   img1 = "<magick-image>", img2 = a,
                   error = NA_character_, stringsAsFactors = FALSE)

  rec <- audit_record(df, hash = "md5", params = list(threshold = 0.3))
  expect_equal(rec$comparisons$pair_id, 1L)
  expect_true(is.na(rec$comparisons$img1_hash))
  expect_equal(rec$comparisons$img2_hash, unname(tools::md5sum(a)))
  expect_equal(rec$header$hash_algorithm, "md5")
  expect_equal(rec$header$params, list(threshold = 0.3))
})

test_that("audit_record() JSON output round-trips", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  a <- write_bytes(dir, "a.png", 1:10)
  missing <- file.path(dir, "missing.png")
  batch <- make_batch(match = c(TRUE, FALSE),
                      reason = c("match", "missing"),
                      diff_count = c(0L, NA), diff_percentage = c(0, NA),
                      img1 = c(a, a), img2 = c(a, missing))
  out <- file.path(dir, "audit.json")

  rec <- audit_record(batch, hash = "md5",
                      params = list(threshold = 0.1, antialiasing = TRUE))
  ret <- withVisible(audit_record(batch, file = out, hash = "md5",
                                  params = list(threshold = 0.1,
                                                antialiasing = TRUE)))
  expect_false(ret$visible)
  expect_equal(ret$value, normalizePath(out))

  raw_json <- paste(readLines(out), collapse = "\n")
  expect_match(raw_json, "\"img2_hash\": null")

  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$header$schema, "odiffr-audit/1")
  expect_equal(parsed$header$hash_algorithm, "md5")
  expect_equal(parsed$header$params, list(threshold = 0.1,
                                          antialiasing = TRUE))
  expect_equal(parsed$comparisons$img1_hash,
               rep(unname(tools::md5sum(a)), 2))
  expect_equal(parsed$comparisons$img2_hash, rec$comparisons$img2_hash)
  expect_equal(parsed$comparisons$reason, c("match", "missing"))
  expect_equal(parsed$comparisons$match, c(TRUE, FALSE))
})

test_that("audit_record() JSON writes null for unknown parameters", {
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  a <- write_bytes(dir, "a.png", 1:10)
  out <- file.path(dir, "audit.json")
  audit_record(make_batch(match = TRUE, reason = "match", img1 = a,
                          img2 = a),
               file = out, hash = "md5")
  expect_match(paste(readLines(out), collapse = "\n"), "\"params\": null")
})

test_that("audit_record() CSV output has one row per comparison", {
  dir <- withr::local_tempdir()
  a <- write_bytes(dir, "a.png", 1:10)
  b <- write_bytes(dir, "b.png", 1:12)
  batch <- make_batch(match = c(TRUE, FALSE),
                      reason = c("match", "pixel-diff"),
                      diff_count = c(0L, 3L), diff_percentage = c(0, 2.5),
                      img1 = c(a, a), img2 = c(a, b))
  out <- file.path(dir, "audit.csv")

  audit_record(batch, file = out, format = "csv", hash = "md5",
               params = list(threshold = 0.05, custom = "x"))
  csv <- utils::read.csv(out, stringsAsFactors = FALSE)

  expect_equal(nrow(csv), 2L)
  expect_true(all(csv$schema == "odiffr-audit/1"))
  expect_true(all(csv$hash_algorithm == "md5"))
  expect_equal(csv$img2_hash, unname(tools::md5sum(c(a, b))))
  expect_equal(csv$param_threshold, c(0.05, 0.05))
  expect_true(all(is.na(csv$param_antialiasing)))
  expect_equal(csv$param_custom, c("x", "x"))
  expect_true(all(paste0("param_", odiffr:::.audit_param_names) %in%
                    names(csv)))
  expect_false("params" %in% names(csv))
})

test_that("audit_record() handles empty batches", {
  dir <- withr::local_tempdir()
  rec <- audit_record(make_batch(), hash = "md5")
  expect_equal(nrow(rec$comparisons), 0L)
  expect_equal(rec$header$n_comparisons, 0L)

  out <- file.path(dir, "empty.csv")
  audit_record(make_batch(), file = out, format = "csv", hash = "md5")
  expect_equal(nrow(utils::read.csv(out)), 0L)
})

test_that("audit_record() validates its inputs", {
  expect_error(audit_record(list(a = 1)), "odiff_result")
  expect_error(audit_record(data.frame(x = 1)), "'img1' and 'img2'")
  batch <- make_batch(match = TRUE, reason = "match")
  expect_error(audit_record(batch, hash = "md5", params = list(1)),
               "named list")
  expect_error(audit_record(batch, hash = "md5", params = "x"),
               "named list")
  expect_error(audit_record(batch, hash = "md5", file = c("a", "b")),
               "single file path")
  expect_error(audit_record(batch, hash = "sha1"))
})

test_that("audit_record() records NA when odiff is unavailable", {
  testthat::local_mocked_bindings(
    .find_odiff_details = function(...) stop("odiff binary not found", call. = FALSE),
    odiff_version = function() NA_character_,
    .package = "odiffr"
  )
  rec <- audit_record(make_batch(match = TRUE, reason = "match"),
                      hash = "md5")
  expect_true(is.na(rec$header$odiff_path))
  expect_true(is.na(rec$header$odiff_hash))
  expect_true(is.na(rec$header$odiff_version))
})

test_that("sha256 falls back to digest when openssl is unavailable", {
  skip_if_not_installed("digest")
  skip_if_not_installed("openssl")
  dir <- withr::local_tempdir()
  a <- write_bytes(dir, "a.png", 1:100)

  testthat::local_mocked_bindings(
    .audit_has_pkg = function(pkg) pkg != "openssl",
    .package = "odiffr"
  )
  hasher <- odiffr:::.audit_hasher("sha256")
  expect_equal(hasher(a), sha256_of(a))
})

test_that("sha256 errors helpfully without openssl and digest", {
  testthat::local_mocked_bindings(
    .audit_has_pkg = function(pkg) FALSE,
    .package = "odiffr"
  )
  batch <- make_batch(match = TRUE, reason = "match")
  expect_error(audit_record(batch), "hash = \"md5\"")
  # md5 needs no extra packages
  expect_equal(audit_record(batch, hash = "md5")$header$hash_algorithm,
               "md5")
})

test_that("JSON output errors helpfully without jsonlite", {
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .audit_has_pkg = function(pkg) pkg != "jsonlite",
    .package = "odiffr"
  )
  batch <- make_batch(match = TRUE, reason = "match")
  out <- file.path(dir, "audit.json")
  expect_error(audit_record(batch, file = out, hash = "md5"),
               "format = \"csv\"")
  expect_false(file.exists(out))

  # Returning the record and writing CSV do not need jsonlite
  expect_type(audit_record(batch, hash = "md5"), "list")
  csv <- file.path(dir, "audit.csv")
  audit_record(batch, file = csv, format = "csv", hash = "md5")
  expect_true(file.exists(csv))
})
