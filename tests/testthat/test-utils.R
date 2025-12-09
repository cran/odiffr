# Tests for utils.R internal functions

# .build_args tests

test_that(".build_args includes diff_mask flag", {
  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    diff_mask = TRUE
  )

  expect_true("--diff-mask" %in% args)
})

test_that(".build_args includes diff_overlay as boolean", {
  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    diff_overlay = TRUE
  )

  expect_true("--diff-overlay" %in% args)
})

test_that(".build_args includes diff_overlay as numeric", {
  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    diff_overlay = 0.5
  )

  expect_true("--diff-overlay=0.5" %in% args)
})

test_that(".build_args includes diff_color", {
  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    diff_color = "#FF0000"
  )

  expect_true("--diff-color=#FF0000" %in% args)
})

test_that(".build_args includes diff_lines flags", {
  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    diff_lines = TRUE
  )

  expect_true("--output-diff-lines" %in% args)
  expect_true("--parsable-stdout" %in% args)
})

test_that(".build_args includes reduce_ram flag", {
  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    reduce_ram = TRUE
  )

  expect_true("--reduce-ram-usage" %in% args)
})

test_that(".build_args includes ignore_regions from data.frame", {
  regions <- data.frame(
    x1 = c(10, 50),
    y1 = c(20, 60),
    x2 = c(30, 70),
    y2 = c(40, 80)
  )

  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    ignore_regions = regions
  )

  expect_true(any(grepl("--ignore=10:20-30:40,50:60-70:80", args)))
})

test_that(".build_args includes all options together", {
  regions <- data.frame(x1 = 10, y1 = 20, x2 = 30, y2 = 40)

  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    diff_output = "diff.png",
    threshold = 0.1,
    antialiasing = TRUE,
    fail_on_layout = TRUE,
    diff_mask = TRUE,
    diff_overlay = 0.3,
    diff_color = "blue",
    diff_lines = TRUE,
    reduce_ram = TRUE,
    ignore_regions = regions
  )

  expect_true("--threshold=0.1" %in% args)
  expect_true("--antialiasing" %in% args)
  expect_true("--fail-on-layout" %in% args)
  expect_true("--diff-mask" %in% args)
  expect_true("--diff-overlay=0.3" %in% args)
  expect_true("--diff-color=blue" %in% args)
  expect_true("--output-diff-lines" %in% args)
  expect_true("--parsable-stdout" %in% args)
  expect_true("--reduce-ram-usage" %in% args)
  expect_true(any(grepl("--ignore=", args)))
  expect_true("a.png" %in% args)
  expect_true("b.png" %in% args)
  expect_true("diff.png" %in% args)
})

# .parse_output tests

test_that(".parse_output handles exit_code 0 (match)", {
  result <- odiffr:::.parse_output(
    stdout = character(0),
    stderr = character(0),
    exit_code = 0L,
    diff_lines_requested = FALSE
  )

  expect_true(result$match)
  expect_equal(result$reason, "match")
  expect_equal(result$exit_code, 0L)
  expect_true(is.na(result$diff_count))
  expect_true(is.na(result$diff_percentage))
})

test_that(".parse_output handles exit_code 21 (layout-diff)", {
  result <- odiffr:::.parse_output(
    stdout = "Layout difference",
    stderr = character(0),
    exit_code = 21L,
    diff_lines_requested = FALSE
  )

  expect_false(result$match)
  expect_equal(result$reason, "layout-diff")
  expect_equal(result$exit_code, 21L)
})

test_that(".parse_output handles exit_code 22 (pixel-diff) with diff count", {
  result <- odiffr:::.parse_output(
    stdout = "Found 1234 different pixels (5.67 %)",
    stderr = character(0),
    exit_code = 22L,
    diff_lines_requested = FALSE
  )

  expect_false(result$match)
  expect_equal(result$reason, "pixel-diff")
  expect_equal(result$diff_count, 1234L)
  expect_equal(result$diff_percentage, 5.67)
})

test_that(".parse_output extracts diff_lines when requested", {
  result <- odiffr:::.parse_output(
    stdout = c("1, 5, 10", "Found 100 different pixels (1.0 %)"),
    stderr = character(0),
    exit_code = 22L,
    diff_lines_requested = TRUE
  )

  expect_false(result$match)
  expect_equal(result$diff_count, 100L)
  # diff_lines should contain parsed line numbers
  expect_true(length(result$diff_lines) > 0)
  expect_true(all(c(1, 5, 10) %in% result$diff_lines))
})

test_that(".parse_output handles unknown exit code", {
  result <- odiffr:::.parse_output(
    stdout = "Some error",
    stderr = "Error occurred",
    exit_code = 1L,
    diff_lines_requested = FALSE
  )

  expect_false(result$match)
  expect_equal(result$reason, "error")
  expect_equal(result$exit_code, 1L)
})

test_that(".parse_output preserves stdout and stderr", {
  result <- odiffr:::.parse_output(
    stdout = "output text",
    stderr = "error text",
    exit_code = 0L,
    diff_lines_requested = FALSE
  )

  expect_equal(result$stdout, "output text")
  expect_equal(result$stderr, "error text")
})

test_that(".parse_output handles changed pixels text variant", {
  result <- odiffr:::.parse_output(
    stdout = "500 changed pixels detected",
    stderr = character(0),
    exit_code = 22L,
    diff_lines_requested = FALSE
  )

  expect_equal(result$diff_count, 500L)
})

# .exit_code_to_reason tests

test_that(".exit_code_to_reason returns correct reasons", {
  expect_equal(odiffr:::.exit_code_to_reason(0), "match")
  expect_equal(odiffr:::.exit_code_to_reason(21), "layout-diff")
  expect_equal(odiffr:::.exit_code_to_reason(22), "pixel-diff")
  expect_equal(odiffr:::.exit_code_to_reason(1), "error")
  expect_equal(odiffr:::.exit_code_to_reason(99), "error")
})

# .validate_image_path tests

test_that(".validate_image_path validates non-existent file", {
  expect_error(
    odiffr:::.validate_image_path("/nonexistent/image.png", "img"),
    "does not exist"
  )
})
test_that(".validate_image_path validates empty string", {
  expect_error(
    odiffr:::.validate_image_path("", "img"),
    "must be a non-empty character string"
  )
})

test_that(".validate_image_path validates NULL", {
  expect_error(
    odiffr:::.validate_image_path(NULL, "img"),
    "must be a non-empty character string"
  )
})

test_that(".validate_image_path returns normalized path for valid file", {
  temp_file <- tempfile(fileext = ".png")
  writeLines("test", temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  result <- odiffr:::.validate_image_path(temp_file, "img")
  expect_equal(result, normalizePath(temp_file, mustWork = TRUE))
})

# .validate_diff_output tests

test_that(".validate_diff_output returns NULL for NULL input", {
  result <- odiffr:::.validate_diff_output(NULL)
  expect_null(result)
})

test_that(".validate_diff_output errors on empty string", {
  expect_error(
    odiffr:::.validate_diff_output(""),
    "must be NULL or a non-empty character string"
  )
})

test_that(".validate_diff_output warns and changes non-png extension", {
  temp_dir <- withr::local_tempdir()
  path <- file.path(temp_dir, "output.jpeg")

  expect_warning(
    result <- odiffr:::.validate_diff_output(path),
    "odiff only outputs PNG format"
  )
  expect_match(result, "\\.png$")
})

test_that(".validate_diff_output creates parent directory if needed", {
  temp_base <- withr::local_tempdir()
  nested_path <- file.path(temp_base, "nested", "dir", "output.png")

  result <- odiffr:::.validate_diff_output(nested_path)

  parent_dir <- dirname(nested_path)
  expect_true(dir.exists(parent_dir))
  expect_match(result, "output\\.png$")
})

test_that(".validate_diff_output accepts valid .png path", {
  temp_dir <- withr::local_tempdir()
  path <- file.path(temp_dir, "output.png")

  result <- odiffr:::.validate_diff_output(path)
  expect_match(result, "\\.png$")
})

test_that(".validate_diff_output handles case-insensitive extension", {
  temp_dir <- withr::local_tempdir()
  path <- file.path(temp_dir, "output.PNG")

  # Should not warn for .PNG (case insensitive)
  expect_silent(result <- odiffr:::.validate_diff_output(path))
  expect_match(result, "\\.PNG$")
})

# .format_regions tests (additional coverage)

test_that(".format_regions returns empty string for NULL", {
  result <- odiffr:::.format_regions(NULL)
  expect_equal(result, "")
})

test_that(".format_regions handles empty list", {
  # Empty list without proper structure - check behavior
  result <- odiffr:::.format_regions(list())
  expect_equal(result, "")
})
