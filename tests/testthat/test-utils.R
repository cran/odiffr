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

  expect_true(shQuote("--diff-color=#FF0000") %in% args)

  args <- odiffr:::.build_args("a.png", "b.png", diff_color = "#FF0000",
                               quote = FALSE)
  expect_true("--diff-color=#FF0000" %in% args)
})

test_that(".build_args always requests parsable stdout", {
  args <- odiffr:::.build_args(img1 = "a.png", img2 = "b.png")
  expect_true("--parsable-stdout" %in% args)
  expect_false("--output-diff-lines" %in% args)
})

test_that(".build_args quotes path arguments", {
  args <- odiffr:::.build_args(
    img1 = "dir with space/a.png",
    img2 = "it's here/b.png",
    diff_output = "out dir/diff.png"
  )
  n <- length(args)
  expect_equal(args[(n - 2):n], c(shQuote("dir with space/a.png"),
                                  shQuote("it's here/b.png"),
                                  shQuote("out dir/diff.png")))

  args <- odiffr:::.build_args("dir with space/a.png", "b.png", quote = FALSE)
  expect_true("dir with space/a.png" %in% args)
})

test_that(".build_args formats numbers without scientific notation", {
  args <- odiffr:::.build_args("a.png", "b.png", threshold = 1e-05,
                               diff_overlay = 1e-04)
  expect_true("--threshold=0.00001" %in% args)
  expect_true("--diff-overlay=0.0001" %in% args)
  expect_false(any(grepl("e-0", args)))
})

test_that(".build_args includes diff_cols flag", {
  args <- odiffr:::.build_args("a.png", "b.png", diff_cols = TRUE)
  expect_true("--output-diff-cols" %in% args)
})

test_that(".build_args omits overlay for diff_overlay = FALSE", {
  args <- odiffr:::.build_args("a.png", "b.png", diff_overlay = FALSE)
  expect_false(any(grepl("--diff-overlay", args)))
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

test_that(".build_args includes enable_asm flag", {
  args <- odiffr:::.build_args(
    img1 = "a.png",
    img2 = "b.png",
    enable_asm = TRUE
  )

  expect_true("--enable-asm" %in% args)
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
    diff_color = "#0000FF",
    diff_lines = TRUE,
    reduce_ram = TRUE,
    enable_asm = TRUE,
    ignore_regions = regions
  )

  expect_true("--threshold=0.1" %in% args)
  expect_true("--antialiasing" %in% args)
  expect_true("--fail-on-layout" %in% args)
  expect_true("--diff-mask" %in% args)
  expect_true("--diff-overlay=0.3" %in% args)
  expect_true(shQuote("--diff-color=#0000FF") %in% args)
  expect_true("--output-diff-lines" %in% args)
  expect_true("--parsable-stdout" %in% args)
  expect_true("--reduce-ram-usage" %in% args)
  expect_true("--enable-asm" %in% args)
  expect_true(any(grepl("--ignore=", args)))
  expect_true(shQuote("a.png") %in% args)
  expect_true(shQuote("b.png") %in% args)
  expect_true(shQuote("diff.png") %in% args)
})

# .parse_output tests (odiff --parsable-stdout format)

test_that(".parse_output handles exit_code 0 (match)", {
  result <- odiffr:::.parse_output(
    stdout = "0",
    stderr = character(0),
    exit_code = 0L,
    diff_lines_requested = FALSE
  )

  expect_true(result$match)
  expect_equal(result$reason, "match")
  expect_equal(result$exit_code, 0L)
  expect_identical(result$diff_count, 0L)
  expect_identical(result$diff_percentage, 0)
  expect_null(result$diff_lines)
  expect_identical(result$error, NA_character_)
})

test_that(".parse_output reports 0 diff for a match even without stdout", {
  result <- odiffr:::.parse_output(character(0), character(0), 0L)
  expect_identical(result$diff_count, 0L)
  expect_identical(result$diff_percentage, 0)
})

test_that(".parse_output handles exit_code 21 (layout-diff)", {
  result <- odiffr:::.parse_output(
    stdout = "layout",
    stderr = character(0),
    exit_code = 21L,
    diff_lines_requested = FALSE
  )

  expect_false(result$match)
  expect_equal(result$reason, "layout-diff")
  expect_equal(result$exit_code, 21L)
  expect_identical(result$diff_count, NA_integer_)
  expect_identical(result$diff_percentage, NA_real_)
  expect_identical(result$error, NA_character_)
})

test_that(".parse_output parses count and percentage", {
  result <- odiffr:::.parse_output(
    stdout = "126;1.26",
    stderr = character(0),
    exit_code = 22L,
    diff_lines_requested = FALSE
  )

  expect_false(result$match)
  expect_equal(result$reason, "pixel-diff")
  expect_identical(result$diff_count, 126L)
  expect_identical(result$diff_percentage, 1.26)
  expect_null(result$diff_lines)
})

test_that(".parse_output parses diff lines without polluting them", {
  result <- odiffr:::.parse_output(
    stdout = "126;1.26;19,20,21",
    stderr = character(0),
    exit_code = 22L,
    diff_lines_requested = TRUE
  )

  expect_identical(result$diff_count, 126L)
  expect_identical(result$diff_percentage, 1.26)
  expect_identical(result$diff_lines, c(19L, 20L, 21L))
})

test_that(".parse_output ignores diff lines when not requested", {
  result <- odiffr:::.parse_output("126;1.26;19,20,21", character(0), 22L)
  expect_null(result$diff_lines)
  expect_false("diff_cols" %in% names(result))
})

test_that(".parse_output parses diff cols with empty lines segment", {
  result <- odiffr:::.parse_output(
    stdout = "126;1.26;;10,11",
    stderr = character(0),
    exit_code = 22L,
    diff_lines_requested = TRUE,
    diff_cols_requested = TRUE
  )

  expect_identical(result$diff_count, 126L)
  expect_null(result$diff_lines)
  expect_identical(result$diff_cols, c(10L, 11L))
})

test_that(".parse_output parses diff lines and cols together", {
  result <- odiffr:::.parse_output(
    stdout = "126;1.26;19,20;10,11",
    stderr = character(0),
    exit_code = 22L,
    diff_lines_requested = TRUE,
    diff_cols_requested = TRUE
  )

  expect_identical(result$diff_lines, c(19L, 20L))
  expect_identical(result$diff_cols, c(10L, 11L))
})

test_that(".parse_output includes NULL diff_cols when requested but absent", {
  result <- odiffr:::.parse_output("0", character(0), 0L,
                                   diff_cols_requested = TRUE)
  expect_true("diff_cols" %in% names(result))
  expect_null(result$diff_cols)
})

test_that(".parse_output does not parse numbers from arbitrary text", {
  result <- odiffr:::.parse_output(
    stdout = c("1, 5, 10", "Found 100 different pixels (1.0 %)"),
    stderr = character(0),
    exit_code = 22L,
    diff_lines_requested = TRUE
  )

  expect_identical(result$diff_count, NA_integer_)
  expect_identical(result$diff_percentage, NA_real_)
  expect_null(result$diff_lines)
})

test_that(".parse_output handles unknown exit code and extracts error", {
  result <- odiffr:::.parse_output(
    stdout = character(0),
    stderr = "Error: Could not load base image: /x/a.png",
    exit_code = 1L,
    diff_lines_requested = FALSE
  )

  expect_false(result$match)
  expect_equal(result$reason, "error")
  expect_equal(result$exit_code, 1L)
  expect_identical(result$diff_count, NA_integer_)
  expect_equal(result$error, "Could not load base image: /x/a.png")
})

test_that(".parse_output joins multiple error lines", {
  result <- odiffr:::.parse_output(
    stdout = character(0),
    stderr = c("some noise", "Error: first", "Error: second"),
    exit_code = 1L
  )
  expect_equal(result$error, "first; second")
})

test_that(".parse_output falls back to whole stderr for errors", {
  result <- odiffr:::.parse_output(character(0), c("boom", "bang"), 1L)
  expect_equal(result$error, "boom; bang")

  result <- odiffr:::.parse_output(character(0), character(0), 3L)
  expect_match(result$error, "status 3")
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
  expect_identical(result$error, NA_character_)
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
  expect_equal(basename(result), "output.png")
})

test_that(".validate_diff_output appends .png when there is no extension", {
  temp_dir <- withr::local_tempdir()
  path <- file.path(temp_dir, "diff")

  expect_warning(
    result <- odiffr:::.validate_diff_output(path),
    "Adding '.png' extension"
  )
  expect_equal(basename(result), "diff.png")

  expect_warning(
    result <- odiffr:::.validate_diff_output(file.path(temp_dir, "diff.")),
    "Adding '.png' extension"
  )
  expect_equal(basename(result), "diff.png")
})

test_that(".validate_diff_output only replaces the final extension", {
  temp_dir <- withr::local_tempdir()
  expect_warning(
    result <- odiffr:::.validate_diff_output(file.path(temp_dir, "a.b.JPG")),
    "from '.JPG' to '.png'"
  )
  expect_equal(basename(result), "a.b.png")
})

test_that("option validators reject invalid input", {
  expect_error(odiffr:::.validate_threshold(NA), "threshold must be")
  expect_error(odiffr:::.validate_threshold(NA_real_), "threshold must be")
  expect_error(odiffr:::.validate_threshold(c(0.1, 0.2)), "threshold must be")
  expect_error(odiffr:::.validate_threshold("0.1"), "threshold must be")
  expect_error(odiffr:::.validate_threshold(Inf), "threshold must be")
  expect_silent(odiffr:::.validate_threshold(NULL))
  expect_silent(odiffr:::.validate_threshold(0))
  expect_silent(odiffr:::.validate_threshold(1))

  expect_error(odiffr:::.validate_diff_color("red"), "diff_color")
  expect_error(odiffr:::.validate_diff_color("#F00"), "diff_color")
  expect_error(odiffr:::.validate_diff_color("#FF00001"), "diff_color")
  expect_error(odiffr:::.validate_diff_color(NA_character_), "diff_color")
  expect_error(odiffr:::.validate_diff_color(c("#FF0000", "#00FF00")),
               "diff_color")
  expect_silent(odiffr:::.validate_diff_color("#ff00AA"))
  expect_silent(odiffr:::.validate_diff_color("FF0000"))
  expect_silent(odiffr:::.validate_diff_color(NULL))

  expect_error(odiffr:::.validate_diff_overlay(NA), "diff_overlay")
  expect_error(odiffr:::.validate_diff_overlay(1.5), "diff_overlay")
  expect_error(odiffr:::.validate_diff_overlay(-0.1), "diff_overlay")
  expect_error(odiffr:::.validate_diff_overlay("yes"), "diff_overlay")
  expect_error(odiffr:::.validate_diff_overlay(c(TRUE, FALSE)), "diff_overlay")
  expect_silent(odiffr:::.validate_diff_overlay(NULL))
  expect_silent(odiffr:::.validate_diff_overlay(TRUE))
  expect_silent(odiffr:::.validate_diff_overlay(FALSE))
  expect_silent(odiffr:::.validate_diff_overlay(0.5))
})

test_that(".validate_timeout rounds sub-second values up", {
  expect_equal(odiffr:::.validate_timeout(60), 60)
  expect_equal(odiffr:::.validate_timeout(0.5), 1)
  expect_equal(odiffr:::.validate_timeout(0.001), 1)
  expect_equal(odiffr:::.validate_timeout(1.2), 2)
  expect_equal(odiffr:::.validate_timeout(0), 0)
  expect_equal(odiffr:::.validate_timeout(Inf), 0)
  expect_error(odiffr:::.validate_timeout(-1), "timeout must be")
  expect_error(odiffr:::.validate_timeout(NA), "timeout must be")
  expect_error(odiffr:::.validate_timeout("10"), "timeout must be")
  expect_error(odiffr:::.validate_timeout(c(1, 2)), "timeout must be")
})

test_that(".format_regions handles odiff_region objects", {
  expect_equal(odiffr:::.format_regions(ignore_region(1, 2, 3, 4)),
               "1:2-3:4")
  expect_equal(
    odiffr:::.format_regions(list(ignore_region(1, 2, 3, 4),
                                  ignore_region(5, 6, 7, 8))),
    "1:2-3:4,5:6-7:8"
  )
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

# Regression tests ---------------------------------------------------------

test_that(".validate_diff_output rejects directory paths", {
  temp_dir <- withr::local_tempdir()
  expect_error(odiffr:::.validate_diff_output(paste0(temp_dir, "/")),
               "must be a file path, not a directory")
  expect_error(odiffr:::.validate_diff_output(paste0(temp_dir, "\\")),
               "must be a file path, not a directory")
  expect_error(odiffr:::.validate_diff_output("diffs/"), "not a directory")
})

test_that(".validate_diff_output returns an absolute path for new files", {
  temp_dir <- withr::local_tempdir()
  withr::local_dir(temp_dir)

  result <- odiffr:::.validate_diff_output(file.path("out", "new.png"))
  expect_false(file.exists(result))
  expect_true(startsWith(result, normalizePath(temp_dir, mustWork = FALSE)))
  expect_equal(basename(result), "new.png")
  expect_equal(normalizePath(dirname(result)),
               normalizePath(file.path(temp_dir, "out")))

  result <- odiffr:::.validate_diff_output("plain.png")
  expect_equal(result, normalizePath(file.path(normalizePath(temp_dir),
                                               "plain.png"), mustWork = FALSE))
})

test_that(".validate_timeout treats values beyond an integer as no timeout", {
  expect_equal(odiffr:::.validate_timeout(.Machine$integer.max),
               .Machine$integer.max)
  expect_equal(odiffr:::.validate_timeout(.Machine$integer.max + 1), 0)
  expect_equal(odiffr:::.validate_timeout(1e10), 0)
  expect_equal(odiffr:::.validate_timeout(1e300), 0)
})

test_that(".image_dimensions() reads PNG headers and other formats", {
  png_file <- create_test_image(30, 20, "red")
  on.exit(unlink(png_file), add = TRUE)
  expect_equal(odiffr:::.image_dimensions(png_file), c(30, 20))

  not_image <- tempfile(fileext = ".png")
  writeLines("not an image at all, just some text", not_image)
  on.exit(unlink(not_image), add = TRUE)
  expect_null(odiffr:::.image_dimensions(not_image))
  expect_null(suppressWarnings(
    odiffr:::.image_dimensions(tempfile(fileext = ".png"))
  ))

  skip_if_not_installed("magick")
  jpg <- tempfile(fileext = ".jpg")
  on.exit(unlink(jpg), add = TRUE)
  magick::image_write(magick::image_read(png_file), jpg, format = "jpeg")
  expect_equal(odiffr:::.image_dimensions(jpg), c(30, 20))
})
