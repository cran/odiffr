test_that("platform detection works", {
  info <- odiffr:::.platform_info()

  expect_type(info, "list")
  expect_named(info, c("os", "arch"))
  expect_true(info$os %in% c("darwin", "linux", "windows", "unknown"))
  expect_true(info$arch %in% c("x64", "arm64", "unknown"))
})

test_that("find_odiff returns a valid path", {
  skip_if_no_odiff()

  path <- find_odiff()

  expect_type(path, "character")
  expect_true(nzchar(path))
  expect_true(file.exists(path))
})
test_that("odiff_available returns logical", {
  result <- odiff_available()

  expect_type(result, "logical")
  expect_length(result, 1)
})

test_that("odiff_version returns character or NA", {
  result <- odiff_version()

  expect_type(result, "character")
  expect_length(result, 1)
})

test_that("odiff_info returns correct structure", {
  info <- odiff_info()

  expect_s3_class(info, "odiff_info")
  expect_named(info, c("os", "arch", "path", "version", "source"))
  expect_true(info$os %in% c("darwin", "linux", "windows"))
  expect_true(info$arch %in% c("x64", "arm64"))
})

test_that("odiff_info prints correctly", {
  info <- odiff_info()

  expect_output(print(info), "odiffr configuration")
  expect_output(print(info), "OS:")
  expect_output(print(info), "Arch:")
})

test_that("options(odiffr.path) is respected", {
  skip_if_no_odiff()

  original <- getOption("odiffr.path")
  on.exit(options(odiffr.path = original), add = TRUE)

  # Get the current binary path first
  current_path <- find_odiff()

  # Set option to the same path (to ensure it's valid)
  options(odiffr.path = current_path)

  path <- find_odiff()
  expect_equal(path, normalizePath(current_path, mustWork = TRUE))

  # Verify source is "option"
  source <- odiffr:::.binary_source()
  expect_equal(source, "option")
})

test_that("invalid odiffr.path warns and falls back", {
  original <- getOption("odiffr.path")
  on.exit(options(odiffr.path = original), add = TRUE)

  # Set option to invalid path
  options(odiffr.path = "/nonexistent/path/to/odiff")

  # Should warn about invalid path (may also error if no fallback available)
  warned <- FALSE
  tryCatch(
    withCallingHandlers(
      find_odiff(),
      warning = function(w) {
        if (grepl("odiffr.path option is set but file not found", w$message)) {
          warned <<- TRUE
        }
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  expect_true(warned)
})
