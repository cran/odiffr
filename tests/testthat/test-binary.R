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

test_that(".cached_binary returns NULL when cache directory doesn't exist", {
  # Use tempfile() to get a random non-existent path

  temp_cache <- tempfile(pattern = "odiffr_test_cache_")

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  result <- odiffr:::.cached_binary()
  expect_null(result)
})

test_that(".cached_binary returns NULL when binary doesn't exist in cache", {
  temp_cache <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  result <- odiffr:::.cached_binary()
  expect_null(result)
})

test_that(".cached_binary returns path when binary exists in cache", {
  temp_cache <- withr::local_tempdir()
  platform <- odiffr:::.platform_info()
  binary_name <- if (platform$os == "windows") "odiff.exe" else "odiff"
  subdir <- paste0(platform$os, "_", platform$arch)
  target_dir <- file.path(temp_cache, "bin", subdir)
  dir.create(target_dir, recursive = TRUE)
  target_path <- file.path(target_dir, binary_name)

  # Create fake binary
  writeLines("fake binary", target_path)

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  result <- odiffr:::.cached_binary()
  expect_equal(normalizePath(result), normalizePath(target_path))
})

test_that(".binary_source returns 'cached' for cached binary", {
  skip_on_cran()

  original_opt <- getOption("odiffr.path")
  on.exit(options(odiffr.path = original_opt), add = TRUE)
  options(odiffr.path = NULL)

  temp_cache <- withr::local_tempdir()
  platform <- odiffr:::.platform_info()
  binary_name <- if (platform$os == "windows") "odiff.exe" else "odiff"
  subdir <- paste0(platform$os, "_", platform$arch)
  target_dir <- file.path(temp_cache, "bin", subdir)
  dir.create(target_dir, recursive = TRUE)
  target_path <- file.path(target_dir, binary_name)

  # Create fake binary
  writeLines("fake binary", target_path)

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  # Also mock Sys.which to return empty (no system binary)
  local_mocked_bindings(
    Sys.which = function(names) {
      result <- ""
      names(result) <- names
      result
    },
    .package = "base"
  )

  result <- odiffr:::.binary_source()
  expect_equal(result, "cached")
})

test_that(".binary_source returns NA when no binary found", {
  original_opt <- getOption("odiffr.path")
  on.exit(options(odiffr.path = original_opt), add = TRUE)
  options(odiffr.path = NULL)

  temp_cache <- withr::local_tempdir()  # Empty cache

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  # Mock Sys.which to return empty
  local_mocked_bindings(
    Sys.which = function(names) {
      result <- ""
      names(result) <- names
      result
    },
    .package = "base"
  )

  result <- odiffr:::.binary_source()
  expect_true(is.na(result))
})

test_that("find_odiff falls back to cached binary", {
  skip_on_cran()

  original_opt <- getOption("odiffr.path")
  on.exit(options(odiffr.path = original_opt), add = TRUE)
  options(odiffr.path = NULL)

  temp_cache <- withr::local_tempdir()
  platform <- odiffr:::.platform_info()
  binary_name <- if (platform$os == "windows") "odiff.exe" else "odiff"
  subdir <- paste0(platform$os, "_", platform$arch)
  target_dir <- file.path(temp_cache, "bin", subdir)
  dir.create(target_dir, recursive = TRUE)
  target_path <- file.path(target_dir, binary_name)

  # Create fake binary
  writeLines("fake binary", target_path)

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  # Mock Sys.which to return empty (no system binary)
  local_mocked_bindings(
    Sys.which = function(names) {
      result <- ""
      names(result) <- names
      result
    },
    .package = "base"
  )

  result <- find_odiff()
  expect_equal(normalizePath(result), normalizePath(target_path))
})

test_that("find_odiff errors when no binary found anywhere", {
  original_opt <- getOption("odiffr.path")
  on.exit(options(odiffr.path = original_opt), add = TRUE)
  options(odiffr.path = NULL)

  temp_cache <- withr::local_tempdir()  # Empty cache

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  # Mock Sys.which to return empty
  local_mocked_bindings(
    Sys.which = function(names) {
      result <- ""
      names(result) <- names
      result
    },
    .package = "base"
  )

  expect_error(
    find_odiff(),
    "odiff binary not found"
  )
})

test_that("odiff_version returns NA when odiff not available", {
  testthat::local_mocked_bindings(
    odiff_available = function() FALSE,
    .package = "odiffr"
  )

  result <- odiff_version()
  expect_true(is.na(result))
})

test_that("odiff_info handles missing binary gracefully", {
  original_opt <- getOption("odiffr.path")
  on.exit(options(odiffr.path = original_opt), add = TRUE)
  options(odiffr.path = NULL)

  temp_cache <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  local_mocked_bindings(
    Sys.which = function(names) {
      result <- ""
      names(result) <- names
      result
    },
    .package = "base"
  )

  info <- odiff_info()

  expect_s3_class(info, "odiff_info")
  expect_true(is.na(info$path))
  expect_true(is.na(info$source))
})

test_that("print.odiff_info handles missing path and version", {
  info <- structure(
    list(
      os = "darwin",
      arch = "arm64",
      path = NA_character_,
      version = NA_character_,
      source = NA_character_
    ),
    class = c("odiff_info", "list")
  )

  expect_output(print(info), "<not found>")
  expect_output(print(info), "<unknown>")
  expect_output(print(info), "<none>")
})
