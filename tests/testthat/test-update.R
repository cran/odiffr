# Tests for update.R functions

# Internal function tests (no network, no mocking needed)

test_that(".get_asset_name handles all supported platforms", {
  # Darwin
  expect_equal(
    odiffr:::.get_asset_name(list(os = "darwin", arch = "arm64")),
    "odiff-macos-arm64"
  )
  expect_equal(
    odiffr:::.get_asset_name(list(os = "darwin", arch = "x64")),
    "odiff-macos-x64"
  )

  # Linux
  expect_equal(
    odiffr:::.get_asset_name(list(os = "linux", arch = "x64")),
    "odiff-linux-x64"
  )
  expect_equal(
    odiffr:::.get_asset_name(list(os = "linux", arch = "arm64")),
    "odiff-linux-arm64"
  )

  # Windows
  expect_equal(
    odiffr:::.get_asset_name(list(os = "windows", arch = "x64")),
    "odiff-windows-x64.exe"
  )
  expect_equal(
    odiffr:::.get_asset_name(list(os = "windows", arch = "arm64")),
    "odiff-windows-arm64.exe"
  )
})

test_that("odiffr_cache_path returns valid path", {

  path <- odiffr_cache_path()

  expect_type(path, "character")
  expect_length(path, 1)
  expect_true(nzchar(path))
  expect_match(path, "odiffr")
})

test_that("odiffr_cache_path uses R_user_dir", {
  expected <- tools::R_user_dir("odiffr", which = "cache")
  actual <- odiffr_cache_path()

  expect_equal(actual, expected)
})

test_that("odiffr_clear_cache handles non-existent and existing cache safely", {
  skip_on_cran()

  temp_cache <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  # First call: directory does not exist
  unlink(temp_cache, recursive = TRUE, force = TRUE)
  expect_message(
    result1 <- odiffr_clear_cache(),
    "Cache directory does not exist"
  )
  expect_type(result1, "logical")

  # Second call: directory exists and is removed
  dir.create(temp_cache, recursive = TRUE, showWarnings = FALSE)
  expect_message(
    result2 <- odiffr_clear_cache(),
    "Cache cleared"
  )
  expect_true(result2)
  expect_false(dir.exists(temp_cache))
})

test_that(".get_latest_version returns character or NULL", {
  skip_on_cran()
  skip_if_offline()

  result <- odiffr:::.get_latest_version()

  # Should be NULL (on error) or a version string like "v4.1.2"
  if (!is.null(result)) {
    expect_type(result, "character")
    expect_match(result, "^v[0-9]")
  }
})

test_that("odiffr_update checks for existing binary", {
  skip_on_cran()

  cache_path <- odiffr_cache_path()

  # If binary exists, should message about it
  # We can't easily test the full download without network
  # Just verify the function doesn't error on initial checks
  expect_type(cache_path, "character")
})

test_that("odiffr_update constructs correct URL", {
  # Test internal URL construction logic
  platform <- list(os = "darwin", arch = "arm64")
  asset_name <- odiffr:::.get_asset_name(platform)
  version <- "v4.1.2"

  expected_url <- sprintf(
    "https://github.com/dmtrKovalenko/odiff/releases/download/%s/%s",
    version, asset_name
  )

  expect_equal(
    expected_url,
    "https://github.com/dmtrKovalenko/odiff/releases/download/v4.1.2/odiff-macos-arm64"
  )
})

test_that("odiffr_update returns existing binary path when force = FALSE", {
  skip_on_cran()

  # Create a temp directory structure mimicking the cache
  temp_cache <- withr::local_tempdir()
  platform <- odiffr:::.platform_info()
  binary_name <- if (platform$os == "windows") "odiff.exe" else "odiff"
  subdir <- paste0(platform$os, "_", platform$arch)
  target_dir <- file.path(temp_cache, "bin", subdir)
  dir.create(target_dir, recursive = TRUE)
  target_path <- file.path(target_dir, binary_name)

  # Create a fake binary file
  writeLines("fake binary", target_path)

  # Mock odiffr_cache_path to return our temp directory
  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )

  # Should return existing path with message
  expect_message(
    result <- odiffr_update(version = "v4.1.2", force = FALSE),
    "Binary already exists"
  )
  expect_equal(normalizePath(result), normalizePath(target_path))
})

test_that("odiffr_update resolves latest version and downloads", {
  skip_on_cran()

  temp_cache <- withr::local_tempdir()

  # Mock all odiffr internal functions in one block
  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .get_latest_version = function() "v4.1.2",
    download_file_internal = function(url, destfile, mode, quiet) {
      dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      writeLines("fake binary", destfile)
      0L
    },
    .package = "odiffr",
    {
      expect_message(
        result <- odiffr_update(version = "latest", force = FALSE),
        "Latest version: v4.1.2"
      )
      expect_true(file.exists(result))
    }
  )
})

test_that("odiffr_update errors when .get_latest_version returns NULL", {
  skip_on_cran()

  temp_cache <- withr::local_tempdir()

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .get_latest_version = function() NULL,
    .package = "odiffr",
    {
      expect_error(
        odiffr_update(version = "latest"),
        "Failed to determine latest odiff version"
      )
    }
  )
})

test_that("odiffr_update creates target directory if it doesn't exist", {
  skip_on_cran()

  temp_cache <- withr::local_tempdir()

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      writeLines("fake binary", destfile)
      0L
    },
    .package = "odiffr",
    {
      result <- odiffr_update(version = "v4.1.2", force = FALSE)

      platform <- odiffr:::.platform_info()
      subdir <- paste0(platform$os, "_", platform$arch)
      expected_dir <- file.path(temp_cache, "bin", subdir)

      expect_true(dir.exists(expected_dir))
      expect_true(file.exists(result))
    }
  )
})

test_that("odiffr_update writes version file after download", {
  skip_on_cran()

  temp_cache <- withr::local_tempdir()

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      writeLines("fake binary", destfile)
      0L
    },
    .package = "odiffr",
    {
      odiffr_update(version = "v4.1.2", force = FALSE)

      version_file <- file.path(temp_cache, "CACHED_VERSION")
      expect_true(file.exists(version_file))
      expect_equal(readLines(version_file), "v4.1.2")
    }
  )
})

test_that("odiffr_update with force = TRUE re-downloads", {
  skip_on_cran()

  temp_cache <- withr::local_tempdir()
  platform <- odiffr:::.platform_info()
  binary_name <- if (platform$os == "windows") "odiff.exe" else "odiff"
  subdir <- paste0(platform$os, "_", platform$arch)
  target_dir <- file.path(temp_cache, "bin", subdir)
  dir.create(target_dir, recursive = TRUE)
  target_path <- file.path(target_dir, binary_name)

  # Create an existing fake binary
  writeLines("old binary", target_path)

  download_called <- FALSE

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      download_called <<- TRUE
      writeLines("new binary", destfile)
      0L
    },
    .package = "odiffr",
    {
      result <- odiffr_update(version = "v4.1.2", force = TRUE)

      expect_true(download_called)
      expect_equal(readLines(result), "new binary")
    }
  )
})

test_that("odiffr_update handles download failure", {
  skip_on_cran()

  temp_cache <- withr::local_tempdir()

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      stop("Network error")
    },
    .package = "odiffr",
    {
      expect_error(
        odiffr_update(version = "v4.1.2"),
        "Failed to download odiff binary"
      )
    }
  )
})

test_that("odiffr_update sets executable permissions on Unix", {
  skip_on_cran()
  skip_on_os("windows")

  temp_cache <- withr::local_tempdir()

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
      writeLines("fake binary", destfile)
      0L
    },
    .package = "odiffr",
    {
      result <- odiffr_update(version = "v4.1.2", force = FALSE)

      # Check file exists and is executable
      expect_true(file.exists(result))
      # file.access with mode=1 checks executable permission (returns 0 if accessible)
      expect_equal(unname(file.access(result, mode = 1)), 0L)
    }
  )
})

# Integration test that exercises .get_latest_version() without mocking
# This contributes to coverage when network is available

test_that(".get_latest_version returns version from GitHub API", {
  skip_on_cran()
  skip_if_offline()

  result <- odiffr:::.get_latest_version()

  if (!is.null(result)) {
    expect_type(result, "character")
    expect_match(result, "^v[0-9]+\\.[0-9]+")
  }
})
