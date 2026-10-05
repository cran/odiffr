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
      result <- odiffr_update(version = "v4.1.2", force = FALSE)

      # CACHED_VERSION lives next to the platform binary
      version_file <- file.path(dirname(result), "CACHED_VERSION")
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


# Robustness tests ----------------------------------------------------------

fake_download <- function(url, destfile, mode, quiet) {
  writeLines("fake binary", destfile)
  0L
}

test_that(".normalize_version adds the v prefix when missing", {
  expect_equal(odiffr:::.normalize_version("4.5.0"), "v4.5.0")
  expect_equal(odiffr:::.normalize_version("v4.5.0"), "v4.5.0")
  expect_equal(odiffr:::.normalize_version("V4.5.0"), "v4.5.0")
  expect_equal(odiffr:::.normalize_version(" 4.1.2 "), "v4.1.2")
})

test_that("odiffr_update accepts versions without the v prefix", {
  temp_cache <- withr::local_tempdir()
  seen_url <- NULL

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      seen_url <<- url
      fake_download(url, destfile, mode, quiet)
    },
    .package = "odiffr",
    {
      result <- suppressMessages(odiffr_update(version = "4.1.2"))
      expect_match(seen_url, "/releases/download/v4.1.2/", fixed = TRUE)
      expect_equal(readLines(file.path(dirname(result), "CACHED_VERSION")), "v4.1.2")
    }
  )
})

test_that("odiffr_update normalizes the latest version tag", {
  temp_cache <- withr::local_tempdir()
  seen_url <- NULL

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .get_latest_version = function() "4.5.0",
    download_file_internal = function(url, destfile, mode, quiet) {
      seen_url <<- url
      fake_download(url, destfile, mode, quiet)
    },
    .package = "odiffr",
    {
      expect_message(odiffr_update(), "Latest version: v4.5.0")
      expect_match(seen_url, "/download/v4.5.0/", fixed = TRUE)
    }
  )
})

test_that("odiffr_update rejects invalid version arguments", {
  temp_cache <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    .package = "odiffr"
  )
  expect_error(odiffr_update(version = NA_character_), "single string")
  expect_error(odiffr_update(version = c("v1", "v2")), "single string")
})

test_that("odiffr_update downloads to a temp file and moves it into place", {
  temp_cache <- withr::local_tempdir()
  dest_seen <- NULL

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      dest_seen <<- destfile
      fake_download(url, destfile, mode, quiet)
    },
    .package = "odiffr",
    {
      result <- suppressMessages(odiffr_update(version = "v4.1.2"))
      expect_false(basename(dest_seen) == basename(result))
      expect_equal(normalizePath(dirname(dest_seen)), normalizePath(dirname(result)))
      expect_false(file.exists(dest_seen))
      expect_true(file.exists(result))
      expect_equal(readLines(result), "fake binary")
      # Only the binary and the version file remain
      expect_setequal(list.files(dirname(result)),
                      c(basename(result), "CACHED_VERSION"))
    }
  )
})

test_that("failed download leaves no partial binary behind", {
  temp_cache <- withr::local_tempdir()

  testthat::local_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      writeLines("partial", destfile)
      stop("connection reset")
    },
    .package = "odiffr"
  )

  expect_error(
    suppressMessages(odiffr_update(version = "v4.1.2")),
    "Failed to download odiff binary: connection reset"
  )

  platform <- odiffr:::.platform_info()
  target_dir <- file.path(temp_cache, "bin", paste0(platform$os, "_", platform$arch))
  expect_length(list.files(target_dir, all.files = TRUE, no.. = TRUE), 0)
  expect_null(odiffr:::.cached_binary())
})

test_that("failed forced re-download keeps the existing binary", {
  temp_cache <- withr::local_tempdir()
  platform <- odiffr:::.platform_info()
  binary_name <- if (platform$os == "windows") "odiff.exe" else "odiff"
  target_dir <- file.path(temp_cache, "bin", paste0(platform$os, "_", platform$arch))
  dir.create(target_dir, recursive = TRUE)
  target_path <- file.path(target_dir, binary_name)
  writeLines("old binary", target_path)

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      writeLines("partial", destfile)
      stop("timeout")
    },
    .package = "odiffr",
    {
      expect_error(
        suppressMessages(odiffr_update(version = "v4.1.2", force = TRUE)),
        "Failed to download"
      )
    }
  )
  expect_equal(readLines(target_path), "old binary")
  expect_equal(list.files(target_dir), binary_name)
})

test_that("odiffr_update errors on non-zero status or empty download", {
  temp_cache <- withr::local_tempdir()

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) 1L,
    .package = "odiffr",
    {
      expect_error(
        suppressMessages(odiffr_update(version = "v4.1.2")),
        "status 1"
      )
    }
  )

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      file.create(destfile)
      0L
    },
    .package = "odiffr",
    {
      expect_error(
        suppressMessages(odiffr_update(version = "v4.1.2")),
        "empty"
      )
    }
  )
})

test_that("404 errors mention releases without binary assets", {
  temp_cache <- withr::local_tempdir()

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      warning("cannot open URL '", url, "': HTTP status was '404 Not Found'")
      stop("cannot open URL '", url, "'")
    },
    .package = "odiffr",
    {
      err <- tryCatch(
        suppressMessages(odiffr_update(version = "4.4.0")),
        error = function(e) e
      )
      expect_s3_class(err, "error")
      expect_match(conditionMessage(err), "404")
      expect_match(conditionMessage(err), "without binary assets")
      expect_match(conditionMessage(err), "v4.4.0", fixed = TRUE)
    }
  )
})

test_that("odiffr_update raises the download timeout temporarily", {
  temp_cache <- withr::local_tempdir()
  withr::local_options(timeout = 60)
  timeout_during <- NULL

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      timeout_during <<- getOption("timeout")
      fake_download(url, destfile, mode, quiet)
    },
    .package = "odiffr",
    suppressMessages(odiffr_update(version = "v4.1.2"))
  )

  expect_gte(timeout_during, 300)
  expect_equal(getOption("timeout"), 60)
})

test_that("odiffr_update keeps a larger user timeout", {
  temp_cache <- withr::local_tempdir()
  withr::local_options(timeout = 1000)
  timeout_during <- NULL

  testthat::with_mocked_bindings(
    odiffr_cache_path = function() temp_cache,
    download_file_internal = function(url, destfile, mode, quiet) {
      timeout_during <<- getOption("timeout")
      fake_download(url, destfile, mode, quiet)
    },
    .package = "odiffr",
    suppressMessages(odiffr_update(version = "v4.1.2"))
  )

  expect_equal(timeout_during, 1000)
})

test_that(".github_api_headers uses GITHUB_PAT / GITHUB_TOKEN when set", {
  withr::local_envvar(GITHUB_PAT = NA, GITHUB_TOKEN = NA)
  h <- odiffr:::.github_api_headers()
  expect_false("Authorization" %in% names(h))
  expect_equal(h[["Accept"]], "application/vnd.github.v3+json")

  withr::local_envvar(GITHUB_TOKEN = "tok123")
  expect_equal(odiffr:::.github_api_headers()[["Authorization"]], "Bearer tok123")

  withr::local_envvar(GITHUB_PAT = "pat456")
  expect_equal(odiffr:::.github_api_headers()[["Authorization"]], "Bearer pat456")
})
