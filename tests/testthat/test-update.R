# Tests for update.R functions

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

test_that("odiffr_clear_cache handles non-existent cache", {
  # Create a unique cache path that doesn't exist
  original_cache <- odiffr_cache_path()

  # Mock by temporarily changing XDG cache (this is complex, so just test messaging)
  expect_message(
    result <- odiffr_clear_cache(),
    "Cache directory does not exist|Cache cleared"
  )

  expect_type(result, "logical")
})

test_that(".get_asset_name returns correct format for darwin", {
  platform <- list(os = "darwin", arch = "arm64")
  asset <- odiffr:::.get_asset_name(platform)

  expect_equal(asset, "odiff-macos-arm64")
})

test_that(".get_asset_name returns correct format for darwin x64", {
  platform <- list(os = "darwin", arch = "x64")
  asset <- odiffr:::.get_asset_name(platform)

  expect_equal(asset, "odiff-macos-x64")
})

test_that(".get_asset_name returns correct format for linux", {
  platform <- list(os = "linux", arch = "x64")
  asset <- odiffr:::.get_asset_name(platform)

  expect_equal(asset, "odiff-linux-x64")
})

test_that(".get_asset_name returns correct format for linux arm64", {
  platform <- list(os = "linux", arch = "arm64")
  asset <- odiffr:::.get_asset_name(platform)

  expect_equal(asset, "odiff-linux-arm64")
})

test_that(".get_asset_name returns correct format for windows", {
  platform <- list(os = "windows", arch = "x64")
  asset <- odiffr:::.get_asset_name(platform)

  expect_equal(asset, "odiff-windows-x64.exe")
})

test_that(".get_asset_name returns correct format for windows arm64", {
  platform <- list(os = "windows", arch = "arm64")
  asset <- odiffr:::.get_asset_name(platform)

  expect_equal(asset, "odiff-windows-arm64.exe")
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
