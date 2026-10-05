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
  expect_named(info, c("os", "arch", "path", "version", "source", "shim"))
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

test_that(".parse_version handles stdout and stderr style output", {
  expect_equal(
    odiffr:::.parse_version(
      "odiff 4.5.0 - SIMD first pixel-by-pixel image comparison tool"
    ),
    "4.5.0"
  )
  expect_equal(odiffr:::.parse_version(c("", "noise", "odiff v3.1.2")),
               "3.1.2")
  expect_true(is.na(odiffr:::.parse_version(character())))
  expect_true(is.na(odiffr:::.parse_version("something else")))
})

test_that("odiff_version caches per binary path", {
  skip_on_os("windows")

  dir <- withr::local_tempdir()
  counter <- file.path(dir, "calls")
  make_fake <- function(name, version) {
    path <- file.path(dir, name)
    writeLines(c(
      "#!/bin/sh",
      sprintf("echo call >> '%s'", counter),
      sprintf("echo 'odiff %s - fake' >&2", version)
    ), path)
    Sys.chmod(path, "0755")
    path
  }
  fake1 <- make_fake("odiff1", "9.8.7")
  fake2 <- make_fake("odiff2", "1.2.3")

  old_cache <- odiffr:::.odiffr_env$version_cache
  withr::defer(assign("version_cache", old_cache,
                      envir = odiffr:::.odiffr_env))
  assign("version_cache", NULL, envir = odiffr:::.odiffr_env)

  withr::local_options(odiffr.path = fake1)
  expect_equal(odiff_version(), "9.8.7")
  expect_equal(odiff_version(), "9.8.7")
  expect_length(readLines(counter), 1)

  # Changing the binary path invalidates the cache
  options(odiffr.path = fake2)
  expect_equal(odiff_version(), "1.2.3")
  expect_length(readLines(counter), 2)
})

test_that("odiff_run(enable_asm = TRUE) does not respawn --version", {
  skip_if_no_odiff()

  odiff_version()  # warm the cache
  calls <- 0L
  testthat::local_mocked_bindings(
    .parse_version = function(lines) {
      calls <<- calls + 1L
      NA_character_
    },
    .package = "odiffr"
  )

  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)
  suppressWarnings(odiff_run(img, img, enable_asm = TRUE))
  suppressWarnings(odiff_run(img, img, enable_asm = TRUE))
  expect_equal(calls, 0L)
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

# --- npm launcher resolution -------------------------------------------------

# Write a minimal file with an ELF header (a "native binary" for detection)
write_fake_native <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(c(as.raw(c(0x7f, 0x45, 0x4c, 0x46)), as.raw(rep(0, 60))), path)
  Sys.chmod(path, "0755")
  path
}

# Write an npm-style Node launcher script
write_node_shim <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(c(
    "#!/usr/bin/env node",
    "const { spawnSync } = require(\"child_process\");"
  ), path)
  Sys.chmod(path, "0755")
  path
}

# Build an odiff-bin >= 4.4 layout under `root`; returns list(shim, binary)
make_npm_layout <- function(root, hoisted = FALSE, binary = TRUE) {
  pkg <- file.path(root, "lib", "node_modules", "odiff-bin")
  shim <- write_node_shim(file.path(pkg, "bin", "odiff"))
  key <- odiffr:::.npm_platform_key()
  exe <- if (.Platform$OS.type == "windows") "odiff.exe" else "odiff"
  bin_path <- if (hoisted) {
    file.path(root, "lib", "node_modules", "@odiff", key, exe)
  } else {
    file.path(pkg, "node_modules", "@odiff", key, exe)
  }
  if (binary) write_fake_native(bin_path)
  list(shim = shim, binary = bin_path)
}

# Make Sys.which("odiff") return `path`; reset caches/options for the test
local_fake_path <- function(path, env = parent.frame()) {
  old_cache <- odiffr:::.odiffr_env$npm_cache
  withr::defer(assign("npm_cache", old_cache, envir = odiffr:::.odiffr_env),
               envir = env)
  assign("npm_cache", NULL, envir = odiffr:::.odiffr_env)
  withr::local_options(odiffr.path = NULL, odiffr.resolve_npm = NULL,
                       .local_envir = env)
  testthat::local_mocked_bindings(
    Sys.which = function(names) stats::setNames(path, names),
    .package = "base",
    .env = env
  )
}

test_that(".is_npm_shim detects Node launchers only", {
  dir <- withr::local_tempdir()
  shim <- write_node_shim(file.path(dir, "shim"))
  native <- write_fake_native(file.path(dir, "native"))
  sh <- file.path(dir, "sh")
  writeLines(c("#!/bin/sh", "exec /usr/bin/odiff \"$@\""), sh)
  empty <- file.path(dir, "empty")
  file.create(empty)

  expect_true(odiffr:::.is_npm_shim(shim, windows = FALSE))
  expect_false(odiffr:::.is_npm_shim(native, windows = FALSE))
  expect_false(odiffr:::.is_npm_shim(sh, windows = FALSE))
  expect_false(odiffr:::.is_npm_shim(empty, windows = FALSE))
  expect_false(odiffr:::.is_npm_shim(file.path(dir, "nope"), windows = FALSE))

  expect_true(odiffr:::.is_native_binary(native))
  expect_false(odiffr:::.is_native_binary(shim))
  expect_false(odiffr:::.is_native_binary(empty))
})

test_that(".is_npm_shim detects Windows npm wrappers", {
  dir <- withr::local_tempdir()
  cmd <- file.path(dir, "odiff.cmd")
  writeLines(c(
    "@ECHO off",
    "SETLOCAL",
    "IF EXIST \"%dp0%\\node.exe\" (SET \"_prog=%dp0%\\node.exe\")",
    "\"%_prog%\" \"%dp0%\\node_modules\\odiff-bin\\bin\\odiff\" %*"
  ), cmd)
  ps1 <- file.path(dir, "odiff.ps1")
  writeLines(c("#!/usr/bin/env pwsh",
               "& \"node$exe\" \"$basedir/node_modules/odiff-bin/bin/odiff\""),
             ps1)
  sh <- file.path(dir, "odiff")
  writeLines(c("#!/bin/sh",
               "exec node \"$basedir/node_modules/odiff-bin/bin/odiff\" \"$@\""),
             sh)
  other_cmd <- file.path(dir, "other.cmd")
  writeLines("@ECHO off", other_cmd)

  expect_true(odiffr:::.is_npm_shim(cmd, windows = TRUE))
  expect_true(odiffr:::.is_npm_shim(ps1, windows = TRUE))
  expect_true(odiffr:::.is_npm_shim(sh, windows = TRUE))
  expect_false(odiffr:::.is_npm_shim(other_cmd, windows = TRUE))
  # Outside Windows, a .cmd file is never a launcher
  expect_false(odiffr:::.is_npm_shim(cmd, windows = FALSE))
})

test_that(".npm_platform_key maps OS and architecture names", {
  expect_equal(odiffr:::.npm_platform_key("windows", "x64"), "win32-x64")
  expect_equal(odiffr:::.npm_platform_key("darwin", "arm64"), "darwin-arm64")
  expect_equal(odiffr:::.npm_platform_key("linux", "x64"), "linux-x64")
})

test_that(".npm_binary_candidates covers the Windows global layout", {
  root <- file.path(tempdir(), "AppData", "Roaming", "npm")
  shim <- file.path(root, "odiff.cmd")
  cands <- odiffr:::.npm_binary_candidates(shim, windows = TRUE,
                                           arch = "x64")
  root_n <- normalizePath(root, winslash = "/", mustWork = FALSE)
  expect_true(file.path(root_n, "node_modules", "odiff-bin", "node_modules",
                        "@odiff", "win32-x64", "odiff.exe") %in% cands)
  expect_true(file.path(root_n, "node_modules", "@odiff", "win32-x64",
                        "odiff.exe") %in% cands)
  expect_true(file.path(root_n, "node_modules", "odiff-bin", "bin",
                        "odiff.exe") %in% cands)
})

test_that(".resolve_npm_shim resolves a Windows .cmd layout", {
  root <- withr::local_tempdir()
  cmd <- file.path(root, "odiff.cmd")
  writeLines(c(
    "@ECHO off",
    "\"%_prog%\" \"%dp0%\\node_modules\\odiff-bin\\bin\\odiff\" %*"
  ), cmd)
  bin <- write_fake_native(file.path(root, "node_modules", "@odiff",
                                     "win32-x64", "odiff.exe"))

  resolved <- odiffr:::.resolve_npm_shim(cmd, windows = TRUE, arch = "x64")
  expect_equal(resolved, normalizePath(bin))

  # Wrong architecture: nothing found
  expect_null(odiffr:::.resolve_npm_shim(cmd, windows = TRUE,
                                         arch = "arm64"))
})

test_that("find_odiff resolves an npm shim to the native binary", {
  root <- withr::local_tempdir()
  layout <- make_npm_layout(root)
  local_fake_path(layout$shim)

  expect_equal(find_odiff(), normalizePath(layout$binary))

  info <- odiff_info()
  expect_equal(info$path, normalizePath(layout$binary))
  expect_equal(info$shim, normalizePath(layout$shim))
  expect_equal(info$source, "system")
  expect_output(print(info), "Via npm:")
})

test_that("find_odiff follows a symlinked global npm launcher", {
  skip_on_os("windows")
  root <- withr::local_tempdir()
  layout <- make_npm_layout(root, hoisted = TRUE)
  dir.create(file.path(root, "bin"))
  link <- file.path(root, "bin", "odiff")
  file.symlink(layout$shim, link)
  local_fake_path(link)

  expect_equal(find_odiff(), normalizePath(layout$binary))
})

test_that("find_odiff uses odiff-bin/bin/odiff.exe from old layouts", {
  root <- withr::local_tempdir()
  pkg <- file.path(root, "lib", "node_modules", "odiff-bin")
  # Old layout: the launcher on the PATH is a node script in the prefix,
  # bin/odiff.exe inside the package is the native binary
  shim <- write_node_shim(file.path(root, "bin", "odiff"))
  exe <- write_fake_native(file.path(pkg, "bin", "odiff.exe"))
  local_fake_path(shim)

  expect_equal(find_odiff(), normalizePath(exe))
})

test_that("old layouts with a native binary on the PATH are used as-is", {
  root <- withr::local_tempdir()
  exe <- write_fake_native(file.path(root, "lib", "node_modules", "odiff-bin",
                                     "bin", "odiff.exe"))
  local_fake_path(exe)

  expect_equal(find_odiff(), normalizePath(exe))
  expect_true(is.na(odiff_info()$shim))
})

test_that("find_odiff falls back to the shim when no binary is found", {
  root <- withr::local_tempdir()
  layout <- make_npm_layout(root, binary = FALSE)
  # A script where the binary should be is not accepted
  write_node_shim(layout$binary)
  local_fake_path(layout$shim)

  expect_equal(find_odiff(), normalizePath(layout$shim))
  expect_true(is.na(odiff_info()$shim))
})

test_that("options(odiffr.resolve_npm = FALSE) disables resolution", {
  root <- withr::local_tempdir()
  layout <- make_npm_layout(root)
  local_fake_path(layout$shim)
  withr::local_options(odiffr.resolve_npm = FALSE)

  expect_equal(find_odiff(), normalizePath(layout$shim))
})

test_that("options(odiffr.path) pointing at a shim is not resolved", {
  root <- withr::local_tempdir()
  layout <- make_npm_layout(root)
  local_fake_path("")
  withr::local_options(odiffr.path = layout$shim)

  expect_equal(find_odiff(), normalizePath(layout$shim))
  expect_true(is.na(odiff_info()$shim))
})

test_that("npm resolution is cached per shim and refreshed on change", {
  root <- withr::local_tempdir()
  layout <- make_npm_layout(root)
  local_fake_path(layout$shim)

  calls <- 0L
  real_resolve <- odiffr:::.resolve_npm_shim
  testthat::local_mocked_bindings(
    .resolve_npm_shim = function(...) {
      calls <<- calls + 1L
      real_resolve(...)
    },
    .package = "odiffr"
  )

  find_odiff()
  find_odiff()
  expect_equal(calls, 1L)

  # Removing the resolved binary invalidates the cached entry
  unlink(layout$binary)
  expect_equal(find_odiff(), normalizePath(layout$shim))
  expect_equal(calls, 2L)
})

test_that("a resolved real binary runs comparisons", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_no_odiff()
  native <- find_odiff()
  skip_if_not(odiffr:::.is_native_binary(native), "no native odiff binary")

  root <- withr::local_tempdir()
  layout <- make_npm_layout(root, binary = FALSE)
  dir.create(dirname(layout$binary), recursive = TRUE)
  file.copy(native, layout$binary)
  Sys.chmod(layout$binary, "0755")
  local_fake_path(layout$shim)

  old_cache <- odiffr:::.odiffr_env$version_cache
  withr::defer(assign("version_cache", old_cache,
                      envir = odiffr:::.odiffr_env))

  expect_equal(find_odiff(), normalizePath(layout$binary))
  expect_match(odiff_version(), "^[0-9]+\\.[0-9]+\\.[0-9]+$")

  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)
  expect_true(odiff_run(img, img)$match)
})

test_that("the npm install on this machine resolves to its native binary", {
  skip_on_cran()
  skip_if_no_odiff()
  withr::local_options(odiffr.path = NULL, odiffr.resolve_npm = NULL)
  sys <- Sys.which("odiff")
  skip_if_not(nzchar(sys), "odiff not on PATH")
  shim <- normalizePath(unname(sys))
  skip_if_not(odiffr:::.is_npm_shim(shim), "odiff on PATH is not an npm shim")

  path <- find_odiff()
  expect_false(identical(path, shim))
  expect_true(odiffr:::.is_native_binary(path))
  expect_equal(odiff_info()$shim, shim)

  img1 <- create_test_image(20, 20, "red")
  img2 <- create_test_image(20, 20, "blue")
  on.exit(unlink(c(img1, img2)), add = TRUE)
  expect_true(odiff_run(img1, img1)$match)
  expect_equal(odiff_run(img1, img2)$reason, "pixel-diff")
})
