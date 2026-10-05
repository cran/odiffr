#' Find the odiff Binary
#'
#' Locates the odiff executable using a priority-based search:
#' 1. User-specified path via `options(odiffr.path = "...")`
#' 2. System PATH (`Sys.which("odiff")`)
#' 3. Cached binary from [install_odiff()] (or `odiffr_update()`)
#'
#' @details
#' **Installing odiff.** If odiff cannot be found and the R session is
#' interactive, `find_odiff()` (and so [compare_images()], [odiff_run()] and
#' the other functions that need the binary) offers once per session to
#' download the latest odiff release to the user cache with
#' [install_odiff()]. Nothing is ever downloaded without asking: the offer is
#' never made in non-interactive sessions, while running tests with testthat,
#' while knitting, or during `R CMD check`. Set
#' `options(odiffr.ask_install = FALSE)` to turn the offer off. If the offer
#' is declined, `find_odiff()` signals an error explaining how to install
#' odiff. [odiff_available()] never makes the offer.
#'

#' **npm installs.** Since odiff 4.4, `npm install -g odiff-bin` puts a
#' small Node.js launcher script on the PATH, which starts Node and then
#' spawns the native binary shipped in an `@odiff/<platform>-<arch>`
#' package. Starting Node adds tens of milliseconds to every comparison, so
#' when the odiff found on the PATH is such a launcher (a `#!` script that
#' runs `node`, or an npm `.cmd`/`.ps1` wrapper on Windows), `find_odiff()`
#' looks for the native binary inside the npm installation and returns it
#' instead. For older `odiff-bin` releases, the native `bin/odiff.exe` inside
#' the package is used. If no native binary can be found, the launcher itself
#' is returned, so a working setup is never broken. The lookup is cached per
#' launcher path and redone when the launcher file changes.
#'
#' Set `options(odiffr.resolve_npm = FALSE)` to disable this and always use
#' the PATH entry as-is. A path given via `options(odiffr.path = ...)` is
#' always used exactly as specified and is never resolved.
#'
#' @return Character string with the absolute path to the odiff executable.
#' @seealso [odiff_info()] to see which binary is used and whether it was
#'   resolved from an npm launcher.
#' @export
#'
#' @examples
#' \dontrun{
#' find_odiff()
#'
#' # Use the npm launcher on the PATH as-is
#' options(odiffr.resolve_npm = FALSE)
#' find_odiff()
#' }
find_odiff <- function() {
  .find_odiff_details(offer = TRUE)$path
}

# Internal: locate the binary and report how it was found.
# Returns list(path, source, shim) where `shim` is the path of the npm
# launcher the binary was resolved from (or NA_character_).
# `offer = TRUE` (used only by find_odiff()) may offer to install odiff
# interactively when it is not found; all silent checks such as
# odiff_available() and odiff_info() use the default `offer = FALSE`.
.find_odiff_details <- function(offer = FALSE) {
  # 1. Check user-specified option (used as-is, never resolved)
  opt_path <- getOption("odiffr.path")
  if (!is.null(opt_path) && nzchar(opt_path)) {
    if (file.exists(opt_path)) {
      return(list(path = normalizePath(opt_path, mustWork = TRUE),
                  source = "option", shim = NA_character_))
    }
    warning(
      "odiffr.path option is set but file not found: ", opt_path,
      call. = FALSE
    )
  }

  # 2. Check system PATH
  sys_path <- Sys.which("odiff")
  if (nzchar(sys_path)) {
    path <- normalizePath(unname(sys_path), mustWork = TRUE)
    resolved <- if (.resolve_npm_enabled()) .cached_npm_resolution(path)
    if (!is.null(resolved)) {
      return(list(path = resolved, source = "system", shim = path))
    }
    return(list(path = path, source = "system", shim = NA_character_))
  }

  # 3. Check cached binary from install_odiff() / odiffr_update()
  cached <- .cached_binary()
  if (!is.null(cached) && file.exists(cached)) {
    return(list(path = cached, source = "cached", shim = NA_character_))
  }

  # 4. Interactively offer to download odiff (never in non-interactive use)
  if (isTRUE(offer) && .should_offer_install()) {
    .odiffr_env$install_asked <- TRUE
    if (isTRUE(.offer_install())) {
      path <- install_odiff()
      return(list(path = normalizePath(path, mustWork = TRUE),
                  source = "cached", shim = NA_character_))
    }
  }

  stop(
    "odiff binary not found. Install it using one of:\n",
    "  - R: odiffr::install_odiff() to download it to the user cache ",
    "(no Node.js needed)\n",
    "  - npm: npm install -g odiff-bin\n",
    "  - Download: https://github.com/dmtrKovalenko/odiff/releases\n",
    "Or set options(odiffr.path = '/path/to/odiff')",
    call. = FALSE
  )
}

# Internal: wrapper for interactive() (enables mocking in tests)
.is_interactive <- function() {
  interactive()
}

# Internal: may find_odiff() offer to download odiff? Only in an interactive
# session that is not running tests, knitting or R CMD check, when the offer
# is not disabled via options(odiffr.ask_install = FALSE) and has not been
# made before in this session.
.should_offer_install <- function() {
  .is_interactive() &&
    !identical(Sys.getenv("TESTTHAT"), "true") &&
    !isTRUE(getOption("knitr.in.progress")) &&
    !nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_")) &&
    !isFALSE(getOption("odiffr.ask_install", TRUE)) &&
    !isTRUE(.odiffr_env$install_asked)
}

# Internal: ask whether to download odiff. Returns TRUE, FALSE or NA.
.offer_install <- function() {
  utils::askYesNo(paste0(
    "odiff is required but was not found. Download the latest odiff ",
    "release to ", odiffr_cache_path(), " now?"
  ))
}

# Internal: forget cached binary lookups (version and npm launcher
# resolution) so a newly installed binary is picked up immediately
.reset_binary_caches <- function() {
  .odiffr_env$version_cache <- NULL
  .odiffr_env$npm_cache <- NULL
  invisible(NULL)
}

#' Check if odiff is Available
#'
#' A silent check: unlike [find_odiff()], it never offers to install odiff,
#' so it is safe to use in skip conditions and scripts.
#'
#' @return Logical `TRUE` if odiff is found and executable, `FALSE` otherwise.
#' @export
#'
#' @examples
#' odiff_available()
odiff_available <- function() {
  # Never offers to install odiff: this is a silent check (e.g. for skips)
  tryCatch(
    {
      path <- .find_odiff_details(offer = FALSE)$path
      file.exists(path)
    },
    error = function(e) FALSE
  )
}

#' Get odiff Version
#'
#' The version is cached per binary: repeated calls do not spawn
#' `odiff --version` again unless the binary path (or the file itself)
#' changes.
#'
#' @return Character string with the odiff version, or `NA_character_` if
#'   unavailable.
#' @export
#'
#' @examples
#' \dontrun{
#' odiff_version()
#' }
odiff_version <- function() {
  if (!odiff_available()) {
    return(NA_character_)
  }

  path <- tryCatch(.find_odiff_details(offer = FALSE)$path,
                   error = function(e) NA_character_)
  if (is.na(path)) {
    return(NA_character_)
  }

  key <- .version_cache_key(path)
  cached <- .odiffr_env$version_cache
  if (!is.null(cached) && identical(cached$key, key)) {
    return(cached$version)
  }

  version <- tryCatch(
    {
      # odiff 4.x prints the version to stderr, older versions to stdout;
      # merge both. Output: "odiff X.Y.Z - SIMD first pixel-by-pixel..."
      result <- suppressWarnings(
        system2(path, "--version", stdout = TRUE, stderr = TRUE,
                timeout = 30)
      )
      .parse_version(result)
    },
    error = function(e) NA_character_
  )

  # Only cache successful lookups so transient failures are retried
  if (!is.na(version)) {
    .odiffr_env$version_cache <- list(key = key, version = version)
  }
  version
}

# Package-level environment for cached state
.odiffr_env <- new.env(parent = emptyenv())

# Cache key for a binary: its path plus size and modification time, so that
# replacing the binary in place (e.g. via odiffr_update()) invalidates it
.version_cache_key <- function(path) {
  info <- suppressWarnings(file.info(path, extra_cols = FALSE))
  list(path = path, size = info$size, mtime = as.numeric(info$mtime))
}

# Extract "X.Y.Z" from `odiff --version` output lines
.parse_version <- function(lines) {
  lines <- as.character(lines)
  lines <- lines[!is.na(lines)]
  pattern <- "odiff\\s+v?([0-9]+\\.[0-9]+\\.[0-9]+)"
  hits <- grep(pattern, lines, ignore.case = TRUE, perl = TRUE, value = TRUE)
  if (length(hits) == 0) {
    # Fall back to any line that mentions odiff and a version number
    hits <- grep("odiff", lines, ignore.case = TRUE, value = TRUE)
    hits <- grep("[0-9]+\\.[0-9]+\\.[0-9]+", hits, value = TRUE)
    if (length(hits) == 0) {
      return(NA_character_)
    }
    return(regmatches(hits[1], regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", hits[1])))
  }
  m <- regmatches(hits[1], regexec(pattern, hits[1], ignore.case = TRUE,
                                   perl = TRUE))[[1]]
  m[2]
}

#' Display odiff Configuration Information
#'
#' @return A list with components:
#'   \describe{
#'     \item{os}{Operating system (darwin, linux, windows)}
#'     \item{arch}{Architecture (arm64, x64)}
#'     \item{path}{Path to the odiff binary}
#'     \item{version}{odiff version string}
#'     \item{source}{Source of the binary (option, system, cached)}
#'     \item{shim}{Path of the npm launcher script found on the PATH that
#'       `path` was resolved from, or `NA` if the binary is used directly.
#'       See [find_odiff()].}
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' odiff_info()
#' }
odiff_info <- function() {
  platform <- .platform_info()
  details <- tryCatch(.find_odiff_details(), error = function(e) NULL)
  path <- if (is.null(details)) NA_character_ else details$path
  shim <- if (is.null(details)) NA_character_ else details$shim
  version <- odiff_version()
  source <- .binary_source()

  structure(
    list(
      os = platform$os,
      arch = platform$arch,
      path = path,
      version = version,
      source = source,
      shim = shim
    ),
    class = c("odiff_info", "list")
  )
}

#' @export
print.odiff_info <- function(x, ...) {
  cat("odiffr configuration\n")
  cat("--------------------\n")
  cat("OS:      ", x$os, "\n")
  cat("Arch:    ", x$arch, "\n")
  cat("Path:    ", if (is.na(x$path)) "<not found>" else x$path, "\n")
  cat("Version: ", if (is.na(x$version)) "<unknown>" else x$version, "\n")
  cat("Source:  ", if (is.na(x$source)) "<none>" else x$source, "\n")
  if (!is.null(x$shim) && !is.na(x$shim)) {
    cat("Via npm: ", x$shim, "\n")
  }
  invisible(x)
}

# Internal: Get platform information
.platform_info <- function() {
  os <- tolower(Sys.info()[["sysname"]])
  arch <- Sys.info()[["machine"]]

  os_normalized <- switch(
    os,
    "darwin" = "darwin",
    "linux" = "linux",
    "windows" = "windows",
    "unknown"
  )

  arch_normalized <- if (arch %in% c("x86_64", "x86-64", "amd64", "AMD64")) {
    "x64"
  } else if (arch %in% c("aarch64", "arm64", "ARM64")) {
    "arm64"
  } else {
    "unknown"
  }

  list(os = os_normalized, arch = arch_normalized)
}

# Internal: Get path to cached binary
.cached_binary <- function() {
  cache_dir <- odiffr_cache_path()
  if (!dir.exists(cache_dir)) {
    return(NULL)
  }

  platform <- .platform_info()
  binary_name <- if (platform$os == "windows") "odiff.exe" else "odiff"
  subdir <- paste0(platform$os, "_", platform$arch)
  path <- file.path(cache_dir, "bin", subdir, binary_name)

  if (!file.exists(path)) {
    return(NULL)
  }

  # Ensure executable on Unix
  if (platform$os != "windows") {
    Sys.chmod(path, mode = "0755")
  }

  normalizePath(path, mustWork = TRUE)
}

# Internal: Determine source of current binary
.binary_source <- function() {
  # Check in priority order
  opt_path <- getOption("odiffr.path")
  if (!is.null(opt_path) && nzchar(opt_path) && file.exists(opt_path)) {
    return("option")
  }

  sys_path <- Sys.which("odiff")
  if (nzchar(sys_path)) {
    return("system")
  }

  cached <- .cached_binary()
  if (!is.null(cached) && file.exists(cached)) {
    return("cached")
  }

  NA_character_
}

# ---------------------------------------------------------------------------
# npm launcher ("shim") resolution
# ---------------------------------------------------------------------------

# Internal: whether npm launcher resolution is enabled
.resolve_npm_enabled <- function() {
  !isFALSE(getOption("odiffr.resolve_npm", TRUE))
}

# Internal: resolve an npm launcher, caching the result per launcher path.
# Returns the native binary path, or NULL if `path` is not a launcher or no
# native binary was found (in which case the launcher itself is used).
.cached_npm_resolution <- function(path) {
  key <- .version_cache_key(path)
  cache <- .odiffr_env$npm_cache
  if (is.null(cache)) cache <- list()

  hit <- cache[[path]]
  if (!is.null(hit) && identical(hit$key, key) &&
      (is.null(hit$resolved) || file.exists(hit$resolved))) {
    return(hit$resolved)
  }

  resolved <- tryCatch(.resolve_npm_shim(path), error = function(e) NULL)
  cache[path] <- list(list(key = key, resolved = resolved))
  .odiffr_env$npm_cache <- cache
  resolved
}

# Internal: if `path` is an npm launcher for odiff, return the native binary
# it would run; otherwise NULL. `windows`, `os` and `arch` are arguments so
# that the Windows layout can be tested on any platform.
.resolve_npm_shim <- function(path, windows = .Platform$OS.type == "windows",
                              os = NULL, arch = NULL) {
  if (!.is_npm_shim(path, windows = windows)) {
    return(NULL)
  }
  candidates <- .npm_binary_candidates(path, windows = windows,
                                       os = os, arch = arch)
  for (candidate in candidates) {
    if (file.exists(candidate) && !dir.exists(candidate) &&
        .is_native_binary(candidate)) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }
  NULL
}

# Internal: read the first `n` bytes of a file (raw(0) on failure)
.read_head <- function(path, n = 2048L) {
  if (!file.exists(path) || dir.exists(path)) {
    return(raw(0))
  }
  tryCatch({
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, "raw", n = n)
  }, error = function(e) raw(0))
}

# Internal: is `path` an npm launcher script? Any platform: a `#!` script
# whose interpreter line mentions node. Windows additionally: an npm
# `.cmd`/`.bat`/`.ps1` wrapper, or an extensionless sh wrapper, that refers
# to node.
.is_npm_shim <- function(path, windows = .Platform$OS.type == "windows") {
  bytes <- .read_head(path)
  bytes <- bytes[bytes != as.raw(0)]
  if (length(bytes) < 2) return(FALSE)
  txt <- rawToChar(bytes)

  if (startsWith(txt, "#!")) {
    first_line <- strsplit(txt, "[\r\n]", useBytes = TRUE)[[1]][1]
    if (grepl("node", first_line, fixed = TRUE, useBytes = TRUE)) {
      return(TRUE)
    }
    # cmd-shim style sh wrappers (#!/bin/sh) that exec node
    return(windows && grepl("node", txt, fixed = TRUE, useBytes = TRUE))
  }

  if (windows) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("cmd", "bat", "ps1")) {
      return(grepl("node", txt, ignore.case = TRUE, useBytes = TRUE))
    }
  }
  FALSE
}

# Internal: does the file start with a native executable magic number
# (ELF, Mach-O or Windows PE)? Scripts never do.
.is_native_binary <- function(path) {
  b <- .read_head(path, 4L)
  if (length(b) < 2) return(FALSE)
  hex <- paste(as.character(b), collapse = "")
  if (startsWith(hex, "4d5a")) return(TRUE)  # "MZ" (PE)
  hex %in% c(
    "7f454c46",                # ELF
    "feedface", "feedfacf",    # Mach-O (big-endian)
    "cefaedfe", "cffaedfe",    # Mach-O (little-endian)
    "cafebabe"                 # Mach-O universal
  )
}

# Internal: npm platform key ("<platform>-<arch>") of the @odiff/* package
.npm_platform_key <- function(os = NULL, arch = NULL) {
  info <- .platform_info()
  if (is.null(os)) os <- info$os
  if (is.null(arch)) {
    arch <- info$arch
    if (identical(arch, "unknown") &&
        identical(tolower(Sys.info()[["machine"]]), "riscv64")) {
      arch <- "riscv64"
    }
  }
  npm_os <- switch(os, "windows" = "win32", os)
  paste0(npm_os, "-", arch)
}

# Internal: candidate native binary paths for an npm launcher, in order of
# preference. Covers global and local installs on Unix and Windows, with the
# @odiff/* package nested under odiff-bin or hoisted next to it, and the
# pre-4.4 layout in which odiff-bin/bin/odiff.exe is the native binary.
.npm_binary_candidates <- function(path,
                                   windows = .Platform$OS.type == "windows",
                                   os = NULL, arch = NULL) {
  if (is.null(os) && windows) os <- "windows"
  key <- .npm_platform_key(os = os, arch = arch)
  bin_name <- if (windows) "odiff.exe" else "odiff"

  # Follow symlinks (e.g. <prefix>/bin/odiff -> odiff-bin/bin/odiff)
  real <- normalizePath(path, winslash = "/", mustWork = FALSE)
  shim_dir <- dirname(real)

  pkgs <- character()
  if (identical(basename(shim_dir), "bin") &&
      identical(basename(dirname(shim_dir)), "odiff-bin")) {
    # The launcher is odiff-bin/bin/odiff itself
    pkgs <- dirname(shim_dir)
  }
  pkgs <- unique(c(
    pkgs,
    # Windows global: %APPDATA%/npm/odiff.cmd + %APPDATA%/npm/node_modules
    file.path(shim_dir, "node_modules", "odiff-bin"),
    # Local install: node_modules/.bin/odiff -> node_modules/odiff-bin
    file.path(dirname(shim_dir), "odiff-bin"),
    # Unix global where <prefix>/bin/odiff is a copy, not a symlink
    file.path(dirname(shim_dir), "lib", "node_modules", "odiff-bin")
  ))

  candidates <- unlist(lapply(pkgs, function(pkg) {
    c(
      file.path(pkg, "node_modules", "@odiff", key, bin_name),
      file.path(dirname(pkg), "@odiff", key, bin_name),
      file.path(pkg, "bin", "odiff.exe")
    )
  }), use.names = FALSE)
  unique(candidates)
}
