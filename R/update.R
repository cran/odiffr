#' Get Cache Directory Path
#'
#' Returns the path to the odiffr cache directory where downloaded binaries
#' are stored.
#'
#' @return Character string with the path to the cache directory.
#' @export
#'
#' @examples
#' odiffr_cache_path()
odiffr_cache_path <- function() {
  tools::R_user_dir("odiffr", which = "cache")
}

#' Clear the odiffr Cache
#'
#' Removes all cached binaries downloaded by `odiffr_update()`.
#'
#' @return Invisibly returns `TRUE` if successful, `FALSE` otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' odiffr_clear_cache()
#' }
odiffr_clear_cache <- function() {
  cache_dir <- odiffr_cache_path()
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    message("Cache cleared: ", cache_dir)
    invisible(TRUE)
  } else {
    message("Cache directory does not exist: ", cache_dir)
    invisible(FALSE)
  }
}

#' Install odiff
#'
#' Downloads the odiff binary for your platform from the odiff GitHub
#' releases to the odiffr user cache ([odiffr_cache_path()]), where
#' [find_odiff()] finds it. This is the easiest way to install odiff: it needs
#' neither Node.js nor npm, nor administrator rights.
#'
#' @inheritParams odiffr_update
#'
#' @details
#' `install_odiff()` is a user-facing wrapper around [odiffr_update()]: it
#' downloads the binary (see [odiffr_update()] for details, e.g. on GitHub
#' API rate limits), clears odiffr's cached binary lookups so that
#' [find_odiff()] and [odiff_version()] use the new binary straight away, and
#' reports the installed version and path.
#'
#' The cached binary is used only when no binary is set via
#' `options(odiffr.path)` and no odiff is found on the PATH; a message says
#' so when another binary takes precedence.
#'
#' odiffr never downloads anything on its own: the binary is only downloaded
#' when `install_odiff()` (or [odiffr_update()]) is called, or when you
#' accept the offer [find_odiff()] makes in interactive sessions.
#'
#' @return The path to the installed binary (invisibly).
#' @seealso [odiffr_update()], [odiffr_clear_cache()], [odiff_info()]
#' @export
#'
#' @examples
#' \dontrun{
#' install_odiff()
#'
#' # A specific version
#' install_odiff(version = "4.5.0", force = TRUE)
#' }
install_odiff <- function(version = "latest", force = FALSE) {
  path <- odiffr_update(version = version, force = force)
  path <- normalizePath(path, mustWork = TRUE)
  .reset_binary_caches()

  installed <- .installed_version(path)
  message(
    "odiff", if (!is.na(installed)) paste0(" ", installed),
    " is installed at: ", path
  )

  active <- tryCatch(.find_odiff_details(offer = FALSE)$path,
                     error = function(e) NA_character_)
  if (!is.na(active) && !identical(active, path)) {
    message(
      "Note: find_odiff() uses ", active, ", which takes precedence over ",
      "the downloaded binary (via options(odiffr.path) or the PATH)."
    )
  }
  invisible(path)
}

# Internal: version recorded next to a binary downloaded by odiffr_update()
.installed_version <- function(path) {
  version_file <- file.path(dirname(path), "CACHED_VERSION")
  version <- if (file.exists(version_file)) {
    tryCatch(readLines(version_file, n = 1L, warn = FALSE),
             error = function(e) character())
  } else {
    character()
  }
  if (length(version) != 1 || !nzchar(trimws(version))) {
    return(NA_character_)
  }
  trimws(version)
}

#' Download Latest odiff Binary
#'
#' Downloads the odiff binary from GitHub releases to the user's cache
#' directory. The downloaded binary will be used by `find_odiff()` if no
#' system-wide installation or user-specified path is found.
#' [install_odiff()] is the recommended, user-facing way to do this.
#'
#' @param version Character string specifying the version to download.
#'   Use `"latest"` (default) to download the most recent release, or
#'   specify a version like `"v4.1.2"` or `"4.1.2"` (the `v` prefix is
#'   optional).
#' @param force Logical; if `TRUE`, re-download even if the binary already
#'   exists in the cache. Default is `FALSE`.
#'
#' @details
#' The latest release is looked up via the GitHub API. If the `GITHUB_PAT`
#' or `GITHUB_TOKEN` environment variable is set, it is sent as a bearer
#' token to avoid API rate limits.
#'
#' The binary is downloaded to a temporary file next to its final location
#' and only moved into place once the download has succeeded, so an
#' interrupted download never leaves a broken binary behind. The download
#' timeout (`getOption("timeout")`) is temporarily raised to at least 300
#' seconds. The downloaded version is recorded in a `CACHED_VERSION` file
#' next to the binary.
#'
#' Note that some odiff releases were published without binary assets; if
#' the download fails with HTTP 404, try another version.
#'
#' @return Character string with the path to the downloaded binary.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download latest version
#' odiffr_update()
#'
#' # Download specific version
#' odiffr_update(version = "v4.1.2")
#'
#' # Force re-download
#' odiffr_update(force = TRUE)
#' }
odiffr_update <- function(version = "latest", force = FALSE) {
  platform <- .platform_info()
  cache_dir <- odiffr_cache_path()

  # Determine target path
  binary_name <- if (platform$os == "windows") "odiff.exe" else "odiff"
  subdir <- paste0(platform$os, "_", platform$arch)
  target_dir <- file.path(cache_dir, "bin", subdir)
  target_path <- file.path(target_dir, binary_name)

  # Check if already exists
  if (!force && file.exists(target_path)) {
    message("Binary already exists at: ", target_path)
    message("Use force = TRUE to re-download.")
    return(invisible(target_path))
  }

  if (!is.character(version) || length(version) != 1 || is.na(version) ||
      !nzchar(trimws(version))) {
    stop("`version` must be a single string, e.g. \"latest\" or \"v4.1.2\".",
         call. = FALSE)
  }

  # Resolve version
  if (identical(tolower(trimws(version)), "latest")) {
    version <- .get_latest_version()
    if (is.null(version)) {
      stop("Failed to determine latest odiff version.", call. = FALSE)
    }
    version <- .normalize_version(version)
    message("Latest version: ", version)
  } else {
    version <- .normalize_version(version)
  }

  # Construct download URL
  asset_name <- .get_asset_name(platform)
  url <- sprintf(
    "https://github.com/dmtrKovalenko/odiff/releases/download/%s/%s",
    version, asset_name
  )

  # Create target directory
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }

  # Download to a temporary file in the target directory, then move it into
  # place, so a failed or partial download never leaves a broken binary
  # where find_odiff() would pick it up.
  message("Downloading odiff ", version, " for ", platform$os, "/", platform$arch, "...")
  message("URL: ", url)

  dl_warning <- NULL
  tmp_path <- tempfile(pattern = paste0(binary_name, ".download-"), tmpdir = target_dir)
  on.exit(if (file.exists(tmp_path)) unlink(tmp_path, force = TRUE), add = TRUE)

  old_timeout <- getOption("timeout")
  if (is.null(old_timeout) || !is.numeric(old_timeout) || old_timeout < 300) {
    options(timeout = 300)
    on.exit(options(timeout = old_timeout), add = TRUE)
  }

  status <- tryCatch(
    withCallingHandlers(
      download_file_internal(
        url = url,
        destfile = tmp_path,
        mode = "wb",
        quiet = FALSE
      ),
      warning = function(w) {
        # download.file() reports HTTP errors as a warning followed by an
        # error; keep the (more informative) warning text for the message.
        dl_warning <<- conditionMessage(w)
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      unlink(tmp_path, force = TRUE)
      .download_error(conditionMessage(e), dl_warning, version)
    }
  )
  if (is.numeric(status) && length(status) == 1 && !is.na(status) && status != 0) {
    unlink(tmp_path, force = TRUE)
    .download_error(paste0("download exited with status ", status), dl_warning, version)
  }
  if (!file.exists(tmp_path) || is.na(file.size(tmp_path)) || file.size(tmp_path) == 0) {
    unlink(tmp_path, force = TRUE)
    .download_error("downloaded file is empty", dl_warning, version)
  }

  # Make executable on Unix
  if (platform$os != "windows") {
    Sys.chmod(tmp_path, mode = "0755")
  }

  # Move into place (same directory, so rename is atomic where supported)
  if (file.exists(target_path)) {
    unlink(target_path, force = TRUE)
  }
  if (!file.rename(tmp_path, target_path)) {
    if (!file.copy(tmp_path, target_path, overwrite = TRUE)) {
      stop("Failed to move downloaded odiff binary to: ", target_path, call. = FALSE)
    }
    if (platform$os != "windows") {
      Sys.chmod(target_path, mode = "0755")
    }
  }

  # Write version file next to the binary (one per platform)
  version_file <- file.path(target_dir, "CACHED_VERSION")
  writeLines(version, version_file)

  message("Successfully downloaded to: ", target_path)
  invisible(target_path)
}

# Internal: Normalize a version string to a release tag ("4.5.0" -> "v4.5.0")
.normalize_version <- function(version) {
  version <- trimws(version)
  version <- sub("^[vV]", "", version)
  paste0("v", version)
}

# Internal: Build an informative download error
.download_error <- function(msg, warning_msg = NULL, version = NULL) {
  full <- paste(c(warning_msg, msg), collapse = "; ")
  hint <- ""
  if (grepl("404", full, fixed = TRUE)) {
    hint <- paste0(
      "\nThe release asset was not found (HTTP 404). Check that odiff ",
      version, " exists; note that some odiff releases (e.g. v4.3.8, v4.4.0) ",
      "were published without binary assets. Try another version, e.g. ",
      "odiffr_update(version = \"v4.1.2\")."
    )
  }
  stop("Failed to download odiff binary: ", full, hint, call. = FALSE)
}

# Internal: HTTP headers for GitHub API requests. Uses GITHUB_PAT or
# GITHUB_TOKEN (if set) to avoid rate limits.
.github_api_headers <- function() {
  headers <- c("Accept" = "application/vnd.github.v3+json")
  token <- Sys.getenv("GITHUB_PAT", unset = "")
  if (!nzchar(token)) token <- Sys.getenv("GITHUB_TOKEN", unset = "")
  if (nzchar(token)) {
    headers <- c(headers, "Authorization" = paste("Bearer", token))
  }
  headers
}

# Internal: Get latest version from GitHub API
.get_latest_version <- function() {
  url <- "https://api.github.com/repos/dmtrKovalenko/odiff/releases/latest"

  tryCatch(
    {
      # Use base R for minimal dependencies
      con <- url(url, headers = .github_api_headers())
      on.exit(close(con), add = TRUE)
      json_text <- paste(readLines(con, warn = FALSE), collapse = "")

      # Simple regex to extract tag_name
      match <- regmatches(
        json_text,
        regexpr('"tag_name"\\s*:\\s*"([^"]+)"', json_text, perl = TRUE)
      )
      if (length(match) > 0) {
        gsub('"tag_name"\\s*:\\s*"([^"]+)"', "\\1", match, perl = TRUE)
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )
}

# Internal: Wrapper for download.file (enables mocking in tests)
# Note: Named without leading dot so testthat::with_mocked_bindings can find it
download_file_internal <- function(url, destfile, mode = "wb", quiet = FALSE) {
  utils::download.file(url = url, destfile = destfile, mode = mode, quiet = quiet)
}

# Internal: Get asset name for platform
.get_asset_name <- function(platform) {
  os_map <- list(
    darwin = "macos",
    linux = "linux",
    windows = "windows"
  )
  os_name <- os_map[[platform$os]]

  arch_name <- platform$arch  # x64 or arm64

  if (platform$os == "windows") {
    sprintf("odiff-%s-%s.exe", os_name, arch_name)
  } else {
    sprintf("odiff-%s-%s", os_name, arch_name)
  }
}
