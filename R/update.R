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

#' Download Latest odiff Binary
#'
#' Downloads the odiff binary from GitHub releases to the user's cache
#' directory. The downloaded binary will be used by `find_odiff()` if no
#' system-wide installation or user-specified path is found.
#'
#' @param version Character string specifying the version to download.
#'   Use `"latest"` (default) to download the most recent release, or
#'   specify a version tag like `"v4.1.2"`.
#' @param force Logical; if `TRUE`, re-download even if the binary already
#'   exists in the cache. Default is `FALSE`.
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

  # Resolve version
  if (version == "latest") {
    version <- .get_latest_version()
    if (is.null(version)) {
      stop("Failed to determine latest odiff version.", call. = FALSE)
    }
    message("Latest version: ", version)
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

  # Download
  message("Downloading odiff ", version, " for ", platform$os, "/", platform$arch, "...")
  message("URL: ", url)

  tryCatch(
    {
      utils::download.file(
        url = url,
        destfile = target_path,
        mode = "wb",
        quiet = FALSE
      )
    },
    error = function(e) {
      stop("Failed to download odiff binary: ", e$message, call. = FALSE)
    }
  )

  # Make executable on Unix
  if (platform$os != "windows") {
    Sys.chmod(target_path, mode = "0755")
  }

  # Write version file
  version_file <- file.path(cache_dir, "CACHED_VERSION")
  writeLines(version, version_file)

  message("Successfully downloaded to: ", target_path)
  invisible(target_path)
}

# Internal: Get latest version from GitHub API
.get_latest_version <- function() {
  url <- "https://api.github.com/repos/dmtrKovalenko/odiff/releases/latest"

  tryCatch(
    {
      # Use base R for minimal dependencies
      con <- url(url, headers = c("Accept" = "application/vnd.github.v3+json"))
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
