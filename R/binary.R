#' Find the odiff Binary
#'
#' Locates the odiff executable using a priority-based search:
#' 1. User-specified path via `options(odiffr.path = "...")`
#' 2. System PATH (`Sys.which("odiff")`)
#' 3. Cached binary from `odiffr_update()`
#'
#' @return Character string with the absolute path to the odiff executable.
#' @export
#'
#' @examples
#' \dontrun{
#' find_odiff()
#' }
find_odiff <- function() {
  # 1. Check user-specified option
  opt_path <- getOption("odiffr.path")
  if (!is.null(opt_path) && nzchar(opt_path)) {
    if (file.exists(opt_path)) {
      return(normalizePath(opt_path, mustWork = TRUE))
    }
    warning(
      "odiffr.path option is set but file not found: ", opt_path,
      call. = FALSE
    )
  }

  # 2. Check system PATH
  sys_path <- Sys.which("odiff")
  if (nzchar(sys_path)) {
    return(normalizePath(unname(sys_path), mustWork = TRUE))
  }

  # 3. Check cached binary from odiffr_update()
  cached <- .cached_binary()
  if (!is.null(cached) && file.exists(cached)) {
    return(cached)
  }

  stop(
    "odiff binary not found. Install it using one of:\n",
    "  - npm: npm install -g odiff-bin\n",
    "  - Download: https://github.com/dmtrKovalenko/odiff/releases\n",
    "  - R: odiffr_update() to download to cache\n",
    "Or set options(odiffr.path = '/path/to/odiff')",
    call. = FALSE
  )
}

#' Check if odiff is Available
#'
#' @return Logical `TRUE` if odiff is found and executable, `FALSE` otherwise.
#' @export
#'
#' @examples
#' odiff_available()
odiff_available <- function() {
  tryCatch(
    {
      path <- find_odiff()
      file.exists(path)
    },
    error = function(e) FALSE
  )
}

#' Get odiff Version
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

  tryCatch(
    {
      path <- find_odiff()
      result <- system2(path, "--help", stdout = TRUE, stderr = TRUE)
      # odiff help output typically starts with version info
      # Parse first line for version
      version_line <- result[1]
      if (grepl("odiff", version_line, ignore.case = TRUE)) {
        # Try to extract version number
        version <- gsub(".*?([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", version_line)
        if (nzchar(version) && version != version_line) {
          return(version)
        }
      }
      NA_character_
    },
    error = function(e) NA_character_
  )
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
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' odiff_info()
#' }
odiff_info <- function() {
  platform <- .platform_info()
  path <- tryCatch(find_odiff(), error = function(e) NA_character_)
  version <- odiff_version()
  source <- .binary_source()

  structure(
    list(
      os = platform$os,
      arch = platform$arch,
      path = path,
      version = version,
      source = source
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
