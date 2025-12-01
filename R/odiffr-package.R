#' Odiffr: Fast Pixel-by-Pixel Image Comparison
#'
#' R bindings to the Odiff command-line tool for blazing-fast, pixel-by-pixel
#' image comparison. Ideal for visual regression testing, quality assurance,
#' and validated environments.
#'
#' @section Main Functions:
#'
#' \describe{
#'   \item{[compare_images()]}{High-level image comparison returning a
#'     tibble/data.frame. Accepts file paths or magick-image objects.}
#'   \item{[odiff_run()]}{Low-level CLI wrapper with full control over
#'     all Odiff options. Returns a detailed result list.}
#'   \item{[ignore_region()]}{Helper to create ignore region specifications.}
#' }
#'
#' @section Binary Management:
#'
#' \describe{
#'   \item{[find_odiff()]}{Locate the Odiff binary using priority search.}
#'   \item{[odiff_available()]}{Check if Odiff is available.}
#'   \item{[odiff_version()]}{Get the Odiff version string.}
#'   \item{[odiff_info()]}{Display full configuration information.}
#'   \item{[odiffr_update()]}{Download latest Odiff binary to user cache.
#'     Useful for updating between package releases.}
#'   \item{[odiffr_cache_path()]}{Get the cache directory path.}
#'   \item{[odiffr_clear_cache()]}{Remove cached binaries.}
#' }
#'
#' @section Binary Detection Priority:
#'
#' The package searches for the Odiff binary in this order:
#' \enumerate{
#'   \item User-specified path via `options(odiffr.path = "/path/to/odiff")`
#'   \item System PATH (`Sys.which("odiff")`)
#'   \item Cached binary from `odiffr_update()`
#' }
#'
#' @section Supported Image Formats:
#'
#' \describe{
#'   \item{Input}{PNG, JPEG, WEBP, TIFF (cross-format comparison supported)}
#'   \item{Output}{PNG only}
#' }
#'
#' @section Exit Codes:
#'
#' \describe{
#'   \item{0}{Images match}
#'   \item{21}{Layout difference (different dimensions)}
#'   \item{22}{Pixel differences found}
#' }
#'
#' @section For Validated Environments:
#'
#' The package is designed for use in validated pharmaceutical and clinical
#' research environments:
#'
#' \itemize{
#'   \item Pin specific binary versions with `options(odiffr.path = "/validated/odiff")`
#'   \item Zero external runtime dependencies (base R only for core functions)
#'   \item Use `odiff_version()` to document binary version for audit trails
#' }
#'
#' @section Author:
#'
#' Ben Wolstenholme
#'
#' @section See Also:
#'
#' \itemize{
#'   \item \url{https://github.com/dmtrKovalenko/odiff} - Odiff project
#'   \item \url{https://github.com/BenWolst/odiffr} - Odiffr package
#' }
#'
#' @docType package
#' @name odiffr-package
#' @aliases odiffr
#' @keywords internal
"_PACKAGE"
