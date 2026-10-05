#' Add a GitHub Actions Workflow for Visual Tests
#'
#' Writes a ready-to-use GitHub Actions workflow that runs your package's
#' testthat tests, including image snapshot tests, with odiff. The workflow
#' installs odiff with [install_odiff()] (no Node.js needed), adds a
#' [snapshot_report()] summary of changed image snapshots to the job summary
#' and, when tests fail, uploads the new snapshots (`*.new.png`) and diff
#' images (`tests/testthat/_odiffr/`) as an artifact.
#'
#' @param path Path of the workflow file, relative to the working directory
#'   (which should be the root of your package). Default:
#'   `".github/workflows/odiffr.yaml"`. Parent directories are created as
#'   needed.
#' @param overwrite Logical; if `TRUE`, replace an existing file at `path`.
#'   Default is `FALSE`, in which case an existing file is an error.
#' @param open Logical; if `TRUE` and the session is interactive, open the
#'   new file with [utils::file.edit()]. Default is `FALSE`.
#'
#' @details
#' The workflow is written only when you call this function; odiffr never
#' writes it by itself. It runs on every push and pull request on
#' `ubuntu-latest`, so record image snapshots on Linux or use a tolerant
#' preset (see [odiff_preset()]). The template is installed with odiffr, see
#' `system.file("templates", "odiffr-ci.yaml", package = "odiffr")`; edit
#' the written file as needed.
#'
#' The tests run with `NOT_CRAN=true`, as snapshot tests are skipped on CRAN.
#'
#' @return The path of the written file (invisibly).
#' @seealso [install_odiff()], [snapshot_report()], [expect_snapshot_image()]
#' @export
#'
#' @examples
#' \dontrun{
#' # From the root of your package
#' use_odiffr_ci()
#' }
#'
#' # In a temporary directory
#' dir <- tempfile()
#' dir.create(dir)
#' path <- use_odiffr_ci(file.path(dir, ".github", "workflows", "odiffr.yaml"))
#' unlink(dir, recursive = TRUE)
use_odiffr_ci <- function(path = ".github/workflows/odiffr.yaml",
                          overwrite = FALSE,
                          open = FALSE) {
  if (!is.character(path) || length(path) != 1 || is.na(path) ||
      !nzchar(path)) {
    stop("`path` must be a single file path.", call. = FALSE)
  }
  if (!isTRUE(overwrite) && !isFALSE(overwrite)) {
    stop("`overwrite` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!isTRUE(open) && !isFALSE(open)) {
    stop("`open` must be TRUE or FALSE.", call. = FALSE)
  }

  template <- .ci_template_path()

  if (dir.exists(path)) {
    stop("`path` is a directory: ", path, call. = FALSE)
  }
  if (file.exists(path) && !overwrite) {
    stop(
      "File already exists: ", path, "\n",
      "Use overwrite = TRUE to replace it.",
      call. = FALSE
    )
  }

  parent <- dirname(path)
  if (!dir.exists(parent) &&
      !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    stop("Failed to create directory: ", parent, call. = FALSE)
  }
  if (!file.copy(template, path, overwrite = TRUE)) {
    stop("Failed to write workflow file: ", path, call. = FALSE)
  }

  message(
    "Wrote GitHub Actions workflow: ", path, "\n",
    "Next steps:\n",
    "  - Commit it and push to GitHub to run your tests with odiff.\n",
    "  - Commit your image snapshots (tests/testthat/_snaps/), ",
    "recorded on Linux or with a tolerant preset.\n",
    "  - Changed snapshots are listed in the job summary and uploaded as ",
    "the 'image-snapshots' artifact."
  )

  if (open && .is_interactive()) {
    utils::file.edit(path)
  }
  invisible(path)
}

# Internal: path of the installed CI workflow template
.ci_template_path <- function() {
  template <- system.file("templates", "odiffr-ci.yaml", package = "odiffr")
  if (!nzchar(template)) {
    stop("odiffr CI workflow template not found; reinstall odiffr.",
         call. = FALSE)
  }
  template
}
