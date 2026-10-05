#' Report Failing Image Snapshots
#'
#' Finds the image snapshots that changed in the last test run (the
#' `<name>.new.png` files testthat leaves next to `<name>.png` in
#' `tests/testthat/_snaps/`), compares each with its baseline using odiff and
#' writes an HTML, Markdown or JUnit XML report. Unlike
#' [testthat::snapshot_review()], this works non-interactively, so it is
#' suited to continuous integration.
#'
#' @param path Path to the snapshot directory. Searched recursively, so
#'   variant subdirectories (`_snaps/<variant>/<test-file>/`) are included.
#'   Default: `"tests/testthat/_snaps"`.
#' @param output_file Path of the report file. Required for `"html"` and
#'   `"junit"`. For `"markdown"`, `NULL` (the default) appends to the GitHub
#'   Actions job summary when the `GITHUB_STEP_SUMMARY` environment variable
#'   is set, and otherwise writes nothing (see [batch_markdown()]).
#' @param format Report format: `"html"` ([batch_report()]), `"markdown"`
#'   ([batch_markdown()]) or `"junit"` ([batch_junit()]).
#' @param threshold Numeric; colour difference threshold between 0.0 and 1.0.
#'   Default is 0.1 (or the value from `preset`). Use the settings of the
#'   tests that produced the snapshots to get the same verdicts.
#' @param antialiasing Logical; if `TRUE`, ignore antialiased pixels.
#'   Default is `FALSE` (or the value from `preset`).
#' @param ... Additional arguments passed to [compare_images()] (and from
#'   there to [odiff_run()]), e.g. `ignore_regions`.
#' @param images For `"html"`: which images to show per snapshot, `"all"`
#'   (default: baseline, new and diff side by side) or `"diff"`.
#' @param embed For `"html"`: if `TRUE` (default), images are embedded so
#'   the report is a single self-contained file, e.g. for a CI artifact.
#' @param title Report title. Default: `"Image snapshot changes"`.
#' @param diff_dir Directory for the diff images. `NULL` (default) uses a
#'   `snapshot-diffs` directory next to `output_file`, or a directory in
#'   [tempdir()] when there is no `output_file`. It must not be inside
#'   `path`, because testthat deletes unknown files in `_snaps/`. Diff
#'   images left there by an earlier report (`NNN_<name>_diff.png`) are
#'   removed first.
#' @param preset Optional comparison preset, see [odiff_preset()]. It
#'   supplies `threshold` and `antialiasing` unless those are given.
#'
#' @details
#' Each `<name>.new.png` is paired with `<name>.png` in the same directory
#' and compared with [compare_images_batch()]. The returned batch has an
#' extra column `snapshot` with the snapshot's path relative to `path` (e.g.
#' `"linux/plots/scatter.png"`). Reports label rows by this relative
#' snapshot path.
#'
#' A `.new.png` file without a baseline cannot be compared; it is reported
#' with `reason = "error"` and the error "no baseline snapshot", so it is
#' not silently missed.
#'
#' If no `.new.png` files are found, an empty batch is returned. The HTML
#' and JUnit reports are still written (with no comparisons); the Markdown
#' report contains "No image snapshot changes.".
#'
#' Snapshots whose new image matches the baseline under the given settings
#' are reported as passing: testthat compares more strictly (or with other
#' settings) than the report.
#'
#' @section Continuous integration:
#' Run the tests without stopping at the first failure, then write the
#' report. In GitHub Actions:
#' \preformatted{
#' - name: Test
#'   run: |
#'     res <- testthat::test_local(stop_on_failure = FALSE)
#'     odiffr::snapshot_report(format = "markdown")
#'     odiffr::snapshot_report(output_file = "snapshot-report.html")
#'     if (any(as.data.frame(res)$failed > 0)) stop("Tests failed")
#'   shell: Rscript {0}
#'
#' - name: Upload snapshot report
#'   if: always()
#'   uses: actions/upload-artifact@v4
#'   with:
#'     name: snapshot-report
#'     path: snapshot-report.html
#' }
#'
#' @return The comparison results, an `odiffr_batch` (invisibly).
#'
#' @seealso [expect_snapshot_image()], [compare_file_odiff()],
#'   [batch_report()], [batch_markdown()], [batch_junit()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # After a test run with failing image snapshots
#' snapshot_report(output_file = "snapshot-report.html")
#'
#' # GitHub Actions job summary
#' snapshot_report(format = "markdown")
#'
#' # JUnit XML for a CI test reporter
#' snapshot_report(output_file = "snapshots.xml", format = "junit")
#' }
snapshot_report <- function(path = "tests/testthat/_snaps",
                            output_file = NULL,
                            format = c("html", "markdown", "junit"),
                            threshold = 0.1,
                            antialiasing = FALSE,
                            ...,
                            images = "all",
                            embed = TRUE,
                            title = "Image snapshot changes",
                            diff_dir = NULL,
                            preset = NULL) {
  format <- match.arg(format)
  images <- match.arg(images, c("all", "diff"))

  if (!is.character(path) || length(path) != 1 || is.na(path) ||
      !dir.exists(path)) {
    stop("Snapshot directory not found: ", paste(path, collapse = ", "),
         call. = FALSE)
  }
  if (format %in% c("html", "junit") && is.null(output_file)) {
    stop("output_file is required for format = \"", format, "\".",
         call. = FALSE)
  }

  if (!is.null(preset)) {
    values <- odiff_preset(preset)
    if (missing(threshold)) threshold <- values$threshold
    if (missing(antialiasing)) antialiasing <- values$antialiasing
  }

  if (is.null(diff_dir)) {
    diff_dir <- if (!is.null(output_file)) {
      file.path(dirname(output_file), "snapshot-diffs")
    } else {
      file.path(tempdir(), "odiffr-snapshot-report")
    }
  }
  if (.path_is_within(diff_dir, path)) {
    stop("diff_dir must not be inside the snapshot directory: testthat ",
         "deletes unknown files there.", call. = FALSE)
  }

  pairs <- .snapshot_pairs(path)

  if (nrow(pairs) > 0 && !odiff_available()) {
    stop("odiff binary not available. Install it with install_odiff().",
         call. = FALSE)
  }

  # Compare snapshots that have a baseline; the others become error rows
  has_base <- file.exists(pairs$img1)
  if (dir.exists(diff_dir)) {
    # Diffs from a previous report would be misleading
    old <- list.files(diff_dir, pattern = "^[0-9]{3,}_.*_diff\\.png$",
                      full.names = TRUE)
    unlink(old)
  }
  compared <- compare_images_batch(
    pairs[has_base, c("img1", "img2"), drop = FALSE],
    diff_dir = if (any(has_base)) diff_dir else NULL,
    threshold = threshold,
    antialiasing = antialiasing,
    ...
  )
  rows <- lapply(seq_len(nrow(pairs)), function(i) {
    if (has_base[[i]]) {
      j <- sum(has_base[seq_len(i)])
      row <- as.data.frame(compared[j, , drop = FALSE],
                           stringsAsFactors = FALSE)
      row <- row[, .batch_columns, drop = FALSE]
      row$pair_id <- i
      row
    } else {
      .batch_row(i, img1 = pairs$img1[[i]], img2 = pairs$img2[[i]],
                 error = "no baseline snapshot")
    }
  })
  batch <- .as_odiffr_batch(rows)
  batch$snapshot <- pairs$snapshot

  switch(
    format,
    html = batch_report(batch, output_file = output_file, title = title,
                        embed = embed, images = images, show_all = TRUE),
    junit = batch_junit(batch, output_file = output_file,
                        suite_name = "image snapshots"),
    markdown = .snapshot_markdown(batch, output_file, title)
  )

  invisible(batch)
}


# Internal: data.frame of `.new.png` files under `path` and their baselines
.snapshot_pairs <- function(path) {
  new <- list.files(path, pattern = "\\.new\\.png$", recursive = TRUE,
                    full.names = FALSE, ignore.case = TRUE)
  new <- sort(new)
  rel <- sub("\\.new\\.png$", ".png", new, ignore.case = TRUE)
  data.frame(
    img1 = file.path(path, rel),
    img2 = file.path(path, new),
    snapshot = rel,
    stringsAsFactors = FALSE
  )
}


# Internal: Markdown output; an empty batch gets a short note instead of
# batch_markdown()'s "No comparisons were run."
.snapshot_markdown <- function(batch, output_file, title) {
  if (nrow(batch) > 0) {
    return(batch_markdown(batch, output_file = output_file, title = title))
  }
  md <- paste0("## ", gsub("[\r\n]+", " ", title), "\n\n",
               "No image snapshot changes.\n")
  if (is.null(output_file)) {
    env <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
    if (nzchar(env)) output_file <- env
  }
  if (is.null(output_file)) {
    return(invisible(md))
  }
  if (file.exists(output_file) && isTRUE(file.size(output_file) > 0)) {
    md <- paste0("\n", md)
  }
  .write_utf8(md, output_file, append = TRUE, newline = FALSE)
  invisible(output_file)
}


# Internal: is `x` the directory `parent` or inside it? Works for paths
# that do not exist yet by normalising their deepest existing ancestor.
.path_is_within <- function(x, parent) {
  norm <- function(p) {
    rest <- character(0)
    while (!file.exists(p) && !identical(dirname(p), p)) {
      rest <- c(basename(p), rest)
      p <- dirname(p)
    }
    p <- normalizePath(p, winslash = "/", mustWork = FALSE)
    p <- sub("/+$", "", p)
    paste(c(p, rest), collapse = "/")
  }
  x <- norm(x)
  parent <- norm(parent)
  identical(x, parent) || startsWith(x, paste0(parent, "/"))
}
