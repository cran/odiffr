#' Compare Two PDF Files Page by Page
#'
#' Render each page of two PDF files to PNG and compare them page by page
#' with odiff. This is useful for checking that rendered documents (for
#' example clinical tables, listings and figures, or Quarto/R Markdown
#' documents rendered to PDF) still look the same after a code, package or
#' R upgrade.
#'
#' Requires the \pkg{pdftools} package (and therefore the poppler library)
#' for rendering. Only PDF input is supported: convert RTF or DOCX outputs
#' to PDF first, for example with LibreOffice
#' (`soffice --headless --convert-to pdf file.rtf`).
#'
#' @param baseline Path to the baseline (reference) PDF file.
#' @param current Path to the current PDF file to compare against
#'   `baseline`.
#' @param pages Integer vector of page numbers to compare, or `NULL`
#'   (default) to compare all pages of both files (the union of their page
#'   ranges). Pages that do not exist in either file raise an error.
#' @param dpi Resolution, in dots per inch, used to render the pages.
#'   Default is 150. Higher values detect smaller differences but take
#'   longer and produce larger images.
#' @param diff_dir Directory to save diff images. If `NULL` (default), no
#'   diff images are created. When given, the rendered pages are also kept
#'   under `file.path(diff_dir, "pages")` (in `baseline/` and `current/`
#'   subdirectories).
#' @param threshold Numeric; colour difference threshold between 0.0 and 1.0.
#'   Default is 0.1.
#' @param antialiasing Logical; if `TRUE`, ignore antialiased pixels.
#'   Default is `FALSE`.
#' @param ignore_regions Regions to ignore on every compared page, as
#'   accepted by [compare_images()] (see [ignore_region()]). Coordinates are
#'   in pixels of the rendered page, so they depend on `dpi`: a point at
#'   `x` inches from the left edge is at pixel `x * dpi`.
#' @param parallel Logical; if `TRUE`, compare pages in parallel. See
#'   [compare_images_batch()] for details.
#' @param ... Additional arguments passed to [compare_images()] via
#'   [compare_images_batch()], e.g. `fail_on_layout = TRUE`.
#'
#' @return A tibble (if available) or data.frame with class `odiffr_batch`,
#'   with one row per compared page, containing the same columns as
#'   [compare_images_batch()] (including `error`) plus a trailing integer
#'   `page` column. `img1` and `img2` are the paths of the rendered page
#'   images. The result can be passed to [summary()], [batch_report()] and
#'   the other functions that accept an `odiffr_batch`.
#'
#' @details
#' Pages are rendered with [pdftools::pdf_convert()] to files named
#' `<pdf name>_p001.png`, `<pdf name>_p002.png`, ... in separate `baseline/`
#' and `current/` directories. If `diff_dir` is `NULL`, they are written to
#' a new directory inside [tempdir()], which persists until the R session
#' ends, so that reports showing the rendered pages can still be created
#' after `compare_pdfs()` returns.
#'
#' When the two files have a different number of pages, a message is
#' emitted and the pages present in only one file are reported using the
#' existing reasons, so that summaries and reports work unchanged:
#' \itemize{
#'   \item A page present in `baseline` but not in `current` is reported
#'     with `match = FALSE`, `reason = "missing"` and the error
#'     `"Page N not present in current PDF"` (like a file missing from the
#'     current directory in [compare_image_dirs()]).
#'   \item A page present in `current` but not in `baseline` is reported
#'     with `match = FALSE`, `reason = "error"` and the error
#'     `"Page N not present in baseline PDF"`.
#' }
#' In both cases `img1`/`img2` point to the rendered page that exists and
#' to the (nonexistent) path the other page would have had.
#'
#' Pages with different sizes (e.g. portrait versus landscape) are compared
#' as images of different dimensions; by default odiff reports the
#' differing pixels, and with `fail_on_layout = TRUE` such pages are
#' reported with `reason = "layout-diff"`.
#'
#' An error is raised if a file does not exist or cannot be read as a PDF
#' (for example a corrupt or password-protected file).
#'
#' @seealso [compare_pdf_dirs()] for comparing directories of PDF files,
#'   [compare_images_batch()], [batch_report()].
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- compare_pdfs("qc/t_demog.pdf", "prod/t_demog.pdf",
#'                     diff_dir = "pdf-diffs")
#' summary(res)
#' res[!res$match, c("page", "reason", "diff_percentage")]
#'
#' # Only the first two pages, at a higher resolution
#' compare_pdfs("old.pdf", "new.pdf", pages = 1:2, dpi = 300)
#'
#' # Ignore a run-date footer: bottom 0.5 inch of a US Letter page at 150 dpi
#' compare_pdfs("old.pdf", "new.pdf", dpi = 150,
#'              ignore_regions = ignore_region(0, 1575, 1275, 1650))
#' }
compare_pdfs <- function(baseline, current,
                         pages = NULL,
                         dpi = 150,
                         diff_dir = NULL,
                         threshold = 0.1,
                         antialiasing = FALSE,
                         ignore_regions = NULL,
                         parallel = FALSE,
                         ...) {
  .require_pdftools()
  .validate_pdf_path(baseline, "baseline")
  .validate_pdf_path(current, "current")
  dpi <- .validate_dpi(dpi)

  n_base <- .pdf_page_count(baseline, "baseline")
  n_curr <- .pdf_page_count(current, "current")

  if (n_base != n_curr) {
    message(sprintf(
      "Page counts differ: baseline has %d page(s), current has %d page(s).",
      n_base, n_curr
    ))
  }

  pages <- .resolve_pdf_pages(pages, max(n_base, n_curr))

  # Where to keep the rendered pages
  render_root <- if (is.null(diff_dir)) {
    tempfile("odiffr_pdf_")
  } else {
    file.path(diff_dir, "pages")
  }
  base_dir <- file.path(render_root, "baseline")
  curr_dir <- file.path(render_root, "current")

  base_pages <- pages[pages <= n_base]
  curr_pages <- pages[pages <= n_curr]
  base_png <- .render_pdf_pages(baseline, base_pages, dpi, base_dir, "baseline")
  curr_png <- .render_pdf_pages(current, curr_pages, dpi, curr_dir, "current")

  # Expected render paths for every requested page (some may not exist)
  img1 <- .pdf_page_paths(baseline, pages, base_dir)
  img2 <- .pdf_page_paths(current, pages, curr_dir)
  img1[match(base_pages, pages)] <- base_png
  img2[match(curr_pages, pages)] <- curr_png

  ids <- seq_along(pages)
  in_base <- pages <= n_base
  in_curr <- pages <= n_curr

  # Remove renders of absent pages left by a previous run in the same
  # diff_dir, so they aren't shown as the current/baseline page
  unlink(c(img1[!in_base], img2[!in_curr]))
  both <- which(in_base & in_curr)

  pairs_list <- lapply(both, function(i) list(img1 = img1[[i]], img2 = img2[[i]]))
  compared <- .compare_pairs(
    pairs_list, ids = ids[both],
    diff_dir = diff_dir, parallel = parallel,
    threshold = threshold, antialiasing = antialiasing,
    ignore_regions = ignore_regions, ...
  )
  rows <- list(.batch_plain(compared))

  only_base <- which(in_base & !in_curr)
  if (length(only_base) > 0) {
    rows[[length(rows) + 1]] <- .batch_row(
      pair_id = ids[only_base],
      match = FALSE,
      reason = "missing",
      img1 = img1[only_base],
      img2 = img2[only_base],
      error = sprintf("Page %d not present in current PDF", pages[only_base])
    )
  }

  only_curr <- which(!in_base & in_curr)
  if (length(only_curr) > 0) {
    rows[[length(rows) + 1]] <- .batch_row(
      pair_id = ids[only_curr],
      match = FALSE,
      reason = "error",
      img1 = img1[only_curr],
      img2 = img2[only_curr],
      error = sprintf("Page %d not present in baseline PDF", pages[only_curr])
    )
  }

  combined <- do.call(rbind, rows)
  combined <- combined[order(combined$pair_id), , drop = FALSE]
  combined$page <- as.integer(pages[combined$pair_id])
  .as_pdf_batch(combined)
}

#' Compare PDF Files in Two Directories
#'
#' Compare every PDF file in a baseline directory with the PDF file of the
#' same relative path in a current directory, page by page, using
#' [compare_pdfs()]. Useful for re-running a full set of outputs (for
#' example all tables, listings and figures of a study) after an R or
#' package upgrade and checking that nothing changed visually.
#'
#' @param baseline_dir Path to the directory containing the baseline PDFs.
#' @param current_dir Path to the directory containing the current PDFs.
#' @param pattern Regular expression matched (case-insensitively) against
#'   file names. Default matches `.pdf` files.
#' @param recursive Logical; if `TRUE`, search subdirectories recursively.
#'   Default is `FALSE`.
#' @param pages,dpi Passed to [compare_pdfs()] for every file.
#' @param diff_dir Directory to save diff images and rendered pages. Each
#'   PDF gets its own subdirectory, named after its relative path. If
#'   `NULL` (default), no diff images are created.
#' @param parallel Logical; if `TRUE`, compare the pages of each file in
#'   parallel. See [compare_images_batch()] for details.
#' @param ... Additional arguments passed to [compare_pdfs()] (e.g.
#'   `threshold`, `antialiasing`, `ignore_regions`, `fail_on_layout`).
#'
#' @return A tibble (if available) or data.frame with class `odiffr_batch`,
#'   combining the results for all files (in baseline file order) with a
#'   sequential `pair_id`, the columns of [compare_pdfs()] (including
#'   `page`) and a trailing `file` column giving the relative path of the
#'   PDF.
#'
#' @details
#' The baseline directory is the source of truth, as in
#' [compare_image_dirs()]:
#' \itemize{
#'   \item A PDF missing from `current_dir` triggers a warning and is
#'     reported as a single row with `match = FALSE`, `reason = "missing"`,
#'     `page = NA` and the error `"File not found in current_dir"`.
#'   \item A PDF that cannot be read (e.g. corrupt or password-protected)
#'     is reported as a single row with `reason = "error"`, `page = NA` and
#'     the error message, instead of aborting the whole comparison.
#'   \item PDFs that exist only in `current_dir` are not compared, but a
#'     message notes how many were found.
#' }
#' Differences in page counts are reported per file as described in
#' [compare_pdfs()].
#'
#' An error is raised if `baseline_dir` contains no files matching
#' `pattern`.
#'
#' @seealso [compare_pdfs()], [compare_image_dirs()], [batch_report()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' res <- compare_pdf_dirs("outputs-r4.3/", "outputs-r4.4/",
#'                         diff_dir = "pdf-diffs", recursive = TRUE)
#' summary(res)
#' unique(res$file[!res$match])
#' batch_report(res, output_file = "pdf-diffs/report.html")
#' }
compare_pdf_dirs <- function(baseline_dir,
                             current_dir,
                             pattern = "\\.pdf$",
                             recursive = FALSE,
                             pages = NULL,
                             dpi = 150,
                             diff_dir = NULL,
                             parallel = FALSE,
                             ...) {
  .require_pdftools()
  .validate_directory(baseline_dir, "baseline_dir")
  .validate_directory(current_dir, "current_dir")

  baseline_files <- list.files(baseline_dir, pattern = pattern,
                               recursive = recursive, ignore.case = TRUE)
  if (length(baseline_files) == 0) {
    stop("No PDF files found in baseline_dir matching pattern: ", pattern,
         call. = FALSE)
  }

  current_files <- list.files(current_dir, pattern = pattern,
                              recursive = recursive, ignore.case = TRUE)
  unmatched <- setdiff(current_files, baseline_files)
  if (length(unmatched) > 0) {
    shown <- unmatched[seq_len(min(3, length(unmatched)))]
    message(sprintf("Note: %d file(s) in current_dir have no baseline: %s%s",
                    length(unmatched), paste(shown, collapse = ", "),
                    if (length(unmatched) > 3) ", ..." else ""))
  }

  base_paths <- file.path(baseline_dir, baseline_files)
  curr_paths <- file.path(current_dir, baseline_files)
  missing <- !file.exists(curr_paths)
  if (any(missing)) {
    shown <- baseline_files[missing][seq_len(min(3, sum(missing)))]
    warning(sum(missing), " file(s) missing from current_dir: ",
            paste(shown, collapse = ", "),
            if (sum(missing) > 3) "..." else "",
            call. = FALSE)
  }

  # Unique per-file subdirectory names for diffs and renders
  sub_dirs <- make.unique(gsub("[^A-Za-z0-9._-]+", "_",
                               tools::file_path_sans_ext(baseline_files)))

  file_row <- function(i, reason, error) {
    row <- .batch_row(
      pair_id = 1L,
      match = FALSE,
      reason = reason,
      img1 = normalizePath(base_paths[[i]], mustWork = FALSE),
      img2 = file.path(normalizePath(current_dir, mustWork = FALSE),
                       baseline_files[[i]]),
      error = error
    )
    row$page <- NA_integer_
    row
  }

  rows <- lapply(seq_along(baseline_files), function(i) {
    res <- if (missing[[i]]) {
      file_row(i, "missing", "File not found in current_dir")
    } else {
      file_diff_dir <- if (is.null(diff_dir)) {
        NULL
      } else {
        file.path(diff_dir, sub_dirs[[i]])
      }
      tryCatch(
        .batch_plain(compare_pdfs(base_paths[[i]], curr_paths[[i]],
                                  pages = pages, dpi = dpi,
                                  diff_dir = file_diff_dir,
                                  parallel = parallel, ...)),
        error = function(e) file_row(i, "error", conditionMessage(e))
      )
    }
    res$file <- rep(baseline_files[[i]], nrow(res))
    res
  })

  combined <- do.call(rbind, rows)
  combined$pair_id <- seq_len(nrow(combined))
  .as_pdf_batch(combined, extra = c("page", "file"))
}

# Internal: stop with an install hint if pdftools is unavailable
.require_pdftools <- function() {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    stop("The pdftools package is required to compare PDF files. ",
         "Install it with install.packages(\"pdftools\").", call. = FALSE)
  }
  invisible(TRUE)
}

# Internal: validate a PDF path argument
.validate_pdf_path <- function(path, arg_name) {
  if (!is.character(path) || length(path) != 1 || is.na(path) ||
      !nzchar(trimws(path))) {
    stop(arg_name, " must be a single PDF file path.", call. = FALSE)
  }
  if (dir.exists(path)) {
    stop(arg_name, " is a directory, not a PDF file: ", path,
         call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(arg_name, " PDF file does not exist: ", path, call. = FALSE)
  }
  invisible(TRUE)
}

# Internal: validate the dpi argument
.validate_dpi <- function(dpi) {
  if (!is.numeric(dpi) || length(dpi) != 1 || is.na(dpi) || dpi <= 0) {
    stop("dpi must be a single positive number.", call. = FALSE)
  }
  dpi
}

# Internal: number of pages of a PDF, with a clear error for unreadable files
.pdf_page_count <- function(path, arg_name) {
  # PDF files start with "%PDF-" within the first 1024 bytes
  head_bytes <- readBin(path, "raw", n = 1024L)
  if (length(grepRaw("%PDF-", head_bytes, fixed = TRUE)) == 0) {
    stop(sprintf("Could not read %s PDF '%s': not a PDF file.",
                 arg_name, path), call. = FALSE)
  }
  n <- tryCatch(
    pdftools::pdf_info(path)$pages,
    error = function(e) {
      stop(sprintf("Could not read %s PDF '%s': %s", arg_name, path,
                   conditionMessage(e)), call. = FALSE)
    }
  )
  as.integer(n)
}

# Internal: validate `pages` against the largest page count
.resolve_pdf_pages <- function(pages, n_max) {
  if (is.null(pages)) {
    return(seq_len(n_max))
  }
  if (!is.numeric(pages) || length(pages) == 0 || anyNA(pages) ||
      any(pages < 1) || any(pages != round(pages))) {
    stop("pages must be NULL or a vector of positive whole page numbers.",
         call. = FALSE)
  }
  pages <- unique(as.integer(pages))
  out <- pages[pages > n_max]
  if (length(out) > 0) {
    stop(sprintf("Page(s) %s not present in either PDF (maximum %d).",
                 paste(out, collapse = ", "), n_max), call. = FALSE)
  }
  pages
}

# Internal: file stem used for rendered page images
.pdf_stem <- function(pdf) {
  stem <- gsub("[^A-Za-z0-9._-]+", "_",
               tools::file_path_sans_ext(basename(pdf)))
  if (!nzchar(stem)) "page" else stem
}

# Internal: render path of each page (whether or not it exists)
.pdf_page_paths <- function(pdf, pages, dir) {
  file.path(normalizePath(dir, mustWork = FALSE),
            sprintf("%s_p%03d.png", .pdf_stem(pdf), pages))
}

# Internal: render `pages` of `pdf` to PNG files in `dir`
.render_pdf_pages <- function(pdf, pages, dpi, dir, arg_name) {
  if (length(pages) == 0) {
    return(character(0))
  }
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  out <- .pdf_page_paths(pdf, pages, dir)
  # pdf_convert() uses a single file name as a sprintf() template
  template <- paste0(gsub("%", "%%", dirname(out[[1]]), fixed = TRUE), "/",
                     gsub("%", "%%", .pdf_stem(pdf), fixed = TRUE),
                     "_p%03d.%s")
  tryCatch(
    pdftools::pdf_convert(pdf, format = "png", pages = pages,
                          filenames = template, dpi = dpi, verbose = FALSE),
    error = function(e) {
      stop(sprintf("Could not render %s PDF '%s': %s", arg_name, pdf,
                   conditionMessage(e)), call. = FALSE)
    }
  )
  out
}

# Internal: an odiffr_batch as a plain data.frame (keeping all columns)
.batch_plain <- function(x) {
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  class(df) <- "data.frame"
  df
}

# Internal: build an odiffr_batch with extra trailing columns
.as_pdf_batch <- function(df, extra = "page") {
  df <- df[, c(.batch_columns, extra), drop = FALSE]
  rownames(df) <- NULL
  if (requireNamespace("tibble", quietly = TRUE)) {
    df <- tibble::as_tibble(df)
  }
  class(df) <- c("odiffr_batch", class(df))
  df
}
