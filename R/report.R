#' Generate HTML Report for Batch Comparison Results
#'
#' Creates a standalone HTML report summarizing batch image comparison results.
#' Includes pass/fail statistics, failure reasons, diff statistics, and
#' thumbnails of the worst offenders.
#'
#' @param object An `odiffr_batch` object from [compare_images_batch()] or
#'   [compare_image_dirs()].
#' @param output_file Path to write the HTML file. If NULL, returns HTML as
#'   a character string. The file is written as UTF-8 and its parent
#'   directory is created if it does not exist.
#' @param title Report title. Default: "odiffr Comparison Report".
#' @param embed If TRUE, embed diff images as base64 data URIs for a fully
#'   self-contained file. If FALSE (default), link to image files on disk
#'   using `file://` URIs.
#' @param relative_paths If TRUE and `output_file` is specified, use paths
#'   relative to the report location for image `src` attributes. This makes
#'   reports portable without embedding. Paths are percent-encoded so that
#'   file names containing spaces, `#`, `?` or `%` work. If no relative path
#'   can be built (e.g. different drives on Windows), a `file://` URI is
#'   used instead. Ignored when `embed = TRUE`. Default: FALSE.
#' @param n_worst Number of worst offenders to display. Default: 10.
#' @param show_all If TRUE, include a table of all comparisons. Default: FALSE.
#' @param images Which images to show for each comparison: `"diff"`
#'   (default) shows only the diff image; `"all"` shows the baseline
#'   (`img1`), current (`img2`) and diff images side by side, each with a
#'   caption. Clicking a thumbnail shows the full-size image (a link to the
#'   file for linked reports, an in-page zoom for embedded ones).
#' @param ... Additional arguments passed to [summary.odiffr_batch()].
#'
#' @return If `output_file` is NULL, returns the HTML as a character string
#'   (invisibly). If `output_file` is specified, writes the file and returns
#'   the file path (invisibly).
#'
#' @details
#' Diff image thumbnails (or embedded images when `embed = TRUE`) are only
#' shown for comparisons where a `diff_output` file was created. This requires
#' using `diff_dir` in [compare_images_batch()] or [compare_image_dirs()].
#' Comparisons without diff images will show "No diff" in the preview column.
#'
#' With `images = "all"`, baseline and current images are linked or embedded
#' in the same way as diff images (`embed`, `relative_paths`). Embedded
#' images get a MIME type based on their file extension (PNG, JPEG, WebP,
#' BMP or TIFF; note that most browsers cannot display TIFF). Images that are
#' not files on disk (for example `"<magick-image>"` inputs) or that no
#' longer exist are shown as a placeholder. The report stays a single HTML
#' file with inline CSS and no JavaScript.
#'
#' Failures without pixel statistics (layout differences, errors, or baseline
#' images with no current counterpart) show "-" for the diff percentage and
#' pixel count. If the results contain an `error` column, its message is
#' shown in the Reason column. An empty batch produces a valid report with a
#' pass rate of "-".
#'
#' @seealso [compare_images_batch()], [compare_image_dirs()],
#'   [summary.odiffr_batch()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- compare_image_dirs("baseline/", "current/", diff_dir = "diffs/")
#'
#' # Generate report file
#' batch_report(results, output_file = "report.html")
#'
#' # Self-contained report with embedded images
#' batch_report(results, output_file = "report.html", embed = TRUE)
#'
#' # Baseline, current and diff images side by side
#' batch_report(results, output_file = "report.html", images = "all")
#'
#' # Get HTML as string
#' html <- batch_report(results)
#' }
batch_report <- function(object,
                         output_file = NULL,
                         title = "odiffr Comparison Report",
                         embed = FALSE,
                         relative_paths = FALSE,
                         n_worst = 10,
                         show_all = FALSE,
                         images = c("diff", "all"),
                         ...) {
  stopifnot(inherits(object, "odiffr_batch"))
  images <- match.arg(images)


  n_worst <- suppressWarnings(as.integer(n_worst))
  if (is.na(n_worst) || n_worst < 0) {
    stop("n_worst must be a non-negative integer.", call. = FALSE)
  }


  summ <- summary(object, n_worst = n_worst, ...)


  html <- .build_html_report(
    batch = object,
    summ = summ,
    title = title,
    embed = embed,
    show_all = show_all,
    output_file = output_file,
    relative_paths = relative_paths,
    images = images
  )


  if (!is.null(output_file)) {
    out_dir <- dirname(output_file)
    if (!dir.exists(out_dir)) {
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    }
    # Write bytes as UTF-8 regardless of the native encoding (Windows).
    con <- file(output_file, open = "wb")
    on.exit(close(con), add = TRUE)
    writeLines(enc2utf8(html), con, useBytes = TRUE)
    invisible(output_file)
  } else {
    invisible(html)
  }
}


.build_html_report <- function(batch, summ, title, embed, show_all,
                               output_file = NULL, relative_paths = FALSE,
                               images = "diff") {
  paste0(
    .html_head(title),
    "<body>\n",
    .html_header(title),
    .html_summary_section(summ),
    .html_worst_section(summ, embed, output_file, relative_paths, images),
    if (show_all) .html_all_results_section(batch, embed, output_file, relative_paths, images) else "",
    .html_footer(),
    "</body>\n</html>"
  )
}


.html_head <- function(title) {
  sprintf('<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>%s</title>
  <style>
%s
  </style>
</head>
', .html_escape(title), .report_css())
}


.report_css <- function() {
  paste0(
    "* { box-sizing: border-box; }\n",
    "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; ",
    "line-height: 1.6; max-width: 1200px; margin: 0 auto; padding: 20px; color: #333; }\n",
    "h1 { border-bottom: 2px solid #333; padding-bottom: 10px; }\n",
    "h2 { color: #555; margin-top: 30px; }\n",
    "h3 { color: #666; margin-top: 20px; font-size: 1em; }\n",
    ".timestamp { color: #888; font-size: 0.9em; }\n",
    ".stats { display: flex; gap: 20px; margin: 20px 0; }\n",
    ".stat { padding: 20px; border-radius: 8px; text-align: center; min-width: 150px; }\n",
    ".stat.passed { background: #d4edda; border: 1px solid #c3e6cb; }\n",
    ".stat.failed { background: #f8d7da; border: 1px solid #f5c6cb; }\n",
    ".stat .value { font-size: 2em; font-weight: bold; display: block; }\n",
    ".stat .label { font-size: 0.9em; color: #666; }\n",
    "table { width: 100%; border-collapse: collapse; margin: 15px 0; }\n",
    "th, td { padding: 10px; text-align: left; border-bottom: 1px solid #ddd; }\n",
    "th { background: #f5f5f5; font-weight: 600; }\n",
    "tr:hover { background: #f9f9f9; }\n",
    ".pass { color: #28a745; }\n",
    ".fail { color: #dc3545; }\n",
    ".diff-preview { max-width: 200px; max-height: 150px; border: 1px solid #ddd; }\n",
    ".no-image { color: #888; font-style: italic; }\n",
    ".img-set { display: flex; gap: 8px; flex-wrap: wrap; }\n",
    ".img-set figure { margin: 0; text-align: center; width: 160px; }\n",
    ".img-set figcaption { font-size: 0.8em; color: #666; }\n",
    ".img-set .no-image { display: flex; align-items: center; justify-content: center; ",
    "width: 160px; height: 100px; border: 1px dashed #ccc; font-size: 0.85em; }\n",
    ".thumb { max-width: 160px; max-height: 120px; border: 1px solid #ddd; cursor: zoom-in; }\n",
    "img.thumb.zoom:focus { position: fixed; top: 50%; left: 50%; transform: translate(-50%, -50%); ",
    "max-width: 95vw; max-height: 95vh; z-index: 10; background: #fff; ",
    "box-shadow: 0 0 0 100vmax rgba(0, 0, 0, 0.6); cursor: zoom-out; outline: none; }\n",
    ".error-msg { display: block; color: #a94442; font-size: 0.85em; white-space: pre-wrap; }\n",
    ".reasons ul { margin: 10px 0; padding-left: 20px; }\n",
    ".diff-stats table { width: auto; }\n",
    ".diff-stats td:first-child { font-weight: 600; padding-right: 20px; }\n",
    "footer { margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; color: #888; font-size: 0.85em; }"
  )
}


.html_header <- function(title) {
  sprintf('<h1>%s</h1>\n<p class="timestamp">Generated: %s</p>\n',
          .html_escape(title),
          format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
}


.html_summary_section <- function(summ) {
  pass_label <- .fmt_pct(summ$pass_rate * 100, 1)
  fail_label <- .fmt_pct((1 - summ$pass_rate) * 100, 1)

  stats_html <- sprintf(
    '<div class="stats">
  <div class="stat passed">
    <span class="value">%d</span>
    <span class="label">Passed (%s)</span>
  </div>

  <div class="stat failed">
    <span class="value">%d</span>
    <span class="label">Failed (%s)</span>
  </div>
</div>\n',
    summ$passed, pass_label, summ$failed, fail_label
  )

  reasons_html <- ""
  if (!is.null(summ$reason_counts) && length(summ$reason_counts) > 0) {
    reason_items <- vapply(names(summ$reason_counts), function(reason) {
      sprintf("  <li>%s: %d</li>", .html_escape(reason), as.integer(summ$reason_counts[[reason]]))
    }, character(1))

    reasons_html <- sprintf(
      '<div class="reasons">\n<h3>Failure Reasons</h3>\n<ul>\n%s\n</ul>\n</div>\n',
      paste(reason_items, collapse = "\n")
    )
  }

  diff_stats_html <- ""
  if (!is.null(summ$diff_stats)) {
    diff_stats_html <- sprintf(
      '<div class="diff-stats">
<h3>Diff Statistics</h3>
<table>
  <tr><td>Min</td><td>%.2f%%</td></tr>
  <tr><td>Median</td><td>%.2f%%</td></tr>
  <tr><td>Mean</td><td>%.2f%%</td></tr>
  <tr><td>Max</td><td>%.2f%%</td></tr>
</table>
</div>\n',
      summ$diff_stats$min,
      summ$diff_stats$median,
      summ$diff_stats$mean,
      summ$diff_stats$max
    )
  }

  paste0(
    '<section class="summary">\n<h2>Summary</h2>\n',
    stats_html,
    reasons_html,
    diff_stats_html,
    '</section>\n'
  )
}


.html_worst_section <- function(summ, embed, output_file = NULL, relative_paths = FALSE,
                                images = "diff") {
  if (is.null(summ$worst) || nrow(summ$worst) == 0) {
    return('<section class="worst-offenders">\n<h2>Worst Offenders</h2>\n<p>No failures to display.</p>\n</section>\n')
  }

  rows <- vapply(seq_len(nrow(summ$worst)), function(i) {
    row <- summ$worst[i, ]
    img_label <- .row_label(row)
    img_html <- .format_row_images(row, images, embed, output_file, relative_paths)

    sprintf(
      '<tr>\n  <td>%d</td>\n  <td>%s</td>\n  <td>%s</td>\n  <td>%s</td>\n  %s\n  <td>%s</td>\n</tr>',
      i,
      .html_escape(img_label),
      .fmt_pct(row$diff_percentage),
      .fmt_count(row$diff_count),
      .html_reason_cell(row),
      img_html
    )
  }, character(1))

  paste0(
    '<section class="worst-offenders">\n<h2>Worst Offenders</h2>\n<table>\n',
    sprintf('<thead><tr><th>#</th><th>Image</th><th>Diff %%</th><th>Pixels</th><th>Reason</th><th>%s</th></tr></thead>\n',
            .images_header(images)),
    '<tbody>\n',
    paste(rows, collapse = "\n"),
    '\n</tbody>\n</table>\n</section>\n'
  )
}


.html_all_results_section <- function(batch, embed, output_file = NULL, relative_paths = FALSE,
                                      images = "diff") {
  if (nrow(batch) == 0) {
    return('<section class="all-results">\n<h2>All Comparisons</h2>\n<p>No comparisons to display.</p>\n</section>\n')
  }

  rows <- vapply(seq_len(nrow(batch)), function(i) {
    row <- batch[i, ]
    is_match <- isTRUE(row$match)
    status_class <- if (is_match) "pass" else "fail"
    status_text <- if (is_match) "PASS" else "FAIL"

    img_label <- .row_label(row)

    diff_pct <- .fmt_pct(row$diff_percentage)
    diff_cnt <- .fmt_count(row$diff_count)
    img_html <- .format_row_images(row, images, embed, output_file, relative_paths)

    sprintf(
      '<tr>\n  <td>%d</td>\n  <td class="%s">%s</td>\n  <td>%s</td>\n  <td>%s</td>\n  <td>%s</td>\n  %s\n  <td>%s</td>\n</tr>',
      as.integer(row$pair_id),
      status_class, status_text,
      .html_escape(img_label),
      diff_pct,
      diff_cnt,
      .html_reason_cell(row),
      img_html
    )
  }, character(1))

  paste0(
    '<section class="all-results">\n<h2>All Comparisons</h2>\n<table>\n',
    sprintf('<thead><tr><th>#</th><th>Status</th><th>Image</th><th>Diff %%</th><th>Pixels</th><th>Reason</th><th>%s</th></tr></thead>\n',
            .images_header(images)),
    '<tbody>\n',
    paste(rows, collapse = "\n"),
    '\n</tbody>\n</table>\n</section>\n'
  )
}


.html_reason_cell <- function(row) {
  reason <- if ("reason" %in% names(row)) row$reason[[1]] else NA_character_
  err <- .row_error(row)

  label <- if (is.na(reason)) {
    "-"
  } else if (identical(reason, "missing")) {
    "missing (no current image)"
  } else {
    .html_escape(reason)
  }

  # Missing rows already have a self-explanatory label
  if (is.na(err) || identical(reason, "missing")) {
    sprintf("<td>%s</td>", label)
  } else {
    err_html <- .html_escape(err)
    sprintf('<td title="%s">%s<span class="error-msg">%s</span></td>',
            err_html, label, err_html)
  }
}


.format_diff_image <- function(path, embed, output_file = NULL, relative_paths = FALSE,
                               windows = .Platform$OS.type == "windows") {
  if (!.image_file_exists(path)) {
    return('<span class="no-image">No diff</span>')
  }
  src <- .image_uri(path, embed, output_file, relative_paths, windows = windows)
  sprintf('<img class="diff-preview" src="%s" alt="diff" />', .html_escape(src))
}


# Internal: header of the image column in the report tables.
.images_header <- function(images) {
  if (identical(images, "all")) "Images" else "Preview"
}


# Internal: HTML for the image column of a batch row: the diff preview only
# (`images = "diff"`) or baseline, current and diff thumbnails side by side
# (`images = "all"`).
.format_row_images <- function(row, images, embed, output_file = NULL,
                               relative_paths = FALSE,
                               windows = .Platform$OS.type == "windows") {
  get_path <- function(col) {
    if (col %in% names(row)) as.character(row[[col]][[1]]) else NA_character_
  }
  diff_path <- get_path("diff_output")
  if (!identical(images, "all")) {
    return(.format_diff_image(diff_path, embed, output_file, relative_paths,
                              windows = windows))
  }

  reason <- if ("reason" %in% names(row)) row$reason[[1]] else NA_character_
  current_missing <- if (identical(reason, "missing")) "Missing" else NULL

  paste0(
    '<div class="img-set">',
    .format_thumbnail(get_path("img1"), "Baseline", embed, output_file,
                      relative_paths, windows = windows),
    .format_thumbnail(get_path("img2"), "Current", embed, output_file,
                      relative_paths, windows = windows,
                      placeholder = current_missing),
    .format_thumbnail(diff_path, "Diff", embed, output_file,
                      relative_paths, windows = windows,
                      placeholder = "No diff"),
    '</div>'
  )
}


# Internal: a captioned, clickable thumbnail for any image file. Linked
# reports wrap the image in a link to the full-size file; embedded reports
# use a CSS-only zoom on focus (linking would duplicate the data URI, and
# browsers block navigation to data: URLs). Paths that are not files on
# disk show a placeholder.
.format_thumbnail <- function(path, caption, embed, output_file = NULL,
                              relative_paths = FALSE,
                              windows = .Platform$OS.type == "windows",
                              placeholder = NULL) {
  alt <- .html_escape(tolower(caption))
  if (!.image_file_exists(path)) {
    if (is.null(placeholder)) {
      placeholder <- if (!is.na(path) && grepl("^<.*>$", path)) {
        "In memory (no file)"
      } else {
        "Not available"
      }
    }
    body <- sprintf('<span class="no-image">%s</span>', .html_escape(placeholder))
  } else {
    src <- .html_escape(.image_uri(path, embed, output_file, relative_paths,
                                   windows = windows))
    body <- if (embed) {
      sprintf('<img class="thumb zoom" tabindex="0" src="%s" alt="%s" title="Click to enlarge" />',
              src, alt)
    } else {
      sprintf('<a href="%s" target="_blank"><img class="thumb" src="%s" alt="%s" /></a>',
              src, src, alt)
    }
  }
  sprintf('<figure>%s<figcaption>%s</figcaption></figure>', body, .html_escape(caption))
}


# Internal: TRUE if `path` names an existing file (not NA, "" or a
# placeholder label such as "<magick-image>").
.image_file_exists <- function(path) {
  path <- as.character(path)
  length(path) == 1 && !.is_placeholder_path(path) && file.exists(path) &&
    !dir.exists(path)
}


# Internal: URI for an image file: a base64 data URI when `embed` is TRUE,
# otherwise a relative URL or file:// URI (see .image_src()). The result
# still needs HTML escaping.
.image_uri <- function(path, embed, output_file = NULL, relative_paths = FALSE,
                       windows = .Platform$OS.type == "windows") {
  if (embed) {
    raw_data <- readBin(path, "raw", file.info(path)$size)
    paste0("data:", .image_mime(path), ";base64,", .base64_encode(raw_data))
  } else {
    .image_src(path, output_file, relative_paths, windows = windows)
  }
}


# Internal: MIME type of an image file from its extension; PNG when the
# extension is unknown (odiff diff images are always PNG).
.image_mime <- function(path) {
  base <- basename(as.character(path))
  ext <- ifelse(grepl(".", base, fixed = TRUE),
                tolower(sub("^.*\\.", "", base)), "")
  types <- c(png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
             webp = "image/webp", bmp = "image/bmp", tif = "image/tiff",
             tiff = "image/tiff")
  out <- unname(types[ext])
  out[is.na(out)] <- "image/png"
  out
}


# Internal: URL to use in an <img src> for a file on disk. Returns a
# percent-encoded relative URL when requested and possible, otherwise a
# file:// URI for the absolute path. The result still needs HTML escaping.
.image_src <- function(path, output_file = NULL, relative_paths = FALSE,
                       windows = .Platform$OS.type == "windows") {
  if (relative_paths && !is.null(output_file)) {
    rel <- .relative_path_or_na(path, output_file, windows = windows)
    if (!is.na(rel)) {
      return(.encode_url_path(rel))
    }
  }
  .file_uri(normalizePath(path, mustWork = FALSE), windows = windows)
}


# Internal: percent-encode each "/"-separated segment of a path, keeping the
# separators and "." / ".." segments as is.
.encode_url_path <- function(path) {
  path <- enc2utf8(as.character(path))
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (length(parts) == 0) return(path)
  enc <- vapply(parts, function(seg) {
    if (seg %in% c("", ".", "..")) {
      seg
    } else {
      utils::URLencode(seg, reserved = TRUE, repeated = TRUE)
    }
  }, character(1), USE.NAMES = FALSE)
  out <- paste(enc, collapse = "/")
  # strsplit() drops a trailing empty segment; restore a trailing slash
  if (endsWith(path, "/") && !endsWith(out, "/")) out <- paste0(out, "/")
  out
}


# Internal: convert an absolute file system path to a file:// URI.
# `windows` selects Windows path semantics (backslashes as separators,
# drive letters, UNC paths) so that the behaviour can be tested anywhere.
.file_uri <- function(path, windows = .Platform$OS.type == "windows") {
  path <- enc2utf8(as.character(path))
  if (windows) {
    path <- gsub("\\", "/", path, fixed = TRUE)

    # UNC path: //server/share/dir/file -> file://server/share/dir/file
    if (grepl("^//[^/]", path)) {
      rest <- sub("^//", "", path)
      host <- sub("/.*$", "", rest)
      tail <- substring(rest, nchar(host) + 1)
      return(paste0("file://", utils::URLencode(host, reserved = TRUE, repeated = TRUE),
                    .encode_url_path(tail)))
    }

    # Drive letter: C:/dir/file -> file:///C:/dir/file
    if (grepl("^[A-Za-z]:", path)) {
      drive <- substr(path, 1, 2)
      rest <- sub("^/+", "", substring(path, 3))
      return(paste0("file:///", drive, "/", .encode_url_path(rest)))
    }
  }

  if (!startsWith(path, "/")) path <- paste0("/", path)
  paste0("file://", .encode_url_path(path))
}


.make_relative_path <- function(target_path, from_file,
                                windows = .Platform$OS.type == "windows") {
  rel <- .relative_path_or_na(target_path, from_file, windows = windows)
  # On failure, return original path
  if (is.na(rel)) target_path else rel
}


# Internal: relative path (with "/" separators) from the directory of
# `from_file` to `target_path`, or NA if none can be built (e.g. different
# drives on Windows). Components are compared case-insensitively on Windows.
.relative_path_or_na <- function(target_path, from_file,
                                 windows = .Platform$OS.type == "windows") {
  # normalizePath with mustWork=FALSE is safe here because we only call this
  # function when the target file exists (checked by .image_file_exists).
  # For from_file, we normalize the directory (which should exist) rather than
  # the file itself (which may not exist yet), to ensure consistent symlink
  # resolution on macOS where /var -> /private/var.
  if (windows) {
    target_path <- gsub("\\", "/", target_path, fixed = TRUE)
    from_file <- gsub("\\", "/", from_file, fixed = TRUE)
  }
  target_abs <- normalizePath(target_path, mustWork = FALSE)
  from_dir <- normalizePath(dirname(from_file), mustWork = FALSE)

  tryCatch({
    # Normalize all paths to use forward slashes for consistent splitting.
    # On Windows, normalizePath() may return backslashes, but we want to split
    # consistently across platforms and output forward slashes for HTML.
    if (windows) {
      target_abs <- gsub("\\", "/", target_abs, fixed = TRUE)
      from_dir <- gsub("\\", "/", from_dir, fixed = TRUE)
    }

    target_parts <- strsplit(target_abs, "/", fixed = TRUE)[[1]]
    from_parts <- strsplit(from_dir, "/", fixed = TRUE)[[1]]

    # Remove empty strings that can result from trailing separators or UNC paths
    target_parts <- target_parts[nzchar(target_parts)]
    from_parts <- from_parts[nzchar(from_parts)]

    if (length(target_parts) == 0 || length(from_parts) == 0) {
      return(NA_character_)
    }

    # Windows file systems (and drive letters) are case-insensitive
    key <- if (windows) tolower else identity
    n <- min(length(target_parts), length(from_parts))
    same <- key(target_parts[seq_len(n)]) == key(from_parts[seq_len(n)])
    common_len <- if (all(same)) n else which(!same)[1] - 1

    # If no common prefix (e.g., different drives on Windows), give up
    if (common_len == 0) {
      return(NA_character_)
    }

    ups <- length(from_parts) - common_len
    remaining <- if (common_len < length(target_parts)) {
      target_parts[(common_len + 1):length(target_parts)]
    } else {
      character(0)
    }
    rel_parts <- c(rep("..", ups), remaining)

    if (length(rel_parts) == 0) {
      return(".")
    }

    paste(rel_parts, collapse = "/")
  }, error = function(e) NA_character_)
}


.html_escape <- function(x) {
  x <- as.character(x)
  na <- is.na(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x[na] <- ""
  x
}


.html_footer <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("odiffr")),
    error = function(e) "dev"
  )
  sprintf('<footer>\n<p>Generated by odiffr %s</p>\n</footer>\n', version)
}


# Internal: RFC 4648 base64 encoding of a raw vector (vectorized, no
# dependencies).
.base64_encode <- function(raw_data) {
  n <- length(raw_data)
  if (n == 0) return("")

  alphabet <- charToRaw(
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  )

  padding <- (3 - n %% 3) %% 3
  bytes <- as.integer(c(raw_data, as.raw(rep(0L, padding))))
  m <- matrix(bytes, nrow = 3L)
  b1 <- m[1L, ]
  b2 <- m[2L, ]
  b3 <- m[3L, ]

  idx <- rbind(
    b1 %/% 4L,
    (b1 %% 4L) * 16L + b2 %/% 16L,
    (b2 %% 16L) * 4L + b3 %/% 64L,
    b3 %% 64L
  )
  out <- alphabet[as.vector(idx) + 1L]

  if (padding > 0) {
    out[(length(out) - padding + 1L):length(out)] <- charToRaw("=")
  }

  rawToChar(out)
}
