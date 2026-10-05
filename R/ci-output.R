#' Write Batch Comparison Results as JUnit XML
#'
#' Converts batch image comparison results to a JUnit XML report, the format
#' understood by most CI systems (GitHub Actions test reporters, GitLab,
#' Jenkins, Azure Pipelines, ...). Each comparison becomes one test case.
#'
#' @param object An `odiffr_batch` object from [compare_images_batch()] or
#'   [compare_image_dirs()].
#' @param output_file Path to write the XML file. If NULL (default), the XML
#'   is returned as a character string. The file is written as UTF-8 and its
#'   parent directory is created if it does not exist.
#' @param suite_name Name of the test suite, also used as the `classname` of
#'   every test case. Default: "odiffr".
#' @param include_passed If TRUE (default), passing comparisons are included
#'   as successful test cases. If FALSE, only failures and errors are written.
#'
#' @return If `output_file` is NULL, the XML as a character string
#'   (invisibly); otherwise the file path (invisibly).
#'
#' @details
#' The report contains a single `<testsuite>` (inside a `<testsuites>` root)
#' whose `tests`, `failures` and `errors` attributes count the test cases
#' written. Test cases are named after the current image file (`img2`), or
#' the baseline (`img1`) when `img2` is not a file (for example
#' `"<magick-image>"`), or `"pair N"` otherwise.
#'
#' * Pixel and layout differences are reported as `<failure>` elements whose
#'   `type` is the comparison reason, e.g.
#'   `message="pixel-diff: 1.26% (126 pixels)" type="pixel-diff"`.
#' * Baseline images without a current counterpart are failures of type
#'   `"missing"`.
#' * Comparisons that could not be run (`reason == "error"`) are reported as
#'   `<error>` elements carrying the error message.
#'
#' The failure body lists the baseline, current and diff image paths. Text is
#' XML-escaped and characters that are not allowed in XML 1.0 are removed.
#'
#' @seealso [batch_markdown()], [batch_report()], [compare_image_dirs()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- compare_image_dirs("baseline/", "current/", diff_dir = "diffs/")
#' batch_junit(results, "odiffr-junit.xml")
#' }
batch_junit <- function(object,
                        output_file = NULL,
                        suite_name = "odiffr",
                        include_passed = TRUE) {
  stopifnot(inherits(object, "odiffr_batch"))
  if (!is.character(suite_name) || length(suite_name) != 1 || is.na(suite_name)) {
    stop("suite_name must be a single string.", call. = FALSE)
  }
  include_passed <- isTRUE(include_passed)

  rows <- seq_len(nrow(object))
  passed <- vapply(rows, function(i) isTRUE(object$match[[i]]), logical(1))
  if (!include_passed) rows <- rows[!passed]

  kinds <- character(0)
  cases <- vapply(rows, function(i) {
    row <- object[i, , drop = FALSE]
    case <- .junit_testcase(row, suite_name)
    kinds <<- c(kinds, attr(case, "kind"))
    case
  }, character(1))

  n_tests <- length(rows)
  n_fail <- sum(kinds == "failure")
  n_err <- sum(kinds == "error")
  suite_attrs <- sprintf(
    'name="%s" tests="%d" failures="%d" errors="%d" skipped="0"',
    .xml_escape(suite_name), n_tests, n_fail, n_err
  )

  xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>\n',
    '<testsuites ', suite_attrs, '>\n',
    '  <testsuite ', suite_attrs,
    sprintf(' timestamp="%s">\n', format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
    paste0(cases, collapse = ""),
    '  </testsuite>\n',
    '</testsuites>\n'
  )

  if (is.null(output_file)) {
    return(invisible(xml))
  }
  .write_utf8(xml, output_file)
  invisible(output_file)
}


# Internal: one <testcase> element for a batch row. The attribute "kind"
# is "passed", "failure" or "error".
.junit_testcase <- function(row, suite_name) {
  name <- .row_label(row, use_img1 = TRUE)
  open_tag <- sprintf('    <testcase name="%s" classname="%s" time="0"',
                      .xml_escape(name), .xml_escape(suite_name))

  if (isTRUE(row$match[[1]])) {
    return(structure(paste0(open_tag, "/>\n"), kind = "passed"))
  }

  reason <- if ("reason" %in% names(row)) row$reason[[1]] else NA_character_
  err <- .row_error(row)
  is_error <- identical(reason, "error")
  type <- if (is.na(reason) || !nzchar(reason)) "failure" else reason

  message <- if (identical(reason, "pixel-diff") && !is.na(row$diff_percentage[[1]])) {
    sprintf("pixel-diff: %s (%s pixels)",
            .fmt_pct(row$diff_percentage[[1]]), .fmt_count(row$diff_count[[1]]))
  } else if (identical(reason, "layout-diff") && is.na(err)) {
    "layout-diff: images have different dimensions"
  } else if (is_error && !is.na(err)) {
    err
  } else {
    .row_reason_text(row)
  }

  get_path <- function(col) {
    if (col %in% names(row)) as.character(row[[col]][[1]]) else NA_character_
  }
  na_dash <- function(x) if (is.na(x) || !nzchar(x)) "-" else x
  details <- c(
    paste0("Baseline: ", na_dash(get_path("img1"))),
    paste0("Current: ", na_dash(get_path("img2"))),
    paste0("Diff image: ", na_dash(get_path("diff_output"))),
    if (!is.na(err)) paste0("Error: ", err)
  )

  element <- if (is_error) "error" else "failure"
  body <- sprintf('      <%s message="%s" type="%s">%s</%s>\n',
                  element, .xml_escape(message, attribute = TRUE),
                  .xml_escape(type), .xml_escape(paste(details, collapse = "\n")),
                  element)
  structure(paste0(open_tag, ">\n", body, "    </testcase>\n"),
            kind = element)
}


# Internal: escape text for XML 1.0 content or attribute values, removing
# characters that are not allowed in XML 1.0 (most C0 control characters,
# surrogates, U+FFFE/U+FFFF) and invalid UTF-8 bytes. With
# `attribute = TRUE`, tabs and line breaks are kept as character references
# (they would otherwise be normalised to spaces by XML parsers).
.xml_escape <- function(x, attribute = FALSE) {
  x <- as.character(x)
  na <- is.na(x)
  x[na] <- ""
  x <- enc2utf8(x)
  x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "")
  x[is.na(x)] <- ""
  x <- vapply(x, .xml_strip_invalid, character(1), USE.NAMES = FALSE)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  if (attribute) {
    x <- gsub("\t", "&#9;", x, fixed = TRUE)
    x <- gsub("\n", "&#10;", x, fixed = TRUE)
    x <- gsub("\r", "&#13;", x, fixed = TRUE)
  }
  x
}


# Internal: drop code points that are not allowed in XML 1.0 from a single
# valid UTF-8 string.
.xml_strip_invalid <- function(s) {
  cp <- utf8ToInt(s)
  if (length(cp) == 0 || anyNA(cp)) return("")
  keep <- cp == 9L | cp == 10L | cp == 13L |
    (cp >= 32L & cp <= 0xD7FF) | (cp >= 0xE000 & cp <= 0xFFFD) |
    cp >= 0x10000
  if (all(keep)) s else intToUtf8(cp[keep])
}


#' Write a Markdown Summary of Batch Comparison Results
#'
#' Produces a GitHub-flavoured Markdown summary of batch image comparison
#' results: a heading, a pass/fail line, a breakdown of failure reasons and a
#' table of the worst offenders. On GitHub Actions the summary is appended to
#' the job summary page by default.
#'
#' @param object An `odiffr_batch` object from [compare_images_batch()] or
#'   [compare_image_dirs()].
#' @param output_file Path to write the Markdown to. If NULL (default), the
#'   file named by the `GITHUB_STEP_SUMMARY` environment variable is used
#'   when that variable is set; otherwise nothing is written.
#' @param title Heading of the summary. Default: "odiffr comparison".
#' @param n_worst Maximum number of failures listed in the table. Default: 10.
#'   Use 0 to omit the table.
#' @param append If TRUE (default), append to `output_file` instead of
#'   overwriting it, as GitHub step summaries are built up by appending.
#'
#' @return If the summary is written to a file, the file path (invisibly);
#'   otherwise the Markdown as a character string (invisibly). Nothing is
#'   printed.
#'
#' @details
#' The worst offenders table has columns Image, Reason, Diff %, Pixels and
#' Error, ordered as in [summary.odiffr_batch()]. Cell text is escaped so that
#' `|`, line breaks, Markdown emphasis characters and HTML-like text such as
#' `<magick-image>` are shown literally. Files are written as UTF-8 and the
#' parent directory is created if needed.
#'
#' @seealso [batch_junit()], [batch_report()], [summary.odiffr_batch()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- compare_image_dirs("baseline/", "current/", diff_dir = "diffs/")
#'
#' # In a GitHub Actions step: appends to the job summary
#' batch_markdown(results)
#'
#' # Anywhere else: get the Markdown as a string
#' md <- batch_markdown(results)
#' cat(md)
#' }
batch_markdown <- function(object,
                           output_file = NULL,
                           title = "odiffr comparison",
                           n_worst = 10,
                           append = TRUE) {
  stopifnot(inherits(object, "odiffr_batch"))

  n_worst <- suppressWarnings(as.integer(n_worst))
  if (length(n_worst) != 1 || is.na(n_worst) || n_worst < 0) {
    stop("n_worst must be a non-negative integer.", call. = FALSE)
  }

  summ <- summary(object, n_worst = n_worst)
  md <- .build_markdown(summ, title)

  if (is.null(output_file)) {
    env <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = "")
    if (nzchar(env)) output_file <- env
  }
  if (is.null(output_file)) {
    return(invisible(md))
  }

  text <- md
  if (isTRUE(append) && file.exists(output_file) && isTRUE(file.size(output_file) > 0)) {
    # Separate from earlier content in the same summary file
    text <- paste0("\n", text)
  }
  .write_utf8(text, output_file, append = isTRUE(append), newline = FALSE)
  invisible(output_file)
}


.build_markdown <- function(summ, title) {
  title <- gsub("[\r\n]+", " ", as.character(title))
  lines <- c(paste0("## ", title), "")

  if (summ$total == 0) {
    lines <- c(lines, "**No comparisons were run.**")
  } else if (summ$failed == 0) {
    lines <- c(lines, sprintf("**All %d %s passed.**", summ$total,
                              .plural(summ$total, "comparison")))
  } else {
    lines <- c(lines, sprintf(
      "**%d of %d %s failed** (%d passed, %s pass rate).",
      summ$failed, summ$total, .plural(summ$total, "comparison"),
      summ$passed, .fmt_pct(summ$pass_rate * 100, 1)
    ))
  }

  if (!is.null(summ$reason_counts) && length(summ$reason_counts) > 0) {
    reasons <- names(summ$reason_counts)
    lines <- c(lines, "", "Failure reasons:", "",
               sprintf("- %s: %d", .md_escape(reasons),
                       as.integer(summ$reason_counts[reasons])))
  }

  worst <- summ$worst
  if (!is.null(worst) && nrow(worst) > 0) {
    cells <- t(vapply(seq_len(nrow(worst)), function(i) {
      row <- worst[i, , drop = FALSE]
      reason <- row$reason[[1]]
      err <- .row_error(row)
      reason_lbl <- if (is.na(reason)) "-" else if (identical(reason, "missing")) {
        "missing (no current image)"
      } else {
        reason
      }
      c(.md_escape(.row_label(row)),
        .md_escape(reason_lbl),
        .fmt_pct(row$diff_percentage[[1]]),
        .fmt_count(row$diff_count[[1]]),
        if (is.na(err) || identical(reason, "missing")) "-" else .md_escape(err))
    }, character(5)))

    heading <- "### Worst offenders"
    note <- if (summ$failed > nrow(worst)) {
      c("", sprintf("Showing %d of %d failures.", nrow(worst), summ$failed))
    }
    lines <- c(
      lines, "", heading, note, "",
      "| # | Image | Reason | Diff % | Pixels | Error |",
      "|---:|:---|:---|---:|---:|:---|",
      sprintf("| %d | %s | %s | %s | %s | %s |", seq_len(nrow(cells)),
              cells[, 1], cells[, 2], cells[, 3], cells[, 4], cells[, 5])
    )
  }

  paste0(paste(lines, collapse = "\n"), "\n")
}


# Internal: escape text for a GitHub-flavoured Markdown table cell.
.md_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("([\\\\`*_\\[\\]|])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\r\n|\r|\n", "<br>", x)
  x
}


.plural <- function(n, word) {
  if (n == 1) word else paste0(word, "s")
}


# Internal: write text to a file as UTF-8 bytes, creating the parent
# directory if needed.
.write_utf8 <- function(text, path, append = FALSE, newline = FALSE) {
  out_dir <- dirname(path)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  con <- file(path, open = if (append) "ab" else "wb")
  on.exit(close(con), add = TRUE)
  writeLines(enc2utf8(text), con, sep = if (newline) "\n" else "", useBytes = TRUE)
  invisible(path)
}
