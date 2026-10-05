#' Summarize Batch Comparison Results
#'
#' Generate a summary of batch image comparison results, including pass/fail
#' statistics, failure reasons, and worst offenders.
#'
#' @importFrom stats median
#' @importFrom utils head
#'
#' @param object An `odiffr_batch` object returned by [compare_images_batch()]
#'   or [compare_image_dirs()].
#' @param n_worst Integer; number of worst offenders to include in the summary.
#'   Default is 5.
#' @param ... Additional arguments (currently unused).
#'
#' @return An `odiffr_batch_summary` object with the following components:
#'   \describe{
#'     \item{total}{Total number of comparisons.}
#'     \item{passed}{Number of matching image pairs.}
#'     \item{failed}{Number of non-matching image pairs.}
#'     \item{pass_rate}{Proportion of passing comparisons (0 to 1), or `NA`
#'       for an empty batch (zero comparisons).}
#'     \item{reason_counts}{Table of failure reasons (NULL if no failures).}
#'     \item{diff_stats}{List with min, median, mean, max diff percentages
#'       (NULL if no failures with diff data).
#'     }
#'     \item{worst}{Data frame of worst offenders, ordered by diff percentage
#'       (descending). Failures without a diff percentage (e.g. layout
#'       differences, errors, or missing files) are listed after those with
#'       one. NULL if no failures.
#'     }
#'   }
#'
#' @details
#' The summary method expects the standard output of [compare_images_batch()],
#' which includes columns: `match`, `reason`, `diff_percentage`, `diff_count`,
#' `pair_id`, and `img2`.
#'
#' @seealso [compare_images_batch()], [compare_image_dirs()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare image pairs and summarize
#' pairs <- data.frame(
#'   img1 = c("baseline/a.png", "baseline/b.png", "baseline/c.png"),
#'   img2 = c("current/a.png", "current/b.png", "current/c.png")
#' )
#' results <- compare_images_batch(pairs)
#' summary(results)
#'
#' # Get summary with more worst offenders
#' summary(results, n_worst = 10)
#' }
summary.odiffr_batch <- function(object, n_worst = 5, ...) {
  # Validate inputs
  stopifnot(inherits(object, "odiffr_batch"))

  n_worst <- suppressWarnings(as.integer(n_worst))
  if (is.na(n_worst) || n_worst < 0) {
    stop("n_worst must be a non-negative integer.", call. = FALSE)
  }

  total <- nrow(object)
  passed <- sum(object$match, na.rm = TRUE)
  failed <- total - passed

  # Breakdown by reason
  reason_counts <- if (failed > 0) {
    table(object$reason[!object$match])
  } else {
    NULL
  }

  # Diff statistics (only for failures with diff_percentage)
  failed_diffs <- object$diff_percentage[!object$match & !is.na(object$diff_percentage)]
  diff_stats <- if (length(failed_diffs) > 0) {
    list(
      min = min(failed_diffs),
      median = median(failed_diffs),
      mean = mean(failed_diffs),
      max = max(failed_diffs)
    )
  } else {
    NULL
  }

  # Worst offenders: rows with a diff_percentage first (descending), then
  # rows without one (layout-diff, error, missing) in their original order.
  worst <- if (failed > 0) {
    failures <- object[!object$match, ]
    dp <- failures$diff_percentage
    failures <- failures[order(is.na(dp), -dp, seq_along(dp)), ]
    head(failures, n_worst)
  } else {
    NULL
  }

  structure(
    list(
      total = total,
      passed = passed,
      failed = failed,
      pass_rate = if (total > 0) passed / total else NA_real_,
      reason_counts = reason_counts,
      diff_stats = diff_stats,
      worst = worst
    ),
    class = "odiffr_batch_summary"
  )
}

#' @rdname summary.odiffr_batch
#' @param x An `odiffr_batch_summary` object.
#' @export
print.odiffr_batch_summary <- function(x, ...) {
  cat("odiffr batch comparison:", x$total, "pairs\n")
  cat(strrep("\u2500", 35), "\n")

  cat(sprintf("Passed: %d (%s)\n", x$passed, .fmt_pct(x$pass_rate * 100, 1)))
  cat(sprintf("Failed: %d (%s)\n", x$failed, .fmt_pct((1 - x$pass_rate) * 100, 1)))

  if (!is.null(x$reason_counts)) {
    for (reason in names(x$reason_counts)) {
      cat(sprintf("  - %s: %d\n", reason, x$reason_counts[[reason]]))
    }
  }

  if (!is.null(x$diff_stats)) {
    cat("\nDiff statistics (failed pairs):\n")
    cat(sprintf("  Min:    %.2f%%\n", x$diff_stats$min))
    cat(sprintf("  Median: %.2f%%\n", x$diff_stats$median))
    cat(sprintf("  Mean:   %.2f%%\n", x$diff_stats$mean))
    cat(sprintf("  Max:    %.2f%%\n", x$diff_stats$max))
  }

  if (!is.null(x$worst) && nrow(x$worst) > 0) {
    cat("\nWorst offenders:\n")
    for (i in seq_len(nrow(x$worst))) {
      row <- x$worst[i, ]
      label <- .row_label(row)
      if (is.na(row$diff_percentage)) {
        # layout-diff / error / missing rows have no pixel statistics
        detail <- .row_reason_text(row)
      } else {
        detail <- sprintf("%s, %s pixels",
                          .fmt_pct(row$diff_percentage),
                          .fmt_count(row$diff_count))
      }
      cat(sprintf("  %d. %s (%s)\n", i, label, detail))
    }
  }

  invisible(x)
}


# Internal: format a percentage, "-" for NA/NaN
.fmt_pct <- function(x, digits = 2) {
  out <- sprintf(paste0("%.", digits, "f%%"), x)
  out[is.na(x)] <- "-"
  out
}

# Internal: format a pixel count, "-" for NA
.fmt_count <- function(x) {
  out <- formatC(as.numeric(x), format = "d", big.mark = "")
  out[is.na(x)] <- "-"
  out
}

# Internal: plain-text reason for a batch row, including the error message
# (if an `error` column is present) and a clear label for missing files.
.row_reason_text <- function(row) {
  reason <- if ("reason" %in% names(row)) row$reason[[1]] else NA_character_
  err <- .row_error(row)
  text <- if (is.na(reason)) {
    "-"
  } else if (identical(reason, "missing")) {
    "missing: no current image"
  } else {
    reason
  }
  # Missing rows already have a self-explanatory label
  if (!is.na(err) && !identical(reason, "missing")) {
    text <- paste0(text, ": ", err)
  }
  text
}

# Internal: TRUE for image paths that do not refer to a file on disk, i.e.
# NA, "" and placeholder labels such as "<magick-image>" or "<plot>".
.is_placeholder_path <- function(x) {
  x <- as.character(x)
  is.na(x) | !nzchar(x) | grepl("^<.*>$", x)
}

# Internal: short display label for a batch row: the basename of the current
# image (img2), or "pair N" when img2 is not a file path (e.g. a magick image
# or plot). With `use_img1 = TRUE`, the baseline (img1) is tried next.
.row_label <- function(row, use_img1 = FALSE) {
  # Snapshot reports carry the snapshot's path relative to _snaps
  if ("snapshot" %in% names(row)) {
    snap <- as.character(row$snapshot[[1]])
    if (!is.na(snap) && nzchar(snap)) return(snap)
  }
  img2 <- if ("img2" %in% names(row)) as.character(row$img2[[1]]) else NA_character_
  if (!.is_placeholder_path(img2)) return(.path_basename(img2))
  if (use_img1 && "img1" %in% names(row)) {
    img1 <- as.character(row$img1[[1]])
    if (!.is_placeholder_path(img1)) return(.path_basename(img1))
  }
  paste0("pair ", row$pair_id[[1]])
}

# Internal: like basename(), but works on the string itself so that UTF-8
# file names survive in non-UTF-8 locales (basename() translates to the
# native encoding and fails on unrepresentable characters).
.path_basename <- function(x, windows = .Platform$OS.type == "windows") {
  sep <- if (windows) "[/\\\\]" else "/"
  x <- sub(paste0(sep, "+$"), "", x)
  sub(paste0("^.*", sep), "", x)
}

# Internal: error message of a batch row, NA if absent (older objects have
# no `error` column).
.row_error <- function(row) {
  if (!"error" %in% names(row)) return(NA_character_)
  err <- as.character(row$error[[1]])
  if (length(err) == 0 || is.na(err) || !nzchar(err)) NA_character_ else err
}


#' Get Failed Comparisons from Batch Results
#'
#' Extract only the failed (non-matching) comparisons from batch results.
#'
#' @param object An `odiffr_batch` object from [compare_images_batch()] or
#'   [compare_image_dirs()].
#'
#' @return A tibble or data.frame containing only rows where `match` is `FALSE`.
#'
#' @seealso [compare_images_batch()], [compare_image_dirs()], [passed_pairs()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- compare_image_dirs("baseline/", "current/")
#' failed <- failed_pairs(results)
#' nrow(failed)  # Number of failures
#' }
failed_pairs <- function(object) {
  stopifnot(inherits(object, "odiffr_batch"))
  object[!object$match, ]
}


#' Get Passed Comparisons from Batch Results
#'
#' Extract only the passed (matching) comparisons from batch results.
#'
#' @param object An `odiffr_batch` object from [compare_images_batch()] or
#'   [compare_image_dirs()].
#'
#' @return A tibble or data.frame containing only rows where `match` is `TRUE`.
#'
#' @seealso [compare_images_batch()], [compare_image_dirs()], [failed_pairs()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- compare_image_dirs("baseline/", "current/")
#' passed <- passed_pairs(results)
#' nrow(passed)  # Number of passing comparisons
#' }
passed_pairs <- function(object) {
  stopifnot(inherits(object, "odiffr_batch"))
  object[object$match, ]
}
