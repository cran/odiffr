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
#'     \item{pass_rate}{Proportion of passing comparisons (0 to 1).}
#'     \item{reason_counts}{Table of failure reasons (NULL if no failures).}
#'     \item{diff_stats}{List with min, median, mean, max diff percentages
#'       (NULL if no failures with diff data).
#'     }
#'     \item{worst}{Data frame of worst offenders by diff percentage
#'       (NULL if no failures).
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

  # Worst offenders
  worst <- if (failed > 0) {
    failures <- object[!object$match, ]
    failures <- failures[order(-failures$diff_percentage, na.last = TRUE), ]
    head(failures, n_worst)
  } else {
    NULL
  }

  structure(
    list(
      total = total,
      passed = passed,
      failed = failed,
      pass_rate = passed / total,
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

  cat(sprintf("Passed: %d (%.1f%%)\n", x$passed, x$pass_rate * 100))
  cat(sprintf("Failed: %d (%.1f%%)\n", x$failed, (1 - x$pass_rate) * 100))

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
      label <- if (!is.na(row$img2) && row$img2 != "<magick-image>") {
        basename(row$img2)
      } else {
        paste0("pair ", row$pair_id)
      }
      cat(sprintf("  %d. %s (%.2f%%, %d pixels)\n",
                  i, label, row$diff_percentage, row$diff_count))
    }
  }

  invisible(x)
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
