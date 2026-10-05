#' Approve Changes as New Baselines
#'
#' Accept the current images of a batch comparison as the new baselines:
#' for each selected failing pair, the current image (`img2`) is copied over
#' the baseline image (`img1`). This is intended for the directory/batch
#' workflow ([compare_image_dirs()], [compare_images_batch()]); review the
#' differences first (e.g. with [batch_report()]), then approve them.
#'
#' @param object An `odiffr_batch` object returned by [compare_image_dirs()]
#'   or [compare_images_batch()].
#' @param which Optional selection of rows to approve. One of:
#'   \itemize{
#'     \item an integer/numeric vector of `pair_id`s,
#'     \item a logical vector with one element per row of `object`,
#'     \item a character vector of image names, matched against the base
#'       names of `img1`/`img2` or against the trailing part of their paths
#'       (e.g. `"subdir/page.png"`).
#'   }
#'   When `NULL` (the default), all failing rows whose `reason` is in
#'   `reasons` are approved. An explicit selection takes precedence over
#'   `reasons`.
#' @param reasons Character vector of failure reasons to approve when
#'   `which` is `NULL`. Defaults to `c("pixel-diff", "layout-diff")`. Add
#'   `"missing"` (together with `remove_missing = TRUE`) to also delete
#'   baselines whose current image no longer exists.
#' @param remove_missing Logical; if `TRUE`, selected rows with
#'   `reason = "missing"` have their baseline file deleted (the screenshot
#'   was intentionally removed). If `FALSE` (the default), such rows are
#'   skipped.
#' @param dry_run Logical; if `TRUE`, no files are changed and the returned
#'   actions describe what would happen. Default is `FALSE`.
#' @param backup_dir Optional directory in which to save a copy of each
#'   baseline before it is overwritten or deleted. Paths relative to the
#'   common parent directory of the affected baselines are preserved; if
#'   there is no usable common directory, files are saved as
#'   `<pair_id>_<basename>`. Baselines that are already identical to the
#'   current image are not backed up.
#'
#' @return Invisibly, a data.frame with one row per considered pair and
#'   columns:
#'   \describe{
#'     \item{pair_id}{Integer; the pair's `pair_id`.}
#'     \item{baseline}{Character; path of the baseline image (`img1`).}
#'     \item{current}{Character; path of the current image (`img2`).}
#'     \item{action}{Character; one of `"updated"`, `"removed"`,
#'       `"skipped"`, `"failed"`, `"would update"`, or `"would remove"`.}
#'     \item{detail}{Character; explanation (e.g. why a row was skipped),
#'       or `NA`.}
#'   }
#'   A one-line summary of the actions is emitted as a message.
#'
#' @details
#' Rows with `reason = "match"` are never modified, and rows with
#' `reason = "error"` cannot be approved (the comparison itself failed, so
#' there is no valid current image to accept); they are reported as skipped.
#' Rows whose `img1`/`img2` are not files (e.g. `"<magick-image>"` labels)
#' are skipped as well.
#'
#' Parent directories of baselines are created as needed. A failed copy or
#' deletion is reported (action `"failed"`) and does not stop the remaining
#' rows from being processed. Approving the same batch twice is harmless.
#'
#' @seealso [compare_image_dirs()], [batch_report()], [failed_pairs()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' results <- compare_image_dirs("baseline/", "current/", diff_dir = "diffs/")
#' batch_report(results, "diffs/report.html")
#'
#' # Preview, then accept all pixel and layout changes
#' approve_changes(results, dry_run = TRUE)
#' approve_changes(results, backup_dir = "baseline-backup/")
#'
#' # Approve selected images only
#' approve_changes(results, which = c("home.png", "settings/profile.png"))
#'
#' # Also delete baselines of screenshots that were removed
#' approve_changes(results, reasons = "missing", remove_missing = TRUE)
#' }
approve_changes <- function(object,
                            which = NULL,
                            reasons = c("pixel-diff", "layout-diff"),
                            remove_missing = FALSE,
                            dry_run = FALSE,
                            backup_dir = NULL) {
  if (!inherits(object, "odiffr_batch")) {
    stop("object must be an odiffr_batch (from compare_image_dirs() or ",
         "compare_images_batch()).", call. = FALSE)
  }
  if ("page" %in% names(object)) {
    stop("approve_changes() does not support compare_pdfs() results: their ",
         "images are rendered pages, not the PDF files. Replace the baseline ",
         "PDFs directly instead.", call. = FALSE)
  }
  required <- c("pair_id", "reason", "img1", "img2")
  if (!all(required %in% names(object))) {
    stop("object must have columns: ", paste(required, collapse = ", "),
         call. = FALSE)
  }
  if (!is.character(reasons)) {
    stop("reasons must be a character vector.", call. = FALSE)
  }
  .approve_check_flag(remove_missing, "remove_missing")
  .approve_check_flag(dry_run, "dry_run")
  if (!is.null(backup_dir) &&
      (!is.character(backup_dir) || length(backup_dir) != 1 ||
       is.na(backup_dir) || !nzchar(backup_dir))) {
    stop("backup_dir must be NULL or a single directory path.", call. = FALSE)
  }

  pair_id <- as.integer(object$pair_id)
  reason <- as.character(object$reason)
  img1 <- as.character(object$img1)
  img2 <- as.character(object$img2)

  explicit <- !is.null(which)
  rows <- if (explicit) {
    .select_approve_rows(which, pair_id, img1, img2)
  } else {
    # All failing rows; those not in `reasons` are reported as skipped
    base::which(!is.na(reason) & reason != "match")
  }

  actions <- data.frame(
    pair_id = pair_id[rows],
    baseline = img1[rows],
    current = img2[rows],
    action = rep("skipped", length(rows)),
    detail = rep(NA_character_, length(rows)),
    stringsAsFactors = FALSE
  )
  rsn <- reason[rows]
  op <- rep(NA_character_, length(rows))  # "update" / "remove"

  for (k in seq_along(rows)) {
    r <- rsn[[k]]
    if (is.na(r)) {
      actions$detail[k] <- "unknown reason"
    } else if (r == "match") {
      actions$detail[k] <- "images already match"
    } else if (r == "error") {
      actions$detail[k] <- "comparison failed (error); cannot be approved"
    } else if (!explicit && !(r %in% reasons)) {
      actions$detail[k] <- sprintf("reason '%s' not in `reasons`", r)
    } else if (!.approve_is_file(actions$baseline[k])) {
      actions$detail[k] <- "baseline is not a file"
    } else if (r == "missing") {
      if (isTRUE(remove_missing)) {
        op[k] <- "remove"
      } else {
        actions$detail[k] <- "current image missing; use remove_missing = TRUE to delete the baseline"
      }
    } else if (!.approve_is_file(actions$current[k])) {
      actions$detail[k] <- "current image is not a file"
    } else if (!file.exists(actions$current[k])) {
      actions$detail[k] <- "current image not found"
    } else if (.approve_same_file(actions$baseline[k], actions$current[k])) {
      actions$detail[k] <- "baseline and current are the same file"
    } else {
      op[k] <- "update"
    }
  }

  if (explicit) {
    n_err <- sum(rsn == "error", na.rm = TRUE)
    if (n_err > 0) {
      message(sprintf(
        "%d selected pair(s) with reason \"error\" cannot be approved: the comparison failed, so there is no valid result to accept.",
        n_err
      ))
    }
  }
  n_label <- sum(grepl("is not a file", actions$detail))
  if (n_label > 0) {
    message(sprintf(
      "Skipping %d pair(s) whose images are not files (e.g. <magick-image>).",
      n_label
    ))
  }

  # A baseline selected by several rows would be overwritten (and backed up)
  # more than once, losing the original; refuse to guess which one wins
  act_rows <- base::which(!is.na(op))
  if (length(act_rows) > 1) {
    keys <- normalizePath(actions$baseline[act_rows], mustWork = FALSE)
    dup <- act_rows[keys %in% keys[duplicated(keys)]]
    if (length(dup) > 0) {
      op[dup] <- NA_character_
      actions$action[dup] <- "failed"
      actions$detail[dup] <- paste(
        "baseline selected by more than one pair; approve them separately",
        "with `which`"
      )
    }
  }

  # Backup locations for rows that will be modified
  backup_paths <- rep(NA_character_, length(rows))
  if (!is.null(backup_dir)) {
    act <- !is.na(op)
    backup_paths[act] <- .approve_backup_paths(actions$baseline[act],
                                       actions$pair_id[act], backup_dir)
  }

  for (k in base::which(!is.na(op))) {
    baseline <- actions$baseline[k]
    current <- actions$current[k]

    if (op[k] == "update") {
      unchanged <- file.exists(baseline) && .approve_identical(baseline, current)
      if (dry_run) {
        actions$action[k] <- "would update"
        if (unchanged) actions$detail[k] <- "already up to date"
        next
      }
      if (!unchanged && !.approve_backup_file(baseline, backup_paths[k])) {
        actions$action[k] <- "failed"
        actions$detail[k] <- paste("could not back up baseline to",
                                   backup_paths[k])
        next
      }
      parent <- dirname(baseline)
      if (!dir.exists(parent)) {
        dir.create(parent, recursive = TRUE, showWarnings = FALSE)
      }
      ok <- suppressWarnings(
        file.copy(current, baseline, overwrite = TRUE, copy.date = TRUE)
      )
      if (isTRUE(ok)) {
        actions$action[k] <- "updated"
        if (unchanged) actions$detail[k] <- "already up to date"
      } else {
        actions$action[k] <- "failed"
        actions$detail[k] <- "could not copy current image over baseline"
      }
    } else {
      exists <- file.exists(baseline)
      if (dry_run) {
        actions$action[k] <- "would remove"
        if (!exists) actions$detail[k] <- "baseline already absent"
        next
      }
      if (!exists) {
        actions$action[k] <- "removed"
        actions$detail[k] <- "baseline already absent"
        next
      }
      if (!.approve_backup_file(baseline, backup_paths[k])) {
        actions$action[k] <- "failed"
        actions$detail[k] <- paste("could not back up baseline to",
                                   backup_paths[k])
        next
      }
      ok <- suppressWarnings(file.remove(baseline))
      if (isTRUE(ok)) {
        actions$action[k] <- "removed"
      } else {
        actions$action[k] <- "failed"
        actions$detail[k] <- "could not delete baseline"
      }
    }
  }

  message(.approve_summary(actions, rsn, dry_run))
  rownames(actions) <- NULL
  invisible(actions)
}

# Internal: validate a TRUE/FALSE argument
.approve_check_flag <- function(x, arg_name) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    stop(arg_name, " must be TRUE or FALSE.", call. = FALSE)
  }
}

# Internal: resolve `which` to row indices of the batch
.select_approve_rows <- function(which, pair_id, img1, img2) {
  n <- length(pair_id)
  if (is.logical(which)) {
    if (length(which) != n) {
      stop(sprintf("Logical `which` must have one element per row (%d), not %d.",
                   n, length(which)), call. = FALSE)
    }
    return(base::which(!is.na(which) & which))
  }

  if (is.numeric(which)) {
    if (anyNA(which) || any(which != round(which))) {
      stop("Numeric `which` must contain whole-number pair_ids.",
           call. = FALSE)
    }
    unknown <- setdiff(which, pair_id)
    if (length(unknown) > 0) {
      stop("Unknown pair_id(s) in `which`: ",
           paste(unknown, collapse = ", "), call. = FALSE)
    }
    return(base::which(pair_id %in% which))
  }

  if (is.character(which)) {
    norm <- function(x) gsub("\\\\", "/", x)
    p1 <- norm(img1)
    p2 <- norm(img2)
    hit <- rep(FALSE, n)
    for (w in norm(which[!is.na(which)])) {
      m <- .approve_path_matches(p1, w) | .approve_path_matches(p2, w)
      if (!any(m)) {
        stop("No pair matches `which` = \"", w, "\".", call. = FALSE)
      }
      hit <- hit | m
    }
    return(base::which(hit))
  }

  stop("`which` must be NULL, pair_ids, a logical vector, or image names.",
       call. = FALSE)
}

# Internal: does each path equal `name`, have it as basename, or end in
# "/<name>"?
.approve_path_matches <- function(paths, name) {
  ok <- !is.na(paths)
  out <- rep(FALSE, length(paths))
  p <- paths[ok]
  out[ok] <- p == name | basename(p) == name |
    endsWith(p, paste0("/", sub("^\\./", "", name)))
  out
}

# Internal: is `x` a usable file path (not NA, not a "<label>")?
.approve_is_file <- function(x) {
  !is.na(x) && nzchar(x) && !grepl("^<.*>$", x)
}

# Internal: do two paths refer to the same file?
.approve_same_file <- function(a, b) {
  identical(normalizePath(a, winslash = "/", mustWork = FALSE),
            normalizePath(b, winslash = "/", mustWork = FALSE))
}

# Internal: are two files byte-identical?
.approve_identical <- function(a, b) {
  if (file.size(a) != file.size(b)) return(FALSE)
  sums <- unname(tools::md5sum(c(a, b)))
  !anyNA(sums) && identical(sums[[1]], sums[[2]])
}

# Internal: compute backup file paths, preserving structure relative to the
# common parent directory of `baselines` where possible
.approve_backup_paths <- function(baselines, pair_ids, backup_dir) {
  if (length(baselines) == 0) return(character())
  full <- normalizePath(baselines, winslash = "/", mustWork = FALSE)
  parts <- strsplit(dirname(full), "/", fixed = TRUE)
  common <- parts[[1]]
  for (p in parts[-1]) {
    len <- min(length(common), length(p))
    eq <- common[seq_len(len)] == p[seq_len(len)]
    first_diff <- match(FALSE, eq)
    common <- common[seq_len(if (is.na(first_diff)) len else first_diff - 1)]
  }
  # Require a common root deeper than the filesystem root
  if (length(common) >= 2 && any(nzchar(common[-1]))) {
    root <- paste0(paste(common, collapse = "/"), "/")
    rel <- substring(full, nchar(root) + 1)
    file.path(backup_dir, rel)
  } else {
    file.path(backup_dir, sprintf("%03d_%s", as.integer(pair_ids),
                                  basename(full)))
  }
}

# Internal: copy `baseline` to `backup` (no-op when backup is NA). Returns
# TRUE on success.
.approve_backup_file <- function(baseline, backup) {
  if (is.na(backup) || !file.exists(baseline)) return(TRUE)
  parent <- dirname(backup)
  if (!dir.exists(parent)) {
    dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  }
  isTRUE(suppressWarnings(
    file.copy(baseline, backup, overwrite = TRUE, copy.date = TRUE)
  ))
}

# Internal: one-line summary message for approve_changes()
.approve_summary <- function(actions, reasons, dry_run) {
  if (nrow(actions) == 0) {
    return("No changes to approve.")
  }
  count <- function(a) sum(actions$action == a)
  plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")

  parts <- character()
  if (dry_run) {
    parts <- c(parts,
               paste("Dry run: would approve", plural(count("would update"), "change")),
               paste("would remove", plural(count("would remove"), "baseline")))
  } else {
    parts <- c(parts,
               paste("Approved", plural(count("updated"), "change")),
               paste("removed", plural(count("removed"), "baseline")))
  }
  skipped <- actions$action == "skipped"
  if (any(skipped)) {
    tab <- table(ifelse(is.na(reasons[skipped]), "unknown", reasons[skipped]))
    detail <- paste(sprintf("%d %s", as.integer(tab), names(tab)),
                    collapse = ", ")
    if (length(tab) == 1) detail <- names(tab)
    parts <- c(parts, sprintf("%s %d (%s)", if (dry_run) "skip" else "skipped",
                              sum(skipped), detail))
  }
  n_failed <- count("failed")
  if (n_failed > 0) parts <- c(parts, sprintf("failed %d", n_failed))
  paste0(paste(parts, collapse = ", "), ".")
}
