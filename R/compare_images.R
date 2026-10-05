#' Compare Two Images
#'
#' High-level function for comparing images with convenient output.
#' Returns a tibble if the tibble package is available, otherwise a data.frame.
#' Accepts file paths, magick-image objects, and plots (ggplot objects,
#' functions that draw a plot, or recorded plots), which are rendered to a
#' temporary PNG file before comparison.
#'
#' @param img1 Path to the first image, a magick-image object, or a plot:
#'   a ggplot object, a function of no arguments that draws a plot (base or
#'   grid graphics) or returns a ggplot, lattice or grid object when called,
#'   or a recorded plot
#'   ([grDevices::recordPlot()]).
#' @param img2 Path to the second image, a magick-image object, or a plot
#'   (see `img1`).
#' @param diff_output Path for the diff output image (PNG only). Use `NULL`
#'   for no diff output, or `TRUE` to auto-generate a temporary file path.
#' @param threshold Numeric; colour difference threshold between 0.0 and 1.0.
#'   Default is 0.1.
#' @param antialiasing Logical; if `TRUE`, ignore antialiased pixels.
#'   Default is `FALSE`.
#' @param fail_on_layout Logical; if `TRUE`, fail if images have different
#'   dimensions. Default is `FALSE`.
#' @param ignore_regions List of regions to ignore during comparison.
#'   Use [ignore_region()] to create regions, or pass a data.frame with
#'   columns `x1`, `y1`, `x2`, `y2`.
#' @param plot_options Options for rendering plot inputs, created with
#'   [plot_options()]. `NULL` (the default) uses `plot_options()`: 7 x 5
#'   inches at 96 dpi on a white background. Ignored for file and
#'   magick-image inputs.
#' @param ... Additional arguments passed to [odiff_run()].
#'
#' @return A tibble (if available) or data.frame with columns:
#'   \describe{
#'     \item{match}{Logical; `TRUE` if images match.}
#'     \item{reason}{Character; comparison result reason.}
#'     \item{diff_count}{Integer; number of different pixels.}
#'     \item{diff_percentage}{Numeric; percentage of different pixels.}
#'     \item{diff_output}{Character; path to diff image, or `NA`.}
#'     \item{img1}{Character; path to first image (`"<magick-image>"` or
#'       `"<plot>"` for magick-image and plot inputs).}
#'     \item{img2}{Character; path to second image (labelled as `img1`).}
#'     \item{error}{Character; the error message reported by odiff when
#'       `reason` is `"error"` (e.g. an image could not be loaded or has an
#'       unsupported format), otherwise `NA`. This is always the last column.}
#'   }
#'
#' @seealso [odiff_run()] for the low-level interface,
#'   [ignore_region()] for creating ignore regions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare two image files
#' result <- compare_images("baseline.png", "current.png")
#' result$match
#'
#' # With diff output
#' result <- compare_images("baseline.png", "current.png", diff_output = TRUE)
#' result$diff_output
#'
#' # Compare magick-image objects (requires magick package)
#' library(magick)
#' img1 <- image_read("baseline.png")
#' img2 <- image_read("current.png")
#' result <- compare_images(img1, img2)
#'
#' # Compare a ggplot against a baseline PNG (rendered with ragg if
#' # installed, otherwise grDevices::png())
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#' result <- compare_images("baseline_plot.png", p,
#'                          plot_options = plot_options(width = 6, height = 4))
#'
#' # Base graphics: pass a function that draws the plot
#' result <- compare_images("baseline_hist.png", function() hist(mtcars$mpg))
#'
#' # Ignore specific regions
#' result <- compare_images("baseline.png", "current.png",
#'                          ignore_regions = list(
#'                            ignore_region(0, 0, 100, 50),    # Header
#'                            ignore_region(0, 500, 800, 600)  # Footer
#'                          ))
#' }
compare_images <- function(img1, img2,
                           diff_output = NULL,
                           threshold = 0.1,
                           antialiasing = FALSE,
                           fail_on_layout = FALSE,
                           ignore_regions = NULL,
                           plot_options = NULL,
                           ...) {
  # Resolve image inputs (paths, magick objects and plots)
  img1_resolved <- .resolve_image_input(img1, "img1",
                                        plot_options = plot_options)
  on.exit(.cleanup_temp_files(img1_resolved), add = TRUE)
  img2_resolved <- .resolve_image_input(img2, "img2",
                                        plot_options = plot_options)
  on.exit(.cleanup_temp_files(img2_resolved), add = TRUE)

  # Handle diff_output = TRUE (auto-generate temp path)
  if (isTRUE(diff_output)) {
    diff_output <- tempfile(fileext = ".png")
  }

  # Run comparison
  result <- odiff_run(
    img1 = img1_resolved$path,
    img2 = img2_resolved$path,
    diff_output = diff_output,
    threshold = threshold,
    antialiasing = antialiasing,
    fail_on_layout = fail_on_layout,
    ignore_regions = ignore_regions,
    ...
  )

  # Build output data frame
  df <- data.frame(
    match = result$match,
    reason = result$reason,
    diff_count = result$diff_count,
    diff_percentage = result$diff_percentage,
    diff_output = if (is.null(result$diff_output)) NA_character_ else result$diff_output,
    img1 = .input_label(img1_resolved, result$img1),
    img2 = .input_label(img2_resolved, result$img2),
    error = .odiff_error_message(result),
    stringsAsFactors = FALSE
  )

  # Return tibble if available, otherwise data.frame
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(df)
  } else {
    df
  }
}

# Internal: extract odiff's error message from an odiff_run() result.
# Reads `result$error` defensively (older odiff_run() versions do not return
# it) and falls back to odiff's stderr/stdout text when the comparison failed.
.odiff_error_message <- function(result) {
  err <- result$error
  if (is.null(err) || length(err) == 0) err <- NA_character_
  err <- as.character(err[[1]])
  if (!is.na(err) && !nzchar(trimws(err))) err <- NA_character_

  if (is.na(err) && identical(result$reason, "error")) {
    out <- c(result$stderr, result$stdout)
    out <- trimws(sub("^\\s*Error:\\s*", "", as.character(out)))
    out <- out[nzchar(out)]
    err <- if (length(out) > 0) {
      paste(out, collapse = "\n")
    } else if (!is.null(result$exit_code) && !is.na(result$exit_code)) {
      sprintf("odiff failed with exit code %d", as.integer(result$exit_code))
    } else {
      "odiff failed"
    }
  }
  err
}

#' Compare Multiple Image Pairs
#'
#' Compare multiple pairs of images in batch. Useful for visual regression
#' testing across many screenshots.
#'
#' @param pairs A data.frame with columns `img1` and `img2` containing
#'   file paths, or a list of named lists with `img1` and `img2` elements.
#' @param diff_dir Directory to save diff images. If `NULL`, no diff images
#'   are created. If provided, diff images are named based on the input
#'   file names.
#' @param parallel Logical; if `TRUE`, compare images in parallel using
#'   multiple CPU cores. Uses `parallel::mclapply` on Unix systems (macOS,
#'   Linux) and falls back to sequential processing on Windows. Default is
#'   `FALSE`. The number of cores is taken from `getOption("mc.cores")` (or
#'   detected), capped at the number of pairs, and limited to 2 when the
#'   `_R_CHECK_LIMIT_CORES_` environment variable is `"TRUE"`/`"true"` or
#'   `"warn"` (as during `R CMD check --as-cran`).
#' @param ... Additional arguments passed to [compare_images()].
#'
#' @return A tibble (if available) or data.frame with class `odiffr_batch`,
#'   containing one row per comparison with all columns from [compare_images()]
#'   (including `error`) plus a leading `pair_id` column. Use [summary()] to
#'   get aggregate statistics. If `pairs` is empty (a zero-row data.frame or
#'   an empty list), an empty `odiffr_batch` with the same columns is
#'   returned.
#'
#' @details
#' A failure while comparing one pair (for example, a file that does not
#' exist, cannot be read, or has an unsupported format) does not abort the
#' batch. Instead, that pair is reported as a row with `match = FALSE`,
#' `reason = "error"`, and the error message in the `error` column. The same
#' applies to failed worker processes when `parallel = TRUE`.
#'
#' @seealso [summary.odiffr_batch()] for summarizing batch results,
#'   [compare_image_dirs()] for directory-based comparison.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a data frame of image pairs
#' pairs <- data.frame(
#'   img1 = c("baseline/page1.png", "baseline/page2.png"),
#'   img2 = c("current/page1.png", "current/page2.png")
#' )
#'
#' # Compare all pairs
#' results <- compare_images_batch(pairs, diff_dir = "diffs/")
#'
#' # Compare in parallel (Unix only)
#' results <- compare_images_batch(pairs, parallel = TRUE)
#'
#' # Check which comparisons failed
#' results[!results$match, ]
#'
#' # Inspect errors (e.g. unreadable or missing files)
#' results[results$reason == "error", c("img1", "img2", "error")]
#' }
compare_images_batch <- function(pairs, diff_dir = NULL, parallel = FALSE, ...) {
  pairs_list <- .as_pairs_list(pairs)
  .compare_pairs(pairs_list, ids = seq_along(pairs_list),
                 diff_dir = diff_dir, parallel = parallel, ...)
}

# Internal: convert and validate `pairs` input into a list of pairs
.as_pairs_list <- function(pairs) {
  if (is.data.frame(pairs)) {
    if (!all(c("img1", "img2") %in% names(pairs))) {
      stop("pairs data.frame must have 'img1' and 'img2' columns.",
           call. = FALSE)
    }
    col <- function(x) if (is.factor(x)) as.character(x) else x
    img1 <- col(pairs$img1)
    img2 <- col(pairs$img2)
    return(lapply(seq_len(nrow(pairs)), function(i) {
      list(img1 = img1[[i]], img2 = img2[[i]])
    }))
  }

  if (!is.list(pairs) || .is_magick_image(pairs)) {
    stop("pairs must be a data.frame or list.", call. = FALSE)
  }

  for (i in seq_along(pairs)) {
    p <- pairs[[i]]
    ok <- is.list(p) && !.is_magick_image(p) &&
      all(c("img1", "img2") %in% names(p)) &&
      !is.null(p$img1) && !is.null(p$img2)
    if (!ok) {
      stop(sprintf(
        "pairs[[%d]] must be a list with 'img1' and 'img2' elements.", i
      ), call. = FALSE)
    }
  }
  unname(pairs)
}

# Internal: column template for batch results
.batch_columns <- c("pair_id", "match", "reason", "diff_count",
                    "diff_percentage", "diff_output", "img1", "img2", "error")

# Internal: describe an image input for result rows
.describe_image_input <- function(x) {
  if (is.character(x) && length(x) == 1 && !is.na(x)) {
    x
  } else if (.is_magick_image(x)) {
    "<magick-image>"
  } else if (.is_plot_input(x)) {
    "<plot>"
  } else {
    NA_character_
  }
}

# Internal: build result row(s) as a plain data.frame with consistent
# column types
.batch_row <- function(pair_id, match = FALSE, reason = "error",
                       diff_count = NA_integer_, diff_percentage = NA_real_,
                       diff_output = NA_character_, img1 = NA_character_,
                       img2 = NA_character_, error = NA_character_) {
  data.frame(
    pair_id = as.integer(pair_id),
    match = as.logical(match),
    reason = as.character(reason),
    diff_count = as.integer(diff_count),
    diff_percentage = as.numeric(diff_percentage),
    diff_output = as.character(diff_output),
    img1 = as.character(img1),
    img2 = as.character(img2),
    error = as.character(error),
    stringsAsFactors = FALSE
  )
}

# Internal: normalise a compare_images() result into a .batch_row()
.normalize_batch_result <- function(res, pair_id) {
  get <- function(name, default) {
    v <- res[[name]]
    if (is.null(v) || length(v) == 0) default else v[[1]]
  }
  .batch_row(
    pair_id = pair_id,
    match = get("match", FALSE),
    reason = get("reason", "error"),
    diff_count = get("diff_count", NA_integer_),
    diff_percentage = get("diff_percentage", NA_real_),
    diff_output = get("diff_output", NA_character_),
    img1 = get("img1", NA_character_),
    img2 = get("img2", NA_character_),
    error = get("error", NA_character_)
  )
}

# Internal: combine result rows into an odiffr_batch
.as_odiffr_batch <- function(rows) {
  if (length(rows) == 0) {
    combined <- .batch_row(integer(), logical(), character(), integer(),
                           numeric(), character(), character(), character(),
                           character())
  } else {
    combined <- do.call(rbind, rows)
  }
  combined <- combined[, .batch_columns, drop = FALSE]
  rownames(combined) <- NULL

  if (requireNamespace("tibble", quietly = TRUE)) {
    result <- tibble::as_tibble(combined)
  } else {
    result <- combined
  }
  class(result) <- c("odiffr_batch", class(result))
  result
}

# Internal: number of cores to use for parallel batch comparison
.batch_n_cores <- function(n_tasks) {
  n_cores <- suppressWarnings(as.integer(
    getOption("mc.cores", parallel::detectCores(logical = FALSE))
  ))
  if (length(n_cores) != 1 || is.na(n_cores) || n_cores < 1) n_cores <- 1L

  # No point spawning more workers than tasks
  n_cores <- max(1L, min(n_cores, n_tasks))

  # Respect CRAN check limits (max 2 cores during R CMD check). Only
  # "TRUE"/"true" and "warn" request a limit; e.g. "false" does not.
  check_limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = "")
  if (tolower(check_limit) %in% c("true", "warn")) {
    n_cores <- min(n_cores, 2L)
  }
  as.integer(n_cores)
}

# Internal: compare a list of (already validated) pairs. `ids` gives the
# pair_id for each element of `pairs_list`.
.compare_pairs <- function(pairs_list, ids, diff_dir = NULL,
                           parallel = FALSE, ...) {
  if (length(pairs_list) == 0) {
    return(.as_odiffr_batch(list()))
  }

  # Create diff directory if needed
  if (!is.null(diff_dir) && !dir.exists(diff_dir)) {
    dir.create(diff_dir, recursive = TRUE)
  }

  error_row <- function(i, msg) {
    pair <- pairs_list[[i]]
    .batch_row(
      pair_id = ids[[i]],
      img1 = .describe_image_input(pair$img1),
      img2 = .describe_image_input(pair$img2),
      error = msg
    )
  }

  # Compare one pair; failures become error rows instead of aborting
  compare_one <- function(i) {
    pair <- pairs_list[[i]]
    tryCatch({
      # Generate diff output path if diff_dir is provided
      diff_output <- NULL
      if (!is.null(diff_dir)) {
        # Include index to prevent filename collisions (especially in parallel)
        base_name <- if (is.character(pair$img2)) {
          tools::file_path_sans_ext(basename(pair$img2))
        } else if (.is_plot_input(pair$img2)) {
          "plot"
        } else {
          "magick"
        }
        diff_output <- file.path(
          diff_dir, sprintf("%03d_%s_diff.png", ids[[i]], base_name)
        )
        # odiff writes no diff image for matching images, so remove one left
        # over from a previous run in the same diff_dir
        if (file.exists(diff_output)) unlink(diff_output)
      }

      result <- compare_images(
        img1 = pair$img1,
        img2 = pair$img2,
        diff_output = diff_output,
        ...
      )
      .normalize_batch_result(result, ids[[i]])
    }, error = function(e) {
      error_row(i, conditionMessage(e))
    })
  }

  if (isTRUE(parallel) && .Platform$OS.type == "unix") {
    results <- parallel::mclapply(
      seq_along(pairs_list),
      compare_one,
      mc.cores = .batch_n_cores(length(pairs_list))
    )
    # Worker failures come back as try-error objects (or NULL if killed)
    results <- lapply(seq_along(pairs_list), function(i) {
      r <- if (i <= length(results)) results[[i]] else NULL
      .parallel_result_or_error(r, function(msg) error_row(i, msg))
    })
  } else {
    # Sequential processing (Windows or parallel = FALSE)
    results <- lapply(seq_along(pairs_list), compare_one)
  }

  .as_odiffr_batch(results)
}

# Internal: turn a parallel worker's return value into a result row,
# converting try-error objects / missing results via `on_error(msg)`
.parallel_result_or_error <- function(r, on_error) {
  if (inherits(r, "try-error")) {
    cond <- attr(r, "condition")
    msg <- if (inherits(cond, "condition")) {
      conditionMessage(cond)
    } else {
      trimws(paste(as.character(r), collapse = "\n"))
    }
    on_error(paste("Parallel worker failed:", msg))
  } else if (!is.data.frame(r)) {
    on_error("Parallel worker returned no result.")
  } else {
    r
  }
}

#' Compare Images in Two Directories
#'
#' Compare all images in a baseline directory against corresponding images in a
#' current directory. Files are matched by relative path (including
#' subdirectories when `recursive = TRUE`).
#'
#' @param baseline_dir Path to the directory containing baseline images.
#' @param current_dir Path to the directory containing current images to
#'   compare against baseline.
#' @param pattern Regular expression pattern to match image files (matched
#'   case-insensitively). The default matches the file extensions accepted
#'   by odiff: `.png`, `.jpg`, `.jpeg`, `.webp`, `.tiff` and `.bmp`. Note
#'   that odiff does not accept the `.tif` extension; such files
#'   matched by a custom pattern are reported with `reason = "error"`.
#' @param recursive Logical; if `TRUE`, search subdirectories recursively.
#'   Default is `FALSE`.
#' @param diff_dir Directory to save diff images. If `NULL`, no diff images
#'   are created.
#' @param parallel Logical; if `TRUE`, compare images in parallel. See
#'   [compare_images_batch()] for details.
#' @param ... Additional arguments passed to [compare_images_batch()].
#'
#' @return A tibble (if available) or data.frame with class `odiffr_batch`,
#'   with one row per baseline image (in baseline file order), containing all
#'   columns from [compare_images()] (including `error`) plus a leading
#'   `pair_id` column.
#'
#' @details
#' The baseline directory is the source of truth. For each image found in
#' `baseline_dir` matching `pattern`:
#' \itemize{
#'   \item If a corresponding file exists in `current_dir` (same relative
#'     path), the two images are compared.
#'   \item If the file is missing from `current_dir`, a warning is issued and
#'     the file is included in the results as a failed row with
#'     `match = FALSE`, `reason = "missing"`, `NA` diff statistics and
#'     `diff_output`, `img2` set to the expected (nonexistent) path, and an
#'     explanatory `error` message. This ensures that a disappearing
#'     screenshot fails the comparison. If every file is missing, all rows
#'     are `"missing"`.
#' }
#'
#' An error is raised if `baseline_dir` contains no images matching
#' `pattern`.
#'
#' Files that exist only in `current_dir` (not in `baseline_dir`) are not
#' compared, but a message is emitted noting how many such files were found.
#'
#' @seealso [compare_images_batch()] for comparing explicit pairs,
#'   [compare_images()] for single comparisons.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Compare all images in two directories
#' results <- compare_image_dirs("baseline/", "current/")
#'
#' # Only compare PNG files
#' results <- compare_image_dirs("baseline/", "current/", pattern = "\\.png$")
#'
#' # Include subdirectories and save diff images
#' results <- compare_image_dirs(
#'   "baseline/",
#'   "current/",
#'   recursive = TRUE,
#'   diff_dir = "diffs/"
#' )
#'
#' # Check which comparisons failed (including missing files)
#' results[!results$match, ]
#' }
compare_image_dirs <- function(baseline_dir,
                               current_dir,
                               pattern = "\\.(png|jpe?g|webp|tiff|bmp)$",
                               recursive = FALSE,
                               diff_dir = NULL,
                               parallel = FALSE,
                               ...) {
  # Validate directories
  .validate_directory(baseline_dir, "baseline_dir")
  .validate_directory(current_dir, "current_dir")

  # Find baseline images
  baseline_files <- list.files(
    baseline_dir,
    pattern = pattern,
    recursive = recursive,
    full.names = FALSE,
    ignore.case = TRUE
  )

  if (length(baseline_files) == 0) {
    stop("No images found in baseline_dir matching pattern: ", pattern,
         call. = FALSE)
  }

  # Check for unmatched files in current_dir
  current_files <- list.files(
    current_dir,
    pattern = pattern,
    recursive = recursive,
    full.names = FALSE,
    ignore.case = TRUE
  )
  unmatched <- setdiff(current_files, baseline_files)
  if (length(unmatched) > 0) {
    shown <- unmatched[seq_len(min(3, length(unmatched)))]
    message(
      sprintf("Note: %d file(s) in current_dir have no baseline: %s%s",
              length(unmatched),
              paste(shown, collapse = ", "),
              if (length(unmatched) > 3) ", ..." else "")
    )
  }

  img1_paths <- file.path(baseline_dir, baseline_files)
  img2_paths <- file.path(current_dir, baseline_files)

  # Check for missing current files
  missing <- !file.exists(img2_paths)
  if (any(missing)) {
    n_missing <- sum(missing)
    missing_files <- baseline_files[missing]
    shown <- missing_files[seq_len(min(3, n_missing))]
    warning(
      n_missing, " file(s) missing from current_dir: ",
      paste(shown, collapse = ", "),
      if (n_missing > 3) "..." else "",
      call. = FALSE
    )
  }

  # Compare existing pairs; pair_id follows baseline file order
  ids <- seq_along(baseline_files)
  present <- which(!missing)
  pairs_list <- lapply(present, function(i) {
    list(img1 = img1_paths[[i]], img2 = img2_paths[[i]])
  })
  compared <- .compare_pairs(pairs_list, ids = ids[present],
                             diff_dir = diff_dir, parallel = parallel, ...)

  if (!any(missing)) {
    return(compared)
  }

  # Rows for files missing from current_dir
  current_norm <- normalizePath(current_dir, mustWork = FALSE)
  missing_idx <- which(missing)
  missing_rows <- .batch_row(
    pair_id = ids[missing_idx],
    match = FALSE,
    reason = "missing",
    img1 = normalizePath(img1_paths[missing_idx], mustWork = FALSE),
    img2 = file.path(current_norm, baseline_files[missing_idx]),
    error = "File not found in current_dir"
  )

  compared_df <- as.data.frame(compared, stringsAsFactors = FALSE)
  class(compared_df) <- "data.frame"
  combined <- rbind(compared_df[, .batch_columns, drop = FALSE], missing_rows)
  combined <- combined[order(combined$pair_id), , drop = FALSE]
  .as_odiffr_batch(list(combined))
}

# Internal helper to validate directory arguments
.validate_directory <- function(path, arg_name) {
  if (!is.character(path) || length(path) != 1) {
    stop(arg_name, " must be a single directory path.", call. = FALSE)
  }
  if (is.na(path) || !nzchar(trimws(path))) {
    stop(arg_name, " must be a non-empty directory path, not NA or \"\".",
         call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop(arg_name, " does not exist: ", path, call. = FALSE)
  }
}

#' Compare Directories and Generate HTML Report
#'
#' Convenience function that compares all images in two directories and
#' generates an HTML report in one step.
#'
#' @inheritParams compare_image_dirs
#' @param output_file Path for the HTML report. Defaults to
#'   `file.path(diff_dir, "report.html")`.
#' @param title Title for the HTML report.
#' @param embed Logical; if `TRUE`, embed images as base64 data URIs for a
#'   self-contained report. If `FALSE` (default), link to image files.
#' @param relative_paths Logical; if `TRUE`, use relative paths for images
#'   in the HTML report. Makes reports portable without embedding. Ignored
#'   when `embed = TRUE`. Default: `FALSE`.
#' @param n_worst Number of worst offenders to display in the report.
#' @param show_all Logical; if `TRUE`, show all comparisons in the report,
#'   not just failures.
#' @param images Which images to show in the report: `"diff"` (default) for
#'   the diff image only, or `"all"` for baseline, current and diff images
#'   side by side. See [batch_report()].
#' @param ... Additional arguments passed to [compare_image_dirs()] (e.g.
#'   `threshold`, `antialiasing`, `pattern`, `recursive`).
#'
#' @return The `odiffr_batch` results (invisibly). The HTML report is written
#'   to `output_file` as a side effect.
#'
#' @seealso [compare_image_dirs()], [batch_report()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # One-liner for QA workflow
#' compare_dirs_report("baseline/", "current/")
#' # -> Creates diffs/ directory with diff images and report.html
#'
#' # With parallel processing and embedded images
#' compare_dirs_report("baseline/", "current/", parallel = TRUE, embed = TRUE)
#'
#' # Pass comparison options via ...
#' compare_dirs_report("baseline/", "current/", threshold = 0.1, antialiasing = TRUE)
#' }
compare_dirs_report <- function(baseline_dir,
                                current_dir,
                                diff_dir = "diffs",
                                output_file = file.path(diff_dir, "report.html"),
                                parallel = FALSE,
                                title = "odiffr Comparison Report",
                                embed = FALSE,
                                relative_paths = FALSE,
                                n_worst = 10,
                                show_all = FALSE,
                                images = c("diff", "all"),
                                ...) {
  images <- match.arg(images)
  results <- compare_image_dirs(
    baseline_dir,
    current_dir,
    diff_dir = diff_dir,
    parallel = parallel,
    ...
  )

  # Ensure parent directory of output_file exists
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  batch_report(
    results,
    output_file = output_file,
    title = title,
    embed = embed,
    relative_paths = relative_paths,
    n_worst = n_worst,
    show_all = show_all,
    images = images
  )
  invisible(results)
}
