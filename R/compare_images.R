#' Compare Two Images
#'
#' High-level function for comparing images with convenient output.
#' Returns a tibble if the tibble package is available, otherwise a data.frame.
#' Accepts file paths or magick-image objects.
#'
#' @param img1 Path to the first image, or a magick-image object.
#' @param img2 Path to the second image, or a magick-image object.
#' @param diff_output Path for the diff output image (PNG only). Use `NULL`
#'   for no diff output, or `TRUE` to auto-generate a temporary file path.
#' @param threshold Numeric; color difference threshold between 0.0 and 1.0.
#'   Default is 0.1.
#' @param antialiasing Logical; if `TRUE`, ignore antialiased pixels.
#'   Default is `FALSE`.
#' @param fail_on_layout Logical; if `TRUE`, fail if images have different
#'   dimensions. Default is `FALSE`.
#' @param ignore_regions List of regions to ignore during comparison.
#'   Use [ignore_region()] to create regions, or pass a data.frame with
#'   columns `x1`, `y1`, `x2`, `y2`.
#' @param ... Additional arguments passed to [odiff_run()].
#'
#' @return A tibble (if available) or data.frame with columns:
#'   \describe{
#'     \item{match}{Logical; `TRUE` if images match.}
#'     \item{reason}{Character; comparison result reason.}
#'     \item{diff_count}{Integer; number of different pixels.}
#'     \item{diff_percentage}{Numeric; percentage of different pixels.}
#'     \item{diff_output}{Character; path to diff image, or `NA`.}
#'     \item{img1}{Character; path to first image.}
#'     \item{img2}{Character; path to second image.}
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
                           ...) {
  # Resolve image inputs (handles both paths and magick objects)
  img1_resolved <- .resolve_image_input(img1, "img1")
  img2_resolved <- .resolve_image_input(img2, "img2")

  # Ensure cleanup of temp files on exit
  on.exit(
    .cleanup_temp_files(img1_resolved, img2_resolved),
    add = TRUE
  )

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
    img1 = if (img1_resolved$temp) "<magick-image>" else result$img1,
    img2 = if (img2_resolved$temp) "<magick-image>" else result$img2,
    stringsAsFactors = FALSE
  )

  # Return tibble if available, otherwise data.frame
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(df)
  } else {
    df
  }
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
#'   `FALSE`.
#' @param ... Additional arguments passed to [compare_images()].
#'
#' @return A tibble (if available) or data.frame with class `odiffr_batch`,
#'   containing one row per comparison with all columns from [compare_images()]
#'   plus a `pair_id` column. Use [summary()] to get aggregate statistics.
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
#' }
compare_images_batch <- function(pairs, diff_dir = NULL, parallel = FALSE, ...) {
  # Handle data.frame input
  if (is.data.frame(pairs)) {
    if (!all(c("img1", "img2") %in% names(pairs))) {
      stop("pairs data.frame must have 'img1' and 'img2' columns.",
           call. = FALSE)
    }
    pairs_list <- lapply(seq_len(nrow(pairs)), function(i) {
      list(img1 = pairs$img1[i], img2 = pairs$img2[i])
    })
  } else if (is.list(pairs)) {
    pairs_list <- pairs
  } else {
    stop("pairs must be a data.frame or list.", call. = FALSE)
  }

  # Create diff directory if needed
  if (!is.null(diff_dir) && !dir.exists(diff_dir)) {
    dir.create(diff_dir, recursive = TRUE)
  }

  # Define comparison function for each pair
  compare_one <- function(i) {
    pair <- pairs_list[[i]]

    # Generate diff output path if diff_dir is provided
    diff_output <- NULL
    if (!is.null(diff_dir)) {
      # Include index to prevent filename collisions (especially in parallel)
      base_name <- tools::file_path_sans_ext(basename(pair$img2))
      diff_output <- file.path(diff_dir, sprintf("%03d_%s_diff.png", i, base_name))
    }

    result <- compare_images(
      img1 = pair$img1,
      img2 = pair$img2,
      diff_output = diff_output,
      ...
    )

    # Add pair_id
    result$pair_id <- i
    result
  }

  # Compare pairs (parallel or sequential)
  if (isTRUE(parallel) && .Platform$OS.type == "unix") {
    # Use parallel::mclapply on Unix systems
    # Respect mc.cores option if set, otherwise detect cores
    n_cores <- getOption("mc.cores", parallel::detectCores(logical = FALSE))
    if (is.na(n_cores) || n_cores < 1) n_cores <- 1

    # Cap cores by number of pairs (no point spawning more workers than tasks)
    n_cores <- min(n_cores, length(pairs_list))

    # Respect CRAN check limits (max 2 cores during R CMD check)
    check_limit <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = "")
    if (nzchar(check_limit) && check_limit %in% c("TRUE", "true", "warn", "false")) {
      n_cores <- min(n_cores, 2L)
    }

    results <- parallel::mclapply(
      seq_along(pairs_list),
      compare_one,
      mc.cores = n_cores
    )
  } else {
    # Sequential processing (Windows or parallel = FALSE)
    results <- lapply(seq_along(pairs_list), compare_one)
  }

  # Combine results
  combined <- do.call(rbind, results)

  # Reorder columns
  col_order <- c("pair_id", setdiff(names(combined), "pair_id"))
  combined <- combined[, col_order]


  # Add class for S3 methods (summary, etc.)
  # Return tibble if available
  if (requireNamespace("tibble", quietly = TRUE)) {
    result <- tibble::as_tibble(combined)
  } else {
    result <- combined
  }
  class(result) <- c("odiffr_batch", class(result))
  result
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
#' @param pattern Regular expression pattern to match image files. Default
#'   matches common image formats (PNG, JPEG, WEBP, TIFF).
#' @param recursive Logical; if `TRUE`, search subdirectories recursively.
#'   Default is `FALSE`.
#' @param diff_dir Directory to save diff images. If `NULL`, no diff images
#'   are created.
#' @param parallel Logical; if `TRUE`, compare images in parallel. See
#'   [compare_images_batch()] for details.
#' @param ... Additional arguments passed to [compare_images_batch()].
#'
#' @return A tibble (if available) or data.frame with one row per comparison,
#'   containing all columns from [compare_images()] plus a `pair_id` column.
#'
#' @details
#' The baseline directory is the source of truth. For each image found in
#' `baseline_dir` matching `pattern`:
#' \itemize{
#'   \item If a corresponding file exists in `current_dir` (same relative
#'     path), it is included in the comparison.
#'   \item If the file is missing from `current_dir`, a warning is issued and
#'     the file is excluded from results.
#' }
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
#' # Check which comparisons failed
#' results[!results$match, ]
#' }
compare_image_dirs <- function(baseline_dir,
                               current_dir,
                               pattern = "\\.(png|jpe?g|webp|tiff?)$",
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

  # Build pairs
  pairs <- data.frame(
    img1 = file.path(baseline_dir, baseline_files),
    img2 = file.path(current_dir, baseline_files),
    stringsAsFactors = FALSE
  )

  # Check for missing current files
  missing <- !file.exists(pairs$img2)
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

  # Filter to existing pairs only
  pairs <- pairs[!missing, , drop = FALSE]

  if (nrow(pairs) == 0) {
    stop("No matching image pairs found.", call. = FALSE)
  }

  # Delegate to batch
  compare_images_batch(pairs, diff_dir = diff_dir, parallel = parallel, ...)
}

# Internal helper to validate directory arguments
.validate_directory <- function(path, arg_name) {
  if (!is.character(path) || length(path) != 1) {
    stop(arg_name, " must be a single directory path.", call. = FALSE)
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
                                ...) {
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
    show_all = show_all
  )
  invisible(results)
}
