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
#' @param ... Additional arguments passed to [compare_images()].
#'
#' @return A tibble (if available) or data.frame with one row per comparison,
#'   containing all columns from [compare_images()] plus a `pair_id` column.
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
#' # Check which comparisons failed
#' results[!results$match, ]
#' }
compare_images_batch <- function(pairs, diff_dir = NULL, ...) {
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

  # Compare each pair
  results <- lapply(seq_along(pairs_list), function(i) {
    pair <- pairs_list[[i]]

    # Generate diff output path if diff_dir is provided
    diff_output <- NULL
    if (!is.null(diff_dir)) {
      # Use basename of img2 for diff filename
      base_name <- tools::file_path_sans_ext(basename(pair$img2))
      diff_output <- file.path(diff_dir, paste0(base_name, "_diff.png"))
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
  })

  # Combine results
  combined <- do.call(rbind, results)

  # Reorder columns
  col_order <- c("pair_id", setdiff(names(combined), "pair_id"))
  combined <- combined[, col_order]

  # Return tibble if available
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(combined)
  } else {
    combined
  }
}
