# Internal utility functions for odiffr

# Build CLI arguments from R parameters
.build_args <- function(img1, img2, diff_output = NULL,
                        threshold = NULL, antialiasing = FALSE,
                        fail_on_layout = FALSE, diff_mask = FALSE,
                        diff_overlay = NULL, diff_color = NULL,
                        diff_lines = FALSE, reduce_ram = FALSE,
                        ignore_regions = NULL) {
  args <- character()

  # Add threshold
  if (!is.null(threshold)) {
    args <- c(args, sprintf("--threshold=%s", threshold))
  }

  # Add flags
  if (isTRUE(antialiasing)) {
    args <- c(args, "--antialiasing")
  }

  if (isTRUE(fail_on_layout)) {
    args <- c(args, "--fail-on-layout")
  }

  if (isTRUE(diff_mask)) {
    args <- c(args, "--diff-mask")
  }

  if (!is.null(diff_overlay)) {
    if (is.logical(diff_overlay) && diff_overlay) {
      args <- c(args, "--diff-overlay")
    } else if (is.numeric(diff_overlay)) {
      args <- c(args, sprintf("--diff-overlay=%s", diff_overlay))
    }
  }

  if (!is.null(diff_color)) {
    args <- c(args, sprintf("--diff-color=%s", diff_color))
  }

  if (isTRUE(diff_lines)) {
    args <- c(args, "--output-diff-lines", "--parsable-stdout")
  }

  if (isTRUE(reduce_ram)) {
    args <- c(args, "--reduce-ram-usage")
  }

  # Add ignore regions
  if (!is.null(ignore_regions) && length(ignore_regions) > 0) {
    region_str <- .format_regions(ignore_regions)
    if (nzchar(region_str)) {
      args <- c(args, sprintf("--ignore=%s", region_str))
    }
  }

  # Add image paths (positional arguments at the end)
  args <- c(args, img1, img2)

  # Add diff output if specified
  if (!is.null(diff_output) && nzchar(diff_output)) {
    args <- c(args, diff_output)
  }

  args
}

# Format ignore regions for CLI
.format_regions <- function(regions) {
  if (is.null(regions)) {
    return("")
  }

  # Handle data.frame FIRST (before single region check, since df has column names)
  if (is.data.frame(regions)) {
    regions <- lapply(seq_len(nrow(regions)), function(i) {
      list(
        x1 = regions$x1[i],
        y1 = regions$y1[i],
        x2 = regions$x2[i],
        y2 = regions$y2[i]
      )
    })
  } else if (!is.null(names(regions)) && all(c("x1", "y1", "x2", "y2") %in% names(regions))) {
    # Handle single region as list
    regions <- list(regions)
  }

  # Format each region as x1:y1-x2:y2
  formatted <- vapply(regions, function(r) {
    sprintf("%d:%d-%d:%d", as.integer(r$x1), as.integer(r$y1),
            as.integer(r$x2), as.integer(r$y2))
  }, character(1))

  paste(formatted, collapse = ",")
}

# Parse odiff stdout output
.parse_output <- function(stdout, stderr, exit_code, diff_lines_requested = FALSE) {
  result <- list(
    match = exit_code == 0L,
    reason = .exit_code_to_reason(exit_code),
    diff_count = NA_integer_,
    diff_percentage = NA_real_,
    diff_lines = NULL,
    exit_code = exit_code,
    stdout = stdout,
    stderr = stderr
  )

  # Parse stdout for diff information
  if (length(stdout) > 0) {
    output_text <- paste(stdout, collapse = "\n")

    # Try to extract diff count (e.g., "Found 1234 different pixels")
    count_match <- regmatches(
      output_text,
      regexpr("([0-9]+)\\s*(?:different|changed)\\s*pixels?", output_text,
              ignore.case = TRUE, perl = TRUE)
    )
    if (length(count_match) > 0) {
      num <- gsub("[^0-9]", "", count_match)
      result$diff_count <- as.integer(num)
    }

    # Try to extract percentage
    pct_match <- regmatches(
      output_text,
      regexpr("([0-9.]+)\\s*%", output_text, perl = TRUE)
    )
    if (length(pct_match) > 0) {
      num <- gsub("[^0-9.]", "", pct_match)
      result$diff_percentage <- as.numeric(num)
    }

    # Parse diff lines if requested
    if (diff_lines_requested) {
      # Lines are output in parsable format
      lines_match <- regmatches(
        output_text,
        gregexpr("[0-9]+", output_text, perl = TRUE)
      )
      if (length(lines_match) > 0 && length(lines_match[[1]]) > 0) {
        # Filter to reasonable line numbers
        line_nums <- as.integer(lines_match[[1]])
        # Exclude very large numbers that are likely pixel counts
        line_nums <- line_nums[line_nums < 100000]
        if (length(line_nums) > 0) {
          result$diff_lines <- sort(unique(line_nums))
        }
      }
    }
  }

  result
}

# Convert exit code to reason string
.exit_code_to_reason <- function(exit_code) {
  switch(
    as.character(exit_code),
    "0" = "match",
    "21" = "layout-diff",
    "22" = "pixel-diff",
    "error"
  )
}

# Validate file path exists and is readable
.validate_image_path <- function(path, arg_name = "path") {
  if (is.null(path) || !is.character(path) || !nzchar(path)) {
    stop(arg_name, " must be a non-empty character string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop(arg_name, " does not exist: ", path, call. = FALSE)
  }
  if (file.access(path, mode = 4) != 0) {
    stop(arg_name, " is not readable: ", path, call. = FALSE)
  }
  normalizePath(path, mustWork = TRUE)
}

# Validate diff output path
.validate_diff_output <- function(path) {
  if (is.null(path)) {
    return(NULL)
  }
  if (!is.character(path) || !nzchar(path)) {
    stop("diff_output must be NULL or a non-empty character string.",
         call. = FALSE)
  }

  # Check extension is .png
  ext <- tolower(tools::file_ext(path))
  if (ext != "png") {
    warning("odiff only outputs PNG format. ",
            "Changing extension from '.", ext, "' to '.png'.",
            call. = FALSE)
    path <- sub(paste0("\\.", ext, "$"), ".png", path, ignore.case = TRUE)
  }

  # Ensure parent directory exists
  parent_dir <- dirname(path)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE)
  }

  normalizePath(path, mustWork = FALSE)
}
