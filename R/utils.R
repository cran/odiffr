# Internal utility functions for odiffr

# Build CLI arguments from R parameters
#
# odiff is always invoked with `--parsable-stdout` so that results can be
# parsed strictly by `.parse_output()`. Because `system2()` pastes `args`
# into a single command line without quoting them, path arguments (and the
# diff colour value) are quoted with `shQuote()` when `quote = TRUE` (the
# default). `shQuote()` uses `type = "sh"` on Unix and `type = "cmd"` on
# Windows, matching how `system2()` itself quotes `command` on each platform
# (so the command is never double-quoted).
.build_args <- function(img1, img2, diff_output = NULL,
                        threshold = NULL, antialiasing = FALSE,
                        fail_on_layout = FALSE, diff_mask = FALSE,
                        diff_overlay = NULL, diff_color = NULL,
                        diff_lines = FALSE, reduce_ram = FALSE,
                        enable_asm = FALSE, ignore_regions = NULL,
                        diff_cols = FALSE, quote = TRUE) {
  q <- if (isTRUE(quote)) function(x) shQuote(x) else identity

  args <- "--parsable-stdout"

  # Add threshold
  if (!is.null(threshold)) {
    args <- c(args, sprintf("--threshold=%s", .format_number(threshold)))
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
    if (is.logical(diff_overlay)) {
      if (isTRUE(diff_overlay)) {
        args <- c(args, "--diff-overlay")
      }
    } else if (is.numeric(diff_overlay)) {
      args <- c(args, sprintf("--diff-overlay=%s",
                              .format_number(diff_overlay)))
    }
  }

  if (!is.null(diff_color)) {
    args <- c(args, q(sprintf("--diff-color=%s", diff_color)))
  }

  if (isTRUE(diff_lines)) {
    args <- c(args, "--output-diff-lines")
  }

  if (isTRUE(diff_cols)) {
    args <- c(args, "--output-diff-cols")
  }

  if (isTRUE(reduce_ram)) {
    args <- c(args, "--reduce-ram-usage")
  }

  if (isTRUE(enable_asm)) {
    args <- c(args, "--enable-asm")
  }

  # Add ignore regions
  if (!is.null(ignore_regions) && length(ignore_regions) > 0) {
    region_str <- .format_regions(ignore_regions)
    if (nzchar(region_str)) {
      args <- c(args, sprintf("--ignore=%s", region_str))
    }
  }

  # Add image paths (positional arguments at the end)
  args <- c(args, q(img1), q(img2))

  # Add diff output if specified
  if (!is.null(diff_output) && nzchar(diff_output)) {
    args <- c(args, q(diff_output))
  }

  args
}

# Format a number for the CLI without scientific notation
# (e.g. 1e-05 -> "0.00001")
.format_number <- function(x) {
  trimws(format(x, scientific = FALSE, digits = 15))
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
    # Handle single region as list (including a single odiff_region)
    regions <- list(regions)
  }

  # Format each region as x1:y1-x2:y2
  formatted <- vapply(regions, function(r) {
    sprintf("%d:%d-%d:%d", as.integer(r$x1), as.integer(r$y1),
            as.integer(r$x2), as.integer(r$y2))
  }, character(1))

  paste(formatted, collapse = ",")
}

# Parse odiff output
#
# odiff is always run with `--parsable-stdout`, which prints exactly one line
# to stdout:
#   match:        "0"
#   pixel diff:   "<count>;<percentage>[;<lines>[;<cols>]]"
#   layout diff:  "layout" (only with --fail-on-layout)
# where <lines> and <cols> are comma-separated integers (the lines segment is
# left empty when only columns were requested). Human-readable messages and
# errors go to stderr and are never parsed for numbers.
.parse_output <- function(stdout, stderr, exit_code,
                          diff_lines_requested = FALSE,
                          diff_cols_requested = FALSE) {
  exit_code <- as.integer(exit_code)
  reason <- .exit_code_to_reason(exit_code)

  result <- list(
    match = identical(exit_code, 0L),
    reason = reason,
    diff_count = NA_integer_,
    diff_percentage = NA_real_,
    diff_lines = NULL,
    exit_code = exit_code,
    stdout = stdout,
    stderr = stderr
  )

  if (reason == "match") {
    result$diff_count <- 0L
    result$diff_percentage <- 0
  }

  diff_cols <- NULL

  if (reason == "pixel-diff") {
    lines <- trimws(as.character(stdout))
    pattern <- "^([0-9]+);([0-9]+(?:\\.[0-9]+)?)(?:;([0-9,]*))?(?:;([0-9,]*))?$"
    hits <- grep(pattern, lines, perl = TRUE, value = TRUE)
    if (length(hits) > 0) {
      line <- hits[length(hits)]
      parts <- regmatches(line, regexec(pattern, line, perl = TRUE))[[1]]
      result$diff_count <- as.integer(parts[2])
      result$diff_percentage <- as.numeric(parts[3])
      if (isTRUE(diff_lines_requested)) {
        result$diff_lines <- .parse_int_list(parts[4])
      }
      diff_cols <- .parse_int_list(parts[5])
    }
  }

  if (isTRUE(diff_cols_requested)) {
    result["diff_cols"] <- list(diff_cols)
  }

  result$error <- .extract_error(stderr, exit_code, reason)

  result
}

# Parse a comma-separated list of integers; NULL when empty
.parse_int_list <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(NULL)
  }
  vals <- suppressWarnings(as.integer(strsplit(x, ",", fixed = TRUE)[[1]]))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) NULL else vals
}

# Extract an error message from odiff's stderr. Returns NA_character_ unless
# the result is an error. Lines starting with "Error:" are joined with "; "
# (prefix stripped); otherwise the whole stderr is used.
.extract_error <- function(stderr, exit_code, reason) {
  if (!identical(reason, "error")) {
    return(NA_character_)
  }
  lines <- trimws(as.character(stderr))
  lines <- lines[!is.na(lines) & nzchar(lines)]
  err <- grep("^Error:", lines, value = TRUE)
  if (length(err) > 0) {
    return(paste(trimws(sub("^Error:", "", err)), collapse = "; "))
  }
  if (length(lines) > 0) {
    return(paste(lines, collapse = "; "))
  }
  sprintf("odiff exited with status %s", exit_code)
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
  if (is.null(path) || !is.character(path) || length(path) != 1 ||
      is.na(path) || !nzchar(path)) {
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
  if (!is.character(path) || length(path) != 1 || is.na(path) ||
      !nzchar(path)) {
    stop("diff_output must be NULL or a non-empty character string.",
         call. = FALSE)
  }

  if (grepl("[/\\\\]$", path)) {
    stop("diff_output must be a file path, not a directory: ", path,
         call. = FALSE)
  }

  # Ensure a .png extension: odiff only writes PNG and fails to save a diff
  # image whose path has no (or another) extension.
  ext <- tools::file_ext(path)
  if (!nzchar(ext)) {
    warning("odiff only outputs PNG format. ",
            "Adding '.png' extension to diff_output.",
            call. = FALSE)
    path <- paste0(sub("\\.$", "", path), ".png")
  } else if (tolower(ext) != "png") {
    warning("odiff only outputs PNG format. ",
            "Changing extension from '.", ext, "' to '.png'.",
            call. = FALSE)
    path <- paste0(substr(path, 1, nchar(path) - nchar(ext)), "png")
  }

  # Ensure parent directory exists
  parent_dir <- dirname(path)
  if (!dir.exists(parent_dir)) {
    dir.create(parent_dir, recursive = TRUE)
  }

  # Absolute path in the platform's native form whether or not the file
  # exists yet (normalizePath() leaves nonexistent paths relative on Unix)
  normalizePath(
    file.path(normalizePath(parent_dir, mustWork = FALSE), basename(path)),
    mustWork = FALSE
  )
}

# Validate odiff_run() options
.validate_threshold <- function(threshold) {
  if (is.null(threshold)) {
    return(invisible(NULL))
  }
  if (!is.numeric(threshold) || length(threshold) != 1 ||
      !is.finite(threshold) || threshold < 0 || threshold > 1) {
    stop("threshold must be a single number between 0 and 1.", call. = FALSE)
  }
  invisible(threshold)
}

.validate_diff_color <- function(diff_color) {
  if (is.null(diff_color)) {
    return(invisible(NULL))
  }
  if (!is.character(diff_color) || length(diff_color) != 1 ||
      is.na(diff_color) || !grepl("^#?[0-9A-Fa-f]{6}$", diff_color)) {
    stop("diff_color must be a hex colour string such as \"#FF0000\" ",
         "or \"FF0000\".", call. = FALSE)
  }
  invisible(diff_color)
}

.validate_diff_overlay <- function(diff_overlay) {
  if (is.null(diff_overlay)) {
    return(invisible(NULL))
  }
  ok <- length(diff_overlay) == 1 && (
    (is.logical(diff_overlay) && !is.na(diff_overlay)) ||
      (is.numeric(diff_overlay) && is.finite(diff_overlay) &&
         diff_overlay >= 0 && diff_overlay <= 1)
  )
  if (!ok) {
    stop("diff_overlay must be NULL, TRUE/FALSE, or a single number ",
         "between 0 and 1.", call. = FALSE)
  }
  invisible(diff_overlay)
}

# Validate timeout and convert it to the whole number of seconds that
# system2() expects (0 = no timeout). system2() truncates to integer seconds,
# so positive sub-second values are rounded up.
.validate_timeout <- function(timeout) {
  if (is.null(timeout) || !is.numeric(timeout) || length(timeout) != 1 ||
      is.na(timeout) || timeout < 0) {
    stop("timeout must be a single non-negative number of seconds ",
         "(0 or Inf for no timeout).", call. = FALSE)
  }
  # system2() needs a timeout that fits in an integer; treat larger values
  # as no timeout
  if (is.infinite(timeout) || timeout == 0 || timeout > .Machine$integer.max) {
    return(0)
  }
  ceiling(timeout)
}

# Image dimensions (width, height) without decoding, or NULL if unknown.
# PNG is read from its IHDR header; other formats use magick if installed.
.image_dimensions <- function(path) {
  if (!file.exists(path)) return(NULL)
  head <- tryCatch({
    con <- file(path, "rb")
    on.exit(close(con), add = TRUE)
    readBin(con, "raw", 24L)
  }, error = function(e) raw(0))
  png_sig <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (length(head) == 24L && identical(head[1:8], png_sig)) {
    be32 <- function(b) sum(as.numeric(as.integer(b)) * 256^(3:0))
    return(c(be32(head[17:20]), be32(head[21:24])))
  }
  if (requireNamespace("magick", quietly = TRUE)) {
    info <- tryCatch(magick::image_info(magick::image_read(path)),
                     error = function(e) NULL)
    if (!is.null(info) && nrow(info) >= 1) {
      return(c(info$width[1], info$height[1]))
    }
  }
  NULL
}
