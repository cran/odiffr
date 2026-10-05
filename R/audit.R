#' Create an Audit Record of Image Comparisons
#'
#' Builds a machine-readable evidence record of one or more image
#' comparisons: which files were compared (with cryptographic hashes and
#' sizes), the outcome of each comparison, the parameters used, and the
#' software environment (odiffr and odiff versions, the odiff binary and its
#' hash, R version, platform and user). The record can be written to a JSON
#' or CSV file and kept alongside validation documentation. This supports
#' audit trails in validated environments; it does not by itself make a
#' process compliant with any regulation.
#'
#' @param x The comparison result(s) to record: an `odiff_result` from
#'   [odiff_run()], a data.frame/tibble from [compare_images()], or an
#'   `odiffr_batch` from [compare_images_batch()] or [compare_image_dirs()].
#' @param file Path of the file to write, or `NULL` (default) to only return
#'   the record.
#' @param format Output file format, `"json"` (default) or `"csv"`. Only
#'   used when `file` is given. JSON requires the jsonlite package.
#' @param hash Hash algorithm for files: `"sha256"` (default) or `"md5"`.
#'   SHA-256 requires the openssl or digest package; MD5 uses base R
#'   ([tools::md5sum()]).
#' @param params Optional named list of the comparison parameters (e.g.
#'   `list(threshold = 0.1, antialiasing = TRUE)`). Results of [odiff_run()]
#'   carry their parameters, which are used when `params` is `NULL`.
#'   [compare_images()] and batch results do not, so pass the parameters
#'   used here to record them; otherwise they are recorded as unknown
#'   (`null` in JSON, `NA` in CSV).
#'
#' @details
#' **Record structure.** The returned list has two elements:
#'
#' `header`, a named list:
#' \describe{
#'   \item{schema}{Character; schema identifier, currently
#'     `"odiffr-audit/1"`.}
#'   \item{created}{Character; creation time in UTC, ISO 8601
#'     (`"YYYY-MM-DDTHH:MM:SSZ"`).}
#'   \item{odiffr_version}{Character; version of the odiffr package.}
#'   \item{odiff_version}{Character; version of the odiff binary, or `NA`.}
#'   \item{odiff_path}{Character; path of the odiff binary that
#'     [find_odiff()] currently selects, or `NA`.}
#'   \item{odiff_hash}{Character; hash of that binary, or `NA`.}
#'   \item{hash_algorithm}{Character; `"sha256"` or `"md5"`.}
#'   \item{r_version}{Character; R version, e.g. `"4.4.1"`.}
#'   \item{platform}{Character; `R.version$platform`.}
#'   \item{sysname, release, machine}{Character; from [Sys.info()].}
#'   \item{user}{Character; `Sys.info()[["user"]]`.}
#'   \item{n_comparisons}{Integer; number of comparisons recorded.}
#'   \item{params}{Named list of comparison parameters, or `NULL` if
#'     unknown.}
#' }
#'
#' `comparisons`, a data.frame with one row per comparison and columns:
#' \describe{
#'   \item{pair_id}{Integer; the batch `pair_id`, or the row number.}
#'   \item{img1, img2}{Character; image paths as recorded in the result.}
#'   \item{img1_hash, img2_hash}{Character; file hashes, `NA` if the file
#'     does not exist (e.g. `"missing"` rows) or is not a file (e.g.
#'     `"<magick-image>"`).}
#'   \item{img1_size, img2_size}{Numeric; file sizes in bytes, or `NA`.}
#'   \item{diff_output}{Character; diff image path, or `NA`.}
#'   \item{diff_output_hash}{Character; hash of the diff image, or `NA`.}
#'   \item{diff_output_size}{Numeric; size of the diff image, or `NA`.}
#'   \item{match}{Logical; whether the images matched.}
#'   \item{reason}{Character; `"match"`, `"pixel-diff"`, `"layout-diff"`,
#'     `"error"` or `"missing"`.}
#'   \item{diff_count}{Integer; number of different pixels, or `NA`.}
#'   \item{diff_percentage}{Numeric; percentage of different pixels, or
#'     `NA`.}
#'   \item{error}{Character; error message, or `NA`.}
#' }
#'
#' Files are hashed when `audit_record()` is called, so call it right after
#' the comparison, before any of the files can change.
#'
#' **JSON** files contain an object with `header` and `comparisons` (an array
#' of row objects); missing values are written as `null`.
#'
#' **CSV** files contain one row per comparison with the `comparisons`
#' columns, preceded by the header fields repeated on every row (except
#' `params`) and followed by one `param_<name>` column per parameter. The
#' standard parameters (`threshold`, `antialiasing`, `fail_on_layout`,
#' `ignore_regions`, `diff_mask`, `diff_overlay`, `diff_color`, `reduce_ram`,
#' `enable_asm`) always have a column (`NA` when unknown); other parameters
#' passed in `params` are added after them.
#'
#' @return The record, a list with elements `header` and `comparisons` (see
#'   Details). If `file` is given, the record is written to it and the
#'   normalised file path is returned invisibly instead.
#'
#' @seealso [odiff_info()], [odiff_version()]
#' @export
#'
#' @examples
#' \dontrun{
#' result <- odiff_run("baseline.png", "current.png", "diff.png",
#'                     threshold = 0.05)
#' rec <- audit_record(result)
#' rec$header$odiff_version
#' rec$comparisons$img1_hash
#'
#' # Write a JSON evidence file
#' audit_record(result, file = "comparison-audit.json")
#'
#' # Batch results: pass the parameters used, write CSV
#' results <- compare_image_dirs("baseline/", "current/", threshold = 0.05)
#' audit_record(results, file = "audit.csv", format = "csv",
#'              params = list(threshold = 0.05))
#' }
audit_record <- function(x, file = NULL, format = c("json", "csv"),
                         hash = c("sha256", "md5"), params = NULL) {
  format <- match.arg(format)
  hash <- match.arg(hash)
  if (!is.null(params) &&
      (!is.list(params) || is.data.frame(params) ||
       (length(params) > 0 &&
        (is.null(names(params)) || any(!nzchar(names(params))))))) {
    stop("params must be NULL or a named list.", call. = FALSE)
  }
  if (!is.null(file) &&
      (!is.character(file) || length(file) != 1 || is.na(file) ||
       !nzchar(file))) {
    stop("file must be NULL or a single file path.", call. = FALSE)
  }
  # Fail early, before hashing anything, if the writer is unavailable
  if (!is.null(file) && format == "json" && !.audit_has_pkg("jsonlite")) {
    stop("Writing JSON audit records requires the 'jsonlite' package. ",
         "Install it with install.packages(\"jsonlite\"), ",
         "or use format = \"csv\".", call. = FALSE)
  }

  hasher <- .audit_hasher(hash)
  rows <- .audit_rows(x)
  if (is.null(params) && inherits(x, "odiff_result") && is.list(x$params)) {
    params <- x$params
  }

  odiff_path <- tryCatch(.find_odiff_details()$path, error = function(e) NA_character_)
  odiff_version <- tryCatch(odiff_version(), error = function(e) {
    NA_character_
  })
  sys <- Sys.info()
  sys_field <- function(name) {
    if (name %in% names(sys)) unname(sys[[name]]) else NA_character_
  }

  header <- list(
    schema = "odiffr-audit/1",
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    odiffr_version = as.character(utils::packageVersion("odiffr")),
    odiff_version = odiff_version,
    odiff_path = odiff_path,
    odiff_hash = hasher(odiff_path),
    hash_algorithm = hash,
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    platform = R.version$platform,
    sysname = sys_field("sysname"),
    release = sys_field("release"),
    machine = sys_field("machine"),
    user = sys_field("user"),
    n_comparisons = nrow(rows),
    params = params
  )

  comparisons <- data.frame(
    pair_id = rows$pair_id,
    img1 = rows$img1,
    img1_hash = vapply(rows$img1, hasher, character(1), USE.NAMES = FALSE),
    img1_size = .audit_file_size(rows$img1),
    img2 = rows$img2,
    img2_hash = vapply(rows$img2, hasher, character(1), USE.NAMES = FALSE),
    img2_size = .audit_file_size(rows$img2),
    diff_output = rows$diff_output,
    diff_output_hash = vapply(rows$diff_output, hasher, character(1),
                              USE.NAMES = FALSE),
    diff_output_size = .audit_file_size(rows$diff_output),
    match = rows$match,
    reason = rows$reason,
    diff_count = rows$diff_count,
    diff_percentage = rows$diff_percentage,
    error = rows$error,
    stringsAsFactors = FALSE
  )

  record <- list(header = header, comparisons = comparisons)

  if (is.null(file)) {
    return(record)
  }

  if (format == "json") {
    json <- jsonlite::toJSON(record, auto_unbox = TRUE, pretty = TRUE,
                             null = "null", na = "null", digits = NA,
                             dataframe = "rows")
    writeLines(json, file, useBytes = TRUE)
  } else {
    utils::write.csv(.audit_flatten(record), file, row.names = FALSE,
                     na = "", fileEncoding = "UTF-8")
  }
  invisible(normalizePath(file, mustWork = TRUE))
}

# Internal: wrapper around requireNamespace() so tests can mock package
# availability
.audit_has_pkg <- function(pkg) {
  requireNamespace(pkg, quietly = TRUE)
}

# Internal: return a function(path) -> hash string (NA if not a file)
.audit_hasher <- function(hash = c("sha256", "md5")) {
  hash <- match.arg(hash)
  impl <- if (hash == "md5") {
    function(path) unname(tools::md5sum(path))
  } else if (.audit_has_pkg("openssl")) {
    function(path) {
      con <- file(path, open = "rb")
      on.exit(close(con), add = TRUE)
      as.character(openssl::sha256(con))
    }
  } else if (.audit_has_pkg("digest")) {
    function(path) digest::digest(file = path, algo = "sha256")
  } else {
    stop("SHA-256 hashing requires the 'openssl' or 'digest' package. ",
         "Install one with install.packages(\"openssl\"), ",
         "or use hash = \"md5\".", call. = FALSE)
  }

  function(path) {
    if (length(path) != 1 || is.na(path) || !nzchar(path) ||
        !file.exists(path) || dir.exists(path)) {
      return(NA_character_)
    }
    out <- tryCatch(impl(path), error = function(e) NA_character_)
    tolower(as.character(out)[1])
  }
}

# Internal: file sizes in bytes, NA for anything that is not a file
.audit_file_size <- function(paths) {
  vapply(paths, function(p) {
    if (is.na(p) || !nzchar(p) || !file.exists(p) || dir.exists(p)) {
      return(NA_real_)
    }
    as.numeric(file.size(p))
  }, numeric(1), USE.NAMES = FALSE)
}

# Internal: normalise supported inputs into a plain data.frame of
# comparison rows
.audit_rows <- function(x) {
  if (inherits(x, "odiff_result")) {
    scalar <- function(v, default) {
      if (is.null(v) || length(v) == 0) default else v[[1]]
    }
    return(data.frame(
      pair_id = 1L,
      img1 = as.character(scalar(x$img1, NA_character_)),
      img2 = as.character(scalar(x$img2, NA_character_)),
      diff_output = as.character(scalar(x$diff_output, NA_character_)),
      match = as.logical(scalar(x$match, NA)),
      reason = as.character(scalar(x$reason, NA_character_)),
      diff_count = as.integer(scalar(x$diff_count, NA_integer_)),
      diff_percentage = as.numeric(scalar(x$diff_percentage, NA_real_)),
      error = as.character(.odiff_error_message(x)),
      stringsAsFactors = FALSE
    ))
  }

  if (is.data.frame(x)) {
    if (!all(c("img1", "img2") %in% names(x))) {
      stop("x must have 'img1' and 'img2' columns.", call. = FALSE)
    }
    n <- nrow(x)
    col <- function(name, default, as_fun) {
      v <- if (name %in% names(x)) x[[name]] else rep(default, n)
      if (is.factor(v)) v <- as.character(v)
      as_fun(v)
    }
    return(data.frame(
      pair_id = if ("pair_id" %in% names(x)) {
        as.integer(x[["pair_id"]])
      } else {
        seq_len(n)
      },
      img1 = col("img1", NA_character_, as.character),
      img2 = col("img2", NA_character_, as.character),
      diff_output = col("diff_output", NA_character_, as.character),
      match = col("match", NA, as.logical),
      reason = col("reason", NA_character_, as.character),
      diff_count = col("diff_count", NA_integer_, as.integer),
      diff_percentage = col("diff_percentage", NA_real_, as.numeric),
      error = col("error", NA_character_, as.character),
      stringsAsFactors = FALSE
    ))
  }

  stop("x must be an odiff_result (from odiff_run()), a compare_images() ",
       "result, or an odiffr_batch.", call. = FALSE)
}

# Internal: standard parameter names (as stored by odiff_run())
.audit_param_names <- c("threshold", "antialiasing", "fail_on_layout",
                        "ignore_regions", "diff_mask", "diff_overlay",
                        "diff_color", "reduce_ram", "enable_asm")

# Internal: flatten a record into one data.frame for CSV output
.audit_flatten <- function(record) {
  header <- record$header
  comparisons <- record$comparisons
  n <- nrow(comparisons)

  header_fields <- header[setdiff(names(header), "params")]
  header_df <- as.data.frame(
    lapply(header_fields, function(v) {
      rep(if (is.null(v) || length(v) == 0) NA else v[[1]], n)
    }),
    stringsAsFactors = FALSE
  )

  params <- header$params
  if (is.null(params)) params <- list()
  param_names <- unique(c(.audit_param_names, names(params)))
  param_cols <- lapply(param_names, function(name) {
    v <- params[[name]]
    v <- if (is.null(v) || length(v) == 0) {
      NA
    } else if (length(v) > 1) {
      paste(as.character(unlist(v)), collapse = ",")
    } else {
      v[[1]]
    }
    rep(v, n)
  })
  names(param_cols) <- paste0("param_", param_names)
  param_df <- as.data.frame(param_cols, stringsAsFactors = FALSE,
                            optional = TRUE)

  out <- cbind(header_df, comparisons, param_df)
  if (n == 0) {
    out <- out[0, , drop = FALSE]
  }
  rownames(out) <- NULL
  out
}
