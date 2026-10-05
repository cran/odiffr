# Construct an odiffr_batch by hand (no odiff needed).
# `error` is only added when supplied, mimicking older objects without it.
make_batch <- function(match = logical(0),
                       reason = character(0),
                       diff_count = rep(NA_integer_, length(match)),
                       diff_percentage = rep(NA_real_, length(match)),
                       diff_output = rep(NA_character_, length(match)),
                       img1 = sprintf("baseline/img%d.png", seq_along(match)),
                       img2 = sprintf("current/img%d.png", seq_along(match)),
                       error = NULL,
                       tibble = FALSE) {
  df <- data.frame(
    pair_id = seq_along(match),
    match = as.logical(match),
    reason = as.character(reason),
    diff_count = as.integer(diff_count),
    diff_percentage = as.numeric(diff_percentage),
    diff_output = as.character(diff_output),
    img1 = as.character(img1),
    img2 = as.character(img2),
    stringsAsFactors = FALSE
  )
  if (!is.null(error)) df$error <- as.character(error)
  if (tibble) df <- tibble::as_tibble(df)
  class(df) <- c("odiffr_batch", class(df))
  df
}

# Write small image files for a hand-made batch. Returns a list of paths
# (img1, img2, diff) under `dir`/baseline, `dir`/current and `dir`/diffs.
# PNG names get real PNGs (if png is installed); other extensions get a few
# dummy bytes, which is enough for linking/embedding tests.
make_batch_files <- function(dir, names = c("a.png", "b.png"), diff = TRUE) {
  dirs <- file.path(dir, c("baseline", "current", "diffs"))
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  write_img <- function(path, value) {
    if (grepl("\\.png$", path, ignore.case = TRUE) &&
        requireNamespace("png", quietly = TRUE)) {
      png::writePNG(array(value, dim = c(4, 4, 3)), path)
    } else {
      writeBin(as.raw(1:16), path)
    }
    path
  }
  img1 <- vapply(file.path(dirs[1], names), write_img, character(1),
                 value = 0, USE.NAMES = FALSE)
  img2 <- vapply(file.path(dirs[2], names), write_img, character(1),
                 value = 1, USE.NAMES = FALSE)
  diff_out <- if (diff) {
    vapply(file.path(dirs[3], sub("\\.[^.]*$", ".png", names)), write_img,
           character(1), value = 0.5, USE.NAMES = FALSE)
  } else {
    rep(NA_character_, length(names))
  }
  list(img1 = img1, img2 = img2, diff = diff_out)
}
