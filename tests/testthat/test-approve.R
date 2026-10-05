# Helpers ---------------------------------------------------------------

# Write a solid-colour PNG at `path`
write_png <- function(path, color = c(1, 0, 0), width = 20, height = 20) {
  skip_if_not_installed("png")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  img <- array(0, dim = c(height, width, 3))
  for (i in 1:3) img[, , i] <- color[i]
  png::writePNG(img, path)
  path
}

# Build baseline/current file pairs on disk plus a matching batch object
make_file_batch <- function(root, reasons) {
  n <- length(reasons)
  img1 <- file.path(root, "baseline", sprintf("img%d.png", seq_len(n)))
  img2 <- file.path(root, "current", sprintf("img%d.png", seq_len(n)))
  for (i in seq_len(n)) {
    write_png(img1[i], c(1, 0, 0))
    if (reasons[i] != "missing") write_png(img2[i], c(0, 0, 1))
  }
  make_batch(match = reasons == "match", reason = reasons,
             img1 = img1, img2 = img2)
}

same_file_content <- function(a, b) {
  unname(tools::md5sum(a)) == unname(tools::md5sum(b))
}

# Unit tests (no odiff needed) -------------------------------------------

test_that("approve_changes validates input", {
  expect_error(approve_changes(data.frame()), "odiffr_batch")
  b <- make_batch(match = FALSE, reason = "pixel-diff")
  expect_error(approve_changes(b, dry_run = NA), "dry_run")
  expect_error(approve_changes(b, remove_missing = "yes"), "remove_missing")
  expect_error(approve_changes(b, reasons = 1), "reasons")
  expect_error(approve_changes(b, backup_dir = c("a", "b")), "backup_dir")
  expect_error(approve_changes(b, which = 99), "Unknown pair_id")
  expect_error(approve_changes(b, which = c(TRUE, FALSE)), "one element per row")
  expect_error(approve_changes(b, which = "nope.png"), "No pair matches")
  expect_error(approve_changes(b, which = list(1)), "`which` must be")
})

test_that("approve_changes updates failing rows and skips others", {
  root <- withr::local_tempdir()
  b <- make_file_batch(root, c("match", "pixel-diff", "layout-diff",
                               "error", "missing"))

  expect_message(
    actions <- approve_changes(b),
    "Approved 2 changes, removed 0 baselines, skipped 2 \\(1 error, 1 missing\\)"
  )
  expect_s3_class(actions, "data.frame")
  expect_named(actions, c("pair_id", "baseline", "current", "action", "detail"))
  expect_equal(actions$pair_id, 2:5)
  expect_equal(actions$action, c("updated", "updated", "skipped", "skipped"))

  expect_true(same_file_content(b$img1[2], b$img2[2]))
  expect_true(same_file_content(b$img1[3], b$img2[3]))
  # match and error rows untouched
  expect_false(same_file_content(b$img1[1], b$img2[1]))
  expect_false(same_file_content(b$img1[4], b$img2[4]))
  # missing row kept
  expect_true(file.exists(b$img1[5]))
})

test_that("approve_changes respects reasons", {
  root <- withr::local_tempdir()
  b <- make_file_batch(root, c("pixel-diff", "layout-diff"))
  actions <- suppressMessages(approve_changes(b, reasons = "layout-diff"))
  expect_equal(actions$action, c("skipped", "updated"))
  expect_match(actions$detail[1], "not in `reasons`")
  expect_false(same_file_content(b$img1[1], b$img2[1]))
})

test_that("dry_run changes nothing", {
  root <- withr::local_tempdir()
  b <- make_file_batch(root, c("pixel-diff", "missing"))
  before <- tools::md5sum(b$img1)

  expect_message(
    actions <- approve_changes(b, reasons = c("pixel-diff", "missing"),
                               remove_missing = TRUE, dry_run = TRUE),
    "Dry run: would approve 1 change, would remove 1 baseline"
  )
  expect_equal(actions$action, c("would update", "would remove"))
  expect_equal(tools::md5sum(b$img1), before)
  expect_true(all(file.exists(b$img1)))
})

test_that("remove_missing deletes baselines only when requested", {
  root <- withr::local_tempdir()
  b <- make_file_batch(root, c("missing", "missing"))

  # Not in reasons by default
  actions <- suppressMessages(approve_changes(b, remove_missing = TRUE))
  expect_equal(actions$action, c("skipped", "skipped"))
  expect_true(all(file.exists(b$img1)))

  # In reasons but remove_missing = FALSE
  actions <- suppressMessages(approve_changes(b, reasons = "missing"))
  expect_equal(actions$action, c("skipped", "skipped"))
  expect_match(actions$detail[1], "remove_missing")
  expect_true(all(file.exists(b$img1)))

  # Explicit selection + remove_missing
  expect_message(
    actions <- approve_changes(b, which = 1L, remove_missing = TRUE),
    "removed 1 baseline"
  )
  expect_equal(actions$action, "removed")
  expect_false(file.exists(b$img1[1]))
  expect_true(file.exists(b$img1[2]))

  # Running again is harmless
  actions <- suppressMessages(
    approve_changes(b, reasons = "missing", remove_missing = TRUE)
  )
  expect_equal(actions$action, c("removed", "removed"))
  expect_equal(actions$detail[1], "baseline already absent")
  expect_false(any(file.exists(b$img1)))
})

test_that("which selects by pair_id, logical and name", {
  root <- withr::local_tempdir()
  b <- make_file_batch(root, c("pixel-diff", "pixel-diff", "pixel-diff"))

  a <- suppressMessages(approve_changes(b, which = 2, dry_run = TRUE))
  expect_equal(a$pair_id, 2L)
  a <- suppressMessages(approve_changes(b, which = c(TRUE, FALSE, TRUE),
                                        dry_run = TRUE))
  expect_equal(a$pair_id, c(1L, 3L))
  a <- suppressMessages(approve_changes(b, which = "img3.png", dry_run = TRUE))
  expect_equal(a$pair_id, 3L)
  a <- suppressMessages(approve_changes(b, which = "baseline/img1.png",
                                        dry_run = TRUE))
  expect_equal(a$pair_id, 1L)
})

test_that("explicitly selected error and match rows are not approved", {
  root <- withr::local_tempdir()
  b <- make_file_batch(root, c("error", "match"))
  msgs <- capture_messages(actions <- approve_changes(b, which = 1:2))
  expect_match(msgs[1], "cannot be approved")
  expect_match(msgs[2], "skipped 2 \\(1 error, 1 match\\)")
  expect_equal(actions$action, c("skipped", "skipped"))
  expect_match(actions$detail[2], "already match")
  expect_false(same_file_content(b$img1[1], b$img2[1]))
})

test_that("magick-image labels are skipped", {
  b <- make_batch(match = c(FALSE, FALSE), reason = c("pixel-diff", "pixel-diff"),
                  img1 = c("<magick-image>", "base.png"),
                  img2 = c("cur.png", "<plot>"))
  msgs <- capture_messages(actions <- approve_changes(b))
  expect_match(msgs[1], "not files")
  expect_match(msgs[2], "skipped 2 \\(pixel-diff\\)")
  expect_equal(actions$action, c("skipped", "skipped"))
})

test_that("missing current file and empty selection are handled", {
  root <- withr::local_tempdir()
  b <- make_file_batch(root, "pixel-diff")
  unlink(b$img2)
  actions <- suppressMessages(approve_changes(b))
  expect_equal(actions$action, "skipped")
  expect_match(actions$detail, "not found")

  b2 <- make_batch(match = TRUE, reason = "match")
  expect_message(actions <- approve_changes(b2), "No changes to approve")
  expect_equal(nrow(actions), 0)
})

test_that("approving twice is harmless", {
  root <- withr::local_tempdir()
  b <- make_file_batch(root, "pixel-diff")
  suppressMessages(approve_changes(b))
  expect_message(actions <- approve_changes(b), "Approved 1 change")
  expect_equal(actions$action, "updated")
  expect_equal(actions$detail, "already up to date")
})

test_that("baseline parent directories are created", {
  root <- withr::local_tempdir()
  cur <- write_png(file.path(root, "current", "new", "deep.png"), c(0, 1, 0))
  base <- file.path(root, "baseline", "new", "deep.png")
  b <- make_batch(match = FALSE, reason = "layout-diff", img1 = base, img2 = cur)
  suppressMessages(approve_changes(b))
  expect_true(same_file_content(base, cur))
})

test_that("backup_dir keeps old baselines with relative structure", {
  root <- withr::local_tempdir()
  img1 <- c(write_png(file.path(root, "baseline", "a.png"), c(1, 0, 0)),
            write_png(file.path(root, "baseline", "sub", "b.png"), c(1, 0, 0)),
            write_png(file.path(root, "baseline", "c.png"), c(1, 0, 0)))
  img2 <- c(write_png(file.path(root, "current", "a.png"), c(0, 0, 1)),
            write_png(file.path(root, "current", "sub", "b.png"), c(0, 0, 1)),
            file.path(root, "current", "c.png"))
  old <- tools::md5sum(img1)
  b <- make_batch(match = c(FALSE, FALSE, FALSE),
                  reason = c("pixel-diff", "pixel-diff", "missing"),
                  img1 = img1, img2 = img2)
  bk <- file.path(root, "backup")

  suppressMessages(approve_changes(b, reasons = c("pixel-diff", "missing"),
                                   remove_missing = TRUE, backup_dir = bk))
  backups <- file.path(bk, c("a.png", "sub/b.png", "c.png"))
  expect_true(all(file.exists(backups)))
  expect_equal(unname(tools::md5sum(backups)), unname(old))
  expect_false(file.exists(img1[3]))

  # Second approval does not overwrite the backups with new baselines
  suppressMessages(approve_changes(b, backup_dir = bk))
  expect_equal(unname(tools::md5sum(backups[1:2])), unname(old[1:2]))
})

test_that("backup paths fall back to pair_id prefix without a common root", {
  p <- .approve_backup_paths(c("/a.png", "/b.png"), c(1L, 12L), "bk")
  expect_equal(p, file.path("bk", c("001_a.png", "012_b.png")))
  p <- .approve_backup_paths("/x/y/z.png", 3L, "bk")
  expect_equal(p, file.path("bk", "z.png"))
})

test_that("approve_changes works on tibble batches", {
  skip_if_not_installed("tibble")
  root <- withr::local_tempdir()
  b <- make_file_batch(root, c("pixel-diff", "match"))
  class(b) <- setdiff(class(b), "odiffr_batch")
  b <- tibble::as_tibble(b)
  class(b) <- c("odiffr_batch", class(b))
  actions <- suppressMessages(approve_changes(b))
  expect_equal(actions$action, "updated")
})

# Integration with compare_image_dirs() ----------------------------------

test_that("approved directory comparison then matches", {
  skip_if_no_odiff()
  root <- withr::local_tempdir()
  base_dir <- file.path(root, "baseline")
  cur_dir <- file.path(root, "current")

  write_png(file.path(base_dir, "same.png"), c(1, 0, 0))
  write_png(file.path(cur_dir, "same.png"), c(1, 0, 0))
  write_png(file.path(base_dir, "pixel.png"), c(1, 0, 0))
  write_png(file.path(cur_dir, "pixel.png"), c(0, 0, 1))
  write_png(file.path(base_dir, "layout.png"), c(1, 0, 0))
  write_png(file.path(cur_dir, "layout.png"), c(1, 0, 0), width = 30)
  write_png(file.path(base_dir, "gone.png"), c(1, 0, 0))

  res <- suppressWarnings(
    compare_image_dirs(base_dir, cur_dir, fail_on_layout = TRUE)
  )
  expect_setequal(res$reason, c("match", "pixel-diff", "layout-diff", "missing"))

  dry <- suppressMessages(approve_changes(res, dry_run = TRUE))
  expect_equal(sort(dry$action), c("skipped", "would update", "would update"))

  expect_message(
    approve_changes(res, reasons = c("pixel-diff", "layout-diff", "missing"),
                    remove_missing = TRUE),
    "Approved 2 changes, removed 1 baseline"
  )

  res2 <- compare_image_dirs(base_dir, cur_dir, fail_on_layout = TRUE)
  expect_equal(nrow(res2), 3)
  expect_true(all(res2$match))
})

# Regression tests ---------------------------------------------------------

test_that("approve_changes refuses compare_pdfs() results", {
  b <- make_batch(match = FALSE, reason = "pixel-diff")
  b$page <- 1L
  expect_error(approve_changes(b), "does not support compare_pdfs")
  expect_error(approve_changes(b, dry_run = TRUE), "compare_pdfs")
})

test_that("approve_changes fails rows that share a baseline", {
  root <- withr::local_tempdir()
  base <- write_png(file.path(root, "baseline", "shared.png"), c(1, 0, 0))
  cur1 <- write_png(file.path(root, "current", "one.png"), c(0, 0, 1))
  cur2 <- write_png(file.path(root, "current", "two.png"), c(0, 1, 0))
  other_base <- write_png(file.path(root, "baseline", "other.png"), c(1, 0, 0))
  other_cur <- write_png(file.path(root, "current", "other.png"), c(0, 0, 1))
  old <- tools::md5sum(base)
  b <- make_batch(match = c(FALSE, FALSE, FALSE),
                  reason = rep("pixel-diff", 3),
                  img1 = c(base, base, other_base),
                  img2 = c(cur1, cur2, other_cur))
  bk <- file.path(root, "backup")

  actions <- suppressMessages(approve_changes(b, backup_dir = bk))
  expect_equal(actions$action, c("failed", "failed", "updated"))
  expect_match(actions$detail[1:2], "more than one pair")
  # The shared baseline is untouched and was not backed up (twice)
  expect_equal(unname(tools::md5sum(base)), unname(old))
  expect_false(any(grepl("shared", list.files(bk, recursive = TRUE))))
  # The unrelated row is still approved
  expect_true(same_file_content(other_base, other_cur))

  # Selecting one of the rows explicitly approves it
  actions <- suppressMessages(approve_changes(b, which = 2L))
  expect_equal(actions$action, "updated")
  expect_true(same_file_content(base, cur2))
})
