# Tests for snapshot_report()

# Write a 20x20 PNG: white, or with a black square when `changed`
write_snap_png <- function(path, changed = FALSE) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  img <- array(1, dim = c(20, 20, 3))
  if (changed) img[5:9, 5:9, ] <- 0
  png::writePNG(img, path)
  path
}

# A fake _snaps tree:
#   plots/changed     baseline + changed .new.png
#   plots/same        baseline + identical .new.png
#   linux/plots/var   variant: baseline + changed .new.png
#   plots/fresh       .new.png without a baseline
#   plots/ok          baseline only (not reported)
local_snaps <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  snaps <- file.path(root, "_snaps")
  write_snap_png(file.path(snaps, "plots", "changed.png"))
  write_snap_png(file.path(snaps, "plots", "changed.new.png"), TRUE)
  write_snap_png(file.path(snaps, "plots", "same.png"))
  write_snap_png(file.path(snaps, "plots", "same.new.png"))
  write_snap_png(file.path(snaps, "linux", "plots", "var.png"))
  write_snap_png(file.path(snaps, "linux", "plots", "var.new.png"), TRUE)
  write_snap_png(file.path(snaps, "plots", "fresh.new.png"))
  write_snap_png(file.path(snaps, "plots", "ok.png"))
  snaps
}

test_that(".snapshot_pairs() pairs .new.png files with baselines", {
  skip_if_not_installed("png")
  snaps <- local_snaps()
  pairs <- odiffr:::.snapshot_pairs(snaps)
  expect_equal(pairs$snapshot, c("linux/plots/var.png", "plots/changed.png",
                                 "plots/fresh.png", "plots/same.png"))
  expect_equal(pairs$img2, file.path(snaps, sub("\\.png$", ".new.png",
                                                pairs$snapshot)))
  expect_equal(pairs$img1, file.path(snaps, pairs$snapshot))
})

test_that("snapshot_report() compares snapshots and returns a batch", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  snaps <- local_snaps()
  out <- file.path(dirname(snaps), "report", "snapshots.html")

  batch <- snapshot_report(snaps, output_file = out)
  expect_s3_class(batch, "odiffr_batch")
  expect_equal(nrow(batch), 4)
  expect_equal(batch$snapshot, c("linux/plots/var.png", "plots/changed.png",
                                 "plots/fresh.png", "plots/same.png"))
  expect_equal(batch$pair_id, 1:4)
  expect_equal(batch$match, c(FALSE, FALSE, FALSE, TRUE))
  expect_equal(batch$reason, c("pixel-diff", "pixel-diff", "error", "match"))
  expect_equal(batch$error[[3]], "no baseline snapshot")
  expect_true(all(is.na(batch$error[-3])))

  # Diff images go next to the report, never into _snaps
  expect_true(all(file.exists(batch$diff_output[1:2])))
  expect_true(all(startsWith(
    normalizePath(batch$diff_output[1:2]),
    normalizePath(file.path(dirname(out), "snapshot-diffs"))
  )))
  expect_length(list.files(snaps, pattern = "diff", recursive = TRUE), 0)

  # Self-contained HTML with baseline, new and diff images
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Image snapshot changes")
  expect_match(html, "data:image/png;base64")
  expect_match(html, "plots/changed.png", fixed = TRUE)
  expect_match(html, "no baseline snapshot", fixed = TRUE)
  expect_false(grepl("file://", html, fixed = TRUE))

  # The returned batch works with the other batch functions
  expect_equal(summary(batch)$failed, 3)
})

test_that("snapshot_report() writes Markdown", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  snaps <- local_snaps()
  md_file <- file.path(dirname(snaps), "summary.md")
  withr::local_envvar(GITHUB_STEP_SUMMARY = "")

  snapshot_report(snaps, output_file = md_file, format = "markdown")
  md <- paste(readLines(md_file), collapse = "\n")
  expect_match(md, "## Image snapshot changes", fixed = TRUE)
  expect_match(md, "**3 of 4 comparisons failed**", fixed = TRUE)
  expect_match(md, "plots/changed.png", fixed = TRUE)
  expect_match(md, "no baseline snapshot", fixed = TRUE)

  # Without output_file: GITHUB_STEP_SUMMARY is used when set
  gh <- file.path(dirname(snaps), "step-summary.md")
  withr::local_envvar(GITHUB_STEP_SUMMARY = gh)
  snapshot_report(snaps, format = "markdown")
  expect_match(paste(readLines(gh), collapse = "\n"),
               "Image snapshot changes")
})

test_that("snapshot_report() writes JUnit XML", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  snaps <- local_snaps()
  xml_file <- file.path(dirname(snaps), "snapshots.xml")

  snapshot_report(snaps, output_file = xml_file, format = "junit")
  xml <- paste(readLines(xml_file), collapse = "\n")
  expect_match(xml, 'tests="4" failures="2" errors="1"', fixed = TRUE)
  expect_match(xml, 'name="image snapshots"', fixed = TRUE)
  expect_match(xml, '<error message="no baseline snapshot"', fixed = TRUE)

  skip_if_not_installed("xml2")
  doc <- xml2::read_xml(xml_file)
  expect_length(xml2::xml_find_all(doc, "//testcase"), 4)
  expect_length(xml2::xml_find_all(doc, "//failure"), 2)
})

test_that("snapshot_report() handles a snapshot directory without changes", {
  skip_if_not_installed("png")
  root <- withr::local_tempdir()
  snaps <- file.path(root, "_snaps")
  write_snap_png(file.path(snaps, "plots", "ok.png"))
  withr::local_envvar(GITHUB_STEP_SUMMARY = "")

  md_file <- file.path(root, "summary.md")
  batch <- snapshot_report(snaps, output_file = md_file, format = "markdown")
  expect_s3_class(batch, "odiffr_batch")
  expect_equal(nrow(batch), 0)
  expect_true("snapshot" %in% names(batch))
  expect_equal(readLines(md_file),
               c("## Image snapshot changes", "", "No image snapshot changes."))

  # Appends to an existing summary
  snapshot_report(snaps, output_file = md_file, format = "markdown")
  expect_equal(sum(readLines(md_file) == "No image snapshot changes."), 2)

  # Without a destination the Markdown is not written anywhere
  expect_silent(snapshot_report(snaps, format = "markdown"))

  html_file <- file.path(root, "report.html")
  snapshot_report(snaps, output_file = html_file)
  expect_true(file.exists(html_file))

  xml_file <- file.path(root, "report.xml")
  snapshot_report(snaps, output_file = xml_file, format = "junit")
  expect_match(paste(readLines(xml_file), collapse = "\n"), 'tests="0"',
               fixed = TRUE)
})

test_that("snapshot_report() uses comparison settings and presets", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  snaps <- local_snaps()
  out <- file.path(dirname(snaps), "r.html")

  ignore <- list(ignore_region(0, 0, 19, 19))
  batch <- snapshot_report(snaps, output_file = out, ignore_regions = ignore)
  expect_equal(batch$match, c(TRUE, TRUE, FALSE, TRUE))

  # A 1-pixel change below the default threshold
  img <- png::readPNG(file.path(snaps, "plots", "same.png"))
  img[10, 10, 3] <- 0.96
  png::writePNG(img, file.path(snaps, "plots", "same.new.png"))
  batch <- snapshot_report(snaps, output_file = out)
  expect_true(batch$match[[4]])
  batch <- snapshot_report(snaps, output_file = out, preset = "strict")
  expect_false(batch$match[[4]])
  batch <- snapshot_report(snaps, output_file = out, preset = "strict",
                           threshold = 0.1)
  expect_true(batch$match[[4]])
})

test_that("snapshot_report() replaces diffs from an earlier report", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  snaps <- local_snaps()
  diff_dir <- file.path(dirname(snaps), "diffs")
  out <- file.path(dirname(snaps), "r.html")
  keep <- write_snap_png(file.path(diff_dir, "keep-me.png"))

  snapshot_report(snaps, output_file = out, diff_dir = diff_dir)
  expect_length(list.files(diff_dir, pattern = "_diff\\.png$"), 2)

  # Once the changed snapshot is fixed, its old diff disappears
  file.copy(file.path(snaps, "plots", "changed.png"),
            file.path(snaps, "plots", "changed.new.png"), overwrite = TRUE)
  batch <- snapshot_report(snaps, output_file = out, diff_dir = diff_dir)
  expect_length(list.files(diff_dir, pattern = "_diff\\.png$"), 1)
  expect_true(file.exists(keep))
})

test_that("snapshot_report() validates its arguments", {
  skip_if_not_installed("png")
  snaps <- local_snaps()

  expect_error(snapshot_report(file.path(snaps, "nope"), format = "markdown"),
               "Snapshot directory not found")
  expect_error(snapshot_report(snaps), "output_file is required")
  expect_error(snapshot_report(snaps, format = "junit"),
               "output_file is required")
  expect_error(snapshot_report(snaps, format = "pdf"))
  expect_error(snapshot_report(snaps, output_file = "r.html", images = "x"))
  expect_error(
    snapshot_report(snaps, output_file = "r.html",
                    diff_dir = file.path(snaps, "plots", "diffs")),
    "must not be inside the snapshot directory"
  )
  expect_error(
    snapshot_report(snaps, output_file = "r.html", preset = "nope"),
    "preset must be one of"
  )
})

test_that("snapshot_report() needs odiff only when there is work to do", {
  skip_if_not_installed("png")
  testthat::local_mocked_bindings(odiff_available = function() FALSE,
                                  .package = "odiffr")
  snaps <- local_snaps()
  expect_error(
    snapshot_report(snaps, output_file = file.path(dirname(snaps), "r.html")),
    "odiff binary not available"
  )

  root <- withr::local_tempdir()
  dir.create(file.path(root, "_snaps"))
  withr::local_envvar(GITHUB_STEP_SUMMARY = "")
  expect_equal(nrow(snapshot_report(file.path(root, "_snaps"),
                                    format = "markdown")), 0)
})

test_that(".path_is_within() handles missing and relative paths", {
  within <- odiffr:::.path_is_within
  root <- withr::local_tempdir()
  expect_true(within(root, root))
  expect_true(within(file.path(root, "a", "b"), root))
  expect_false(within(file.path(dirname(root), "other"), root))
  expect_false(within(paste0(root, "x"), root))
  withr::with_dir(root, {
    dir.create("_snaps")
    expect_true(within(file.path("_snaps", "new", "dir"), "_snaps"))
    expect_false(within("diffs", "_snaps"))
  })
})
