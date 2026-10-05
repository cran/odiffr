# Tests for batch_junit() and batch_markdown()

ci_batch <- function(...) {
  make_batch(
    match = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    reason = c("match", "pixel-diff", "layout-diff", "error", "missing"),
    diff_count = c(0L, 126L, NA, NA, NA),
    diff_percentage = c(0, 1.26, NA, NA, NA),
    diff_output = c(NA, "diffs/b.png", NA, NA, NA),
    img1 = sprintf("baseline/%s.png", letters[1:5]),
    img2 = c("current/a.png", "current/b.png", "current/c.png",
             "<magick-image>", "current/e.png"),
    error = c(NA, NA, NA, "Could not load image", "File not found in current_dir"),
    ...
  )
}


# JUnit ---------------------------------------------------------------------------

test_that("batch_junit returns parseable XML with correct counts", {
  skip_if_not_installed("xml2")
  xml <- batch_junit(ci_batch())
  expect_type(xml, "character")
  expect_invisible(batch_junit(ci_batch()))

  doc <- xml2::read_xml(xml)
  suite <- xml2::xml_find_first(doc, "//testsuite")
  expect_equal(xml2::xml_attr(suite, "name"), "odiffr")
  expect_equal(xml2::xml_attr(suite, "tests"), "5")
  expect_equal(xml2::xml_attr(suite, "failures"), "3")
  expect_equal(xml2::xml_attr(suite, "errors"), "1")
  expect_length(xml2::xml_find_all(doc, "//testsuite"), 1)

  cases <- xml2::xml_find_all(doc, "//testcase")
  expect_length(cases, 5)
  expect_equal(xml2::xml_attr(cases, "name"),
               c("a.png", "b.png", "c.png", "d.png", "e.png"))
  expect_true(all(xml2::xml_attr(cases, "classname") == "odiffr"))

  failures <- xml2::xml_find_all(doc, "//failure")
  expect_equal(xml2::xml_attr(failures, "type"),
               c("pixel-diff", "layout-diff", "missing"))
  expect_equal(xml2::xml_attr(failures[[1]], "message"),
               "pixel-diff: 1.26% (126 pixels)")
  expect_match(xml2::xml_text(failures[[1]]), "Diff image: diffs/b.png", fixed = TRUE)
  expect_match(xml2::xml_text(failures[[1]]), "Baseline: baseline/b.png", fixed = TRUE)

  err <- xml2::xml_find_all(doc, "//error")
  expect_length(err, 1)
  expect_equal(xml2::xml_attr(err, "message"), "Could not load image")
  expect_match(xml2::xml_text(err), "Current: <magick-image>", fixed = TRUE)
})

test_that("batch_junit uses suite_name and include_passed", {
  skip_if_not_installed("xml2")
  doc <- xml2::read_xml(batch_junit(ci_batch(), suite_name = "visual & co",
                                    include_passed = FALSE))
  suite <- xml2::xml_find_first(doc, "//testsuite")
  expect_equal(xml2::xml_attr(suite, "name"), "visual & co")
  expect_equal(xml2::xml_attr(suite, "tests"), "4")
  cases <- xml2::xml_find_all(doc, "//testcase")
  expect_length(cases, 4)
  expect_true(all(xml2::xml_attr(cases, "classname") == "visual & co"))
})

test_that("batch_junit escapes special and invalid characters", {
  skip_if_not_installed("xml2")
  batch <- make_batch(
    match = c(FALSE, FALSE),
    reason = c("error", "pixel-diff"),
    diff_count = c(NA, 1L), diff_percentage = c(NA, 0.01),
    img2 = c("current/a&b <\"q\">'s.png", "current/café ✓.png"),
    error = c("bad <tag> & \"quote\" 'apos'\nsecond line\x01\x1b end", NA)
  )
  xml <- batch_junit(batch)
  expect_false(grepl("\x01", xml, fixed = TRUE))
  doc <- xml2::read_xml(xml)
  cases <- xml2::xml_find_all(doc, "//testcase")
  expect_equal(xml2::xml_attr(cases, "name"),
               c("a&b <\"q\">'s.png", "café ✓.png"))
  err <- xml2::xml_find_first(doc, "//error")
  expect_equal(xml2::xml_attr(err, "message"),
               "bad <tag> & \"quote\" 'apos'\nsecond line end")
})

test_that("batch_junit names fall back to img1 and pair id", {
  skip_if_not_installed("xml2")
  batch <- make_batch(match = c(TRUE, TRUE), reason = c("match", "match"),
                      img1 = c("base/x.png", "<magick-image>"),
                      img2 = c("<plot>", "<magick-image>"))
  doc <- xml2::read_xml(batch_junit(batch))
  expect_equal(xml2::xml_attr(xml2::xml_find_all(doc, "//testcase"), "name"),
               c("x.png", "pair 2"))
})

test_that("batch_junit handles empty batches and old objects", {
  skip_if_not_installed("xml2")
  doc <- xml2::read_xml(batch_junit(make_batch()))
  expect_equal(xml2::xml_attr(xml2::xml_find_first(doc, "//testsuite"), "tests"), "0")

  old <- make_batch(match = FALSE, reason = "error", tibble = TRUE)
  doc <- xml2::read_xml(batch_junit(old))
  expect_length(xml2::xml_find_all(doc, "//error"), 1)
  expect_error(batch_junit(data.frame()))
})

test_that("batch_junit writes UTF-8 to a file and creates directories", {
  skip_if_not_installed("xml2")
  root <- withr::local_tempdir()
  out <- file.path(root, "a", "b", "junit.xml")
  batch <- make_batch(match = FALSE, reason = "pixel-diff", diff_count = 1L,
                      diff_percentage = 0.1, img2 = "cur/ü.png")
  res <- batch_junit(batch, out)
  expect_equal(res, out)
  expect_invisible(batch_junit(batch, out))
  doc <- xml2::read_xml(out)
  expect_equal(xml2::xml_attr(xml2::xml_find_first(doc, "//testcase"), "name"),
               "ü.png")
})


# Markdown ------------------------------------------------------------------------

test_that("batch_markdown returns markdown silently when not writing", {
  withr::local_envvar(GITHUB_STEP_SUMMARY = NA)
  batch <- ci_batch()
  expect_silent(md <- batch_markdown(batch))
  expect_invisible(batch_markdown(batch))
  expect_type(md, "character")
  expect_match(md, "^## odiffr comparison\n")
  expect_match(md, "**4 of 5 comparisons failed**", fixed = TRUE)
  expect_match(md, "- pixel-diff: 1", fixed = TRUE)
  expect_match(md, "- missing: 1", fixed = TRUE)
  expect_match(md, "| # | Image | Reason | Diff % | Pixels | Error |", fixed = TRUE)
  expect_match(md, "| 1 | b.png | pixel-diff | 1.26% | 126 | - |", fixed = TRUE)
  expect_match(md, "| pair 4 | error | - | - | Could not load image |", fixed = TRUE)
  expect_match(md, "missing (no current image) | - | - | - |", fixed = TRUE)
  expect_false(grepl("NA", md, fixed = TRUE))
})

test_that("batch_markdown escapes table cells", {
  withr::local_envvar(GITHUB_STEP_SUMMARY = NA)
  batch <- make_batch(match = FALSE, reason = "error",
                      img2 = "cur/a|b*c_.png",
                      error = "line one | pipe\nline <two>")
  md <- batch_markdown(batch, title = "My\ntitle")
  expect_match(md, "## My title", fixed = TRUE)
  expect_match(md, "a\\|b\\*c\\_.png", fixed = TRUE)
  expect_match(md, "line one \\| pipe<br>line &lt;two&gt;", fixed = TRUE)
  rows <- grep("^\\| 1 ", strsplit(md, "\n")[[1]], value = TRUE)
  expect_length(rows, 1)
  # Every unescaped pipe is a column separator: 7 for 6 columns
  expect_equal(lengths(regmatches(rows, gregexpr("(?<!\\\\)\\|", rows, perl = TRUE))), 7)
})

test_that("batch_markdown handles passing, empty and truncated results", {
  withr::local_envvar(GITHUB_STEP_SUMMARY = NA)
  ok <- make_batch(match = c(TRUE, TRUE), reason = c("match", "match"),
                   diff_count = c(0L, 0L), diff_percentage = c(0, 0))
  md <- batch_markdown(ok)
  expect_match(md, "**All 2 comparisons passed.**", fixed = TRUE)
  expect_false(grepl("Worst offenders", md))

  expect_match(batch_markdown(make_batch()), "No comparisons were run", fixed = TRUE)

  md2 <- batch_markdown(ci_batch(), n_worst = 2)
  expect_match(md2, "Showing 2 of 4 failures.", fixed = TRUE)
  expect_false(grepl("| 3 |", md2, fixed = TRUE))

  md0 <- batch_markdown(ci_batch(), n_worst = 0)
  expect_false(grepl("Worst offenders", md0))
  expect_error(batch_markdown(ci_batch(), n_worst = -1), "n_worst")
})

test_that("batch_markdown appends to GITHUB_STEP_SUMMARY by default", {
  root <- withr::local_tempdir()
  summary_file <- file.path(root, "step_summary.md")
  writeLines("# Existing content", summary_file)
  withr::local_envvar(GITHUB_STEP_SUMMARY = summary_file)

  res <- batch_markdown(ci_batch())
  expect_equal(res, summary_file)
  batch_markdown(ci_batch(), title = "Second")
  content <- readLines(summary_file)
  expect_equal(content[1], "# Existing content")
  expect_equal(sum(content == "## odiffr comparison"), 1)
  expect_equal(sum(content == "## Second"), 1)
})

test_that("batch_markdown writes to output_file and can overwrite", {
  withr::local_envvar(GITHUB_STEP_SUMMARY = NA)
  root <- withr::local_tempdir()
  out <- file.path(root, "sub", "summary.md")
  expect_equal(batch_markdown(ci_batch(), out), out)
  batch_markdown(ci_batch(), out, title = "Again", append = FALSE)
  content <- readLines(out, encoding = "UTF-8")
  expect_equal(content[1], "## Again")
  expect_false(any(content == "## odiffr comparison"))
})


# End-to-end ----------------------------------------------------------------------

test_that("CI outputs work with real compare_image_dirs() results", {
  skip_if_no_odiff()
  skip_if_not_installed("xml2")
  withr::local_envvar(GITHUB_STEP_SUMMARY = NA)

  root <- withr::local_tempdir()
  base <- file.path(root, "baseline")
  cur <- file.path(root, "current")
  dir.create(base)
  dir.create(cur)
  red <- create_test_image(20, 20, "red")
  blue <- create_test_image(20, 20, "blue")
  file.copy(red, file.path(base, "same.png"))
  file.copy(red, file.path(cur, "same.png"))
  file.copy(red, file.path(base, "changed.png"))
  file.copy(blue, file.path(cur, "changed.png"))
  file.copy(red, file.path(base, "gone.png"))
  unlink(c(red, blue))

  res <- suppressWarnings(
    compare_image_dirs(base, cur, diff_dir = file.path(root, "diffs"))
  )

  junit <- file.path(root, "odiffr-junit.xml")
  batch_junit(res, junit)
  doc <- xml2::read_xml(junit)
  suite <- xml2::xml_find_first(doc, "//testsuite")
  expect_equal(xml2::xml_attr(suite, "tests"), "3")
  expect_equal(xml2::xml_attr(suite, "failures"), "2")
  expect_equal(xml2::xml_attr(suite, "errors"), "0")
  types <- xml2::xml_attr(xml2::xml_find_all(doc, "//failure"), "type")
  expect_setequal(types, c("pixel-diff", "missing"))

  md <- batch_markdown(res)
  expect_match(md, "**2 of 3 comparisons failed**", fixed = TRUE)
  expect_match(md, "| 1 | changed.png | pixel-diff | 100.00% | 400 |", fixed = TRUE)
  expect_match(md, "gone.png | missing (no current image)", fixed = TRUE)
})
