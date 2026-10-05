# Tests for compare_pdfs() and compare_pdf_dirs()

# Write a multi-page PDF with simple plots. `changed` gives page numbers
# that are drawn differently.
make_test_pdf <- function(n_pages = 3, changed = integer(), path = NULL,
                          width = 4, height = 3) {
  if (is.null(path)) path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path, width = width, height = height)
  on.exit(grDevices::dev.off())
  for (i in seq_len(n_pages)) {
    graphics::par(mar = c(2, 2, 1, 1))
    graphics::plot(seq_len(10), seq_len(10) * i, pch = 19, main = paste("Page", i))
    if (i %in% changed) {
      graphics::rect(2, 2 * i, 6, 6 * i, col = "red")
    }
  }
  path
}

skip_if_no_pdf_support <- function() {
  skip_if_no_odiff()
  skip_if_not_installed("pdftools")
}

test_that("compare_pdfs() validates its inputs", {
  skip_if_not_installed("pdftools")
  pdf1 <- make_test_pdf(1)

  expect_error(compare_pdfs(tempfile(fileext = ".pdf"), pdf1),
               "baseline PDF file does not exist")
  expect_error(compare_pdfs(pdf1, tempfile(fileext = ".pdf")),
               "current PDF file does not exist")
  expect_error(compare_pdfs(c(pdf1, pdf1), pdf1), "single PDF file path")
  expect_error(compare_pdfs(NA_character_, pdf1), "single PDF file path")
  expect_error(compare_pdfs(tempdir(), pdf1), "is a directory")
  expect_error(compare_pdfs(pdf1, pdf1, dpi = 0), "dpi must be")
  expect_error(compare_pdfs(pdf1, pdf1, pages = 0), "positive whole")
  expect_error(compare_pdfs(pdf1, pdf1, pages = 1.5), "positive whole")
  expect_error(compare_pdfs(pdf1, pdf1, pages = 5), "not present in either")
})

test_that("compare_pdfs() errors clearly for files that are not PDFs", {
  skip_if_not_installed("pdftools")
  pdf1 <- make_test_pdf(1)
  not_pdf <- tempfile(fileext = ".pdf")
  writeLines("this is not a pdf", not_pdf)

  expect_error(compare_pdfs(not_pdf, pdf1), "Could not read baseline PDF")
  expect_error(compare_pdfs(pdf1, not_pdf), "Could not read current PDF")

  empty <- tempfile(fileext = ".pdf")
  file.create(empty)
  expect_error(compare_pdfs(empty, pdf1), "not a PDF file")

  # Corrupt file with a PDF header: the pdftools error is passed on
  corrupt <- tempfile(fileext = ".pdf")
  writeLines(c("%PDF-1.4", "garbage"), corrupt)
  expect_error(suppressWarnings(compare_pdfs(corrupt, pdf1)),
               "Could not read baseline PDF")
})

test_that("identical PDFs match on every page", {
  skip_if_no_pdf_support()
  pdf1 <- make_test_pdf(3)
  pdf2 <- make_test_pdf(3)

  res <- compare_pdfs(pdf1, pdf2)

  expect_s3_class(res, "odiffr_batch")
  expect_equal(nrow(res), 3)
  expect_true(all(res$match))
  expect_equal(res$page, 1:3)
  expect_type(res$page, "integer")
  expect_equal(res$pair_id, 1:3)
  expect_equal(names(res), c(.batch_columns, "page"))
  expect_true(all(is.na(res$error)))
  expect_true(all(file.exists(res$img1)))
  expect_true(all(file.exists(res$img2)))
  expect_match(basename(res$img2), "_p00[1-3]\\.png$")
  # Rendered pages live in the session temp dir
  expect_true(startsWith(normalizePath(res$img1[1]),
                         normalizePath(tempdir())))
})

test_that("a changed page is the only failure and gets a diff image", {
  skip_if_no_pdf_support()
  pdf1 <- make_test_pdf(3)
  pdf2 <- make_test_pdf(3, changed = 2)
  diff_dir <- withr::local_tempdir()

  res <- compare_pdfs(pdf1, pdf2, diff_dir = diff_dir)

  expect_equal(res$match, c(TRUE, FALSE, TRUE))
  expect_equal(res$reason[2], "pixel-diff")
  expect_gt(res$diff_percentage[2], 0)
  expect_true(file.exists(res$diff_output[2]))
  expect_true(all(is.na(res$diff_output[c(1, 3)])))
  # Renders are kept under diff_dir/pages
  expect_true(all(file.exists(
    file.path(diff_dir, "pages", c("baseline", "current"))
  )))
  expect_true(startsWith(normalizePath(res$img1[1]),
                         normalizePath(file.path(diff_dir, "pages", "baseline"))))
})

test_that("comparison options are passed on to odiff", {
  skip_if_no_pdf_support()
  pdf1 <- make_test_pdf(1)
  pdf2 <- make_test_pdf(1, changed = 1)

  res <- compare_pdfs(pdf1, pdf2, dpi = 72)
  expect_false(res$match)

  # Ignoring the whole page makes it match
  res <- compare_pdfs(pdf1, pdf2, dpi = 72,
                      ignore_regions = ignore_region(0, 0, 4 * 72 - 1, 3 * 72 - 1))
  expect_true(res$match)
})

test_that("different page sizes are a pixel diff or layout diff", {
  skip_if_no_pdf_support()
  pdf1 <- make_test_pdf(1, width = 4, height = 3)
  pdf2 <- make_test_pdf(1, width = 3, height = 4)

  res <- compare_pdfs(pdf1, pdf2, dpi = 72)
  expect_false(res$match)

  res <- compare_pdfs(pdf1, pdf2, dpi = 72, fail_on_layout = TRUE)
  expect_false(res$match)
  expect_equal(res$reason, "layout-diff")
})

test_that("pages missing from current are reported as 'missing'", {
  skip_if_no_pdf_support()
  pdf1 <- make_test_pdf(3)
  pdf2 <- make_test_pdf(2)

  expect_message(res <- compare_pdfs(pdf1, pdf2), "Page counts differ")

  expect_equal(nrow(res), 3)
  expect_equal(res$page, 1:3)
  expect_equal(res$match, c(TRUE, TRUE, FALSE))
  expect_equal(res$reason[3], "missing")
  expect_equal(res$error[3], "Page 3 not present in current PDF")
  expect_true(file.exists(res$img1[3]))
  expect_false(file.exists(res$img2[3]))
  expect_true(is.na(res$diff_percentage[3]))
})

test_that("extra pages in current are reported as 'error'", {
  skip_if_no_pdf_support()
  pdf1 <- make_test_pdf(2)
  pdf2 <- make_test_pdf(4)

  expect_message(res <- compare_pdfs(pdf1, pdf2), "Page counts differ")

  expect_equal(nrow(res), 4)
  expect_equal(res$pair_id, 1:4)
  expect_equal(res$match, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(res$reason[3:4], c("error", "error"))
  expect_equal(res$error[3:4], c("Page 3 not present in baseline PDF",
                                 "Page 4 not present in baseline PDF"))
  expect_false(any(file.exists(res$img1[3:4])))
  expect_true(all(file.exists(res$img2[3:4])))
})

test_that("pages selects a subset of pages", {
  skip_if_no_pdf_support()
  pdf1 <- make_test_pdf(4)
  pdf2 <- make_test_pdf(4, changed = 2)

  res <- compare_pdfs(pdf1, pdf2, pages = c(3, 1))
  expect_equal(res$page, c(3L, 1L))
  expect_equal(res$pair_id, 1:2)
  expect_true(all(res$match))

  res <- compare_pdfs(pdf1, pdf2, pages = 2)
  expect_false(res$match)

  # Selected page beyond current's page count
  pdf3 <- make_test_pdf(2)
  expect_message(res <- compare_pdfs(pdf1, pdf3, pages = c(1, 4)))
  expect_equal(res$match, c(TRUE, FALSE))
  expect_equal(res$reason[2], "missing")
})

test_that("dpi controls the rendered image size", {
  skip_if_no_pdf_support()
  skip_if_not_installed("png")
  pdf1 <- make_test_pdf(1, width = 4, height = 3)

  res72 <- compare_pdfs(pdf1, pdf1, dpi = 72)
  res144 <- compare_pdfs(pdf1, pdf1, dpi = 144)

  expect_equal(dim(png::readPNG(res72$img1))[1:2], c(216, 288))
  expect_equal(dim(png::readPNG(res144$img1))[1:2], c(432, 576))
})

test_that("compare_pdfs() works with parallel = TRUE", {
  skip_if_no_pdf_support()
  skip_on_os("windows")
  pdf1 <- make_test_pdf(3)
  pdf2 <- make_test_pdf(3, changed = 3)

  res <- withr::with_options(list(mc.cores = 2),
                             compare_pdfs(pdf1, pdf2, parallel = TRUE))
  expect_equal(res$match, c(TRUE, TRUE, FALSE))
})

test_that("compare_pdfs() results work with summary and reports", {
  skip_if_no_pdf_support()
  # On GitHub Actions batch_markdown() would append to the real job summary
  withr::local_envvar(GITHUB_STEP_SUMMARY = NA)
  pdf1 <- make_test_pdf(3)
  pdf2 <- make_test_pdf(4, changed = 2)
  diff_dir <- withr::local_tempdir()

  res <- suppressMessages(compare_pdfs(pdf1, pdf2, diff_dir = diff_dir))

  s <- summary(res)
  expect_equal(s$total, 4)
  expect_equal(s$passed, 2)
  expect_equal(as.integer(s$reason_counts[["error"]]), 1L)
  expect_output(print(s), "pixel-diff")

  expect_equal(nrow(failed_pairs(res)), 2)
  expect_equal(failed_pairs(res)$page, c(2L, 4L))

  out <- file.path(diff_dir, "report.html")
  batch_report(res, output_file = out, show_all = TRUE)
  html <- paste(readLines(out), collapse = "\n")
  expect_match(html, "_p002.png", fixed = TRUE)
  expect_match(html, "Page 4 not present in baseline PDF", fixed = TRUE)

  if ("images" %in% names(formals(batch_report))) {
    out_all <- file.path(diff_dir, "report-all.html")
    expect_no_error(batch_report(res, output_file = out_all, images = "all",
                                 show_all = TRUE))
    expect_true(file.exists(out_all))
  }

  if (exists("batch_markdown", mode = "function")) {
    md <- batch_markdown(res)
    expect_true(any(grepl("_p002.png", md, fixed = TRUE)))
  }

  if (exists("batch_junit", mode = "function")) {
    junit_file <- file.path(diff_dir, "junit.xml")
    batch_junit(res, junit_file)
    xml <- paste(readLines(junit_file), collapse = "\n")
    expect_match(xml, "<testsuite", fixed = TRUE)
    expect_match(xml, "_p002.png", fixed = TRUE)
  }
})

test_that("compare_pdf_dirs() compares matching files and reports missing ones", {
  skip_if_no_pdf_support()
  base_dir <- withr::local_tempdir()
  curr_dir <- withr::local_tempdir()
  diff_dir <- withr::local_tempdir()

  make_test_pdf(2, path = file.path(base_dir, "a.pdf"))
  make_test_pdf(2, path = file.path(curr_dir, "a.pdf"))
  make_test_pdf(3, path = file.path(base_dir, "b.pdf"))
  make_test_pdf(3, changed = 3, path = file.path(curr_dir, "b.pdf"))
  make_test_pdf(1, path = file.path(base_dir, "c.pdf"))  # missing in current
  make_test_pdf(1, path = file.path(curr_dir, "new.pdf"))  # no baseline
  writeLines("not a pdf", file.path(base_dir, "d.pdf"))
  file.copy(file.path(base_dir, "d.pdf"), file.path(curr_dir, "d.pdf"))

  expect_warning(
    expect_message(
      res <- compare_pdf_dirs(base_dir, curr_dir, diff_dir = diff_dir),
      "1 file\\(s\\) in current_dir have no baseline"
    ),
    "1 file\\(s\\) missing from current_dir: c.pdf"
  )

  expect_s3_class(res, "odiffr_batch")
  expect_equal(names(res), c(.batch_columns, "page", "file"))
  expect_equal(res$pair_id, seq_len(nrow(res)))
  expect_equal(res$file, c("a.pdf", "a.pdf", "b.pdf", "b.pdf", "b.pdf",
                           "c.pdf", "d.pdf"))
  expect_equal(res$page, c(1:2, 1:3, NA, NA))
  expect_equal(res$match, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(res$reason[6:7], c("missing", "error"))
  expect_equal(res$error[6], "File not found in current_dir")
  expect_match(res$error[7], "Could not read baseline PDF")
  expect_true(file.exists(res$diff_output[5]))
  expect_true(dir.exists(file.path(diff_dir, "b", "pages", "baseline")))

  expect_equal(summary(res)$failed, 3)
  expect_type(batch_report(res), "character")
})

test_that("compare_pdf_dirs() validates directories", {
  skip_if_not_installed("pdftools")
  empty <- withr::local_tempdir()
  expect_error(compare_pdf_dirs(file.path(empty, "nope"), empty),
               "baseline_dir does not exist")
  expect_error(compare_pdf_dirs(empty, empty), "No PDF files found")
})

test_that("compare_pdf_dirs() supports recursive matching", {
  skip_if_no_pdf_support()
  base_dir <- withr::local_tempdir()
  curr_dir <- withr::local_tempdir()
  dir.create(file.path(base_dir, "tables"))
  dir.create(file.path(curr_dir, "tables"))
  make_test_pdf(1, path = file.path(base_dir, "tables", "t1.pdf"))
  make_test_pdf(1, path = file.path(curr_dir, "tables", "t1.pdf"))
  make_test_pdf(1, path = file.path(base_dir, "t1.pdf"))
  make_test_pdf(1, changed = 1, path = file.path(curr_dir, "t1.pdf"))
  diff_dir <- withr::local_tempdir()

  res <- compare_pdf_dirs(base_dir, curr_dir, recursive = TRUE,
                          diff_dir = diff_dir)
  expect_setequal(res$file, c("tables/t1.pdf", "t1.pdf"))
  expect_equal(res$match[res$file == "tables/t1.pdf"], TRUE)
  expect_equal(res$match[res$file == "t1.pdf"], FALSE)
  # Same base name in different folders does not collide
  expect_false(res$img1[1] == res$img1[2])
})

test_that("a reused diff_dir does not keep renders of pages now missing", {
  skip_if_no_pdf_support()
  dir <- withr::local_tempdir()
  diff_dir <- file.path(dir, "diffs")
  baseline <- make_test_pdf(3, path = file.path(dir, "baseline.pdf"))
  current <- make_test_pdf(3, path = file.path(dir, "current.pdf"))

  res <- compare_pdfs(baseline, current, diff_dir = diff_dir, dpi = 30)
  expect_true(all(res$match))
  expect_true(file.exists(res$img2[3]))
  stale <- res$img2[3]

  # The current PDF loses its last page
  make_test_pdf(2, path = current)
  expect_message(
    res <- compare_pdfs(baseline, current, diff_dir = diff_dir, dpi = 30),
    "Page counts differ"
  )
  expect_equal(res$reason[3], "missing")
  expect_equal(normalizePath(res$img2[3], mustWork = FALSE),
               normalizePath(stale, mustWork = FALSE))
  expect_false(file.exists(res$img2[3]))
  expect_true(file.exists(res$img1[3]))

  # ... and pages missing from the baseline likewise
  make_test_pdf(3, path = current)
  compare_pdfs(baseline, current, diff_dir = diff_dir, dpi = 30)
  make_test_pdf(2, path = baseline)
  expect_message(
    res <- compare_pdfs(baseline, current, diff_dir = diff_dir, dpi = 30),
    "Page counts differ"
  )
  expect_equal(res$reason[3], "error")
  expect_false(file.exists(res$img1[3]))
})
