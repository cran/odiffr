## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
# install.packages("pdftools")

## -----------------------------------------------------------------------------
# library(odiffr)
# 
# res <- compare_pdfs(
#   baseline = "outputs-before/f_km_os.pdf",
#   current  = "outputs-after/f_km_os.pdf",
#   diff_dir = "pdf-diffs"
# )
# res

## -----------------------------------------------------------------------------
# summary(res)
# 
# # Which pages differ?
# failed_pairs(res)[, c("page", "reason", "diff_percentage", "diff_output")]

## -----------------------------------------------------------------------------
# compare_pdfs("before.pdf", "after.pdf", pages = c(1, 5))

## -----------------------------------------------------------------------------
# compare_pdfs("before.pdf", "after.pdf", fail_on_layout = TRUE)

## -----------------------------------------------------------------------------
# res <- compare_pdf_dirs(
#   "outputs-r4.3/",
#   "outputs-r4.4/",
#   recursive = TRUE,
#   diff_dir = "pdf-diffs"
# )
# 
# summary(res)
# 
# # Files with at least one differing page
# unique(res$file[!res$match])

## -----------------------------------------------------------------------------
# batch_report(
#   res,
#   output_file = "pdf-diffs/report.html",
#   images = "all",
#   show_all = TRUE
# )

## -----------------------------------------------------------------------------
# batch_junit(res, "pdf-diffs/junit.xml")

## -----------------------------------------------------------------------------
# dpi <- 150
# footer <- ignore_region(
#   x1 = 0,
#   y1 = (11 - 0.5) * dpi,
#   x2 = 8.5 * dpi,
#   y2 = 11 * dpi
# )
# 
# compare_pdfs("before.pdf", "after.pdf", dpi = dpi, ignore_regions = footer)

