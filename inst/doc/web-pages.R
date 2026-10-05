## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----eval = FALSE-------------------------------------------------------------
# webshot2::webshot(
#   "https://example.com",
#   file = "current/home.png",
#   vwidth = 1280, vheight = 800,
#   delay = 0.5           # let the page settle
# )

## ----eval = FALSE-------------------------------------------------------------
# widget <- DT::datatable(head(iris))  # any htmlwidget
# 
# html <- tempfile(fileext = ".html")
# htmlwidgets::saveWidget(widget, html, selfcontained = TRUE)
# webshot2::webshot(html, file = "current/table.png",
#                   vwidth = 800, vheight = 600, delay = 1)

## ----eval = FALSE-------------------------------------------------------------
# library(odiffr)
# 
# result <- do.call(compare_images, c(
#   list("baseline/home.png", "current/home.png", diff_output = "home-diff.png"),
#   odiff_preset("screenshot")
# ))
# result$match

## ----eval = FALSE-------------------------------------------------------------
# results <- do.call(compare_image_dirs, c(
#   list("baseline/", "current/", diff_dir = "diffs/"),
#   odiff_preset("screenshot")
# ))
# batch_report(results, output_file = "report.html", images = "all",
#              embed = TRUE)

## ----eval = FALSE-------------------------------------------------------------
# test_that("report looks the same", {
#   skip_if_not_installed("webshot2")
#   path <- tempfile(fileext = ".png")
#   webshot2::webshot("report.html", file = path,
#                     vwidth = 1024, vheight = 768)
# 
#   expect_snapshot_image(
#     path,
#     name = "report",
#     preset = "screenshot",
#     variant = Sys.info()[["sysname"]]  # fonts differ between systems
#   )
# })

