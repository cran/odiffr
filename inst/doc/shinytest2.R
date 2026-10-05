## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----eval = FALSE-------------------------------------------------------------
# library(shinytest2)
# 
# test_that("app renders", {
#   app <- AppDriver$new(variant = platform_variant(), name = "app")
#   app$expect_screenshot(
#     compare = odiffr::compare_file_odiff(preset = "screenshot")
#   )
# })

## ----eval = FALSE-------------------------------------------------------------
# app$expect_screenshot(
#   compare = odiffr::compare_file_odiff(
#     preset = "screenshot",
#     threshold = 0.15,
#     ignore_regions = list(odiffr::ignore_region(0, 0, 300, 40))
#   )
# )

## ----eval = FALSE-------------------------------------------------------------
# # tests/testthat/setup.R
# compare_screenshot <- odiffr::compare_file_odiff(preset = "screenshot")
# 
# expect_app_screenshot <- function(app, ...) {
#   app$expect_screenshot(..., compare = compare_screenshot)
# }

## ----eval = FALSE-------------------------------------------------------------
# test_that("filters update the table", {
#   app <- AppDriver$new(variant = platform_variant(), name = "filters")
#   app$set_inputs(species = "setosa")
#   expect_app_screenshot(app)
#   expect_app_screenshot(app, selector = "#table")
# })

## ----eval = FALSE-------------------------------------------------------------
# options(odiffr.snapshot_diff_dir = file.path(tempdir(), "screenshot-diffs"))

## ----eval = FALSE-------------------------------------------------------------
# testthat::snapshot_review("app")

