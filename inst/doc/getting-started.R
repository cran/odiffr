## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = odiffr::odiff_available()
)

## ----eval=FALSE---------------------------------------------------------------
# # From CRAN (when available)
# install.packages("odiffr")
# 
# # Development version
# pak::pak("BenWolst/odiffr")

## ----setup--------------------------------------------------------------------
library(odiffr)

## -----------------------------------------------------------------------------
# Verify Odiff is available
odiff_available()

# View configuration details
odiff_info()

## ----eval=FALSE---------------------------------------------------------------
# result <- compare_images("baseline.png", "current.png")
# result
# #> # A tibble: 1 × 7
# #>   match reason     diff_count diff_percentage diff_output img1         img2
# #>   <lgl> <chr>           <int>           <dbl> <chr>       <chr>        <chr>
# #> 1 FALSE pixel-diff       1234            2.45 NA          baseline.png current.png

## ----eval=FALSE---------------------------------------------------------------
# # Specify output path
# result <- compare_images("baseline.png", "current.png",
#                          diff_output = "diff.png")
# 
# # Or use TRUE for auto-generated temp file
# result <- compare_images("baseline.png", "current.png",
#                          diff_output = TRUE)
# result$diff_output
# #> [1] "/tmp/RtmpXXXXXX/file12345.png"

## ----eval=FALSE---------------------------------------------------------------
# # Very strict comparison
# result <- compare_images("img1.png", "img2.png", threshold = 0.01)
# 
# # More lenient (ignore minor color variations)
# result <- compare_images("img1.png", "img2.png", threshold = 0.2)

## ----eval=FALSE---------------------------------------------------------------
# result <- compare_images("img1.png", "img2.png", antialiasing = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# result <- compare_images("img1.png", "img2.png",
#   ignore_regions = list(
#     ignore_region(x1 = 0, y1 = 0, x2 = 200, y2 = 50),    # Header
#     ignore_region(x1 = 0, y1 = 900, x2 = 1920, y2 = 1080) # Footer
#   )
# )

## ----eval=FALSE---------------------------------------------------------------
# pairs <- data.frame(
#   img1 = c("baseline/page1.png", "baseline/page2.png", "baseline/page3.png"),
#   img2 = c("current/page1.png", "current/page2.png", "current/page3.png")
# )
# 
# results <- compare_images_batch(pairs, diff_dir = "diffs/")
# 
# # View failures
# results[!results$match, ]

## ----eval=FALSE---------------------------------------------------------------
# library(magick)
# 
# # Read and preprocess images
# img1 <- image_read("baseline.png") |>
#   image_resize("800x600") |>
#   image_convert(colorspace = "sRGB")
# 
# img2 <- image_read("current.png") |>
#   image_resize("800x600") |>
#   image_convert(colorspace = "sRGB")
# 
# # Compare directly
# result <- compare_images(img1, img2)

## ----eval=FALSE---------------------------------------------------------------
# result <- odiff_run(
#   img1 = "baseline.png",
#   img2 = "current.png",
#   diff_output = "diff.png",
#   threshold = 0.1,
#   antialiasing = TRUE,
#   fail_on_layout = TRUE,
#   diff_mask = FALSE,
#   diff_overlay = 0.5,
#   diff_color = "#FF00FF",
#   diff_lines = TRUE,
#   reduce_ram = FALSE,
#   ignore_regions = list(ignore_region(10, 10, 100, 50)),
#   timeout = 60
# )
# 
# # Detailed result
# result$match
# result$reason
# result$diff_count
# result$diff_percentage
# result$diff_lines
# result$exit_code
# result$duration

## ----eval=FALSE---------------------------------------------------------------
# # Latest version
# odiffr_update()
# 
# # Specific version
# odiffr_update(version = "v4.1.2")

## ----eval=FALSE---------------------------------------------------------------
# options(odiffr.path = "/validated/bin/odiff-4.1.2")

## -----------------------------------------------------------------------------
# View cache location
odiffr_cache_path()

## ----eval=FALSE---------------------------------------------------------------
# # Clear cached binaries
# odiffr_clear_cache()

## ----eval=FALSE---------------------------------------------------------------
# library(testthat)
# library(odiffr)
# 
# test_that("dashboard renders correctly", {
#   # Generate current screenshot
#   webshot2::webshot("http://localhost:3838/dashboard", "current.png")
# 
#   # Compare to baseline
#   result <- compare_images(
#     "baselines/dashboard.png",
#     "current.png",
#     diff_output = "diffs/dashboard_diff.png",
#     threshold = 0.1,
#     antialiasing = TRUE
#   )
# 
#   expect_true(result$match,
#     info = sprintf("%.2f%% pixels differ", result$diff_percentage))
# })

## ----eval=FALSE---------------------------------------------------------------
# # Pin to a specific validated binary
# options(odiffr.path = "/validated/bin/odiff-4.1.2")
# 
# # Document version for validation
# info <- odiff_info()
# sprintf("Using odiff %s from %s", info$version, info$source)

