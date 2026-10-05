## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = odiffr::odiff_available()
)

## ----eval=FALSE---------------------------------------------------------------
# # From CRAN
# install.packages("odiffr")
# 
# # Development version
# pak::pak("BenWolst/odiffr")

## ----eval=FALSE---------------------------------------------------------------
# odiffr::install_odiff()

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
# # More lenient (ignore minor colour variations)
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
# # Compare baseline/ vs current/ directories
# results <- compare_image_dirs("baseline/", "current/")
# 
# # Include subdirectories
# results <- compare_image_dirs("baseline/", "current/", recursive = TRUE)
# 
# # Only compare PNG files
# results <- compare_image_dirs("baseline/", "current/", pattern = "\\.png$")

## ----eval=FALSE---------------------------------------------------------------
# results <- compare_image_dirs("baseline/", "current/")
# 
# # Get only failures
# failures <- failed_pairs(results)
# nrow(failures)
# #> [1] 8
# 
# # Get only passes
# passes <- passed_pairs(results)
# nrow(passes)
# #> [1] 42

## ----eval=FALSE---------------------------------------------------------------
# results <- compare_image_dirs("baseline/", "current/")
# summary(results)
# #> odiffr batch comparison: 50 pairs
# #> ───────────────────────────────────
# #> Passed: 42 (84.0%)
# #> Failed: 8 (16.0%)
# #>   - pixel-diff: 6
# #>   - layout-diff: 2
# #>
# #> Diff statistics (failed pairs):
# #>   Min:    0.15%
# #>   Median: 2.34%
# #>   Mean:   3.21%
# #>   Max:    12.45%
# #>
# #> Worst offenders:
# #>   1. page_a.png (12.45%, 1245 pixels)
# #>   2. page_b.png (8.32%, 832 pixels)

## ----eval=FALSE---------------------------------------------------------------
# # Compare in parallel on macOS/Linux
# results <- compare_images_batch(pairs, parallel = TRUE)
# 
# # Also works with directory comparison
# results <- compare_image_dirs("baseline/", "current/", parallel = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # Run batch comparison with diff images
# results <- compare_image_dirs(
#   "baseline/",
#   "current/",
#   diff_dir = "diffs/"
# )
# 
# # Generate HTML report (links to diff images)
# batch_report(results, output_file = "qa-report.html")
# 
# # Self-contained report with embedded images (for sharing)
# batch_report(results, output_file = "qa-report.html", embed = TRUE)
# 
# # Portable report with relative paths (move report + diffs together)
# batch_report(results, output_file = "output/report.html", relative_paths = TRUE)
# 
# # Customize the report
# batch_report(
#   results,
#   output_file = "report.html",
#   title = "Dashboard Visual Regression",
#   n_worst = 20,        # Show top 20 failures
#   show_all = TRUE,     # Include all comparisons, not just failures
#   images = "all"       # Baseline, current and diff side by side
# )

## ----eval=FALSE---------------------------------------------------------------
# # Compare and generate report in one step
# compare_dirs_report("baseline/", "current/")
# # -> Creates diffs/ directory with diff images and report.html
# 
# # Self-contained report with embedded images (recommended for sharing)
# compare_dirs_report("baseline/", "current/", embed = TRUE)
# 
# # See all comparisons, not just failures
# compare_dirs_report("baseline/", "current/", show_all = TRUE)
# 
# # Portable report with relative image paths
# compare_dirs_report("baseline/", "current/", relative_paths = TRUE)
# 
# # Combine options: parallel processing with embedded report
# compare_dirs_report("baseline/", "current/", parallel = TRUE, embed = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# results <- compare_image_dirs("baseline/", "current/", diff_dir = "diffs/")
# 
# # Review the differences first
# batch_report(results, "diffs/report.html")
# 
# # Preview what would change, then copy current images over the baselines
# approve_changes(results, dry_run = TRUE)
# approve_changes(results)
# 
# # Approve selected images only, keeping a backup of the old baselines
# approve_changes(results, which = "home.png", backup_dir = "baseline-backup/")
# 
# # Also delete baselines whose screenshot was intentionally removed
# approve_changes(results, reasons = "missing", remove_missing = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# # Baseline, current and diff image side by side (base graphics)
# result <- odiff_run("baseline.png", "current.png", diff_output = "diff.png")
# plot(result)
# plot(result, which = "diff")
# 
# # Get the diff image of a compare_images() result or a batch row
# img <- diff_image(compare_images("baseline.png", "current.png",
#                                  diff_output = TRUE))  # magick-image
# plot(diff_image(failed_pairs(results)[1, ], as = "raster"))  # no magick

## ----eval=FALSE---------------------------------------------------------------
# use_odiffr_ci()

## ----eval=FALSE---------------------------------------------------------------
# # In your CI script
# results <- compare_dirs_report("baseline/", "current/")
# 
# # Fail the build if any images differ
# if (any(!results$match)) {
#   stop("Visual regression detected! See diffs/ for details.")
# }

## ----eval=FALSE---------------------------------------------------------------
# results <- compare_image_dirs("baseline/", "current/", diff_dir = "diffs/")
# 
# batch_markdown(results)                    # GitHub job summary
# batch_junit(results, "odiffr-junit.xml")   # JUnit XML
# batch_report(results, "diffs/report.html", images = "all", embed = TRUE)

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
#   enable_asm = TRUE,
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
# install_odiff()
# 
# # Specific version, replacing the cached binary
# install_odiff(version = "v4.1.2", force = TRUE)

## ----eval=FALSE---------------------------------------------------------------
# options(odiffr.path = "/validated/bin/odiff-4.1.2")

## ----eval=FALSE---------------------------------------------------------------
# info <- odiff_info()
# info$path  # the native binary
# info$shim  # the npm launcher it was resolved from (NA if none)
# 
# # Opt out and use the PATH entry as-is
# options(odiffr.resolve_npm = FALSE)

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
#   skip_if_no_odiff()
# 
#   # Generate current screenshot (using your preferred method)
#   webshot2::webshot("http://localhost:3838/dashboard", "current.png")
# 
#   # Compare to baseline using expect_images_match()
#   expect_images_match(
#     "current.png",
#     "baselines/dashboard.png",
#     threshold = 0.1,
#     antialiasing = TRUE
#   )
# })
# 
# test_that("button changes on hover", {
#   skip_if_no_odiff()
# 
#   # Assert that images are different
#   expect_images_differ(
#     "button_normal.png",
#     "button_hover.png"
#   )
# })

## ----eval=FALSE---------------------------------------------------------------
# library(ggplot2)
# 
# test_that("scatter plot matches baseline", {
#   p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
# 
#   expect_images_match(
#     p,
#     test_path("baselines/scatter.png"),
#     plot_options = plot_options(width = 6, height = 4, res = 96)
#   )
# })
# 
# # compare_images() accepts plots too; they are labelled "<plot>"
# compare_images("baseline_hist.png", function() hist(mtcars$mpg))

## ----eval=FALSE---------------------------------------------------------------
# test_that("plots are stable", {
#   p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
#   expect_snapshot_image(p)  # snapshot name: "p.png"
# 
#   expect_snapshot_image(
#     function() hist(mtcars$mpg),
#     name = "mpg-hist",          # required for inline expressions
#     antialiasing = TRUE,
#     variant = Sys.info()[["sysname"]]  # separate snapshots per OS
#   )
# })

## ----eval=FALSE---------------------------------------------------------------
# testthat::snapshot_review()
# testthat::snapshot_accept()

## ----eval=FALSE---------------------------------------------------------------
# # Disable diff image saving
# options(odiffr.save_diff = FALSE)
# 
# # Use a custom directory
# options(odiffr.diff_dir = "my_diffs/")

## ----eval=FALSE---------------------------------------------------------------
# # Pin to a specific validated binary
# options(odiffr.path = "/validated/bin/odiff-4.1.2")
# 
# # Document version for validation
# info <- odiff_info()
# sprintf("Using odiff %s from %s", info$version, info$source)

## ----eval=FALSE---------------------------------------------------------------
# # odiff_run() results carry their parameters
# result <- odiff_run("baseline.png", "current.png", "diff.png",
#                     threshold = 0.05)
# rec <- audit_record(result)
# rec$header$odiff_version
# rec$comparisons[, c("img1_hash", "img2_hash", "match")]
# 
# # Write a JSON evidence file (requires jsonlite)
# audit_record(result, file = "validation/comparison-audit.json")
# 
# # Batch results: pass the parameters you used; CSV has one row per pair
# results <- compare_image_dirs("baseline/", "current/", threshold = 0.05)
# audit_record(results, file = "validation/audit.csv", format = "csv",
#              params = list(threshold = 0.05))

