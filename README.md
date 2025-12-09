# Odiffr <img src="man/figures/logo.png" align="right" height="139" alt="Odiffr logo" />

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/odiffr)](https://CRAN.R-project.org/package=odiffr)
[![R-CMD-check](https://github.com/BenWolst/odiffr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BenWolst/odiffr/actions/workflows/R-CMD-check.yaml)
[![Codecov](https://codecov.io/gh/BenWolst/odiffr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/BenWolst/odiffr)

<!-- badges: end -->

Fast pixel-by-pixel image comparison for R, powered by [odiff](https://github.com/dmtrKovalenko/odiff).

## Features

- **Blazing fast**: ~6x faster than ImageMagick, optimized with SIMD (SSE2, AVX2, AVX512, NEON)
- **Cross-platform**: Works on Windows, macOS (Intel & Apple Silicon), and Linux
- **Flexible**: Accepts file paths or magick-image objects
- **Configurable**: Threshold, antialiasing detection, region ignoring
- **HTML reports**: Generate standalone QA reports with `batch_report()`
- **testthat integration**: `expect_images_match()` and `expect_images_differ()` for visual regression testing

## Installation

### System Requirements

Odiffr requires the Odiff binary to be installed on your system:

```bash
# npm (cross-platform, recommended)
npm install -g odiff-bin

# Or download binaries from GitHub releases
# https://github.com/dmtrKovalenko/odiff/releases
```

### Install Odiffr

```r
# Install from CRAN (when available)
install.packages("odiffr")

# Or install the development version from GitHub
# install.packages("pak")
pak::pak("BenWolst/odiffr")
```

### Alternative: Download via R

If you cannot install Odiff system-wide:

```r
odiffr::odiffr_update()  # Downloads to user cache
```

## Quick Start

```r
library(odiffr)

# Compare two images
result <- compare_images("baseline.png", "current.png")
result$match
#> [1] FALSE

result$diff_percentage
#> [1] 2.45

# Generate a diff image
result <- compare_images("baseline.png", "current.png", diff_output = "diff.png")
```

## Usage

### Basic Comparison

```r
# High-level API (returns tibble if available)
result <- compare_images("img1.png", "img2.png")

# Low-level API (returns detailed list)
result <- odiff_run("img1.png", "img2.png")
```

### With Options

```r
# Adjust sensitivity threshold (0-1, lower = more precise)
result <- compare_images("img1.png", "img2.png", threshold = 0.05)

# Ignore antialiased pixels
result <- compare_images("img1.png", "img2.png", antialiasing = TRUE)

# Fail immediately if dimensions differ
result <- compare_images("img1.png", "img2.png", fail_on_layout = TRUE)
```

### Ignore Regions

```r
# Ignore specific areas (e.g., timestamps, dynamic content)
result <- compare_images("img1.png", "img2.png",
  ignore_regions = list(
    ignore_region(0, 0, 200, 50),     # Header
    ignore_region(0, 500, 800, 600)   # Footer
  )
)
```

### Batch Comparison

```r
# Compare multiple image pairs
pairs <- data.frame(
  img1 = c("baseline/page1.png", "baseline/page2.png"),
  img2 = c("current/page1.png", "current/page2.png")
)

results <- compare_images_batch(pairs, diff_dir = "diffs/")

# Extract failures or passes
failed_pairs(results)
passed_pairs(results)

# Compare entire directories
results <- compare_image_dirs("baseline/", "current/", recursive = TRUE)

# Get summary statistics
summary(results)
#> odiffr batch comparison: 50 pairs
#> Passed: 42 (84.0%)
#> Failed: 8 (16.0%)

# Use parallel processing (Unix only)
results <- compare_images_batch(pairs, parallel = TRUE)
```

### HTML Reports

```r
# One-liner: compare directories and generate HTML report
compare_dirs_report("baseline/", "current/")
# -> Creates diffs/ directory with diff images and report.html

# Or step-by-step for more control
results <- compare_image_dirs("baseline/", "current/", diff_dir = "diffs/")
batch_report(results, output_file = "qa-report.html")

# Self-contained report with embedded images
batch_report(results, output_file = "qa-report.html", embed = TRUE)

# Portable report with relative image paths
batch_report(results, output_file = "output/report.html", relative_paths = TRUE)
```

### CI Integration

Run visual regression tests in GitHub Actions and upload diff artifacts:

```yaml
# .github/workflows/visual-regression.yaml
name: Visual Regression

on: [push, pull_request]

jobs:
  visual-test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: r-lib/actions/setup-r@v2

      - name: Install dependencies
        run: |
          install.packages(c("odiffr", "webshot2"))
          odiffr::odiffr_update()
        shell: Rscript {0}

      - name: Generate screenshots
        run: Rscript scripts/generate-screenshots.R

      - name: Compare images
        run: |
          library(odiffr)
          results <- compare_dirs_report("baseline/", "current/")
          if (any(!results$match)) stop("Visual regression detected!")
        shell: Rscript {0}

      - name: Upload diffs
        if: failure()
        uses: actions/upload-artifact@v4
        with:
          name: visual-diffs
          path: diffs/
```

### With magick Package

```r
library(magick)

# Compare magick-image objects directly
img1 <- image_read("baseline.png") |> image_resize("800x600")
img2 <- image_read("current.png") |> image_resize("800x600")

result <- compare_images(img1, img2)
```

### Visual Regression Testing

```r
library(testthat)
library(odiffr)

test_that("dashboard renders correctly", {

  expect_images_match(
    "screenshots/current.png",
    "screenshots/baseline.png",
    threshold = 0.1
  )
})

test_that("button changes on hover", {
  expect_images_differ(
    "button_normal.png",
    "button_hover.png"
  )
})
```

On failure, diff images are automatically saved to `tests/testthat/_odiffr/`.

## Binary Management

```r
# Check if Odiff is available
odiff_available()

# Get version and configuration info
odiff_info()

# Update to latest version (downloads to user cache)
odiffr_update()

# Use a specific binary
options(odiffr.path = "/path/to/odiff")
```

### Binary Detection Priority

1. `options(odiffr.path = "...")` - User override
2. System PATH (`Sys.which("odiff")`)
3. Cached binary from `odiffr_update()`

## Supported Formats

| Type   | Formats               |
| ------ | --------------------- |
| Input  | PNG, JPEG, WEBP, TIFF |
| Output | PNG only              |

Cross-format comparison is supported (e.g., compare JPEG to PNG).

## For Validated Environments

Odiffr is designed for use in validated pharmaceutical and clinical research:

- **Pinnable**: Lock to specific validated binary with `options(odiffr.path = ...)`
- **Auditable**: Use `odiff_version()` to document binary version for audit trails
- **Base R core**: Zero external runtime dependencies for core functions

```r
# Pin to a specific validated binary
options(odiffr.path = "/validated/bin/odiff-4.1.2")

# Document in validation scripts
info <- odiff_info()
sprintf("Using odiff %s from %s", info$version, info$source)
```

## Performance

Odiff is approximately 6x faster than ImageMagick for pixel comparison, thanks to SIMD optimizations. Performance scales well with image size.

## Related

- [odiff](https://github.com/dmtrKovalenko/odiff) - The underlying CLI tool
- [magick](https://cran.r-project.org/package=magick) - R wrapper for ImageMagick
- [testthat](https://cran.r-project.org/package=testthat) - For visual regression tests

## License

MIT
