# Odiffr 0.5.1

## Bug Fixes

* `odiff_version()` now correctly parses the version from `odiff --version`
  output instead of `--help`, which did not contain version information.
* `batch_report()` with `relative_paths = TRUE` now produces correct relative
  paths on Windows by normalizing path separators before computing relative
  paths.

# Odiffr 0.5.0

## New Features

* `batch_report()` gains a `relative_paths` parameter. When `TRUE`, image
  paths in HTML reports are relative to the report location, making reports
  portable without embedding images.
* `compare_image_dirs()` now emits a message when files in `current_dir`
  have no corresponding baseline, helping catch missing or extra images early.
* New accessor functions `failed_pairs()` and `passed_pairs()` make it
  easy to filter batch comparison results.

## Documentation

* Expanded README and vignette coverage for batch workflows, HTML reports,
  and CI/testthat integration, including examples using `embed = TRUE`,
  `show_all = TRUE`, and `relative_paths = TRUE`.

# Odiffr 0.4.1

## New Features

* `compare_dirs_report()` convenience function combines `compare_image_dirs()`
  and `batch_report()` into a single call for the common QA workflow of
  comparing two directories and generating an HTML report.

## Documentation

* Added CI integration examples showing how to run visual regression tests
  in GitHub Actions and upload diff artifacts on failure.

# Odiffr 0.4.0

## HTML Diff Reports

* `batch_report()`: Generate standalone HTML reports from batch comparison
  results. Reports include pass/fail statistics, failure reason breakdown,
  diff statistics, and thumbnails of worst offenders.
* Configurable image embedding: Use `embed = TRUE` for self-contained reports
  with base64-encoded images, or `embed = FALSE` (default) to link to files.
* Customizable: Set report title, number of worst offenders to display, and
  optionally include all comparisons (not just failures) with `show_all = TRUE`.

# Odiffr 0.3.0

## Directory Comparison

* `compare_image_dirs()`: Compare all images in two directories by matching
  relative paths. Baseline directory is source of truth; missing files in
  current directory trigger warnings and are excluded from results.

## Batch Results Summary

* `summary()` method for batch results: Get aggregate statistics including
  pass/fail counts, failure reason breakdown, diff statistics (min, median,
  mean, max), and worst offenders ranked by diff percentage.
* `compare_images_batch()` and `compare_image_dirs()` now return objects with
  class `odiffr_batch` for S3 method dispatch.

## Parallel Batch Processing

* New `parallel` parameter for `compare_images_batch()` and `compare_image_dirs()`:
  Set `parallel = TRUE` to compare images using multiple CPU cores.
* Uses `parallel::mclapply` on Unix systems (macOS, Linux) for faster batch
  comparisons.
* Automatically falls back to sequential processing on Windows.

# Odiffr 0.2.0

## testthat Integration

* `expect_images_match()`: Assert two images are visually identical
* `expect_images_differ()`: Assert two images are visually different
* Automatic diff image saving to `tests/testthat/_odiffr/` on failure
* Configurable via `options(odiffr.save_diff)` and `options(odiffr.diff_dir)`

# Odiffr 0.1.0

Initial release.

## Features

* `compare_images()`: High-level image comparison returning tibble/data.frame
* `compare_images_batch()`: Batch comparison of multiple image pairs
* `odiff_run()`: Low-level CLI wrapper with full option control
* `ignore_region()`: Helper for creating ignore region specifications

## Binary Management

* `find_odiff()`: Locate Odiff binary with priority-based search
* `odiff_available()`: Check if Odiff is available
* `odiff_version()`: Get Odiff version string
* `odiff_info()`: Display full configuration information
* `odiffr_update()`: Download Odiff binary to user cache (fallback option)
* `odiffr_cache_path()`: Get cache directory path
* `odiffr_clear_cache()`: Remove cached binaries

## System Requirements

Requires Odiff (>= 3.0.0) to be installed. Install via:

* npm (cross-platform): `npm install -g odiff-bin`
* Manual: Download from https://github.com/dmtrKovalenko/odiff/releases

Alternatively, use `odiffr_update()` to download to user cache.

## Platform Support

Works on any platform where Odiff is available:

* macOS (ARM64 and x64)
* Linux (ARM64 and x64)
* Windows (ARM64 and x64)
