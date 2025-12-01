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
