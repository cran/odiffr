# Tests for expect_snapshot_image() and compare_file_odiff()

# Write a 100x100 PNG of `color`, optionally with one changed pixel / a
# changed region
write_snapshot_png <- function(path, color = c(1, 0, 0), modify = "none") {
  img <- array(0, dim = c(100, 100, 3))
  for (i in 1:3) img[, , i] <- color[i]
  if (modify == "pixel") {
    img[50, 50, ] <- c(color[1], color[2], 0.04)  # tiny colour change
  } else if (modify == "region") {
    img[30:70, 30:70, ] <- 1
  }
  png::writePNG(img, path)
  path
}

# The generated test files use `odiffr:::` so they also work under
# pkgload::load_all() before NAMESPACE is regenerated.

# Set up a throwaway package-like directory with one test file that snapshots
# `image` (a PNG path). Returns the path of the test file.
local_snapshot_project <- function(body, env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  test_dir <- file.path(dir, "tests", "testthat")
  dir.create(test_dir, recursive = TRUE)
  test_file <- file.path(test_dir, "test-img.R")
  writeLines(c("testthat::local_edition(3)", body), test_file)
  test_file
}

# Run a test file and return a one-row summary of its results
run_snapshot_test <- function(test_file) {
  withr::local_envvar(CI = "false", NOT_CRAN = "true")
  messages <- character(0)
  res <- withCallingHandlers(
    testthat::test_file(test_file, reporter = "silent",
                        stop_on_failure = FALSE),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  df <- as.data.frame(res)
  list(
    messages = messages,
    failed = sum(df$failed),
    warning = sum(df$warning),
    skipped = sum(df$skipped),
    error = any(df$error),
    passed = sum(df$passed),
    results = res
  )
}

snap_body <- function(image, ...) {
  args <- list(...)
  extra <- if (length(args)) {
    paste0(", ", paste(names(args), vapply(args, function(a) paste(deparse(a), collapse = ""), ""), sep = " = ",
                       collapse = ", "))
  } else {
    ""
  }
  c(
    sprintf("img <- %s", deparse(image)),
    "test_that('image snapshot', {",
    sprintf("  odiffr:::expect_snapshot_image(img, name = 'square'%s)", extra),
    "})"
  )
}

# compare_file_odiff() --------------------------------------------------------

test_that("compare_file_odiff() returns a comparison function", {
  cmp <- compare_file_odiff()
  expect_type(cmp, "closure")
  expect_named(formals(cmp), c("old", "new"))
})

test_that("compare_file_odiff() compares files with odiff", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  # Keep diff images out of the source tree
  withr::local_options(odiffr.diff_dir = withr::local_tempdir())
  dir <- withr::local_tempdir()
  a <- write_snapshot_png(file.path(dir, "a.png"))
  b <- write_snapshot_png(file.path(dir, "b.png"))
  c <- write_snapshot_png(file.path(dir, "c.png"), modify = "region")
  d <- write_snapshot_png(file.path(dir, "d.png"), modify = "pixel")

  expect_true(compare_file_odiff()(a, b))
  expect_message(expect_false(compare_file_odiff(diff_dir = FALSE)(a, c)))
  # A tiny colour change is below the threshold, but not byte-identical
  expect_false(identical(readBin(a, "raw", 1e5), readBin(d, "raw", 1e5)))
  expect_true(compare_file_odiff(threshold = 0.1)(a, d))
  expect_message(expect_false(
    compare_file_odiff(threshold = 0, diff_dir = FALSE)(a, d)
  ))
  # Ignore regions are honoured
  expect_true(compare_file_odiff(
    ignore_regions = list(ignore_region(25, 25, 75, 75))
  )(a, c))
})

test_that("compare_file_odiff() honours fail_on_layout", {
  skip_if_no_odiff()
  a <- create_test_image(100, 100, "red")
  b <- create_test_image(120, 100, "red")
  on.exit(unlink(c(a, b)), add = TRUE)
  expect_message(expect_false(
    compare_file_odiff(fail_on_layout = TRUE, diff_dir = FALSE)(a, b)
  ))
})

test_that("compare_file_odiff() warns and returns FALSE when odiff errors", {
  skip_if_no_odiff()
  # Keep diff images out of the source tree
  withr::local_options(odiffr.diff_dir = withr::local_tempdir())
  dir <- withr::local_tempdir()
  a <- write_snapshot_png(file.path(dir, "a.png"))
  bad <- file.path(dir, "bad.png")
  writeLines("not an image", bad)
  expect_warning(
    res <- compare_file_odiff()(bad, a),
    "odiff could not compare 'bad.png' with 'a.png'"
  )
  expect_false(res)
})

# Snapshot names -------------------------------------------------------------

test_that("snapshot names are derived and validated", {
  nm <- odiffr:::.snapshot_image_name
  expect_equal(nm("dir/plot.png", NULL, "\"dir/plot.png\""), "plot.png")
  expect_equal(nm("dir/plot.jpg", NULL, "x"), "plot.jpg.png")
  expect_equal(nm(function() 1, NULL, "my_plot"), "my_plot.png")
  expect_equal(nm(1, NULL, "p.2"), "p.2.png")
  expect_equal(nm(1, "scatter", "x"), "scatter.png")
  expect_equal(nm(1, "scatter.PNG", "x"), "scatter.PNG")
  expect_error(nm(1, NULL, "function() plot(1)"), "Supply `name`")
  expect_error(nm(1, NULL, c("function() {", "plot(1)", "}")), "Supply `name`")
  expect_error(nm(1, NULL, "f(x)"), "Supply `name`")
  expect_error(nm(1, "a/b", "x"), "not a path")
  expect_error(nm(1, "", "x"), "non-empty")
  expect_error(nm(1, c("a", "b"), "x"), "non-empty")
})

test_that("expect_snapshot_image() requires a name for inline functions", {
  expect_error(
    expect_snapshot_image(function() plot(1:3)),
    "Supply `name`"
  )
})

test_that("expect_snapshot_image() skips when odiff is unavailable", {
  testthat::local_mocked_bindings(odiff_available = function() FALSE,
                                  .package = "odiffr")
  expect_condition(
    expect_snapshot_image("x.png"),
    class = "skip"
  )
})

# Snapshot input conversion ------------------------------------------------

test_that(".snapshot_image_file() always returns a fresh PNG copy", {
  skip_if_not_installed("png")
  src <- create_test_image(10, 10, "blue")
  on.exit(unlink(src), add = TRUE)

  out <- odiffr:::.snapshot_image_file(src)
  on.exit(unlink(out), add = TRUE)
  expect_false(identical(normalizePath(out), normalizePath(src)))
  expect_identical(readBin(out, "raw", 1e5), readBin(src, "raw", 1e5))

  out2 <- odiffr:::.snapshot_image_file(function() plot(1:3),
                                        plot_options(width = 2, height = 2,
                                                     res = 20))
  on.exit(unlink(out2), add = TRUE)
  expect_equal(dim(png::readPNG(out2))[1:2], c(40, 40))
})

test_that(".snapshot_image_file() converts non-PNG files with magick", {
  skip_if_not_installed("magick")
  src <- create_test_image(10, 10, "green")
  jpg <- tempfile(fileext = ".jpg")
  on.exit(unlink(c(src, jpg)), add = TRUE)
  magick::image_write(magick::image_read(src), jpg, format = "jpeg")

  out <- odiffr:::.snapshot_image_file(jpg)
  on.exit(unlink(out), add = TRUE)
  expect_equal(magick::image_info(magick::image_read(out))$format, "PNG")
})

# Full snapshot workflow -----------------------------------------------------

test_that("expect_snapshot_image() creates, passes and fails snapshots", {
  skip_if_no_odiff()
  skip_if_not_installed("png")

  img_dir <- withr::local_tempdir()
  img <- write_snapshot_png(file.path(img_dir, "img.png"))
  test_file <- local_snapshot_project(snap_body(img))
  snap_dir <- file.path(dirname(test_file), "_snaps", "img")
  snap <- file.path(snap_dir, "square.png")
  snap_new <- file.path(snap_dir, "square.new.png")

  # 1. First run creates the snapshot (with a warning)
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)
  expect_false(res$error)
  expect_equal(res$warning, 1)
  expect_true(file.exists(snap))
  expect_identical(readBin(snap, "raw", 1e5), readBin(img, "raw", 1e5))

  # 2. Identical image passes
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)
  expect_equal(res$warning, 0)
  expect_false(res$error)
  expect_false(file.exists(snap_new))

  # 3. A tiny change below the threshold passes: odiff is used, not byte
  #    equality
  write_snapshot_png(img, modify = "pixel")
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)
  expect_false(res$error)
  expect_false(file.exists(snap_new))

  # 4. A visible change fails and writes <name>.new.png
  write_snapshot_png(img, modify = "region")
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 1)
  expect_false(res$error)
  expect_true(file.exists(snap_new))
  expect_identical(readBin(snap_new, "raw", 1e5), readBin(img, "raw", 1e5))
  # The baseline itself is untouched
  expect_false(identical(readBin(snap, "raw", 1e5),
                         readBin(img, "raw", 1e5)))

  # 5. Restoring the original image passes again and removes the .new file
  write_snapshot_png(img)
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)
  expect_false(file.exists(snap_new))
})

test_that("expect_snapshot_image() uses threshold and ignore_regions", {
  skip_if_no_odiff()
  skip_if_not_installed("png")

  img_dir <- withr::local_tempdir()
  img <- write_snapshot_png(file.path(img_dir, "img.png"))
  test_file <- local_snapshot_project(
    snap_body(img, ignore_regions = list(ignore_region(25, 25, 75, 75)))
  )
  run_snapshot_test(test_file)

  # The changed region is ignored
  write_snapshot_png(img, modify = "region")
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)

  # With threshold = 0, the tiny change is detected
  img2 <- write_snapshot_png(file.path(img_dir, "img2.png"))
  test_file2 <- local_snapshot_project(snap_body(img2, threshold = 0))
  run_snapshot_test(test_file2)
  write_snapshot_png(img2, modify = "pixel")
  res <- run_snapshot_test(test_file2)
  expect_equal(res$failed, 1)
})

test_that("expect_snapshot_image() snapshots plots and supports variants", {
  skip_if_no_odiff()

  body <- c(
    "draw <- function() plot(1:10)",
    "test_that('plot snapshot', {",
    "  odiffr:::expect_snapshot_image(draw, variant = 'linux',",
    "    plot_options = odiffr:::plot_options(width = 3, height = 3, res = 30))",
    "})"
  )
  test_file <- local_snapshot_project(body)
  snap <- file.path(dirname(test_file), "_snaps", "linux", "img", "draw.png")

  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)
  expect_false(res$error)
  expect_true(file.exists(snap))
  expect_equal(dim(png::readPNG(snap))[1:2], c(90, 90))

  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)
  expect_equal(res$warning, 0)

  # Changing the plot fails
  writeLines(sub("plot(1:10)", "plot(10:1)", readLines(test_file),
                 fixed = TRUE), test_file)
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 1)
})

test_that("expect_snapshot_image() snapshots ggplot objects", {
  skip_if_no_odiff()
  skip_if_not_installed("ggplot2")

  body <- c(
    "p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +",
    "  ggplot2::geom_point()",
    "test_that('ggplot snapshot', {",
    "  odiffr:::expect_snapshot_image(p,",
    "    plot_options = odiffr:::plot_options(width = 3, height = 3, res = 30))",
    "})"
  )
  test_file <- local_snapshot_project(body)
  snap <- file.path(dirname(test_file), "_snaps", "img", "p.png")

  run_snapshot_test(test_file)
  expect_true(file.exists(snap))
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)
  expect_equal(res$warning, 0)
})

# Presets -------------------------------------------------------------------

test_that("odiff_preset() returns documented values", {
  expect_equal(odiff_preset("strict"),
               list(threshold = 0, antialiasing = FALSE))
  expect_equal(odiff_preset("default"),
               list(threshold = 0.1, antialiasing = FALSE))
  expect_equal(odiff_preset("screenshot"),
               list(threshold = 0.1, antialiasing = TRUE))
  expect_equal(odiff_preset("cross_platform"),
               list(threshold = 0.2, antialiasing = TRUE))
  expect_equal(odiff_preset(), odiff_preset("strict"))
  expect_error(odiff_preset("loose"), "preset must be one of")
  expect_error(odiff_preset(c("strict", "default")), "preset must be one of")
  expect_error(odiff_preset(NA_character_), "preset must be one of")
  expect_error(odiff_preset(1), "preset must be one of")
})

test_that("default preset matches the defaults of compare_file_odiff()", {
  f <- formals(compare_file_odiff)
  expect_equal(odiff_preset("default"),
               list(threshold = f$threshold, antialiasing = f$antialiasing))
})

test_that("compare_file_odiff() takes defaults from a preset", {
  env <- function(cmp) environment(cmp)
  cmp <- compare_file_odiff(preset = "cross_platform")
  expect_equal(env(cmp)$threshold, 0.2)
  expect_true(env(cmp)$antialiasing)

  # Explicit arguments win over the preset
  cmp <- compare_file_odiff(threshold = 0.05, preset = "cross_platform")
  expect_equal(env(cmp)$threshold, 0.05)
  expect_true(env(cmp)$antialiasing)
  cmp <- compare_file_odiff(antialiasing = FALSE, preset = "screenshot")
  expect_false(env(cmp)$antialiasing)

  # No preset: the usual defaults
  cmp <- compare_file_odiff()
  expect_equal(env(cmp)$threshold, 0.1)
  expect_false(env(cmp)$antialiasing)

  expect_error(compare_file_odiff(preset = "nope"), "preset must be one of")
  expect_error(compare_file_odiff(diff_dir = 1), "diff_dir must be")
  expect_error(compare_file_odiff(diff_dir = c("a", "b")), "diff_dir must be")
})

test_that("compare_file_odiff() keeps its positional arguments", {
  expect_equal(
    names(formals(compare_file_odiff))[1:5],
    c("threshold", "antialiasing", "ignore_regions", "fail_on_layout", "...")
  )
  cmp <- compare_file_odiff(0.3, TRUE)
  expect_equal(environment(cmp)$threshold, 0.3)
  expect_true(environment(cmp)$antialiasing)
})

test_that("presets pass anti-aliasing noise but catch real changes", {
  # Calibration of the "screenshot" and "cross_platform" presets. The
  # images differ only in anti-aliased edges (sub-pixel shifts) or by a
  # genuine 10x10 px change.
  skip_if_no_odiff()
  skip_on_cran()
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  draw <- function(dx = 0, border = NA, add = FALSE) {
    grid::grid.newpage()
    grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
    grid::pushViewport(grid::viewport(
      x = grid::unit(0.5, "npc") + grid::unit(dx, "points"),
      y = grid::unit(0.5, "npc") + grid::unit(dx / 2, "points")
    ))
    grid::grid.roundrect(0.3, 0.6, 0.4, 0.3, r = grid::unit(8, "pt"),
                         gp = grid::gpar(fill = "#3c8dbc", col = border))
    grid::grid.circle(0.75, 0.6, 0.12,
                      gp = grid::gpar(fill = "#f39c12", col = border))
    grid::grid.roundrect(0.5, 0.2, 0.8, 0.15, r = grid::unit(4, "pt"),
                         gp = grid::gpar(fill = "#eeeeee", col = border))
    grid::popViewport()
    if (add) {
      grid::grid.rect(grid::unit(20, "points"), grid::unit(150, "points"),
                      grid::unit(10, "points"), grid::unit(10, "points"),
                      just = c("left", "bottom"),
                      gp = grid::gpar(fill = "red", col = NA))
    }
  }
  render <- function(name, ...) {
    path <- file.path(dir, name)
    ragg::agg_png(path, width = 300, height = 200, res = 72)
    on.exit(grDevices::dev.off())
    draw(...)
    path
  }
  recolour <- function(src, name) {
    img <- png::readPNG(src)[, , 1:3]
    img[75:84, 70:79, 1] <- 0
    img[75:84, 70:79, 2] <- 0xa6 / 255
    img[75:84, 70:79, 3] <- 0x5a / 255  # blue -> green
    path <- file.path(dir, name)
    png::writePNG(img, path)
    path
  }
  matches <- function(a, b, preset) {
    suppressMessages(
      compare_file_odiff(preset = preset, diff_dir = FALSE)(a, b)
    )
  }

  base <- render("base.png")
  shifted <- render("shifted.png", dx = 0.25)
  shifted_half <- render("shifted_half.png", dx = 0.5)
  border <- render("border.png", border = "#cccccc")
  border_shifted <- render("border_shifted.png", dx = 0.25,
                           border = "#cccccc")
  border_half <- render("border_half.png", dx = 0.5, border = "#cccccc")
  added <- render("added.png", add = TRUE)
  green <- recolour(base, "green.png")

  # Anti-aliasing-only differences fail with the default settings ...
  expect_false(matches(base, shifted, "default"))
  expect_false(matches(base, shifted_half, "default"))
  expect_false(matches(border, border_shifted, "default"))
  # ... but pass with "screenshot"
  expect_true(matches(base, shifted, "screenshot"))
  expect_true(matches(base, shifted_half, "screenshot"))
  expect_true(matches(border, border_shifted, "screenshot"))
  # A half-pixel shift of a bordered shape needs "cross_platform"
  expect_true(matches(border, border_half, "cross_platform"))

  # Genuine changes fail with every preset
  for (preset in c("strict", "default", "screenshot", "cross_platform")) {
    expect_false(matches(base, added, preset), label = preset)
  }
  # A recolouring of similar brightness is caught up to "screenshot"
  for (preset in c("strict", "default", "screenshot")) {
    expect_false(matches(base, green, preset), label = preset)
  }

  # Strict catches everything
  expect_false(matches(base, shifted, "strict"))
})

# Diff images ----------------------------------------------------------------

test_that(".snapshot_diff_path() mirrors the _snaps layout", {
  p <- odiffr:::.snapshot_diff_path
  expect_equal(p("_snaps/plots/scatter.png", "out"),
               file.path("out", "plots", "scatter_diff.png"))
  expect_equal(p("tests/testthat/_snaps/linux/plots/scatter.png", "out"),
               file.path("out", "linux", "plots", "scatter_diff.png"))
  expect_equal(p(file.path(tempdir(), "elsewhere", "old.png"), "out"),
               file.path("out", "old_diff.png"))
  expect_equal(p("_snaps/app-001.png", "out"),
               file.path("out", "app-001_diff.png"))
  expect_null(p("_snaps/plots/scatter.png", FALSE))
})

test_that(".resolve_snapshot_diff_dir() follows options", {
  r <- odiffr:::.resolve_snapshot_diff_dir
  expect_null(r(FALSE))
  expect_equal(r("my/dir"), "my/dir")
  withr::with_options(list(odiffr.save_diff = FALSE), {
    expect_null(r(NULL))
    expect_equal(r("my/dir"), "my/dir")
  })
  withr::with_options(list(odiffr.diff_dir = "custom"), {
    expect_equal(r(NULL), "custom")
  })
  # In a test run: tests/testthat/_odiffr
  expect_equal(r(NULL), testthat::test_path("_odiffr"))
  # Outside tests: tests/testthat/_odiffr in a package root, else tempdir()
  testthat::local_mocked_bindings(is_testing = function() FALSE,
                                  .package = "testthat")
  withr::with_dir(withr::local_tempdir(), {
    expect_equal(r(NULL), file.path(tempdir(), "odiffr-snapshot-diffs"))
    dir.create(file.path("tests", "testthat"), recursive = TRUE)
    expect_equal(r(NULL), file.path("tests", "testthat", "_odiffr"))
  })
})

test_that("compare_file_odiff() writes a diff image outside _snaps", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  snap_dir <- file.path(dir, "_snaps", "img")
  dir.create(snap_dir, recursive = TRUE)
  old <- write_snapshot_png(file.path(snap_dir, "square.png"))
  # testthat passes the new image as a temporary file
  new <- write_snapshot_png(tempfile(fileext = ".png"), modify = "region")
  on.exit(unlink(new), add = TRUE)
  diff_dir <- file.path(dir, "diffs")
  expected <- file.path(diff_dir, "img", "square_diff.png")

  cmp <- compare_file_odiff(diff_dir = diff_dir)
  expect_message(
    res <- cmp(old, new),
    "odiff: 16.81% pixels differ \\(1,681 px\\) in 'square.png'; diff image: .*square_diff.png"
  )
  expect_false(res)
  expect_true(file.exists(expected))
  expect_equal(list.files(snap_dir), "square.png")

  # Re-running overwrites the same file
  expect_message(cmp(old, new), "diff image")
  expect_equal(list.files(diff_dir, recursive = TRUE), "img/square_diff.png")

  # A passing comparison removes the stale diff, silently
  expect_silent(res <- cmp(old, old))
  expect_true(res)
  expect_false(file.exists(expected))
})

test_that("compare_file_odiff() also works with a .new.png path", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  snap_dir <- file.path(dir, "_snaps", "linux", "app")
  dir.create(snap_dir, recursive = TRUE)
  old <- write_snapshot_png(file.path(snap_dir, "app-001.png"))
  new <- write_snapshot_png(file.path(snap_dir, "app-001.new.png"),
                            modify = "region")
  diff_dir <- file.path(dir, "diffs")
  expect_message(
    res <- compare_file_odiff(diff_dir = diff_dir)(old, new),
    "app-001_diff.png"
  )
  expect_false(res)
  expect_true(file.exists(file.path(diff_dir, "linux", "app",
                                    "app-001_diff.png")))
})

test_that("diff images can be disabled", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  old <- write_snapshot_png(file.path(dir, "a.png"))
  new <- write_snapshot_png(file.path(dir, "b.png"), modify = "region")

  expect_message(res <- compare_file_odiff(diff_dir = FALSE)(old, new),
                 "no diff image")
  expect_false(res)

  withr::local_options(odiffr.save_diff = FALSE)
  expect_message(compare_file_odiff()(old, new), "no diff image")
  expect_equal(list.files(dir), c("a.png", "b.png"))
})

test_that("layout differences are reported without a diff image", {
  skip_if_no_odiff()
  dir <- withr::local_tempdir()
  a <- create_test_image(100, 100, "red")
  b <- create_test_image(120, 100, "red")
  on.exit(unlink(c(a, b)), add = TRUE)
  expect_message(
    res <- compare_file_odiff(diff_dir = dir)(a, b),
    "image dimensions differ.*no diff image"
  )
  expect_false(res)
  expect_length(list.files(dir, recursive = TRUE), 0)
})

test_that("the odiffr.snapshot_diff_dir option sets the default", {
  withr::local_options(odiffr.snapshot_diff_dir = "from-option")
  expect_equal(environment(compare_file_odiff())$diff_dir, "from-option")
})

test_that("expect_snapshot_image() writes diff images and honours preset", {
  skip_if_no_odiff()
  skip_if_not_installed("png")

  img_dir <- withr::local_tempdir()
  img <- write_snapshot_png(file.path(img_dir, "img.png"))
  test_file <- local_snapshot_project(snap_body(img))
  test_dir <- dirname(test_file)
  diff <- file.path(test_dir, "_odiffr", "img", "square_diff.png")
  run_snapshot_test(test_file)

  # Failure: a message (not a warning) and a diff image outside _snaps
  write_snapshot_png(img, modify = "region")
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 1)
  expect_equal(res$warning, 0)
  expect_true(file.exists(diff))
  expect_match(res$messages, "diff image: .*_odiffr/img/square_diff.png",
               all = FALSE)
  expect_setequal(list.files(file.path(test_dir, "_snaps", "img")),
                  c("square.png", "square.new.png"))

  # Passing again removes the diff
  write_snapshot_png(img)
  res <- run_snapshot_test(test_file)
  expect_equal(res$failed, 0)
  expect_false(file.exists(diff))
  # ... and its now empty directory, but not the _odiffr root
  expect_false(dir.exists(dirname(diff)))
  expect_true(dir.exists(file.path(test_dir, "_odiffr")))

  # preset = "strict" detects the tiny change that the default ignores
  img2 <- write_snapshot_png(file.path(img_dir, "img2.png"))
  test_file2 <- local_snapshot_project(snap_body(img2, preset = "strict"))
  run_snapshot_test(test_file2)
  write_snapshot_png(img2, modify = "pixel")
  res <- run_snapshot_test(test_file2)
  expect_equal(res$failed, 1)

  # ... unless threshold is given explicitly
  img3 <- write_snapshot_png(file.path(img_dir, "img3.png"))
  test_file3 <- local_snapshot_project(
    snap_body(img3, preset = "strict", threshold = 0.1)
  )
  run_snapshot_test(test_file3)
  write_snapshot_png(img3, modify = "pixel")
  res <- run_snapshot_test(test_file3)
  expect_equal(res$failed, 0)
})

test_that("expect_snapshot_image() validates preset without odiff", {
  testthat::local_mocked_bindings(odiff_available = function() FALSE,
                                  .package = "odiffr")
  expect_error(expect_snapshot_image("x.png", preset = "nope"),
               "preset must be one of")
})

# Regression tests ---------------------------------------------------------

test_that("snapshot names are not derived from temporary files", {
  nm <- odiffr:::.snapshot_image_name
  tmp <- tempfile(fileext = ".png")
  expect_error(nm(tmp, NULL, "tmp"), "temporary file.*Supply `name`")
  expect_error(nm(file.path(withr::local_tempdir(), "plot.png"), NULL, "p"),
               "temporary file")
  # With a name it is fine
  expect_equal(nm(tmp, "plot", "tmp"), "plot.png")
  # Paths outside tempdir() still use their basename
  expect_equal(nm("/srv/odiffr-test/shots/home.png", NULL, "x"), "home.png")
  expect_equal(nm(file.path("output", "home.png"), NULL, "x"), "home.png")
})

test_that("expect_snapshot_image() errors for a temporary file without name", {
  skip_if_no_odiff()
  skip_if_not_installed("png")

  body <- c(
    "img <- tempfile(fileext = '.png')",
    "png::writePNG(array(0.5, dim = c(10, 10, 3)), img)",
    "test_that('temp snapshot', {",
    "  expect_error(odiffr:::expect_snapshot_image(img), 'Supply `name`')",
    "})"
  )
  test_file <- local_snapshot_project(body)
  res <- run_snapshot_test(test_file)
  expect_false(res$error)
  expect_equal(res$failed, 0)
  expect_equal(res$passed, 1)
  expect_false(dir.exists(file.path(dirname(test_file), "_snaps", "img")))
})

test_that(".remove_empty_dirs() removes empty dirs up to (not incl.) root", {
  rm_empty <- odiffr:::.remove_empty_dirs
  root <- file.path(withr::local_tempdir(), "diffs")
  deep <- file.path(root, "linux", "plots", "img")
  dir.create(deep, recursive = TRUE)
  dir.create(file.path(root, "other"))
  writeLines("x", file.path(root, "other", "keep.txt"))

  # NULL root does nothing
  expect_invisible(rm_empty(deep, NULL))
  expect_true(dir.exists(deep))

  rm_empty(deep, root)
  expect_false(dir.exists(file.path(root, "linux")))
  expect_true(dir.exists(root))
  expect_true(file.exists(file.path(root, "other", "keep.txt")))

  # Non-empty directories (and their parents) are kept
  rm_empty(file.path(root, "other"), root)
  expect_true(dir.exists(file.path(root, "other")))
  sub <- file.path(root, "a", "b")
  dir.create(sub, recursive = TRUE)
  writeLines("x", file.path(root, "a", "file.txt"))
  rm_empty(sub, root)
  expect_false(dir.exists(sub))
  expect_true(dir.exists(file.path(root, "a")))

  # The root itself is never removed, even when empty
  empty_root <- withr::local_tempdir()
  rm_empty(empty_root, empty_root)
  expect_true(dir.exists(empty_root))

  # Directories outside root are left alone
  outside <- withr::local_tempdir()
  rm_empty(outside, root)
  expect_true(dir.exists(outside))
})

test_that("compare_file_odiff() removes empty diff dirs after a pass", {
  skip_if_no_odiff()
  skip_if_not_installed("png")
  dir <- withr::local_tempdir()
  snap_dir <- file.path(dir, "_snaps", "linux", "img")
  dir.create(snap_dir, recursive = TRUE)
  old <- write_snapshot_png(file.path(snap_dir, "square.png"))
  new <- write_snapshot_png(file.path(dir, "new.png"), modify = "region")
  diff_dir <- file.path(dir, "diffs")
  cmp <- compare_file_odiff(diff_dir = diff_dir)

  expect_message(expect_false(cmp(old, new)), "diff image")
  expect_true(file.exists(file.path(diff_dir, "linux", "img",
                                    "square_diff.png")))

  # Passing removes the diff and its now empty directories, not the root
  expect_true(cmp(old, old))
  expect_false(dir.exists(file.path(diff_dir, "linux")))
  expect_true(dir.exists(diff_dir))

  # Directories holding other diffs are kept
  expect_message(cmp(old, new), "diff image")
  other <- file.path(diff_dir, "linux", "other_diff.png")
  writeLines("x", other)
  expect_true(cmp(old, old))
  expect_false(dir.exists(file.path(diff_dir, "linux", "img")))
  expect_true(file.exists(other))
})
