# Tests for plot rendering (R/render.R) and plot inputs to compare_images()

png_dim <- function(path) {
  dim(png::readPNG(path))[1:2]
}

draw_points <- function() {
  graphics::plot(1:10, main = "points")
}

# plot_options() -------------------------------------------------------------

test_that("plot_options() returns validated options with defaults", {
  opts <- plot_options()
  expect_s3_class(opts, "odiffr_plot_options")
  expect_equal(
    unclass(opts),
    list(width = 7, height = 5, units = "in", res = 96, bg = "white")
  )
  expect_output(print(opts), "7 x 5 in, 96 dpi")

  opts <- plot_options(width = 300, height = 200, units = "px", bg = "grey")
  expect_equal(opts$units, "px")
  expect_equal(opts$bg, "grey")
})

test_that("plot_options() rejects invalid values", {
  expect_error(plot_options(width = -1), "width must be")
  expect_error(plot_options(height = "a"), "height must be")
  expect_error(plot_options(res = NA_real_), "res must be")
  expect_error(plot_options(units = "ft"), "units must be")
  expect_error(plot_options(bg = c("white", "red")), "bg must be")
})

test_that(".as_plot_options() accepts NULL, plot_options() and named lists", {
  expect_equal(odiffr:::.as_plot_options(NULL), unclass(plot_options()))
  expect_equal(odiffr:::.as_plot_options(plot_options(width = 3))$width, 3)
  expect_equal(odiffr:::.as_plot_options(list(width = 2, res = 50))$res, 50)
  expect_error(odiffr:::.as_plot_options(list(wdth = 2)), "plot_options")
  expect_error(odiffr:::.as_plot_options("big"), "plot_options must be")
})

# Type detection --------------------------------------------------------------

test_that("plot input types are detected", {
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  expect_true(odiffr:::.is_ggplot(p))
  expect_true(odiffr:::.is_plot_input(p))
  expect_true(odiffr:::.is_plot_input(draw_points))
  expect_true(odiffr:::.is_plot_input(structure(list(), class = "recordedplot")))
  expect_false(odiffr:::.is_ggplot(list(a = 1)))
  expect_false(odiffr:::.is_plot_input("plot.png"))
  expect_false(odiffr:::.is_plot_input(1))
})

test_that(".callable_without_args() checks formals", {
  expect_true(odiffr:::.callable_without_args(function() NULL))
  expect_true(odiffr:::.callable_without_args(function(n = 5, ...) NULL))
  expect_false(odiffr:::.callable_without_args(function(x) NULL))
})

# .render_plot_to_png() -------------------------------------------------------

test_that(".render_plot_to_png() renders a plotting function", {
  skip_if_not_installed("png")
  path <- odiffr:::.render_plot_to_png(draw_points, width = 4, height = 3,
                                       res = 50)
  on.exit(unlink(path), add = TRUE)
  expect_true(file.exists(path))
  expect_equal(png_dim(path), c(150, 200))
})

test_that(".render_plot_to_png() renders a ggplot", {
  skip_if_not_installed("png")
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  path <- odiffr:::.render_plot_to_png(p, width = 300, height = 200,
                                       units = "px")
  on.exit(unlink(path), add = TRUE)
  expect_equal(png_dim(path), c(200, 300))
})

test_that("a function returning a ggplot is printed", {
  skip_if_not_installed("png")
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  path1 <- odiffr:::.render_plot_to_png(p, res = 30)
  path2 <- odiffr:::.render_plot_to_png(function() p, res = 30)
  on.exit(unlink(c(path1, path2)), add = TRUE)
  expect_identical(png::readPNG(path1), png::readPNG(path2))
})

test_that(".render_plot_to_png() replays a recorded plot", {
  skip_if_not_installed("png")
  grDevices::pdf(NULL)
  grDevices::dev.control("enable")
  draw_points()
  rec <- grDevices::recordPlot()
  grDevices::dev.off()

  path1 <- odiffr:::.render_plot_to_png(rec, res = 40)
  path2 <- odiffr:::.render_plot_to_png(draw_points, res = 40)
  on.exit(unlink(c(path1, path2)), add = TRUE)
  expect_identical(png::readPNG(path1), png::readPNG(path2))
})

test_that(".render_plot_to_png() closes its device and restores the previous one", {
  grDevices::pdf(NULL)
  prev <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(prev), add = TRUE)
  n_before <- length(grDevices::dev.list())

  path <- odiffr:::.render_plot_to_png(draw_points, res = 30)
  unlink(path)
  expect_equal(grDevices::dev.cur(), prev)
  expect_length(grDevices::dev.list(), n_before)

  # Also on error
  expect_error(
    odiffr:::.render_plot_to_png(function() stop("plot failed"), res = 30),
    "plot failed"
  )
  expect_equal(grDevices::dev.cur(), prev)
  expect_length(grDevices::dev.list(), n_before)
})

test_that(".render_plot_to_png() leaves no device open when none was open", {
  skip_if(!is.null(grDevices::dev.list()), "a graphics device is open")
  path <- odiffr:::.render_plot_to_png(draw_points, res = 30)
  unlink(path)
  expect_null(grDevices::dev.list())
})

test_that(".render_plot_to_png() falls back to grDevices::png() without ragg", {
  skip_if_not_installed("png")
  skip_if_not(capabilities("png") || capabilities("cairo"),
              "grDevices::png() not supported")
  testthat::local_mocked_bindings(.has_ragg = function() FALSE,
                                  .package = "odiffr")
  path <- odiffr:::.render_plot_to_png(draw_points, width = 2, height = 2,
                                       res = 50)
  on.exit(unlink(path), add = TRUE)
  expect_equal(png_dim(path), c(100, 100))
})

test_that(".render_plot_to_png() validates its input", {
  expect_error(odiffr:::.render_plot_to_png("x.png"), "must be a ggplot")
  expect_error(odiffr:::.render_plot_to_png(function(x) plot(x)),
               "callable with no arguments")
})

# .resolve_image_input() with plots ------------------------------------------

test_that(".resolve_image_input() renders plots to temp files", {
  res <- odiffr:::.resolve_image_input(draw_points, "img",
                                       plot_options = plot_options(res = 30))
  on.exit(unlink(res$path), add = TRUE)
  expect_true(file.exists(res$path))
  expect_true(res$temp)
  expect_equal(odiffr:::.input_kind(res), "plot")
  expect_equal(odiffr:::.input_label(res, res$path), "<plot>")

  odiffr:::.cleanup_temp_files(res)
  expect_false(file.exists(res$path))
})

test_that(".resolve_image_input() reports the input kind", {
  img <- create_test_image(10, 10, "red")
  on.exit(unlink(img), add = TRUE)
  res <- odiffr:::.resolve_image_input(img, "img")
  expect_equal(odiffr:::.input_kind(res), "file")
  expect_equal(odiffr:::.input_kind(list(path = "a", temp = TRUE)), "magick")

  skip_if_not_installed("magick")
  res <- odiffr:::.resolve_image_input(magick::image_read(img), "img")
  on.exit(unlink(res$path), add = TRUE)
  expect_equal(odiffr:::.input_kind(res), "magick")
  expect_equal(odiffr:::.input_label(res, res$path), "<magick-image>")
})

test_that(".resolve_image_input() wraps rendering errors", {
  expect_error(
    odiffr:::.resolve_image_input(function() stop("nope"), "img2"),
    "Failed to render img2 as a plot: nope"
  )
})

test_that("unsupported inputs list all accepted types", {
  expect_error(
    odiffr:::.resolve_image_input(42, "img1"),
    "file path.*magick-image.*ggplot.*function.*recorded plot"
  )
})

# compare_images() with plots ------------------------------------------------

test_that("compare_images() compares a plot against a baseline PNG", {
  skip_if_no_odiff()
  opts <- plot_options(width = 3, height = 3, res = 40)
  baseline <- odiffr:::.render_plot_with_options(draw_points, opts)
  on.exit(unlink(baseline), add = TRUE)

  res <- compare_images(baseline, draw_points, plot_options = opts)
  expect_true(res$match)
  expect_equal(res$img2, "<plot>")
  expect_false(res$img1 == "<plot>")

  res <- compare_images(baseline, function() graphics::plot(10:1),
                        plot_options = opts)
  expect_false(res$match)
  expect_equal(res$reason, "pixel-diff")

  # Different size -> layout difference
  res <- compare_images(baseline, draw_points, fail_on_layout = TRUE,
                        plot_options = plot_options(width = 4, height = 3,
                                                    res = 40))
  expect_equal(res$reason, "layout-diff")
})

test_that("compare_images() accepts ggplot objects for both inputs", {
  skip_if_no_odiff()
  skip_if_not_installed("ggplot2")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) + ggplot2::geom_point()
  res <- compare_images(p, p, plot_options = plot_options(res = 30))
  expect_true(res$match)
  expect_equal(c(res$img1, res$img2), c("<plot>", "<plot>"))
})

test_that("compare_images() removes rendered temp files", {
  skip_if_no_odiff()
  before <- list.files(tempdir(), pattern = "\\.png$")
  compare_images(draw_points, draw_points,
                 plot_options = plot_options(res = 30))
  after <- list.files(tempdir(), pattern = "\\.png$")
  expect_setequal(after, before)
})

test_that("plot_options keeps positional arguments backwards compatible", {
  expect_identical(
    names(formals(compare_images))[1:8],
    c("img1", "img2", "diff_output", "threshold", "antialiasing",
      "fail_on_layout", "ignore_regions", "plot_options")
  )
})

test_that("expect_images_match() accepts a plot as actual and a PNG as expected", {
  skip_if_no_odiff()
  withr::local_options(odiffr.save_diff = FALSE)
  opts <- plot_options(width = 3, height = 3, res = 40)
  baseline <- odiffr:::.render_plot_with_options(draw_points, opts)
  on.exit(unlink(baseline), add = TRUE)

  # plot_options flows through ... to compare_images()
  res <- expect_images_match(draw_points, baseline, plot_options = opts)
  expect_true(res$match)

  expect_failure(
    expect_images_match(function() graphics::plot(10:1), baseline,
                        plot_options = opts),
    "does not match"
  )
  expect_images_differ(function() graphics::plot(10:1), baseline,
                       plot_options = opts)
})

test_that("expect_images_match() names diff files for plot inputs", {
  skip_if_no_odiff()
  diff_dir <- withr::local_tempdir()
  withr::local_options(odiffr.diff_dir = diff_dir)
  opts <- plot_options(width = 3, height = 3, res = 40)
  baseline <- odiffr:::.render_plot_with_options(draw_points, opts)
  on.exit(unlink(baseline), add = TRUE)

  changed <- function() graphics::plot(10:1)
  expect_failure(expect_images_match(changed, baseline, plot_options = opts))
  diffs <- list.files(diff_dir)
  expect_length(diffs, 1)
  expect_match(diffs, "^changed_vs_")
})

test_that("batch comparison accepts plot inputs in list pairs", {
  skip_if_no_odiff()
  opts <- plot_options(width = 3, height = 3, res = 40)
  baseline <- odiffr:::.render_plot_with_options(draw_points, opts)
  on.exit(unlink(baseline), add = TRUE)

  pairs <- list(
    list(img1 = baseline, img2 = draw_points),
    list(img1 = baseline, img2 = function() graphics::plot(10:1)),
    list(img1 = baseline, img2 = function() stop("broken"))
  )
  res <- compare_images_batch(pairs, plot_options = opts)
  expect_equal(res$match, c(TRUE, FALSE, FALSE))
  expect_equal(res$reason, c("match", "pixel-diff", "error"))
  expect_equal(res$img2, rep("<plot>", 3))
  expect_match(res$error[3], "broken")
})

test_that("a function returning a lattice plot is printed", {
  skip_if_no_odiff()
  skip_if_not_installed("lattice")
  opts <- plot_options(width = 3, height = 3, res = 40)
  up <- function() lattice::xyplot(1:10 ~ 1:10)
  down <- function() lattice::xyplot(10:1 ~ 1:10)

  res <- compare_images(up, down, plot_options = opts)
  expect_false(res$match)
  expect_equal(res$reason, "pixel-diff")
  expect_true(compare_images(up, up, plot_options = opts)$match)

  # Not a blank page
  path <- odiffr:::.render_plot_with_options(up, opts)
  on.exit(unlink(path), add = TRUE)
  expect_gt(length(unique(as.vector(png::readPNG(path)[, , 1:3]))), 1)
})

test_that("a function returning a grob is drawn", {
  skip_if_not_installed("png")
  opts <- plot_options(width = 2, height = 2, res = 20)
  path <- odiffr:::.render_plot_with_options(
    function() grid::rectGrob(gp = grid::gpar(fill = "red", col = NA)), opts
  )
  on.exit(unlink(path), add = TRUE)
  img <- png::readPNG(path)
  centre <- img[20, 20, 1:3]
  expect_equal(centre, c(1, 0, 0))

  path2 <- odiffr:::.render_plot_with_options(
    function() grid::gList(grid::rectGrob(gp = grid::gpar(fill = "blue",
                                                         col = NA))),
    opts
  )
  on.exit(unlink(path2), add = TRUE)
  expect_equal(png::readPNG(path2)[20, 20, 1:3], c(0, 0, 1))
})
