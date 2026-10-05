local_null_device <- function(env = parent.frame()) {
  path <- tempfile(fileext = ".png")
  grDevices::png(path)
  dev <- grDevices::dev.cur()
  withr::defer({
    grDevices::dev.off(dev)
    unlink(path)
  }, envir = env)
  invisible(dev)
}

fake_result <- function(img1, img2, diff_output = NULL, match = FALSE,
                        reason = "pixel-diff", diff_percentage = 1.26) {
  structure(
    list(match = match, reason = reason, diff_count = 10L,
         diff_percentage = diff_percentage, img1 = img1, img2 = img2,
         diff_output = diff_output),
    class = c("odiff_result", "list")
  )
}

test_that("plot.odiff_result draws all panels without error", {
  skip_if_not_installed("png")
  img1 <- create_test_image(100, 60, "red")
  img2 <- create_modified_image(img1, "pixel")
  diff <- create_test_image(40, 20, "white")
  x <- fake_result(img1, img2, diff)

  local_null_device()
  op <- graphics::par("mfrow")
  for (w in c("all", "diff", "baseline", "current")) {
    expect_identical(plot(x, which = w), x)
  }
  expect_equal(graphics::par("mfrow"), op)
  expect_error(plot(x, which = "nope"))
})

test_that("plot.odiff_result handles missing diff image", {
  skip_if_not_installed("png")
  img1 <- create_test_image()
  x <- fake_result(img1, img1, NULL, match = TRUE, reason = "match",
                   diff_percentage = 0)
  local_null_device()
  expect_silent(plot(x))
  expect_silent(plot(x, which = "diff"))
  expect_equal(.no_diff_note(x), "No diff image\n(images match)")
  x2 <- fake_result(img1, img1, NULL)
  expect_match(.no_diff_note(x2), "not requested")
  x3 <- fake_result(img1, "does-not-exist.png", NULL)
  expect_silent(plot(x3))
})

test_that("diff panel title reflects the result", {
  expect_equal(.diff_panel_title(list(reason = "pixel-diff",
                                      diff_percentage = 1.2611)),
               "Diff (1.26%)")
  expect_equal(.diff_panel_title(list(reason = "layout-diff",
                                      diff_percentage = NA)),
               "Diff (layout differs)")
  expect_equal(.diff_panel_title(list(reason = "match",
                                      diff_percentage = NULL)), "Diff")
})

test_that("plot.odiff_result reads non-PNG images via magick", {
  skip_if_not_installed("magick")
  skip_if_not_installed("png")
  png_path <- create_test_image(30, 10)
  jpg <- tempfile(fileext = ".jpg")
  magick::image_write(magick::image_read(png_path), jpg, format = "jpeg")
  expect_false(.is_png_file(jpg))
  r <- .read_image_raster(jpg)
  expect_s3_class(r, "raster")
  expect_equal(dim(r), c(10L, 30L))
  local_null_device()
  expect_silent(plot(fake_result(jpg, png_path, NULL)))
})

test_that("grey + alpha PNGs are converted to raster", {
  a <- array(0.5, dim = c(4, 6, 2))
  r <- .array_to_raster(a)
  expect_s3_class(r, "raster")
  expect_equal(dim(r), c(4L, 6L))
})

test_that("plot works on a real odiff_run() result", {
  skip_if_no_odiff()
  img1 <- create_test_image()
  img2 <- create_modified_image(img1, "region")
  out <- tempfile(fileext = ".png")
  res <- odiff_run(img1, img2, diff_output = out)
  local_null_device()
  expect_silent(plot(res))
  expect_true(.view_file_exists(res$diff_output))
})

test_that("diff_image returns raster and magick images", {
  skip_if_not_installed("png")
  diff <- create_test_image(40, 20, "white")
  x <- fake_result("a.png", "b.png", diff)

  r <- diff_image(x, as = "raster")
  expect_s3_class(r, "raster")
  expect_equal(dim(r), c(20L, 40L))

  skip_if_not_installed("magick")
  m <- diff_image(x)
  expect_s3_class(m, "magick-image")
  info <- magick::image_info(m)
  expect_equal(c(info$width, info$height), c(40, 20))
})

test_that("diff_image accepts data frames, batch rows and paths", {
  skip_if_not_installed("png")
  diff <- create_test_image(10, 10)
  df <- data.frame(match = FALSE, reason = "pixel-diff", diff_output = diff,
                   stringsAsFactors = FALSE)
  expect_s3_class(diff_image(df, as = "raster"), "raster")
  expect_s3_class(diff_image(diff, as = "raster"), "raster")

  b <- make_batch(match = c(FALSE, TRUE), reason = c("pixel-diff", "match"),
                  diff_output = c(diff, NA))
  expect_s3_class(diff_image(b[1, ], as = "raster"), "raster")
  expect_error(diff_image(b, as = "raster"), "exactly one row")
  expect_error(diff_image(b[2, ], as = "raster"), "images match")
})

test_that("diff_image errors clearly without a diff image", {
  x <- fake_result("a.png", "b.png", NULL)
  expect_error(diff_image(x), "no diff_output was requested")
  x$match <- TRUE
  x$reason <- "match"
  expect_error(diff_image(x), "images match")
  expect_error(diff_image(fake_result("a", "b", "nope/diff.png")),
               "not found")
  expect_error(diff_image(data.frame(a = 1)), "diff_output")
  expect_error(diff_image(42), "must be an odiff_result")
  err <- data.frame(reason = "error", diff_output = NA_character_)
  expect_error(diff_image(err), "comparison failed")
})

test_that("diff_image works on compare_images() results", {
  skip_if_no_odiff()
  img1 <- create_test_image()
  img2 <- create_modified_image(img1, "region")
  res <- compare_images(img1, img2, diff_output = TRUE)
  r <- diff_image(res, as = "raster")
  expect_equal(dim(r), c(100L, 100L))
})
