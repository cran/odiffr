# Tests for use_odiffr_ci()

template_lines <- function() {
  readLines(system.file("templates", "odiffr-ci.yaml", package = "odiffr"))
}

test_that("the workflow template is installed", {
  path <- system.file("templates", "odiffr-ci.yaml", package = "odiffr")
  expect_true(nzchar(path))
  txt <- paste(readLines(path), collapse = "\n")
  expect_match(txt, "odiffr::install_odiff()", fixed = TRUE)
  expect_match(txt, "odiffr::snapshot_report(format = \"markdown\")",
               fixed = TRUE)
  expect_match(txt, "actions/upload-artifact@v4", fixed = TRUE)
})

test_that("use_odiffr_ci() writes the workflow and creates directories", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  expect_message(res <- withVisible(use_odiffr_ci()), "Next steps")
  expect_false(res$visible)
  expect_equal(res$value, ".github/workflows/odiffr.yaml")
  expect_true(dir.exists(file.path(dir, ".github", "workflows")))
  expect_identical(readLines(res$value), template_lines())
})

test_that("use_odiffr_ci() accepts a custom path", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "a", "b", "visual.yml")
  expect_message(out <- use_odiffr_ci(path), path, fixed = TRUE)
  expect_equal(out, path)
  expect_identical(readLines(path), template_lines())
})

test_that("use_odiffr_ci() refuses to overwrite unless asked", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "odiffr.yaml")
  writeLines("custom workflow", path)

  expect_error(use_odiffr_ci(path), "already exists")
  expect_error(use_odiffr_ci(path), "overwrite = TRUE", fixed = TRUE)
  expect_identical(readLines(path), "custom workflow")

  expect_message(use_odiffr_ci(path, overwrite = TRUE), "Wrote")
  expect_identical(readLines(path), template_lines())
})

test_that("use_odiffr_ci() validates its arguments", {
  dir <- withr::local_tempdir()
  expect_error(use_odiffr_ci(c("a", "b")), "single file path")
  expect_error(use_odiffr_ci(NA_character_), "single file path")
  expect_error(use_odiffr_ci(file.path(dir, "x.yaml"), overwrite = NA),
               "overwrite")
  expect_error(use_odiffr_ci(file.path(dir, "x.yaml"), open = "yes"),
               "open")
  expect_error(use_odiffr_ci(dir), "is a directory")
})

test_that("use_odiffr_ci(open = TRUE) opens the file only interactively", {
  dir <- withr::local_tempdir()
  opened <- character()
  testthat::local_mocked_bindings(
    file.edit = function(...) {
      opened <<- c(opened, ...)
      invisible(NULL)
    },
    .package = "utils"
  )

  testthat::local_mocked_bindings(.is_interactive = function() FALSE,
                                  .package = "odiffr")
  suppressMessages(use_odiffr_ci(file.path(dir, "a.yaml"), open = TRUE))
  expect_length(opened, 0)

  testthat::local_mocked_bindings(.is_interactive = function() TRUE,
                                  .package = "odiffr")
  suppressMessages(use_odiffr_ci(file.path(dir, "b.yaml"), open = TRUE))
  expect_equal(opened, file.path(dir, "b.yaml"))
})
