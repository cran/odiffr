# Tests for install_odiff() and the interactive offer to install odiff

# Make odiff unfindable: no option, nothing on the PATH, an empty cache.
# Also resets the "already asked" flag for the test.
local_no_odiff <- function(env = parent.frame()) {
  cache <- withr::local_tempdir(.local_envir = env)
  old_asked <- odiffr:::.odiffr_env$install_asked
  withr::defer(assign("install_asked", old_asked, envir = odiffr:::.odiffr_env),
               envir = env)
  assign("install_asked", NULL, envir = odiffr:::.odiffr_env)
  withr::local_options(odiffr.path = NULL, odiffr.ask_install = NULL,
                       .local_envir = env)
  testthat::local_mocked_bindings(
    Sys.which = function(names) stats::setNames(rep("", length(names)), names),
    .package = "base",
    .env = env
  )
  testthat::local_mocked_bindings(
    odiffr_cache_path = function() cache,
    .package = "odiffr",
    .env = env
  )
  cache
}

# Simulate an interactive session outside testthat, knitr and R CMD check
local_offer_context <- function(env = parent.frame()) {
  withr::local_envvar(TESTTHAT = "false", `_R_CHECK_PACKAGE_NAME_` = "",
                      .local_envir = env)
  withr::local_options(knitr.in.progress = NULL, odiffr.ask_install = NULL,
                       .local_envir = env)
  testthat::local_mocked_bindings(
    .is_interactive = function() TRUE,
    .package = "odiffr",
    .env = env
  )
}

# Fake binary download into the (mocked) cache
fake_download <- function(url, destfile, mode, quiet) {
  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  writeLines("fake binary", destfile)
  0L
}

# ---------------------------------------------------------------------------
# .should_offer_install()
# ---------------------------------------------------------------------------

test_that(".should_offer_install() is FALSE when not interactive", {
  local_no_odiff()
  local_offer_context()
  testthat::local_mocked_bindings(.is_interactive = function() FALSE,
                                  .package = "odiffr")
  expect_false(odiffr:::.should_offer_install())
})

test_that(".should_offer_install() is FALSE under testthat", {
  local_no_odiff()
  testthat::local_mocked_bindings(.is_interactive = function() TRUE,
                                  .package = "odiffr")
  withr::local_envvar(TESTTHAT = "true", `_R_CHECK_PACKAGE_NAME_` = "")
  withr::local_options(knitr.in.progress = NULL)
  expect_false(odiffr:::.should_offer_install())
})

test_that(".should_offer_install() is FALSE while knitting", {
  local_no_odiff()
  local_offer_context()
  withr::local_options(knitr.in.progress = TRUE)
  expect_false(odiffr:::.should_offer_install())
})

test_that(".should_offer_install() is FALSE during R CMD check", {
  local_no_odiff()
  local_offer_context()
  withr::local_envvar(`_R_CHECK_PACKAGE_NAME_` = "somepkg")
  expect_false(odiffr:::.should_offer_install())
})

test_that(".should_offer_install() is FALSE when disabled by option", {
  local_no_odiff()
  local_offer_context()
  withr::local_options(odiffr.ask_install = FALSE)
  expect_false(odiffr:::.should_offer_install())
})

test_that(".should_offer_install() is FALSE when already asked", {
  local_no_odiff()
  local_offer_context()
  expect_true(odiffr:::.should_offer_install())
  assign("install_asked", TRUE, envir = odiffr:::.odiffr_env)
  expect_false(odiffr:::.should_offer_install())
})

test_that(".should_offer_install() is FALSE in the real test session", {
  # Whatever the session, tests run with TESTTHAT=true
  expect_false(odiffr:::.should_offer_install())
})

# ---------------------------------------------------------------------------
# find_odiff() offer
# ---------------------------------------------------------------------------

test_that("find_odiff() installs odiff when the offer is accepted", {
  cache <- local_no_odiff()
  installed <- file.path(cache, "odiff")
  writeLines("fake binary", installed)
  asked <- 0L
  installs <- 0L
  testthat::local_mocked_bindings(
    .should_offer_install = function() TRUE,
    .offer_install = function() {
      asked <<- asked + 1L
      TRUE
    },
    install_odiff = function(version = "latest", force = FALSE) {
      installs <<- installs + 1L
      invisible(installed)
    },
    .package = "odiffr"
  )

  expect_equal(find_odiff(), normalizePath(installed))
  expect_equal(asked, 1L)
  expect_equal(installs, 1L)
  expect_true(odiffr:::.odiffr_env$install_asked)
})

test_that("declining the offer errors and is not asked again", {
  local_no_odiff()
  local_offer_context()
  asked <- 0L
  testthat::local_mocked_bindings(
    .offer_install = function() {
      asked <<- asked + 1L
      FALSE
    },
    install_odiff = function(...) stop("install_odiff() must not be called"),
    .package = "odiffr"
  )

  expect_error(find_odiff(), "install_odiff\\(\\)")
  expect_equal(asked, 1L)
  expect_error(find_odiff(), "odiff binary not found")
  expect_equal(asked, 1L)
})

test_that("an NA answer (cancel) is treated as no", {
  local_no_odiff()
  local_offer_context()
  testthat::local_mocked_bindings(
    .offer_install = function() NA,
    install_odiff = function(...) stop("install_odiff() must not be called"),
    .package = "odiffr"
  )
  expect_error(find_odiff(), "odiff binary not found")
  expect_true(odiffr:::.odiffr_env$install_asked)
})

test_that("find_odiff() does not offer when the decision helper says no", {
  local_no_odiff()
  testthat::local_mocked_bindings(
    .should_offer_install = function() FALSE,
    .offer_install = function() stop("must not prompt"),
    .package = "odiffr"
  )
  expect_error(find_odiff(), "install_odiff\\(\\)")
})

test_that("the not-found error lists install_odiff() first", {
  local_no_odiff()
  msg <- tryCatch(find_odiff(), error = conditionMessage)
  lines <- strsplit(msg, "\n", fixed = TRUE)[[1]]
  expect_match(lines[2], "install_odiff()", fixed = TRUE)
  expect_match(msg, "npm install -g odiff-bin", fixed = TRUE)
  expect_match(msg, "github.com/dmtrKovalenko/odiff/releases", fixed = TRUE)
})

test_that("odiff_available() and friends never prompt", {
  local_no_odiff()
  local_offer_context()
  testthat::local_mocked_bindings(
    .offer_install = function() stop("must not prompt"),
    install_odiff = function(...) stop("must not install"),
    .package = "odiffr"
  )
  expect_false(odiff_available())
  expect_true(is.na(odiff_version()))
  info <- odiff_info()
  expect_true(is.na(info$path))
  expect_message(odiffr:::.onAttach(NULL, "odiffr"), "odiff binary not found")
  # The offer is still available to code that needs the binary
  expect_null(odiffr:::.odiffr_env$install_asked)
})

test_that(".offer_install() asks with utils::askYesNo()", {
  question <- NULL
  testthat::local_mocked_bindings(
    askYesNo = function(msg, ...) {
      question <<- msg
      TRUE
    },
    .package = "utils"
  )
  expect_true(odiffr:::.offer_install())
  expect_match(question, "odiff is required but was not found", fixed = TRUE)
  expect_match(question, "the latest odiff release", fixed = TRUE)
  expect_match(question, odiffr_cache_path(), fixed = TRUE)
})

# ---------------------------------------------------------------------------
# install_odiff()
# ---------------------------------------------------------------------------

test_that("install_odiff() delegates to odiffr_update() and clears caches", {
  skip_on_cran()
  cache <- local_no_odiff()
  old <- list(version = odiffr:::.odiffr_env$version_cache,
              npm = odiffr:::.odiffr_env$npm_cache)
  withr::defer({
    assign("version_cache", old$version, envir = odiffr:::.odiffr_env)
    assign("npm_cache", old$npm, envir = odiffr:::.odiffr_env)
  })
  assign("version_cache", list(key = "stale", version = "0.0.1"),
         envir = odiffr:::.odiffr_env)
  assign("npm_cache", list(stale = list()), envir = odiffr:::.odiffr_env)

  urls <- character()
  testthat::local_mocked_bindings(
    .get_latest_version = function() "v4.5.0",
    download_file_internal = function(url, destfile, mode, quiet) {
      urls <<- c(urls, url)
      fake_download(url, destfile, mode, quiet)
    },
    .package = "odiffr"
  )

  msgs <- testthat::capture_messages(path <- install_odiff())
  expect_length(urls, 1)
  expect_match(urls, "/v4.5.0/", fixed = TRUE)
  expect_true(file.exists(path))
  expect_true(startsWith(path, normalizePath(cache)))
  expect_match(paste(msgs, collapse = ""),
               "odiff v4.5.0 is installed at: ", fixed = TRUE)
  expect_match(paste(msgs, collapse = ""), path, fixed = TRUE)
  expect_null(odiffr:::.odiffr_env$version_cache)
  expect_null(odiffr:::.odiffr_env$npm_cache)

  # find_odiff() picks up the new binary immediately
  expect_equal(find_odiff(), path)
})

test_that("install_odiff() returns its path invisibly", {
  skip_on_cran()
  local_no_odiff()
  testthat::local_mocked_bindings(
    download_file_internal = fake_download,
    .package = "odiffr"
  )
  res <- withVisible(suppressMessages(install_odiff(version = "4.1.2")))
  expect_false(res$visible)
  expect_true(file.exists(res$value))
})

test_that("install_odiff() passes version and force to odiffr_update()", {
  args <- NULL
  dir <- withr::local_tempdir()
  bin <- file.path(dir, "odiff")
  writeLines("fake binary", bin)
  testthat::local_mocked_bindings(
    odiffr_update = function(version, force) {
      args <<- list(version = version, force = force)
      invisible(bin)
    },
    .package = "odiffr"
  )
  withr::local_options(odiffr.path = bin)
  expect_message(install_odiff(version = "v4.1.2", force = TRUE),
                 "odiff is installed at:")
  expect_equal(args, list(version = "v4.1.2", force = TRUE))
})

test_that("install_odiff() notes when another binary takes precedence", {
  dir <- withr::local_tempdir()
  bin <- file.path(dir, "odiff")
  other <- file.path(dir, "other-odiff")
  writeLines("fake binary", bin)
  writeLines("fake binary", other)
  testthat::local_mocked_bindings(
    odiffr_update = function(version, force) invisible(bin),
    .package = "odiffr"
  )
  withr::local_options(odiffr.path = other)
  expect_message(install_odiff(), "takes precedence")
})
