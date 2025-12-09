# Tests for zzz.R (.onAttach)

test_that(".onAttach shows message when odiff is not available", {
  testthat::local_mocked_bindings(
    odiff_available = function() FALSE,
    .package = "odiffr"
  )

  expect_message(
    odiffr:::.onAttach(NULL, "odiffr"),
    "odiff binary not found"
  )
})

test_that(".onAttach shows installation instructions when odiff is not available", {
  testthat::local_mocked_bindings(
    odiff_available = function() FALSE,
    .package = "odiffr"
  )

  expect_message(
    odiffr:::.onAttach(NULL, "odiffr"),
    "npm install -g odiff-bin"
  )
})

test_that(".onAttach is silent when odiff is available", {
  testthat::local_mocked_bindings(
    odiff_available = function() TRUE,
    .package = "odiffr"
  )

  expect_silent(odiffr:::.onAttach(NULL, "odiffr"))
})
