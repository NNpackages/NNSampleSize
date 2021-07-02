testthat::context("String length")

testthat::test_that("str_length is number of characters", {
  testthat::expect_equal(stringr::str_length("a"), 1)
  testthat::expect_equal(stringr::str_length("ab"), 2)
  testthat::expect_equal(stringr::str_length("abc"), 3)
})

testthat::test_that("str_length of factor is length of level", {
  testthat::expect_equal(stringr::str_length(factor("a")), 1)
  testthat::expect_equal(stringr::str_length(factor("ab")), 2)
  testthat::expect_equal(stringr::str_length(factor("abc")), 3)
})

testthat::test_that("str_length of missing is missing", {
  testthat::expect_equal(stringr::str_length(NA), NA_integer_)
  testthat::expect_equal(stringr::str_length(c(NA, 1)), c(NA, 1))
  testthat::expect_equal(stringr::str_length("NA"), 2)
})
