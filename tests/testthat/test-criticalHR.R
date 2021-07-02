testthat::context("Test criticalHR function")

testthat::test_that("criticalHR returns correct value", {
  testthat::expect_equal(criticalHR(500,0.5), 0.9562639)
})

testthat::test_that("criticalHR gives warning when n_events is negative", {
  testthat::expect_warning(criticalHR(-1,0.5))
})

