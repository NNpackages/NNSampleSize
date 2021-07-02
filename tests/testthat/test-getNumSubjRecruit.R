testthat::context("Test getNumSubjRecruit function")

testthat::test_that("getNumSubjRecruit returns correct value", {
  testthat::expect_equal(getNumSubjRecruit(0.3,0.5,0.6,456)$numSubjRecruit, 1839)
})
