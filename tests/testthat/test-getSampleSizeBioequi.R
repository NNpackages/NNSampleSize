testthat::context("Test getSampleSizeBioequi function")

testthat::test_that("getSampleSizeBioequi works as intended",{
  testthat::expect_equal(getSampleSizeBioequi(targetpower=0.8, theta0_AUC=0.95, theta0_CMAX=0.95 , CV_AUC=0.2, CV_CMAX=0.25, studyDesign="2x2")$`Target power`, 0.8)
})

testthat::test_that("getSampleSizeBioequi fails when no studyDesign is given",{
  testthat::expect_error(getSampleSizeBioequi(targetpower=0.8, theta0_AUC=0.95, theta0_CMAX=0.95 , CV_AUC=0.2, CV_CMAX=0.25))
})

testthat::test_that("getSampleSizeBioequi fails when targetpower is larger than or equal to 1",{
  testthat::expect_error(testthat::expect_warning(getSampleSizeBioequi(targetpower=2, theta0_AUC=0.95, theta0_CMAX=0.95 , CV_AUC=0.2, CV_CMAX=0.25, studyDesign="2x2")))
})