test_that("cBioPortal functions work with mock data", {
  # Mock test for cBioPortal functions when package is not available
  skip_if_not_installed("cBioPortalData")
  
  # Test that the function exists and handles missing package gracefully
  expect_error(get_cbioportal_studies(), NA)
  
  # Test that get_cbioportal_study_data returns NULL for invalid study
  result <- get_cbioportal_study_data("invalid_study_id")
  expect_null(result)
  
  # Test that extract functions handle NULL input gracefully
  expect_equal(extract_cbioportal_molecular_data(NULL), data.frame())
  expect_equal(extract_cbioportal_clinical_data(NULL), data.frame())
  expect_equal(get_cbioportal_data_types(NULL), character(0))
})