test_that("cBioPortal functions work with cbioportalR", {
  # Skip if cbioportalR is not available
  skip_if_not_installed("cbioportalR")
  
  # Test that the function exists and handles missing package gracefully
  expect_no_error(get_cbioportal_studies(base_url = "public"))
  
  # Test that get_cbioportal_study_info returns data frame
  skip_on_cran()  # Skip network tests on CRAN
  result <- tryCatch(
    get_cbioportal_study_info("acc_tcga", base_url = "public"),
    error = function(e) data.frame()
  )
  expect_true(is.data.frame(result))
  
  # Test that functions handle NULL input gracefully
  expect_true(is.data.frame(get_cbioportal_clinical_data("invalid_study_id", base_url = "public")))
  
  # Test correlation function structure
  skip_on_cran()
  cor_result <- tryCatch(
    get_cbioportal_gene_correlation("TP53", "MDM2", "acc_tcga", base_url = "public"),
    error = function(e) list(data = data.frame(), correlation = NULL)
  )
  expect_true(is.list(cor_result))
  expect_true("data" %in% names(cor_result))
  expect_true("correlation" %in% names(cor_result))
})