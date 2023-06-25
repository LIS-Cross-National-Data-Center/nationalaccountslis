library(testthat)
library(nationalaccountslis)

weights <- c("hpopwgt")

lis_dashboard_formulas <- list(D11R = "hi11 + hi13",
    `B3GR+D41R+D42R+D45R` = "hi12 + hicapital",
    D62R = "hpublic",
    `D5P+D61P-D12R` = "hxitsc + hxptax",
    `D61P-D12R` = "hxscont",
    `dhi_B6G-K1R` = "hi11 + hi13 + hi12 + hicapital + hpublic - hxitsc - hxptax")

# then start writing your tests
test_that("compute_estimates_lis_microdata returns a tibble", {
  # write to temporary files
  temp_h_file <- fs::path(paste0(tempdir(), "/it14ih.dta"))
  temp_p_file <- fs::path(paste0(tempdir(), "/it14ip.dta"))


  haven::write_dta(it14ih, temp_h_file)
  haven::write_dta(it14ip, temp_p_file)

  result <- compute_estimates_lis_microdata("it14i", tempdir(), weights, lis_dashboard_formulas)

  # remove temporary files
  file.remove(temp_h_file, temp_p_file)
  
  expect_is(result, "tbl_df")
})


test_that("compute_estimates_lis_microdata returns a dataframe with grossed up estimates", {
  temp_h_file <- fs::path(paste0(tempdir(), "/it14ih.dta"))
  temp_p_file <- fs::path(paste0(tempdir(), "/it14ip.dta"))

  haven::write_dta(it14ih, temp_h_file)
  haven::write_dta(it14ip, temp_p_file)

  result <- compute_estimates_lis_microdata("it14i", tempdir(), weights, lis_dashboard_formulas)
  
  file.remove(temp_h_file, temp_p_file)
  
  # assumes result should have a column named "ccyyd" and columns named after formulas
  expect_setequal(names(result), c("ccyyd", names(lis_dashboard_formulas)))  
})


test_that("compute_estimates_lis_microdata returns a dataframe with the right number of rows and correct `ccyyd` column", {
  temp_ith_file <- fs::path(paste0(tempdir(), "/it14ih.dta"))
  temp_itp_file <- fs::path(paste0(tempdir(), "/it14ip.dta"))
  temp_ush_file <- fs::path(paste0(tempdir(), "/us16ih.dta"))
  temp_usp_file <- fs::path(paste0(tempdir(), "/us16ip.dta"))
  haven::write_dta(it14ih, temp_ith_file)
  haven::write_dta(it14ip, temp_itp_file)
  haven::write_dta(us16ih, temp_ush_file)
  haven::write_dta(us16ip, temp_usp_file)
    
  result <- compute_estimates_lis_microdata(c("it14i", "us16i"), tempdir(), weights, lis_dashboard_formulas)
  
  file.remove(temp_ith_file, temp_itp_file, temp_ush_file, temp_usp_file)
  
  # assumes result should have a column named "ccyyd" and columns named after formulas
  expect_setequal(nrow(result), 2) 
  expect_setequal(result[["ccyyd"]], c("it14i", "us16i"))
})