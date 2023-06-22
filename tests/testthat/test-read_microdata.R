library(testthat)
library(nationalaccountslis)

# read_lis_microdata
# write tests for `read_lis_microdata`
# `read_lis_microdata` returns a dataframe with the needed_vars and weights
# `read_lis_microdata` returns a dataframe with the needed_vars and formulas

# `read_lis_microdata` returns a dataframe with the needed_vars and weights and formulas and relation
# `read_lis_microdata` returns a dataframe with the needed_vars and weights and formulas and relation and others

# read_lis_microdata
test_that("read_lis_microdata returns a tibble", {
  # write to temporary files
  temp_h_file <- fs::path(paste0(tempdir(), "/it14ih.dta"))
  temp_p_file <- fs::path(paste0(tempdir(), "/it14ip.dta"))
  temp_wh_file <- fs::path(paste0(tempdir(), "/it14wh.dta"))
  temp_wp_file <- fs::path(paste0(tempdir(), "/it14wp.dta"))

  haven::write_dta(it14ih, temp_h_file)
  haven::write_dta(it14ip, temp_p_file)
  haven::write_dta(it14wh, temp_wh_file)
  haven::write_dta(it14wp, temp_wp_file)

  weights <- c("hpopwgt")
 lis_dashboard_formulas <- list(D11R = "hi11 + hi13",
    `B3GR+D41R+D42R+D45R` = "hi12 + hicapital",
    D62R = "hpublic",
    `D5P+D61P-D12R` = "hxitsc + hxptax",
    `D61P-D12R` = "hxscont",
    `dhi_B6G-K1R` = "hi11 + hi13 + hi12 + hicapital + hpublic - hxitsc - hxptax")
  
  result <- read_lis_microdata("it14i", tempdir(), weights, lis_dashboard_formulas)
  
  # remove temporary files
  file.remove(temp_h_file, temp_p_file, temp_wh_file, temp_wp_file)
  
  expect_is(result, "tbl_df")
})

test_that("read_lis_microdata returns a dataframe with the weights and formula variables", {
  # write to temporary files
  temp_h_file <- fs::path(paste0(tempdir(), "/it14ih.dta"))
  temp_p_file <- fs::path(paste0(tempdir(), "/it14ip.dta"))
  temp_wh_file <- fs::path(paste0(tempdir(), "/it14wh.dta"))
  temp_wp_file <- fs::path(paste0(tempdir(), "/it14wp.dta"))

  haven::write_dta(it14ih, temp_h_file)
  haven::write_dta(it14ip, temp_p_file)
  haven::write_dta(it14wh, temp_wh_file)
  haven::write_dta(it14wp, temp_wp_file)

  weights <- c("ppopwgt")
  formulas <- list(a = "pi11 + pitotal",
    b = "pilabour")
  
  result <- read_lis_microdata("it14i", tempdir(), weights, formulas)
  
  # remove temporary files
  file.remove(temp_h_file, temp_p_file, temp_wh_file, temp_wp_file)
  
  expect_setequal(names(result), c("hid", "ppopwgt", "pi11", "pitotal", "pilabour", "relation", "pid"))
})



# read_lis_dataset
test_that("read_lis_dataset returns a tibble", {
  # write to temporary files
  temp_h_file <- fs::path(paste0(tempdir(), "/it14ih.dta"))
  temp_p_file <- fs::path(paste0(tempdir(), "/it14ip.dta"))

  haven::write_dta(it14ih, temp_h_file)
  haven::write_dta(it14ip, temp_p_file)

  needed_vars <- c("hid", "hi11", "hi13")
  
  result <- read_lis_dataset("it14i", tempdir(), needed_vars)
  
  # remove temporary files
  file.remove(temp_h_file, temp_p_file)
  
  expect_is(result, "tbl_df")
})


test_that("read_lis_dataset returns a dataframe with the needed_vars", {
  temp_h_file <- fs::path(paste0(tempdir(), "/it14ih.dta"))
  temp_p_file <- fs::path(paste0(tempdir(), "/it14ip.dta"))

  haven::write_dta(it14ih, temp_h_file)
  haven::write_dta(it14ip, temp_p_file)

  needed_vars <- c("hid", "pi11", "pitotal")
  
  result <- read_lis_dataset("it14i", tempdir(), needed_vars)
  
  file.remove(temp_h_file, temp_p_file)
  
  expect_setequal(names(result), c("hid", needed_vars))  # assumes 'hid' is always present
})


# read_lws_dataset
test_that("read_lws_dataset returns a tibble", {
  # write to temporary files
  temp_h_file <- fs::path(paste0(tempdir(), "/it14wh.dta"))
  temp_p_file <- fs::path(paste0(tempdir(), "/it14wp.dta"))

  haven::write_dta(it14wh, temp_h_file)
  haven::write_dta(it14wp, temp_p_file)

  needed_vars <- c("hid", "pi11", "pitotal")
  
  result <- read_lws_dataset("it14w", tempdir(), needed_vars)
  
  # remove temporary files
  file.remove(temp_h_file, temp_p_file)
  
  expect_is(result, "tbl_df")
})


test_that("read_lws_dataset returns a dataframe with the needed_vars", {
  temp_h_file <- fs::path(paste0(tempdir(), "/it14wh.dta"))
  temp_p_file <- fs::path(paste0(tempdir(), "/it14wp.dta"))
  haven::write_dta(it14wh, temp_h_file)
  haven::write_dta(it14wp, temp_p_file)

  needed_vars <- c("hid", "inum", "pi11", "pitotal")
 
  result <- read_lws_dataset("it14w", tempdir(), needed_vars)
  
  file.remove(temp_h_file, temp_p_file)
  
  expect_setequal(names(result), c("hid", needed_vars))  # assumes 'hid' is always present
})

