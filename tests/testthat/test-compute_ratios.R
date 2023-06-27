library(testthat)
library(dplyr)

# define the test
test_that("Test compute_coverage_ratios function", {

  # create some example inputs
  df_estimates_microdata <- tibble::tibble(ccyyd = c("it14i", "us16i"), D11R = c(47859.8641742516,
109354.76425589), `B3GR+D41R+D42R+D45R` = c(25368.5130559221,
15289.57654062), D62R = c(30228.6992965287, 18846.6431916889),
    `D5P+D61P-D12R` = c(27571.2088770702, 34287.10187735), `D61P-D12R` = c(0,
    8485.05627313), `B6GR-K1R` = c(75885.8676496322, 109203.882110849
    ))

  na_table <- data.frame(country = c("ITA", "USA"), year = c("2014", "2016"
), ccyy = c("it14", "us16"), D41R = c(43531.3, 1603177.925),
    B6GR = c(1098216.5, 14116105.416), D5P = c(198927.6, 1958224.62
    ), K1R = c(87010.3, 684905.336), D42R = c(115779.3, 1045722.643
    ), D4R = c(184786.6, 2660201.861), D45R = c(992.4, 0), D11R = c(469553.1,
    8091238.583), D12R = c(172384.5, 1874869.567), B3GR = c(229011.3,
    1833512.454), D62R = c(354996.4, 4011205.8), D61P = c(244908.5, 
    2532760.6))

  # call the function
  df_result <- compute_coverage_ratios(df_estimates_microdata, na_table)

  # check if the output is a data.frame
  expect_is(df_result, "data.frame")

  # check if the columns of the resulting dataframe match the expected ones
  expected_cols <- c("ccyyd", "variable", "value_mi", "value_na", "ratio")
  expect_equal(colnames(df_result), expected_cols)

})



test_that("check_col_names_are_equal returns no error for identical dataframes", {
  df1 <- data.frame(ccyy = 1:5, ccyyd = 1:5, a = 1:5, b = 1:5)
  df2 <- data.frame(ccyy = 6:10, ccyyd = 6:10, a = 6:10, b = 6:10)

  expect_silent(check_col_names_are_equal(df1, df2))
})

test_that("check_col_names_are_equal returns an error for dataframes with different column names", {
  df1 <- data.frame(ccyy = 1:5, ccyyd = 1:5, a = 1:5, b = 1:5, c = 6:10)
  df2 <- data.frame(ccyy = 6:10, ccyyd = 6:10, a = 6:10, c = 6:10)

  expect_error(check_col_names_are_equal(df1, df2), "Variables in 'df_estimates_microdata' need to be present in 'na_table'.")
})

test_that("check_col_names_are_equal returns an error if na_table has extra columns", {
  df1 <- data.frame(ccyy = 1:5, ccyyd = 1:5, a = 1:5, b = 1:5)
  df2 <- data.frame(ccyy = 6:10, ccyyd = 6:10, a = 6:10, b = 6:10, c = 6:10)

  expect_error(check_col_names_are_equal(df1, df2), "Variables in 'na_table' need to be present in 'df_estimates_microdata'.")
})
