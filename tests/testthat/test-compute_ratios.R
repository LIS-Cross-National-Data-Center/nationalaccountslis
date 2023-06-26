library(testthat)

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
