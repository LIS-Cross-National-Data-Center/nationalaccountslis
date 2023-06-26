library(testthat)

# Given dataframe
na_data <- data.frame(
  
  country = rep("ITA", 36), 
  variable = rep(c("NFD11R", "NFD41R", "NFD61P", "NFB6GR", "NFD5P", "NFB3GR", "NFK1R", "NFD42R", "NFD62R", "NFD12R", "NFD4R", "NFD45R"), 3), 
  sector = c(rep("S1", 12), rep("S14", 12), rep("S14_S15", 12)), 
  year = rep("2014", 36), 
  value = as.character(c(
    469553.1, 144088.3, 244908.5, 1611124.3, 235927, 229011.3, 296165.7, 147999, 354996.4, 172384.5, 334748.4, 5362.9, 
    469553.1, 43531.3, 244908.5, 1098216.5, 198927.6, 229011.3, 87010.3, 115779.3, 354996.4, 172384.5, 184786.6, 992.4, 
    469553.1, 44787.5, 244908.5, 1106926.1, 198974.1, 229011.3, 87516.9, 115779.3, 354996.4, 172384.5, 186046.2, 992.4))

)

formulas_na <- nationalaccountslis::lis_dashboard_na_formulas

test_that("produce_oecd_na_table function returns expected output", {
  # Test that the function works with normal input
  result <- produce_oecd_na_table(na_data, formulas_na)
  expect_is(result, "data.frame")

  # Test that the result has the expected columns
  expected_cols <- c("country", "year", "ccyy", names(formulas_na))
  expect_equal(all(expected_cols %in% names(result)), TRUE)
})

test_that("produce_oecd_na_table function handles special variable cases", {
  result <- produce_oecd_na_table(na_data, formulas_na)

  # Test that '_S14', '_S14_S15' and '_S1' variables are dropped from the result
  unexpected_cols <- c("D11R_S14", "D12R_S14", "B3GR_S14", "D62R_S14", "D61P_S14",
                       "D11R_S14_S15", "D12R_S14_S15", "B3GR_S14_S15", "D62R_S14_S15", "D61P_S14_S15",
                       "D11R_S1", "D12R_S1", "B3GR_S1", "D62R_S1", "D61P_S1")
  expect_false(any(unexpected_cols %in% names(result)))
})
