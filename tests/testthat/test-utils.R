library(testthat)

test_that("convert_iso2_to_iso3 works correctly", {
    expect_equal(convert_iso2_to_iso3(c("US", "DE", "JP")),
        c("USA", "DEU", "JPN"))
    expect_equal(convert_iso2_to_iso3(c("au", "ca", "cn")),
        c("AUS", "CAN", "CHN"))
    expect_equal(convert_iso2_to_iso3("BR"), "BRA")
    expect_equal(convert_iso2_to_iso3("UK"), "GBR")
    expect_equal(convert_iso2_to_iso3("GB"), "GBR")
    expect_equal(convert_iso2_to_iso3(c("ES", "FR", "IT")),
        c("ESP", "FRA", "ITA"))
})

test_that("convert_iso2_to_iso3 throws error if country not in map_iso2_iso3", {
    expect_error(convert_iso2_to_iso3("ZZ"), "Not all countries are in map_iso2_iso3")
    expect_error(convert_iso2_to_iso3(c("ZZ", "US")), "Not all countries are in map_iso2_iso3")
})

test_that("compute_formulas correctly computes formulas", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  formulas <- list(Z = "a + b", Y = "c+d", X = "a - b", W = "c * d")
  
  result <- compute_formulas(df, formulas)
  
  expected_df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12, Z = c(5, 7, 9), Y = c(17, 19, 21), 
                            X = c(-3, -3, -3), W = c(70, 88, 108))
  
  expect_equal(result, expected_df)
})


test_that("compute_formulas gives an informative error when a formula refers to non-existent columns", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
  formulas <- list(Z = "a + e", Y = "c+d")  # 'e' does not exist in df
  
  expect_error(compute_formulas(df, formulas), "Variables e not found in the data frame.")
})
