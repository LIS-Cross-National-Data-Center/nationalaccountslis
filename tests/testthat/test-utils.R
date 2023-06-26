library(testthat)


# convert_iso2_to_iso3

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


# Testing convert_iso3_to_iso2 function
test_that("Testing convert_iso3_to_iso2 function", {
  expect_equal(convert_iso3_to_iso2(c("USA", "DEU", "JPN")), c("US", "DE", "JP"))
  expect_equal(convert_iso3_to_iso2(c("FRA", "BRA", "IND")), c("FR", "BR", "IN"))
  expect_equal(convert_iso3_to_iso2(c("AUT", "BEL", "CAN")), c("AT", "BE", "CA"))
  
  # Testing invalid input
  expect_error(convert_iso3_to_iso2("ZZZ"))
})


# compute_formulas

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


# parse_all_vars_from_formulas

lis_dashboard_formulas <- list(D11R = "hi11 + hi13",
    `B3GR+D41R+D42R+D45R` = "hi12 + hicapital",
    D62R = "hpublic",
    `D5P+D61P-D12R` = "hxitsc + hxptax",
    `D61P-D12R` = "hxscont",
    `dhi_B6G-K1R` = "hi11 + hi13 + hi12 + hicapital + hpublic - hxitsc - hxptax")

test_that("parse_all_vars_from_formulas returns correct variables", {

  result <- parse_all_vars_from_formulas(lis_dashboard_formulas)
  
  expected <- c("hi11", "hi13", "hi12", "hicapital", "hpublic", "hxitsc", "hxptax", "hxscont")
  
  expect_equal(sort(result), sort(expected))
})

test_that("parse_all_vars_from_formulas returns an empty list for no input", {
  result <- parse_all_vars_from_formulas(list())
  expect_equal(result, character())
})

test_that("parse_all_vars_from_formulas handles empty formulas", {
  formulas <- list(D11R = "",
                             D62R = "hpublic",
                             `D5P+D61P-D12R` = "")
  
  result <- parse_all_vars_from_formulas(formulas)
  
  expect_equal(result, "hpublic")
})


# apply_na_processing_formulas

# Testing function
test_apply_formulas <- function() {
  df_input_1 <- data.frame(a = c(1, NA, 3),
                   b = c(NA, 2, 4),
                   c = c(5, 6, 7),
                   m = c(1, NA, 3),
                   l = c(NA, 2, 4))

  list_formulas <- list(
    X = "!is.na(a) ~ a , !is.na(b) ~ b , TRUE ~ c",
    Y = "!is.na(m) ~ m , !is.na(l) ~ l , TRUE ~ NA"
  )

  df_output_1 <- apply_na_processing_formulas(df, list_formulas)
  
  test_that("number of rows is preserved", {
    expect_equal(nrow(df_input_1), nrow(df_output_1))
  })
  
  test_that("number of columns is increased", {
    expect_equal(ncol(df_input_1) + length(list_formulas), ncol(df_output_1))
  })
  
  test_that("correct columns are created", {
    expect_true("X" %in% colnames(df_output_1))
    expect_true("Y" %in% colnames(df_output_1))
  })
  
  test_that("values are correctly assigned", {
    expect_equal(df_output_1$X, c(1, 2, 7))
    expect_equal(df_output_1$Y, c(1, 2, NA))
  })

  # Test with no NA values
  df2 <- data.frame(a = c(1, 2, 3),
                    b = c(4, 5, 6),
                    c = c(7, 8, 9),
                    m = c(10, 11, 12),
                    l = c(13, 14, 15))

  df2_output <- apply_na_processing_formulas(df2, list_formulas)

  test_that("No NA test - values are correctly assigned", {
    expect_equal(df2_output$X, c(1, 2, 3))
    expect_equal(df2_output$Y, c(10, 11, 12))
  })

  # Test with single row dataframe
  df3 <- data.frame(a = c(NA),
                    b = c(3),
                    c = c(7),
                    m = c(NA),
                    l = c(5))

  df3_output <- apply_na_processing_formulas(df3, list_formulas)

  test_that("Single row test - values are correctly assigned", {
    expect_equal(df3_output$X, 3)
    expect_equal(df3_output$Y, 5)
  })

  # Test with empty dataframe
  df4 <- data.frame()

  df4_output <- apply_na_processing_formulas(df4, list_formulas)

  test_that("Empty dataframe test - no new columns are created", {
    expect_equal(ncol(df4), ncol(df4_output))
  })

    df_input_5 <- data.frame(a = c(NA, NA, NA),
                    b = c(NA, NA, NA),
                    c = c(NA, NA, NA),
                    m = c(NA, NA, NA),
                    l = c(NA, NA, NA))

  list_formulas <- list(
    X = "!is.na(a) ~ a | !is.na(b) ~ b | TRUE ~ c",
    Y = "!is.na(m) ~ m | !is.na(l) ~ l | TRUE ~ NA"
  )

  df_output_5 <- apply_na_processing_formulas(df1, list_formulas)

  test_that("NA test - values are correctly assigned", {
    expect_equal(df_output_5$X, c(NA, NA, NA))
    expect_equal(df_output_5$Y, c(NA, NA, NA))
  })

}

test_apply_formulas_comma <- function() {
  df <- data.frame(a = c(1, NA, 3),
                   b = c(NA, 2, 4),
                   c = c(5, 6, 7),
                   m = c(1, NA, 3),
                   l = c(NA, 2, 4))

  list_formulas1 <- list(
    X = "!is.na(a) ~ a, !is.na(b) ~ b, TRUE ~ c",
    Y = "!is.na(m) ~ m, !is.na(l) ~ l, TRUE ~ NA"
  )

  df_output1 <- apply_na_processing_formulas(df, list_formulas1)

  test_that("comma separator test - values are correctly assigned", {
    expect_equal(df_output1$X, c(1, 2, 3))
    expect_equal(df_output1$Y, c(1, 2, 3))
  })

  list_formulas2 <- list(
    X = "!is.na(a) ~ a ,!is.na(b) ~ b ,TRUE ~ c",
    Y = "!is.na(m) ~ m ,!is.na(l) ~ l ,TRUE ~ NA"
  )

  df_output2 <- apply_na_processing_formulas(df, list_formulas2)

  test_that("comma separator test with space after - values are correctly assigned", {
    expect_equal(df_output2$X, c(1, 2, 3))
    expect_equal(df_output2$Y, c(1, 2, 3))
  })

  list_formulas3 <- list(
    X = "!is.na(a) ~ a, !is.na(b) ~ b, TRUE ~ c",
    Y = "!is.na(m) ~ m, !is.na(l) ~ l, TRUE ~ NA"
  )

  df_output3 <- apply_na_processing_formulas(df, list_formulas3)

  test_that("comma separator test with space before - values are correctly assigned", {
    expect_equal(df_output3$X, c(1, 2, 3))
    expect_equal(df_output3$Y, c(1, 2, 3))
  })
}

test_apply_formulas_comma()


test_apply_formulas_missing_var <- function() {
  df <- data.frame(a = c(1, NA, 3),
                   b = c(NA, 2, 4),
                   c = c(5, 6, 7),
                   m = c(1, NA, 3),
                   l = c(NA, 2, 4))

  list_formulas1 <- list(
    X = "!is.na(a) ~ a, !is.na(b) ~ b, TRUE ~ c",
    Y = "!is.na(m) ~ m, !is.na(z) ~ l, TRUE ~ NA"
  )

  df_output1 <- apply_na_processing_formulas(df, list_formulas1)

  test_that("missing variable test - values are correctly assigned", {
    expect_equal(df_output1$X, c(1, 2, 3))
    expect_equal(df_output1$Y, c(1, NA, 3))
  })
}

test_apply_formulas_missing_var()