library(testthat)

# Test that function accepts ISO2 or ISO3 codes
test_that("Function accepts ISO2 or ISO3 codes", {
    expect_silent(download_na_oecd(1967, countries = c("AU", "AT", "BE")))
    expect_silent(download_na_oecd(1967, countries = c("AUS", "AUT", "BEL")))
})

# Test that function does not accept codes of length other than 2 or 3
test_that("Function does not accept codes of length other than 2 or 3", {
    expect_error(download_na_oecd(1967, countries = c("A", "B", "C")),
                 "Values in 'countries' need to be country codes in ISO2 or ISO3.")
    expect_error(download_na_oecd(1967, countries = c("AUS1", "AUT2", "BEL3")),
                 "Values in 'countries' need to be country codes in ISO2 or ISO3.")
})


# Test that function does not accept non-character vectors for countries
test_that("Function does not accept non-character vectors for countries", {
    expect_error(download_na_oecd(1967, countries = c(1, 2, 3)),
                 "Values in 'countries' need to be country codes in ISO2 or ISO3.")
})
