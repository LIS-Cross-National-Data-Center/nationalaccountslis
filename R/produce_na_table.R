#' Produce the National Accounts table for the OECD data
#' 
#' @description
#' Produce the National Accounts table for the OECD detailed non-financial data.
#' 
#' Cleans the National Accounts data with the following steps:
#'  - remove prefix `NF` from variable
#'  - use instructions in `formulas_na` to process the data (i.e. 
#'    compute items using all available sectors)
#'  - reshape the data to wide format (i.e. `country`, `year` and one column per variable)
#' 
#' @param na_data A dataframe with the National Accounts data. E.g. as returned by `download_na_oecd()`.
#'  Must contain the following columns: `country`, `variable`, `sector`, `year`, `value`.
#' @param formulas_na A list with strings containing formulas to produce the National Accounts table.
#' 
#' @return A dataframe with the National Accounts table. It contains the following columns:
#'   `country`, `year` and one column per variable.
#' 
#' @examples
#' \dontrun{
#' # download the National Accounts data
#' na_data <- download_na_oecd()
#' 
#' # produce the National Accounts table
#' na_table <- produce_oecd_na_table(na_data)
#'} 
#' @export
produce_oecd_na_table <- function(na_data, formulas_na = nationalaccountslis::lis_dashboard_na_formulas){

    assertthat::assert_that(all(c("country", "variable", "sector", "year", "value") %in% names(na_data)),
        msg = "The input data frame must contain the following columns: country, variable, sector, year, value.")

    # remove prefix `NF` from variable
    na_data$variable <- stringr::str_remove(na_data$variable, pattern = "^NF")

    # check which elements of `na_data$variable` are in `formulas_na`
    formulas_na <- formulas_na[names(formulas_na) %in% na_data$variable]

    # paste `variable` and `sector` together
    na_data <- dplyr::mutate(na_data, variable = paste(variable, sector, sep = "_"))

    # change `value` to numeric
    na_data$value <- as.numeric(na_data$value)

    # reshape `na_data` to wide format
    na_data_wide <- tidyr::pivot_wider(na_data, id_cols = c("country", "year"), names_from = "variable", values_from = "value")

    na_data_wide <- apply_na_processing_formulas(na_data_wide, formulas_na)

    # drop variables that are from S1 or S14_S15
    na_data_wide <- dplyr::select(na_data_wide, -ends_with("_S1"), -ends_with("_S14_S15"))

    #TODO: PUT THE FOLLOWING INTO A FUNCTION
    # drop variables that end with '_S14' IF the same variable already exists (e.g. drop D11R_S14 if D11R already exists)
    variables_S14 <- names(na_data_wide)[stringr::str_detect(names(na_data_wide), pattern = "_S14$")]
    variables_to_drop <- variables_S14[stringr::str_replace(variables_S14, pattern = "_S14$", "") %in% names(na_data_wide)]

    na_data_wide <- dplyr::select(na_data_wide, -variables_to_drop)

    # drop '_S14' from variable names
    names(na_data_wide) <- stringr::str_remove(names(na_data_wide), pattern = "_S14$")

    # compute `ccyy` variable
    na_data_wide <- dplyr::mutate(na_data_wide, 
        ccyy = paste0(tolower(convert_iso3_to_iso2(country)), stringr::str_sub(year, 3, 4)))

    na_data_wide <- dplyr::select(na_data_wide, country, year, ccyy, everything())

    return(na_data_wide)
}