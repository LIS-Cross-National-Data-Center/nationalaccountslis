#' Compute National Accounts ratios
#' 
#' @description
#' Compute the National Accounts ratios from the items in microdata and National Accounts.
#' 
#' @param df_estimates_microdata A dataframe with the National Accounts estimates from the microdata.
#'   Must contain the `ccyyd` column and one column per variable, with column names matching the 
#'   formulas in `formulas_microdata`. E.g. as returned by `compute_estimates_lis_microdata()`.
#' @param na_table A dataframe with the National Accounts data. E.g. as returned by `produce_oecd_na_table()`.
#'   Must contain the following columns: `country`, `year` and one column per variable. E.g.
#'   data.frame(country = c("ITA", "USA"), year = c(1985, 1985), D11R = c(100, 200), D12R = c(300, 400))
#' 
#' @return A dataframe with the National Accounts ratios.
#' 
#' @export
compute_national_accounts_ratios <- function(df_estimates_microdata, na_table, 
    formulas_ratios = as.list(set_names(names(nationalaccountslis::lis_dashboard_microdata_formulas)))){

    df_estimates_microdata <- microdata_estimates
    na_table <- na_data_clean

    na_table <- compute_formulas(na_table, as.list(set_names(formulas_ratios)))
    
    na_table <- dplyr::select(na_table, ccyy, 
        any_of(names(formulas_ratios)))

    # compute 'ccyy' variable from 'ccyyd'
    df_estimates_microdata <- dplyr::mutate(df_estimates_microdata, ccyy = stringr::str_sub(ccyyd, 1, 4))
    
    # check that both dataframes have the same columns
    check_col_names_are_equal(df_estimates_microdata, na_table)

    # merge `df_estimates_microdata` and `na_table`
    df_merged <- dplyr::left_join(df_estimates_microdata, na_table, 
        by = c("ccyy"),
        suffix = c("_mi", "_na"))

    # to long format
    df_merged <- tidyr::pivot_longer(df_merged, cols = -c(ccyy, ccyyd), names_to = "variable",
        values_to = "value")
    
    # create new column from 'variable' depending on the ending ('_mi' or '_na')
    df_merged <- dplyr::mutate(df_merged,
        source = stringr::str_sub(variable, -2, -1),
        variable = stringr::str_sub(variable, 1, -4)) 
    
    df_merged %>%
        tidyr::pivot_wider(names_from = source, id_cols = c("ccyyd", "variable"), names_prefix = "value_", values_from = value) %>%
        dplyr::mutate(ratio = value_mi / value_na)

}

#' Check that the names in two dataframes are equal
#' 
#' @description
#' Check that the names in two dataframes (estimates from microdata and NA table) are equal, 
#'  excluding the `ccyy` and `ccyyd` columns.
check_col_names_are_equal <- function(df_estimates_microdata, na_table){

    # assert that df_estimates_microdata and na_table have the same variables (excluding 'ccyy' and 'ccyyd'')
    names_na_table <- names(na_table)
    names_na_table <- names_na_table[!names_na_table %in% c("ccyy", "ccyyd")]

    names_df_estimates_microdata <- names(df_estimates_microdata)
    names_df_estimates_microdata <- names_df_estimates_microdata[!names_df_estimates_microdata %in% c("ccyy", "ccyyd")]

    assertthat::assert_that(all(names_na_table %in% names_df_estimates_microdata),
        msg = "Variables in 'na_table' need to be present in 'df_estimates_microdata'.")

    assertthat::assert_that(all(names_df_estimates_microdata %in% names_na_table),
        msg = "Variables in 'df_estimates_microdata' need to be present in 'na_table'.")

}