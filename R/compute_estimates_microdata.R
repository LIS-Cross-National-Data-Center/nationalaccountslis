
#' Compute National Account estimates from LIS microdata
#' 
#' @description 
#' Computes National Account estimates from LIS microdata. It grosses up variables 
#'   using population weights to reproduce the National Accounts numbers.
#' 
#' @param datasets A vector of strings with the names of the datasets to read. The
#'  fourth character must identify the database of the dataset (e.g. 'i' for LIS, 
#'  'w' for LWS, 'e' for ERFLIS). E.g. 'it14i'
#' @param data_path A string or vector of strings with the path to the folder containing the LIS datasets.
#'  Must either have length 1 (all datasets in the same path) or the same length as datasets.
#' @param weights A string or vector of strings with the names of the weight variable(s) to use.
#'  Must either have length 1 (all datasets use the same weight variable) or the same length as formulas.
#' @param formulas A list of formulas to compute the National Accounts estimates.
#' 
#' @return A list of dataframes with the National Accounts estimates from the microdata.
#' 
#' @export
compute_estimates_lis_microdata <- function(datasets, data_path, weights, formulas=nationalaccountslis::lis_dashboard_formulas){

    # check that the 4th character of each dataset is either 'i', 'w' or 'e'
    assertthat::assert_that(all(stringr::str_sub(datasets, 4, 4) %in% c("i", "w", "e")),
        msg = "The 4th character of each dataset must be either 'i', 'w' or 'e'.")

    # if data_path has length 1, repeat it to have the same length as datasets
    if(length(data_path)==1){
        data_path <- rep(data_path, length(datasets))
    }

    # for each dataset and data_path (start iteration with purrr::map2):
    purrr::map2(datasets, data_path, 
    .f = ~compute_estimates_single_lis_file(dataset = .x, data_path = .y, weights = weights, formulas = formulas), 
        weights, formulas)

}


#' Compute National Account estimates from a single LIS microdata file
#' 
#' @description
#' Computes National Account estimates from a single LIS microdata file. It grosses up variables
#'  using population weights to reproduce the National Accounts items.
#' 
#' @param dataset A string with the name of the dataset to read.
#' @param data_path A string with the path to the folder containing the LIS datasets.
#' @param weights A string or vector of strings with the names of the weight variable(s) to use.
#'   Must either have length 1 (all datasets use the same weight variable) or the same length as formulas.
#' @param formulas A list of strings witht the formulas to compute the National Accounts estimates.
#' 
#' @details 
#' Lower level function called by compute_estimates_lis_microdata. It is not intended to be called directly.
#' 
#' @return A dataframe with the National Accounts estimates from the microdata.
compute_estimates_single_lis_file <- function(dataset, data_path, weights, formulas){

    # ** load data
    df <- read_lis_microdata(dataset, data_path, weights, formulas)

    # ** create new variables
    df <- compute_formulas(df, formulas)

    # ** compute estimates
    # gross up variables using population weights
    compute_gross_up_estimate(df, weights, formulas)

}


#' Compute grossed up estimates from LIS microdata
#' 
#' @param df A dataframe with the LIS microdata.
#' @param weights A string or vector of strings with the names of the weight variable(s) to use.
#'  Must either have length 1 (all datasets use the same weight variable) or the same length as formulas.
#' @param formulas A list of strings witht the formulas to compute the National Accounts estimates.
compute_gross_up_estimate <- function(df, weights, formulas){

    # gross up estimate when there's only one weight variable
    if(length(weights)==1){

        # output variables 
        df_output_vars <- df[,names(formulas)]

        purrr::map_dbl(df_output_vars, .f = function(var, weight){

            sum(var * weight /1000000, na.rm = TRUE)

        }, weight = df[[weights]])
    
    
    }else{
        # gross up estimate when there are multiple weight variables
        assertthat::assert_that(length(weights)==length(formulas),
            msg = "The number of weight variables must be the same as the number of formulas.")

        df_output_vars <- df[,names(formulas)]

        df_weights <- purrr::map_dfc(weights, .f = function(weight, df){

            df[[weight]]

        }, df = df)

        purrr::map2_dbl(.x = df_output_vars, .y = df_weights, .f = function(var, weight){

            sum(var * weight /1000000, na.rm = TRUE)

        })
    }
}
