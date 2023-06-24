#' Read a LIS or LWS dataset
#' 
#' @description
#' Reads a LIS or LWS dataset and returns a dataframe with the variables needed for the
#' National Accounts estimates.
#' 
#' @param dataset A string with the name of the dataset to read. E.g. 'it14i'. The
#'  fifth character must identify the database of the dataset (e.g. 'i' for LIS,
#'  'w' for LWS, 'e' for ERFLIS).
#' @param data_path A string with the path to the folder containing the LIS datasets.
#' @param formulas A list of formulas to compute the National Accounts estimates.
#' 
#' @return A dataframe with the variables needed for the National Accounts estimates.
read_lis_microdata <- function(dataset, data_path, weights, formulas){

    # for each element in formulas:
    needed_vars <- parse_all_vars_from_formulas(formulas = formulas)
    needed_vars <- c(needed_vars, "hid", "pid", unique(weights), "relation")

    database_letter <- stringr::str_sub(dataset, 5, 5)

    assertthat::assert_that(database_letter %in% c("i", "w", "e"),
        msg = "The 5th character of `dataset` must be either 'i', 'w' or 'e'.")

    # if dataset is from LIS:
    if(database_letter %in% c("i", "e")){
        df <- read_lis_dataset(dataset = dataset, data_path = data_path, needed_vars = needed_vars)
    } else if (database_letter == "w") {
       df <- read_lws_dataset(dataset = dataset, data_path = data_path, needed_vars = needed_vars)
    }

    return(df)
}


#' Read a LIS dataset
#' 
#' @description
#' Reads a LIS dataset and returns a dataframe with the variables needed for the
#'  National Accounts estimates.
#' 
#' @param dataset A string with the name of the dataset to read. E.g. 'it14i'
#' @param data_path A string with the path to the folder containing the LIS datasets.
#' @param needed_vars A vector of strings with the names of the variables needed for the
#'  National Accounts estimates.
#' 
#' @return A dataframe with the variables needed for the National Accounts estimates.
read_lis_dataset <- function(dataset, data_path, needed_vars){
    # ** read household file
    df_h <- haven::read_dta(fs::path(stringr::str_c(data_path, "/", dataset, "h.dta")),
        col_select = dplyr::any_of(needed_vars))
    
    # ** read person file
    df_p <- haven::read_dta(fs::path(stringr::str_c(data_path, "/", dataset, "p.dta")),
        col_select = dplyr::any_of(needed_vars))

    return(dplyr::left_join(df_p, df_h, by = c("hid")))

}

#' @describeIn read_lis_dataset Read a LWS dataset
read_lws_dataset <- function(dataset, data_path, needed_vars){
    needed_vars <- c(needed_vars, "inum")

    # ** read household file
    df_h <- haven::read_dta(fs::path(stringr::str_c(data_path, "/", dataset, "h.dta")),
        col_select = dplyr::any_of(needed_vars))
    
    # ** read person file
    df_p <- haven::read_dta(fs::path(stringr::str_c(data_path, "/", dataset, "p.dta")),
        col_select = dplyr::any_of(needed_vars))

    return(dplyr::left_join(df_p, df_h, by = c("hid", "inum")))

}