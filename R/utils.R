#' Convert ISO2 country codes to ISOW3
#'
#' @param countries A vector of ISO2 country codes.
#'
#' @return A vector of ISO3 country codes.
#'
#' @examples
#' convert_iso2_to_iso3(c("US", "DE", "JP"))
#'
#' @export
convert_iso2_to_iso3 <- function(countries){
    countries <- toupper(countries)

    map_iso2_iso3 <- list(
        "AU" = "AUS", "AT" = "AUT", "BE" = "BEL", "CA" = "CAN", 
        "CL" = "CHL", "CN" = "CHN", "CO" = "COL", "CZ" = "CZE",
        "DK" = "DNK", "EE" = "EST", "FI" = "FIN", "FR" = "FRA", 
        "DE" = "DEU", "HU" = "HUN", "IE" = "IRL", "IS" = "ISL",
        "IL" = "ISR", "IT" = "ITA", "LT" = "LTU", "LU" = "LUX", 
        "JP" = "JPN", "KR" = "KOR", "MX" = "MEX", "NL" = "NLD",
        "NO" = "NOR", "PL" = "POL", "ES" = "ESP", "SE" = "SWE", 
        "RU" = "RUS", "RO" = "ROU", "SI" = "SVN", "CH" = "CHE",
        "GB" = "GBR",
        "UK" = "GBR", "US" = "USA",
        "BR" = "BRA", "GE" = "GEO", "IN" = "IND", "RS" = "SRB"
    )

    # assert that all countries are in map_iso2_iso3
    assertthat::assert_that(all(countries %in% names(map_iso2_iso3)),
    msg = "Not all countries are in map_iso2_iso3")

    # match iso2 to iso3
    countries <- unname((unlist(map_iso2_iso3[countries])))

    return(countries)
}


#' Compute Formulas
#'
#' This function evaluates each formula in a list of formulas with respect to a data frame,
#' adding the results as new columns to the data frame.
#'
#' @param df A data frame containing the data to which the formulas should be applied.
#' @param formulas A named list of formulas, with each formula represented as a string.
#'
#' @return A data frame that is a copy of the input data frame, but with additional columns
#'         for each formula in the input list. The name of each new column is the name of
#'         the corresponding formula in the input list.
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
#' formulas <- list(Z = "a + b", Y = "c+d")
#' compute_formulas(df, formulas)
compute_formulas <- function(df, formulas) {

  df_new <- df

  for (name in names(formulas)) {
    vars <- all.vars(parse(text=formulas[[name]]))
    assertthat::assert_that(all(vars %in% names(df)), msg = paste("Variables", 
                                                      paste(vars[!(vars %in% names(df))], collapse = ", "), 
                                                      "not found in the data frame."))
    df_new[[name]] <- eval(parse(text=formulas[[name]]), envir=df)
  }
  df_new

}


#' Parse All Variables from Formulas
#'
#' This function takes a list of formulas as input and returns all variables used in the formulas.
#'
#' @param formulas A list of formulas, with each formula represented as a string.
#'
#' @return A character vector with all variables used in the formulas.
parse_all_vars_from_formulas <- function(formulas){
    parsed_vars <- unname(unique(unlist(purrr::map(formulas, .f = ~all.vars(parse(text=.x))))))

    if(is.null(parsed_vars)){
        return(character())
    } else {
        return(parsed_vars)
    }
}
