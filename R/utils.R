#' Convert ISO2 country codes to ISOW3
#'
#' @param countries A vector of ISO2 country codes.
#'
#' @return A vector of ISO3 country codes.
#'
#' @examples
#' \dontrun{
#' convert_iso2_to_iso3(c("US", "DE", "JP"))
#' }
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

#' Convert ISO3 country codes to ISO2
#'
#' @param countries A vector of ISO3 country codes.
#'
#' @return A vector of ISO2 country codes.
#'
#' @examples
#' \dontrun{
#' convert_iso3_to_iso2(c("USA", "DEU", "JPN"))
#' }
convert_iso3_to_iso2 <- function(countries){
    countries <- toupper(countries)

    map_iso3_iso2 <- list(
        "AUS" = "AU", "AUT" = "AT", "BEL" = "BE", "CAN" = "CA", 
        "CHL" = "CL", "CHN" = "CN", "COL" = "CO", "CZE" = "CZ",
        "DNK" = "DK", "EST" = "EE", "FIN" = "FI", "FRA" = "FR", 
        "DEU" = "DE", "HUN" = "HU", "IRL" = "IE", "ISL" = "IS",
        "ISR" = "IL", "ITA" = "IT", "LTU" = "LT", "LUX" = "LU", 
        "JPN" = "JP", "KOR" = "KR", "MEX" = "MX", "NLD" = "NL",
        "NOR" = "NO", "POL" = "PL", "ESP" = "ES", "SWE" = "SE", 
        "RUS" = "RU", "ROU" = "RO", "SVN" = "SI", "CHE" = "CH",
        "GBR" = "GB",
        "USA" = "US",
        "BRA" = "BR", "GEO" = "GE", "IND" = "IN", "SRB" = "RS"
    )

    # assert that all countries are in map_iso3_iso2
    assertthat::assert_that(all(countries %in% names(map_iso3_iso2)),
    msg = "Not all countries are in map_iso3_iso2")

    # match iso3 to iso2
    countries <- unname((unlist(map_iso3_iso2[countries])))

    return(countries)
}


#' Compute Formulas
#'
#' This function evaluates each formula in a list of formulas with respect to a data frame,
#' adding the results as new columns to the data frame.
#'
#' @param df A data frame containing the data to which the formulas should be applied.
#' @param formulas A named list of formulas, with each formula represented as a string.
#'   E.g. list(Z = "a + b", Y = "c+d")
#'
#' @return A data frame that is a copy of the input data frame, but with additional columns
#'         for each formula in the input list. The name of each new column is the name of
#'         the corresponding formula in the input list.
#'
#' @examples 
#' \dontrun{
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9, d = 10:12)
#' formulas <- list(Z = "a + b", Y = "c+d")
#' compute_formulas(df, formulas)
#' }
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



#' Apply NA Processing Formulas
#' 
#' @description 
#' This function takes a data.frame and a list of strings containing formulas as input and applies the formulas to process the data.frame.
#'   
#' @param df A data frame containing the data to which the formulas should be applied.
#' @param list_formulas A named list of formulas, with each formula represented as a string.
#'  E.g. list(
#'    X = "!is.na(a) ~ a, !is.na(b) ~ b, TRUE ~ c",
#' Y = "!is.na(m) ~ m, !is.na(l) ~ l, TRUE ~ NA"
#' )
#' 
#' @return A data frame that is a copy of the input data frame, but with additional columns
#'        for each formula in the input list. The name of each new column is the name of
#'       the corresponding formula in the input list.
#' 
#' @examples 
#' \dontrun{
#' df <- data.frame(a = c(1, NA, 3), b = c(NA, 5, 6), c = c(7, 8, 9))
#' list_formulas <- list(
#'  X = "!is.na(a) ~ a, !is.na(b) ~ b, TRUE ~ c",
#'  Y = "!is.na(m) ~ m, !is.na(l) ~ l, TRUE ~ NA"
#' )
#' apply_na_processing_formulas(df, list_formulas)
#' }
 apply_na_processing_formulas <- function(df, list_formulas) {
  for (var in names(list_formulas)) {
    exprs <- strsplit(list_formulas[[var]], "\\s*,\\s*")[[1]]
    
    # Create a new empty column
    df <- dplyr::mutate(df, !!var := NA)
    
    for (expr in exprs) {
      cond_val <- strsplit(expr, " ~ ")[[1]]
      cond <- parse_expr(cond_val[1])
      val <- parse_expr(cond_val[2])
      
      # Loop through rows
      for(i in seq_len(nrow(df))) {
        tryCatch({
          if (is.na(df[i, var]) & eval_tidy(cond, data = df[i,])) {
            df[i, var] <- eval_tidy(val, data = df[i,])
          }
        }, error = function(e) NULL)
      }
    }
  }
  df
}