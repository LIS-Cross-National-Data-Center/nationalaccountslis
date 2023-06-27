#' Download National Accounts from OECD REST API
#' 
#' @description 
#' Download National Accounts from OECD REST API.
#' 
#' @param start_year The first year to download data for.
#' @param countries A vector of ISO2 or ISO3 country codes. E.g.
#'  c("AUS", "AUT", "BEL"). Defaults to the countries used in the LIS Compare.IT
#' @param items A vector of National Account items to download. E.g.
#'  c("NFD11R", "NFD12R", "NFB3GR").
#'   Defaults to the items used in the LIS Compare.IT Dashboard.
#'
#' @return A data frame with the following columns: country, variable,
#'  sector, year, value.
#'
#' @examples
#' \dontrun{
#' download_na_oecd(start_year = 1967, countries = c("AUS", "AUT", "BEL"),
#'  items = c("NFD11R", "NFD12R", "NFB3GR"))
#' }
#' @export
download_na_oecd <- function(start_year = 1967, countries = nationalaccountslis::lis_dashboard_countries,
                    items = nationalaccountslis::lis_dashboard_items){

        # Deal with countries
        countries <- toupper(countries)

        # ** assess that all countries are of length 2 or 3
        assertthat::assert_that(all(stringr::str_length(countries) == 2 |
            all(stringr::str_length(countries) == 3)),
            msg = "Values in 'countries' need to be country codes in ISO2 or ISO3.")

        # ** assess if all countries are of length 2
        if(all(nchar(countries) == 2)){
            countries <- convert_iso2_to_iso3(countries)
        }


    downloaded_df <- OECD::get_dataset("SNA_TABLE14A",
                                    filter = list(countries,
                                                  items,
                                                c("S1", "S14", "S14_S15")),
                                    start_time = start_year)

    return(dplyr::select(downloaded_df, country = LOCATION, variable = TRANSACT,
                       sector = SECTOR, year = Time, value = ObsValue))

}