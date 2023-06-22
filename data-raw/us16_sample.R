us16ih <- dplyr::as_tibble(haven::read_dta(
  "data-raw/us16ih.dta"
))

us16ip <- dplyr::as_tibble(haven::read_dta(
  "data-raw/us16ip.dta"
))

usethis::use_data(us16ih, us16ip, overwrite = TRUE,
    internal=FALSE)