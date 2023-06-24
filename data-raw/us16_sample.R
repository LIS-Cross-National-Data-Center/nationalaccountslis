us16ih <- dplyr::as_tibble(haven::read_dta(
  "data-raw/us16ih.dta"
))

us16ip <- dplyr::as_tibble(haven::read_dta(
  "data-raw/us16ip.dta"
))

us16ip <- dplyr::select(us16ip, hid:pwgta)

usethis::use_data(us16ih, us16ip, overwrite = TRUE,
    internal=FALSE)