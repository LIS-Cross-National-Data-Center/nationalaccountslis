it14ih <- dplyr::as_tibble(haven::read_dta(
  "data-raw/it14ih.dta"
))

it14ip <- dplyr::as_tibble(haven::read_dta(
  "data-raw/it14ip.dta"
))

usethis::use_data(it14ih, it14ip, overwrite = TRUE,
    internal=FALSE)