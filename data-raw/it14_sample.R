it14ih <- dplyr::as_tibble(haven::read_dta(
  "data-raw/it14ih.dta"
))

it14ip <- dplyr::as_tibble(haven::read_dta(
  "data-raw/it14ip.dta"
))

it14ip <- dplyr::select(it14ip, hid:pid)

it14wh <- dplyr::as_tibble(haven::read_dta(
  "data-raw/it14wh.dta"
))

it14wp <- dplyr::as_tibble(haven::read_dta(
  "data-raw/it14wp.dta"
))

usethis::use_data(it14ih, it14ip, it14wh, it14wp, overwrite = TRUE,
    internal=FALSE)