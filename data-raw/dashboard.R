lis_dashboard_items <- c("NFD11R", "NFD12R", "NFB3GR", "NFD41R",
    "NFD42R", "NFD45R", "NFD62R", "NFD5P", "NFD61P", "NFB6GR",
    "NFK1R") 

lis_dashboard_countries <- c("AUS", "AUT",
                         "CAN", "CHN", "DEU", "LTU", "LUX",
                         "NOR", "POL", "RUS", "ESP",
                         "CHE", "GBR", "USA", "FRA", "ITA", "SWE"
                         )

lis_dashboard_formulas <- list(D11R = "hi11 + hi13",
    `B3GR+D41R+D42R+D45R` = "hi12 + hicapital",
    D62R = "hpublic",
    `D5P+D61P-D12R` = "hxitsc + hxptax",
    `D61P-D12R` = "hxscont",
    `dhi_B6G-K1R` = "hi11 + hi13 + hi12 + hicapital + hpublic - hxitsc - hxptax")

usethis::use_data(lis_dashboard_items, dashboard_countries, dashboard_formulas,
    overwrite = TRUE, internal = FALSE)
