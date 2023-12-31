# Objects to reproduce LIS methodology in the Compare.it dashboard: https://comparability.lisdatacenter.org/shiny/comparability/

lis_dashboard_items <- c("NFD11R", "NFB3GR", "NFD4R", "NFD41R", "NFD42R",
                  "NFD45R",
                  "NFD62R",
                  "NFD5P", "NFD61P", "NFD12R", "NFB6GR", "NFK1R") 

lis_dashboard_countries <- c("AUS", "AUT",
                         "CAN", "CHN", "DEU", "LTU", "LUX",
                         "NOR", "POL", "RUS", "ESP",
                         "CHE", "GBR", "USA", "FRA", "ITA", "SWE"
                         )

lis_dashboard_microdata_formulas <- list(
    D11R = "hi11 + hi13",
    `B3GR+D41R+D42R+D45R` = "hi12 + hicapital",
    D62R = "hpublic",
    `D5P+D61P-D12R` = "hxitsc + hxptax",
    `D61P-D12R` = "hxscont",
    `B6GR-K1R` = "hi11 + hi13 + hi12 + hicapital + hpublic - hxitsc - hxptax")

lis_dashboard_na_formulas <- list(
    D11R = "!is.na(D11R_S14) ~ D11R_S14 , !is.na(D11R_S14_S15) ~ D11R_S14_S15 , !is.na(D11R_S1) ~ D11R_S1",
    D12R = "!is.na(D12R_S14) ~ D12R_S14 , !is.na(D12R_S14_S15) ~ D12R_S14_S15 , !is.na(D12R_S1) ~ D12R_S1",
    B3GR = "!is.na(B3GR_S14) ~ B3GR_S14 , !is.na(B3GR_S14_S15) ~ B3GR_S14_S15 , !is.na(B3GR_S1) ~ B3GR_S1",
    D62R = "!is.na(D62R_S14) ~ D62R_S14 , !is.na(D62R_S14_S15) ~ D62R_S14_S15 , !is.na(D62R_S1) ~ D62R_S1",
    D61P = "!is.na(D61P_S14) ~ D61P_S14 , !is.na(D61P_S14_S15) ~ D61P_S14_S15 , !is.na(D61P_S1) ~ D61P_S1"
)

usethis::use_data(lis_dashboard_items, lis_dashboard_countries, lis_dashboard_microdata_formulas, lis_dashboard_na_formulas,
    overwrite = TRUE, internal = FALSE)
