# nationalaccountslis


## Overview

`nationalaccountslis` is an R package to compute national accounts coverage ratios with the [LIS Data Center](https://www.lisdatacenter.org/) methodology. 

It allows users to:
* Reproduce the coverage ratios published in the [LIS Compare.IT](https://comparability.lisdatacenter.org/shiny/comparability/) dashboard.
* Download, clean and prepare national accounts data from the OECD.
* Compute estimates from microdata.
* Compute and plot coverage ratios.

## Installation
You can install the package from this GitHub repo with:
```r
devtools::install_github("https://github.com/JosepER/nationalaccountslis)
```

## Version
This package is currently in Alpha version.

## Usage
```r
library(nationalaccountslis)

# download the national accounts using the OECD API
na_data <- download_na_oecd(start_year = 1985, countries = c("ITA", "USA"),
                 items = lis_dashboard_items)

# clean the downloaded national accounts data and create a wide table
na_table <- produce_oecd_na_table(na_data, nationalaccountslis::lis_dashboard_na_formulas)

# compute the estimates from microdata
microdata_estimates <- nationalaccountslis::compute_estimates_lis_microdata(datasets = c("it14i", "us16i"),
                                data_path = "data-raw/", # <- Replace with the path to local files.
                                weights= "hpopwgt",
                                formulas= nationalaccountslis::lis_dashboard_microdata_formulas)

# Compute the coverage ratios 
compute_national_accounts_ratios(microdata_estimates, na_table)
```