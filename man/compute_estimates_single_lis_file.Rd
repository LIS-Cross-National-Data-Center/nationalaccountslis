% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_estimates_microdata.R
\name{compute_estimates_single_lis_file}
\alias{compute_estimates_single_lis_file}
\title{Compute National Account estimates from a single LIS microdata file}
\usage{
compute_estimates_single_lis_file(
  dataset,
  data_path,
  weights,
  formulas_microdata
)
}
\arguments{
\item{dataset}{A string with the name of the dataset to read.}

\item{data_path}{A string with the path to the folder containing the LIS datasets.}

\item{weights}{A string or vector of strings with the names of the weight variable(s) to use.
Must either have length 1 (all datasets use the same weight variable) or the same length as formulas.}

\item{formulas_microdata}{A list of strings witht the formulas to compute the National Accounts estimates.}
}
\value{
A dataframe with the National Accounts estimates from the microdata.
}
\description{
Computes National Account estimates from a single LIS microdata file. It grosses up variables
using population weights to reproduce the National Accounts items.
}
\details{
Lower level function called by compute_estimates_lis_microdata. It is not intended to be called directly.
}
