% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/final_infectious_area_index.R
\name{final_infectious_area_index}
\alias{final_infectious_area_index}
\title{Get the infectious area index at the end of a growing season}
\usage{
final_infectious_area_index(.result, parms)
}
\arguments{
\item{.result}{A data.frame as produced within `simulate()`}

\item{parms}{List of parameters}
}
\value{
A matrix containing the final leaf area index, where each row is a field, and each column a genotype.
}
\description{
Get the infectious area index at the end of a growing season
}
