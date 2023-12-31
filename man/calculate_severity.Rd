% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_severity.R
\name{calculate_severity}
\alias{calculate_severity}
\title{Calculate the severity in each field at each time point}
\usage{
calculate_severity(df, severity_type = 1)
}
\arguments{
\item{df}{A data.frame returned from simulate()}

\item{severity_type}{An integer specifying the equation for severity:
1 = I / (S + L + I)
2 = I / (S + L + I + R)
3 = (L + I) / (S + L + I)
4 = (L + I) / (S + L + I + R)
5 = (L + I + R) / (S + L + I + R)}
}
\value{
A data.frame containing a severity column for each field
}
\description{
Calculate the severity in each field at each time point
}
\examples{
parameters() |>
  simulate() |>
  calculate_severity()
}
