% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.vpts.R
\name{as.vpts}
\alias{as.vpts}
\title{Convert a dataframe into a vpts object}
\usage{
as.vpts(data)
}
\arguments{
\item{data}{a dataframe created from a VPTS CSV file}
}
\value{
a bioRad vpts object
}
\description{
Convert a dataframe into a vpts object
}
\examples{
# locate example file in VPTS CSV format:
df <- read.csv(system.file("extdata", "example_vpts.csv", package = "bioRad"))
# convert the data.frame to a vpts object:
as.vpts(df)
}
