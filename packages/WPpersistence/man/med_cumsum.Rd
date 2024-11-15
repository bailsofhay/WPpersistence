% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/med_cumsum.R
\name{med_cumsum}
\alias{med_cumsum}
\title{med_cumsum}
\usage{
med_cumsum(dat)
}
\arguments{
\item{dat}{a dataframe where each column represents a year and each row represents a pixel or observation. Values must be binary, with 1 representing presence and 0 representing absence.No NA values should exist in input dataframe.}
}
\value{
A numeric vector containing the median cumulative sum RLE value across years (input columns) for each row of the input dataframe.
}
\description{
This function computes the median cumulative sum of a binary set of numbers using Run Length Encoding (RLE). This function was created to compute the median persistence of water primrose invasion in the Sacramento-San Joaquin River Delta.
}
\examples{
# example code
\dontrun{
x = data.frame(x2014 = 1, x2015 = 0, x2016 = 1, x2017 = 1, x2018 = 1, x2019 = 1, x2020 = 0)
med_cumsum(dat = x)
}

}
\author{
Bailey D. Morrison
}
