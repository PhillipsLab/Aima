% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_interp.R
\name{match_interp}
\alias{match_interp}
\title{Function for interpolation of physiological data}
\usage{
match_interp(data, data2)
}
\arguments{
\item{data}{a data frame containing only numeric columns to be interpolated.}

\item{data2}{a data frame of length \code{x}, the length to be interpolated.
to}
}
\value{
a data frame of length nrow(data2).
}
\description{
Interpolate all data columns to a specified length using linear interpolation.
}
