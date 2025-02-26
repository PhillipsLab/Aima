% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_baseline.R
\name{run_baseline}
\alias{run_baseline}
\title{Analyze data from a baseline assessment of hemodynamics and/or sympathetic
nerve activity.}
\usage{
run_baseline(data, rate = 1000, normalize_time = T, trial_length = 5)
}
\arguments{
\item{data}{a data frame output from \link{load_labchart} to be analyzed.}

\item{rate}{a numeric, the sampling rate of the data. Note if the data was
downsampled out of \code{LabChart} the corrected sampling rate should be
specified in frames per second. Default is 1000.}

\item{normalize_time}{a logical, if \code{TRUE} data will be interpolated
to one datapoint per second using an internal function \link{match_interp}.
If \code{FALSE} the existing sampling rate will be retained. Default is
\code{TRUE}.}

\item{trial_length}{the length of the baseline trial in minutes. This will
be used if \code{normalize_time} is \code{TRUE} to convert the data to a
one datapoint per second structure. Default is 5 minutes.}
}
\value{
a data frame containing the baseline assessment data with or without
  interpolation
}
\description{
The required object for this function is the output of \link{load_labchart}
}
