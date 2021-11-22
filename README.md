# README

Aima is an R package to analyze hemodynamic and sympathetic nerve activity data.

Housed in this repository is a series of functions to aid the analysis,
summarization, as well as the visualization of these data.

## System requirements

Aima relies on functions from the following R packages:

```
	dplyr (>= 1.0.2),
	purrr (>= 0.3.4),
	tibble (>= 3.0.4),
	magrittr (>= 2.0.1),
	ggplot2 (>= 1.5),
	patchwork (>= 1.1.1),
	readr (>= 1.3.1),
	tidyr (>= 1.1.2),
	viridis (>= 0.5.1),
	zoo (>= 1.8),
	stats,
	Rdpack (>= 0.7)
```

Aima has been tested with R version 3.6.0 and higher.

## Installation

To install Aima, first install the devtools package, if it is not already installed:

```r
> install.packages("devtools")
```

Then, install Aima from GitHub:

```r
> devtools::install_github("jordansquair/Aima")
```

This should take no more than a few minutes.

## Usage

The main functions of Aima are to load, process, and visualize data from
baseline and lower-body negative pressure assessments. However, these
functions could be extrapolated by users for other purposes.

For example, Aima can be used to process and retrieve baseline values from
an example dataset:

```r
> data("baseline")

> dat = clean_artifacts(baseline) %>% run_baseline()
```

To get the summary statistics, users can run the "analyze_" functions.

```r
> summary = analyze_baseline(dat)

```

Similar functions are provided to analyze lower-body negative pressure data.
