% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sci1freq.R
\name{sci1freq}
\alias{sci1freq}
\title{sci1freq}
\arguments{
\item{mvars}{The full range of variables you don't want to compare.}

\item{x}{Enter the variables to be layered. If you fill in consecutive variables, by default they will be split into 3 layers.}

\item{data}{Enter your data.}

\item{dec}{The precision of the data, which defaults to 2 decimal places.}

\item{nonnormal}{When the data belongs to a non-normal distribution, this parameter is needed to indicate which is variable is non-normally distributed.}

\item{statistic}{Statistical effect values. Usually, it is the default F, and selecting T will return a statistical effect value.}

\item{fisher}{Fisher's exact test. The default is FALSE.}

\item{correct}{Chi square test for continuity correction.The default is FALSE.}

\item{Overall}{Generate summary data.The default is FALSE.}

\item{smd}{The default is FALSE. If it is true, return the SMD value.}
}
\value{
A data frame.
}
\description{
You can use it to draw a baseline table of data.Creates 'Table 1', i.e., description of baseline patient
             characteristics, which is essential in every medical research.
             Supports both continuous and categorical variables, as well as
             p-values and standardized mean differences.
}
\details{
Table 1 represents the relationship between the baseline values of the data.
         This function can be easily done.
}
