% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotsmd.R
\name{plotsmd}
\alias{plotsmd}
\title{plotsmd}
\usage{
plotsmd(vars,unmatchdata,matchdata,refline=NULL,title=NULL,xlab='SMD',ylab='variable')
}
\arguments{
\item{vars}{List the variables you need to compare.}

\item{unmatchdata}{Data before conducting propensity matching.}

\item{matchdata}{The data after propensity score matching.}

\item{refline}{Set a reference line with a default value of 0.1.}

\item{title}{The title of the image.}

\item{xlab}{The name of the X-axis.}

\item{ylab}{The name of the Y-axis.}
}
\value{
A picture.
}
\description{
You can use it to draw a baseline table of data.
}
\details{
The differences between variables can be represented using SMD. This program can draw SMD graphs of variable differences.
}
