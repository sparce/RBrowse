% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/testPlot.R
\name{testPlot}
\alias{testPlot}
\title{Server code for an RBrowse test plot}
\usage{
testPlot(input, output, session, data_file, overview, bin_width = NULL)
}
\arguments{
\item{input}{The shiny input object. Passed automatically from \code{callModule}}

\item{output}{The shiny output object. Passed automatically from \code{callModule}}

\item{session}{The shiny session object. Passed automatically from \code{callModule}}

\item{data_file}{File path to a data file.}

\item{overview}{Returned data from calling the \code{\link{overviewPlot}} module. Needed to coordinate the view ranges}

\item{bin_width}{Width of the bins in the data file. Included as an example of how you can use
additional arguments}
}
\value{
passes through the output from \code{trackedPlot}, ie. a reactive list containing
the plot
}
\description{
The \code{testPlot} is a test run at configuring plots using the \code{\link{trackedPlot}} module.
It will be used as an example for creating a number of modules designed to produce common
types of genomic tracked plots (eg. manhattanPlot, bamPlot, snpPlot, geneStructurePlot, ...)
It shows how to produce the necessary inputs for \code{trackedPlot} and how to call it
}
