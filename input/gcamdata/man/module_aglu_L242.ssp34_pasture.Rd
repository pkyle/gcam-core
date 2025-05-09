% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zaglu_L242.ssp34_pasture.R
\name{module_aglu_L242.ssp34_pasture}
\alias{module_aglu_L242.ssp34_pasture}
\title{module_aglu_L242.ssp34_pasture}
\usage{
module_aglu_L242.ssp34_pasture(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L242.LN2_HistUnmgdAllocation_SSP34}, \code{L242.LN2_UnmgdAllocation_SSP34}, \code{L242.LN2_HistMgdAllocation_SSP34}, \code{L242.LN2_MgdAllocation_SSP34}. The corresponding file in the
original data system was \code{L242.ssp34_pasture.R} (aglu level2).
}
\description{
Construct the managed and unmanaged pasture allocation datasets for SSP3 and SSP4
}
\details{
For unmanaged and managed pasture, adjust data so that their ratio is 0.25 and add node leaf names.
We do this for both historical and model base periods, isolating low-growth regions (defined by \code{aglu.LOW_GROWTH_PCGDP}).
}
\author{
BBL June 2017
}
