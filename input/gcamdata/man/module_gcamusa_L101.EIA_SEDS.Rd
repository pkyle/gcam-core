% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zgcamusa_L101.EIA_SEDS.R
\name{module_gcamusa_L101.EIA_SEDS}
\alias{module_gcamusa_L101.EIA_SEDS}
\title{module_gcamusa_L101.EIA_SEDS}
\usage{
module_gcamusa_L101.EIA_SEDS(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L101.EIA_use_all_Bbtu}, \code{L101.inEIA_EJ_state_S_F}, \code{L101.inEIA_EJ_state_S_F_all_years}. The corresponding file in the
original data system was \code{LA101.EIA_SEDS.R} (gcam-usa level1).
}
\description{
Produce two output tables from the EIA state energy database:
\itemize{
 \item{L101.inEIA_EJ_state_S_F: Energy data by GCAM sector and fuel, state, and year; energy units in EJ, years from 1971-2010, includes only rows that have a defined sector and fuel}
 \item{L101.EIA_use_all_Bbtu: Energy data by EIA sector and fuel code, GCAM sector and fuel, MSN, state, and year; energy units in Billion BTU, years from 1960-2011, includes all original data}
}
}
\details{
See above
}
\author{
AS April 2017
}
