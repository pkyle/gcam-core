% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zgcamusa_L1321.cement.R
\name{module_gcamusa_L1321.cement}
\alias{module_gcamusa_L1321.cement}
\title{module_gcamusa_L1321.Cement}
\usage{
module_gcamusa_L1321.cement(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L1321.out_Mt_state_cement_Yh}, \code{L1321.IO_GJkg_state_cement_F_Yh}, \code{L1321.in_EJ_state_cement_F_Y}. The corresponding file in the
original data system was \code{LA1321.Cement.R} (gcam-usa level1).
}
\description{
Allocate across the states national cement production, input-output cofficients, and energy inputs to cement production
}
\details{
The tables for cement production, i.e., out, and energy inputs, i.e., in, were calculated by applying state shares to national data.

The state shares were determined by the states' relative values of cement shipments.

The input-out coefficients were downscaled to the states in proportion to the national data.
}
\author{
AS May 2017
}
