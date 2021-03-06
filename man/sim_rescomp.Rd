% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_rescomp.R
\name{sim_rescomp}
\alias{sim_rescomp}
\title{Simulate resource competition (a convenience wrapper for
\code{deSolve::ode()})}
\usage{
sim_rescomp(pars, times, y, events = NULL, ...)
}
\arguments{
\item{pars}{S3 object of class \code{rescomp} returned by
\code{rescomp::spec_rescomp()}.}

\item{times}{Optional list of up to two giving total simulation time and pulsing
sequence where relevant (use \code{rescomp::time_vals()}). If provided,
overrides values given in parms.}

\item{y}{Optional vector of initial values for state variables. If provided,
overrides values given in \code{pars}.}

\item{events}{NULL (events specified in \code{spec_rescomp()})}

\item{...}{Other arguments passed to \code{deSolve::ode()}}
}
\value{
A list of two comprising i) the model dynamics and ii) model
specifications.
}
\description{
Simulate resource competition (a convenience wrapper for
\code{deSolve::ode()})
}
\examples{
pars <- spec_rescomp()
sim_rescomp(pars = pars)
}
