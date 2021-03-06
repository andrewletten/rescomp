% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frame_and_name.R
\name{frame_and_name}
\alias{frame_and_name}
\title{Convert object of class deSolve to a data frame and name columns}
\usage{
frame_and_name(model)
}
\arguments{
\item{model}{List output from \code{sim_rescomp()}. First element is an object of
class deSolve. Second element is an S3 object of class \code{rescomp}.}
}
\value{
data frame
}
\description{
Convert object of class deSolve to a data frame and name columns
}
\examples{
pars <- spec_rescomp()
m1 <- sim_rescomp(pars)
frame_and_name(m1)

}
