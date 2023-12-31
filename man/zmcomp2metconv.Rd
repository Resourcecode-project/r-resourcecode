% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{zmcomp2metconv}
\alias{zmcomp2metconv}
\title{Vector conversion}
\usage{
zmcomp2metconv(u, v = NULL)
}
\arguments{
\item{u}{zonal velocity (1D vector) or matrix with zonal and meridional velocity (Nx2 matrix)}

\item{v}{meridional velocity (1D vector)}
}
\value{
a Nx2 matrix with the norm and direction (meteorological convention)
}
\description{
Converts wind or current zonal and meridional velocity components to
magnitude and direction according to meteorological convention.
}
\examples{
 u = matrix(rnorm(200),nrow=100,ncol=2)
 vdir = zmcomp2metconv(u)
}
