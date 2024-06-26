% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{closest_point_FIELD}
\alias{closest_point_FIELD}
\title{Find the closest point of the FIELD grid to the specified position}
\usage{
closest_point_FIELD(x, lat = NULL, closest = 1L, ...)
}
\arguments{
\item{x}{vector of coordinates in the form longitude/latitude data frame}

\item{lat}{alternatively, x and lat can be vector of the same length}

\item{closest}{an integer to specify the number of point to output.}

\item{...}{currently unused}
}
\value{
a list with two components: the closest point(s) of the grid and the distance (s).
}
\description{
Find the closest point of the FIELD grid to the specified position
}
\examples{
semrev_west <- closest_point_FIELD(c(-2.786, 47.239))
semrev_west
rscd_field[semrev_west[[1]], ]
}
