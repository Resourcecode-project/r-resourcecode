#' Value Matching
#'
#' @param x value to search
#' @param table table of values
#'
#' @return the opposite of x %in% table
#' @export
#'
#' @examples
#' 1:10 %in% c(1,3,5,9)
#' 1:10 %nin% c(1,3,5,9)
"%nin%" <- function(x,table){!(x %in% table)}


#' Find the closest point of the FIELD grid to the specified position
#'
#' @param x vector of coordinates in the form longitude/latitude data frame
#' @param lat alternatively, x and lat can be vector of the same length
#' @param closest an integer to specify the number of point to output.
#' @param ... currently unused
#'
#' @return a list with two components: the closest point(s) of the grid and the distance (s).
#' @export
#'
#' @examples semrev_west = closest_point_FIELD(c(-2.786,47.239))
#' semrev_west
#' rscd_field[semrev_west[[1]],]
closest_point_FIELD = function(x,lat=NULL,closest=1L,...){
  if(!is.null(lat)){
    stopifnot(length(x)==length(lat))
    return(closest_point_FIELD(cbind(x,lat)))
  }

  stopifnot(is.integer(closest) & closest>=1L)
  dist = geosphere::distGeo(x,resourcecode::rscd_field[,c(2,3)])
  ind_min = order(dist)[1:closest]

  return(list(
          point=ind_min,
          distance=dist[ind_min]
          )
  )
}

#' Find the closest point of the SPEC grid to the specified position
#'
#' @param x vector of coordinates in the form longitude/latitude data frame
#' @param lat alternatively, x and lat can be vector of the same length
#' @param closest an integer to specify the number of point to output.
#' @param ... currently unused
#'
#' @return a list with two components: the closest point(s) of the grid and the distance (s).
#' @export
#'
#' @examples semrev_west = closest_point_SPEC(c(-2.786,47.239))
#' semrev_west
#'  rscd_spectral[semrev_west[[1]],]
closest_point_SPEC= function(x,lat=NULL,closest=1L,...){
  if(!is.null(lat)){
    stopifnot(length(x)==length(lat))
    return(closest_point_FIELD(cbind(x,lat)))
  }

  stopifnot(is.integer(closest) & closest>=1L)
  dist = geosphere::distGeo(x,resourcecode::rscd_spectral[,c(1,2)])
  ind_min = order(dist)[1:closest]

  return(list(
    point=ind_min,
    distance=dist[ind_min]
  )
  )
}

#' Vector conversion
#'
#' Converts wind or current zonal and meridional velocity components to
#' magnitude and direction according to meteorological convention.
#'
#' @param u zonal velocity (1D vector) or matrix with zonal and meridional velocity (Nx2 matrix)
#' @param v meridional velocity (1D vector)
#'
#' @return
#' @export
#'
#' @examples
zmcomp2metconv = function(u,v=NULL){
  if(is.vector(u)){
    stopifnot(length(v)==length(u))
    u = cbind(u,v)
  }

  stopifnot(is.matrix(u) & dim(u)[2]==2)

  V = sqrt(u[,1]^2+u[,2]^2)
  D = (270 - atan2(u[,2],u[,1])*180/pi)%%360
  return(cbind(V,D))
}
