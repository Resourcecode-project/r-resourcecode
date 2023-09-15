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
    return(closest_point_SPEC(cbind(x,lat)))
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
#' @return a Nx2 matrix with the norm and direction (meteorological convention)
#' @export
#'
#' @examples
#'  u = matrix(rnorm(200),nrow=100,ncol=2)
#'  vdir = zmcomp2metconv(u)
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

#' JONWSAP spectrum
#'
#' Creates a JONWSAP density spectrum (one-sided), defined by its integral parameters.
#'
#' Reference : O.G.Houmb and T.Overvik, "Parametrization of Wave Spectra and Long Term Joint Distribution of Wave Height and Period,"
#'        in Proceedings, First International Conference on Behaviour of Offshore Structures (BOSS), Trondheim 1976.
#'        23rd International Towing Tank Conference, vol. II, pp. 544-551
#'      ITTC Committee, 2002, "The Specialist Committee on Waves - Final Report and Recommendations to the 23rd ITTC",
#'        Proc. ITTC, vol. II, pp. 505-736.
#'
#' @param hs Hs (default: 5m)
#' @param tp Period (default: 10s)
#' @param fmax higher frequency of the spectrum or vector of frequencies (default to resourcecode frequency vector)
#' @param df frequency step (unused if fmax=vector of frequencies)
#' @param gam peak enhancement factor (default: 3.3)
#'
#' @return Density spectrum with corresponding parameters
#' @export
#'
#' @examples
#' S1 = jonswap(tp=15)
#' S2 = jonswap(tp=15,fmax=0.95,df=0.003)
#' plot(S1,type='l',ylim=c(0,72))
#' lines(S2,col='red')
#' abline(v=1/15)
jonswap = function(hs=5,tp=15,fmax=resourcecode::rscd_freq,df,gam=3.3){

  if(length(fmax)>1){ #Case when the frequency vector if given
    freq = fmax
    fmin = min(freq)
    fmax = max(freq)
    df = min(diff(freq))
    frq = seq(from=fmin,to=fmax+df,by=df) #generate a uniform-sampling to ease computations
  } else {
    if(is.null(df)){stop("df must be provided when fmax is a single value")}
    fmin=df
    frq = freq = seq(from=df,to=fmax+df,by=df)
  }
  nptsp = length(frq)

  #Compute the parameters of the spectrum
  fm = 1/tp
  lgam = log(gam)
  fr = frq*tp
  ifm = trunc(fm/df)

  #sigma = c(.07*ones(1,ifm),.09*ones(1,nptsp-ifm))
  #sigma=rep(c(0.07,0.09),c(ifm,nptsp-ifm))
  sigma=c(0.07*rep(1,ifm),0.09*rep(1,nptsp-ifm))
  frm4 = fr^-4
  sp = (1-fr)/sigma
  sp = exp(-0.5*(sp^2))
  sp = (frm4/fr)*exp(-1.25*frm4)*exp(lgam*sp)

  sp = sp*(hs/4)^2/sum(sp*df)
  sp = Re(c(0,sp)) ; sp = sp[1:nptsp]
  fr = c(0,frq) ; fr = fr[1:nptsp]


  sp = stats::approx(frq,sp,freq)
  names(sp) = c("freq","spec")
  attr(sp,'Note') = paste0("JONSWAP Spectrum, Hs=",hs,", Tp=",tp,", gamma=",gam)
  return(tibble::as_tibble(sp))
}


