#' Compute the dispersion relation of waves
#'  Find *k* s.t. (2.pi.f)^2 = g.k.tanh(k.d)
#'
#' @param frequencies frequency vector
#' @param depth depth (m)
#' @param iter_max maximum number of iterations in the non linear solver algorithm
#' @param tol tolerance for termination.
#'
#' @return the wave numbers (same size as frequencies)
#' @export
#'
#' @examples
#' freq = seq(from=0,to=1,length.out=100)
#' k1 = dispersion(freq, depth=1)
#' k10 = dispersion(freq, depth=10)
#' kInf = dispersion(freq, depth=Inf)
#' plot(freq,k1,type="l")
#' lines(freq,k10,col='red')
#' lines(freq,kInf,col='green')
dispersion = function(frequencies,depth,iter_max=200,tol=1e-6){
  g=9.81
  infinite_depth_dispersion = (4 * pi^2 / g) * frequencies^2

  if(is.infinite(depth)){
    return(infinite_depth_dispersion)
  }

  frequencies = as.vector(frequencies)
  out = frequencies

  for(f in seq_along(frequencies)){
    if(frequencies[f]==0){
      out[f] = 0
    } else {
      c0 = (2 * pi * frequencies[f]) ^ 2
      k0 = 4.0243 * frequencies[f] ^ 2
      xk = k0
      ftest = 99
      for(ii in 1:iter_max){
        z = xk * depth
        y = tanh(z)
        ff = c0 - g * xk * y
        dff = g * (z * (y^2 - 1) - y)
        xk_old = xk
        xk = xk_old - ff / dff
        ftest = abs((xk - xk_old) / xk_old)
        if(ftest <= tol){
          break
        }
      }
      ff = c0 - g * xk * y
      out[f] = xk
    }
  }
  out
}

#' Converts a 2D spectrum time series to a 1D spectrum
#'
#' @param spec a structure with the needed fields, as an output from 'get_2Dspectrum' for example
#' @param ... unused yet
#'
#' @return a structure comparable to 'get_1Dspectrum'.
#' @export
#'
#' @examples
#'  spec = get_2Dspectrum("SEMREVO",start="1994-01-01",end="1994-02-28")
#'  spec1D_RSCD = get_1Dspectrum("SEMREVO",start="1994-01-01",end="1994-02-28")
#'  spec1D = convert_spectrum_2D1D(spec)
#'  #Check the differences, should be low
#'  max(abs(spec1D_RSCD$ef-spec1D$ef))
#'
#'  #Plot the different spectrum
#'  plot(spec1D$freq,spec1D$ef[,1],type='l',log='xy')
#'  lines(spec1D_RSCD$freq,spec1D_RSCD$ef[,1],col='red')
convert_spectrum_2D1D = function(spec,...){

  ddir = diff(spec$dir)[1]*pi/180 #computes the discretization in direction

  #Averages the directional spectrum along the direction
  #Simple sum is preferred to pracma::trapz for consistency with WWIII internals
  spec$ef = apply(spec$efth*ddir,seq_along(dim(spec$efth))[-1],sum)

  #Computes the Mean directions and directionnal spreadings
  # (see 2.229 to 2.236 of WWIII user manual for the definitions

  c1 = sweep(spec$efth,1,cos(spec$dir*pi/180),FUN = '*') # cos(theta)*F(theta,f)
  s1 = sweep(spec$efth,1,sin(spec$dir*pi/180),FUN = '*') # sin*F(theta,f)
  c2 = sweep(spec$efth,1,cos(2*spec$dir*pi/180),FUN = '*') # cos(2*theta)*F(theta,f)
  s2 = sweep(spec$efth,1,sin(2*spec$dir*pi/180),FUN = '*') # sin(2*theta)*F(theta,f)

  #Same notation as the definitions above
  a1 = apply(c1*ddir,c(2,3),sum)
  b1 = apply(s1*ddir,c(2,3),sum)
  a2 = apply(c2*ddir,c(2,3),sum)
  b2 = apply(s2*ddir,c(2,3),sum)

  spec$th1m = (atan2(b1,a1)*180/pi + 180) %% 360
  spec$th2m = (atan2(b2,a2)*180/pi + 180) %% 360
  spec$sth1m = sqrt(.5 * (1 - sqrt((a1^2+b1^2)/ spec$ef^2)))*180/pi
  spec$sth1m = sqrt(.5 * (1 - sqrt((a2^2+b2^2)/ spec$ef^2)))*180/pi

  #Removes the 2D directional spectrum
  spec$efth = NULL
  spec$dir=NULL

  spec
}
