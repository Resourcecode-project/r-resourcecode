#' Compute the orbital speed at a given depth from the wave elevation 1D spectra
#'
#' @param S 1D spectral data: TxM matrix
#' @param freq the M frequencies
#' @param z distance above the floor at which we want the orbital speed (single numeric)
#' @param depth depth time series (vector length T. Recycled if a single value is given)
#' @param spec TRUE if the spectral speed are needed. Otherwise, returns the RMS (default)
#'
#' @return depending on spec, a lsit or a data.frame
#' @export
#'
#' @examples
#'  # Compute orbital speed for varying Hs
#'  S = t(sapply(1:10, \(h) jonswap(h)$spec))
#'  orb_speeds = calc_obtital_speeds(S,rscd_freq,depth=100,z=10)
#'  plot(1:10,orb_speeds[,1],type='l'
#'     ,ylim=range(orb_speeds),
#'     xlab="Hs (m)",
#'     ylab="Orbital speed RMS (m/s)")
#'  lines(1:10,orb_speeds[,2],type='l',col='red')
calc_obtital_speeds = function(S,freq,z=0,depth=Inf,spec=FALSE){
  # z: distance above sea floor

  dims = dim(S)
  n_time =dims[1]
  n_freq= dims[2]

  if(length(depth)==1){depth=rep(depth,n_time)}

  stopifnot(n_freq==length(freq))
  stopifnot(all(z>=0))
  stopifnot(all(z<=depth))

  #Compute k efficiently when depth is discretized

  k = outer(freq,unique(depth),Vectorize(resourcecode::dispersion,vectorize.args = c("frequencies","depth")))
  mat_k = t(k[,match(depth,unique(depth))])

  mat_d = matrix(depth,nrow=n_time,ncol=n_freq)
  mat_depth = matrix(z,nrow=n_time,ncol=n_freq)
  mat_freq = matrix(freq,nrow=n_time,ncol = n_freq,byrow = T)


  csh1 = cosh(mat_k*mat_depth)
  ssh1 = sinh(mat_k*mat_depth)
  ssh2 = sinh(mat_k*mat_d)

  Su=(2*pi*mat_freq*csh1/ssh2)^2 * S

  Sv=(2*pi*mat_freq*ssh1/ssh2)^2 * S

  if(spec){
    out = array(NA,dim = c(dim(Su),2))
    out[,,1] = Su
    out[,,2] = Sv
    return(out)
  } else {
    u_rms = sqrt(2*resourcecode::fastTrapz(freq,Su,2))
    v_rms = sqrt(2*resourcecode::fastTrapz(freq,Sv,2))
  return(cbind(u_rms,v_rms))
  }
}


