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
#' @param x a structure with the needed fields, as an output from 'get_2Dspectrum' for example
#' @param ... unused yet
#'
#' @return a structure comparable to 'get_1Dspectrum'.
#' @export
#'
#' @examples
convert_spectrum_2D1D = function(x,...){

}
