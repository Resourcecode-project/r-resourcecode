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
#'
#'  #Images
#'  lims = c(0,360)
#'  r <- as.POSIXct(round(range(spec1D$forcings$time), "hours"))
#'  par(mfcol=c(2,1))
#'  image(spec1D$forcings$time,spec1D$freq,t(spec1D$th1m),zlim=lims,xlab="Time",ylab="Freq (Hz)", xaxt = "n",main="Directionnal spreading")
#'  axis.POSIXct(1,spec1D$forcings$time,at=seq(r[1], r[2], by = "week"),format = "%Y-%m-%d",las=2)
#'  image(spec1D_RSCD$forcings$time,spec1D_RSCD$freq,t(spec1D_RSCD$th1m),zlim=lims,xlab="Time",ylab="Freq (Hz)", xaxt = "n")
#'  axis.POSIXct(1,spec1D$forcings$time,at=seq(r[1], r[2], by = "week"),format = "%Y-%m-%d",las=2)
convert_spectrum_2D1D = function(spec,...){

  ddir = diff(spec$dir)[1]*pi/180 #computes the discretization in direction

  #Averages the directional spectrum along the direction
  #Simple sum is preferred to pracma::trapz for consistency with WWIII internals
  spec$ef = apply(spec$efth*ddir,seq_along(dim(spec$efth))[-1],sum)

  #Computes the Mean directions and directionnal spreadings
  # (see 2.229 to 2.236 of WWIII user manual for the definitions

  c1 = sweep(spec$efth,1,as.array(cos(spec$dir*pi/180)),FUN = '*') # cos(theta)*F(theta,f)
  s1 = sweep(spec$efth,1,as.array(sin(spec$dir*pi/180)),FUN = '*') # sin*F(theta,f)
  c2 = sweep(spec$efth,1,as.array(cos(2*spec$dir*pi/180)),FUN = '*') # cos(2*theta)*F(theta,f)
  s2 = sweep(spec$efth,1,as.array(sin(2*spec$dir*pi/180)),FUN = '*') # sin(2*theta)*F(theta,f)

  #Same notation as the definitions above
  a1 = apply(c1*ddir,c(2,3),sum)
  b1 = apply(s1*ddir,c(2,3),sum)
  a2 = apply(c2*ddir,c(2,3),sum)
  b2 = apply(s2*ddir,c(2,3),sum)

  spec$th1m = (atan2(b1,a1)*180/pi + 180) %% 360
  spec$th2m = (atan2(b2,a2)*180/pi + 180) %% 360
  spec$sth1m = sqrt(2 * (1 - sqrt((a1^2+b1^2)/ spec$ef^2)))*180/pi
  spec$sth2m = sqrt(2 * (1 - sqrt((a2^2+b2^2)/ spec$ef^2)))*180/pi

  #Removes the 2D directional spectrum
  spec$efth = NULL
  spec$dir=NULL

  #reorder the elements to match the output from `get_1D_spectra`
  spec[c(1:4,8:12,5:7)]
}

#' Compute sea_state parameter from wave directional spectrum
#'
#' @param spec either a 1D or 2D spectrum
#' @param ...
#'
#' @return a tibble with the sea-state parameters computed from the time series of 2D spectrum
#' @export
#'
#' @examples
#' rscd_params = get_parameters(node='119947',start="1994-01-01",end="1994-02-28 23:00:00",parameters = c('hs','tp','cge','t01','dp','dir'))
#' spec = get_2Dspectrum("SEMREVO",start="1994-01-01",end="1994-02-28")
#' param_calc = compute_sea_state_2Dspectrum(spec)
#' par(mfcol=c(2,2))
#' plot(param_calc$time,param_calc$hs,type='l',xlab="Time",ylab="Hs (m)")
#' lines(rscd_params$time,rscd_params$hs,col='red')
#' plot(param_calc$time,param_calc$cge,type='l',xlab="Time",ylab="CgE (kW/m)")
#' lines(rscd_params$time,rscd_params$cge,col='red')
#' plot(param_calc$time,param_calc$tp,type='l',xlab="Time",ylab="Tp (s)")
#' lines(rscd_params$time,rscd_params$tp,col='red')
#' plot(param_calc$time,param_calc$dir,type='l',xlab="Time",ylab="Peak direction (°)")
#' lines(rscd_params$time,rscd_params$dir,col='red')
compute_sea_state_2Dspectrum = function(spec,...){


  #Define an internal function that will do the job for a time-step
  # spectrum: 1D spectrum
  sea_state_2d_raw = function(spec2D,depth,freq,dir){

    water_density = 1026
    g = 9.81

    #Compute 1d spectrum
    ddir = diff(dir)[1]*pi/180
    Ef = apply(spec2D*ddir,2,sum)

    #Compute spectral moments
    m0 = pracma::trapz(x=freq,Ef)
    m1 = pracma::trapz(x=freq,freq*Ef)
    m2 = pracma::trapz(x=freq,freq^2*Ef)
    me = pracma::trapz(x=freq,freq^(-1)*Ef)

    hs = 4 * sqrt(m0)
    T01 = m0/m1
    T02 = sqrt(m0/m2)
    Te = me/m0

    # fp evaluaton using spline fitting around Ef peak
    nk = length(freq)
    freqp = approx(1:nk,freq,xout = seq(from=1,to=nk,length=30*nk)) #Augment frequency resolution by 30
    Efp = spline(freq,Ef,xout = freqp$y,method = "natural") # natural (i.e. "cubic") spline

    fp = Efp$x[which.max(Efp$y)]
    Tp = 1 / fp

    # Spectral Bandwidth and Peakedness parameter (Goda 1970)
    nu = sqrt( (m0*m2) / (m1^2) -1)
    mu = sqrt( 1 - m1^2 / (m0*m2) )

    #wave length
    k = dispersion(freq, depth, iter_max =200, tol=1e-6)
    kd = k * depth
    km = pracma::trapz(k * Ef, x=freq) / m0
    lm = 2 * pi / km

    # Group velocity
    c1 = 1 + 2 * kd / sinh(2 * kd)
    c2 = sqrt(g * tanh(kd) / k)
    cg = 0.5 * c1 * c2

    # Energy flux
    cgef = pracma::trapz(cg * Ef, x=freq)
    CgE = water_density * g * cgef /1000 #convert to kW/m to be consistent with outputs from WWIII


    #Compute direction from  and spreading (°)
    aa = sweep(spec2D,1,as.array(cos(dir*pi/180)),FUN = '*')
    bb = sweep(spec2D,1,as.array(sin(dir*pi/180)),FUN = '*')

    af = apply(aa*ddir,2,sum)
    bf = apply(bb*ddir,2,sum)

    am = pracma::trapz(af,x=freq)
    bm = pracma::trapz(bf,x=freq)

    dir_m = (atan2(bm,am) * 180/pi) %% 360
    spr = sqrt( 2 * (1 - sqrt((am^2 + bm^2)/m0^2) ))
    spr = (spr * 180/pi) %% 360

    #Compute mean direction at peak frequency (°)
    iEfm = which.max(Ef) #peak of the spectrum
    aap = spec2D[,iEfm]*cos(dir*pi/180)
    bbp = spec2D[,iEfm]*sin(dir*pi/180)

    apm = sum(aap*ddir)
    bpm = sum(bbp*ddir)

    dir_p = (atan2(bpm, apm) * 180 / pi) %% 360

    S2 = spec2D^2
    Qpf = apply(sweep(S2,1,freq,FUN = "*")*ddir,2,sum)
    MQ = pracma::trapz(Qpf,x=freq)
    Qp = ((2 * MQ / (m0^2)) * 180 / pi) %% 360

    tibble::tibble(hs=hs,
                   tp=Tp,
                   t01 = T01,
                   t02=T02,
                   t0m1=Te,
                   dpt=depth,
                   dir=(dir_m+180)%%360,
                   dp = (dir_p+180)%%360,
                   spr = spr,
                   qp =Qp,
                   mu=mu,
                   nu=nu,
                   cge = CgE,
                   km=km,
                   lm=lm
                   )
  }

  #We want to apply the function for each time step, we reorder to have the correct inputs
  spec_reordrerd = tibble::tibble(tibble::as_tibble_col(purrr::array_branch(spec$efth,3),column_name = "spec2D"),depth=spec$forcings$dpt)

  out = purrr::list_rbind(purrr:::pmap(spec_reordrerd,.f = sea_state_2d_raw,freq=spec$freq,dir=spec$dir))

  cbind(time = spec$forcings$time,out)
}
