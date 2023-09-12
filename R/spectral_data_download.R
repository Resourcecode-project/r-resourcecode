#' Compute Jonswap spectrum with f (Hz) formulation (Sf = 2*pi*Sw)
#'
#' @param hs Significant wave height (m)
#' @param tp Peak period (s)
#' @param gamma the peakness factor (e.g. 1 or 3.3)
#' @param freq the frequency vector where the spectrum is to be computed (Hz)
#'
#' @return a vector containing the spectrum on input freq
#' @export
#'
#' @examples Sp = jonswap(1,10,3.3)
jonswap = function(hs, tp, gamma=3.3, freq){

}

#' Download the 2D spectrum data from IFREMER ftp for a single data.
#'
#' No consistency checks are made, this function should not be called directly by the user
#'
#' @param point the location name (string) requested.
#' @param year the year (as a string) requested.
#' @param month the month number, as a string,
#'
#' @return a list with the sea-state spectrum and forcings
#' @keywords internal
get_2Dspectrum_raw = function(point,year,month){
  base = "ftp://ftp.ifremer.fr/ifremer/dataref/ww3/resourcecode/HINDCAST/"
  url = paste0(base
               ,year
               ,"/"
               ,month
               ,"/SPEC_NC/RSCD_WW3-RSCD-UG-"
               ,point
               ,"_"
               ,year
               ,month
               ,"_spec.nc"
  )
  temp = tempfile(fileext = ".nc")
  curl::curl_download(url,destfile = temp,mode="wb")

  nc = ncdf4::nc_open(temp)
  on.exit(ncdf4::nc_close(nc))

  freq = nc$dim$frequency$vals
  dir = nc$dim$direction$vals
  dir_ordred = order(dir)

  time = nc$dim$time$vals
  time = as.POSIXct(time*24*3600, origin="1990-01-01Z00:00:00",tz="UTC")

  var_out = list("longitude", "latitude", "frequency1", "frequency2","efth")
  out = lapply(var_out, ncdf4::ncvar_get,nc=nc)
  names(out) = var_out

  out$longitude = out$longitude[1]
  out$latitude = out$latitude[1]
  out$freq = freq
  out$dir = dir[dir_ordred]

  var_out_1D = list( "dpt", "wnd" , "wnddir", "cur", "curdir")
  forcings = lapply(var_out_1D, ncdf4::ncvar_get,nc=nc)
  names(forcings) = var_out_1D

  out$forcings=tibble::tibble(time=time, tibble::as_tibble(forcings))

  out$efth <- 10^(out$efth) - 1e-12
  out$efth = out$efth[dir_ordred,,]

  out$station=point
  out
}

#' Download the 1D spectrum data from IFREMER ftp for a single data.
#'
#' No consistency checks are made, this function should not be called directly by the user
#'
#' @param point the location name (string) requested.
#' @param year the year (as a string) requested.
#' @param month the month number, as a string,
#'
#' @return a list with the sea-state 1D spectrum and forcings
#' @keywords internal
get_1Dspectrum_raw = function(point,year,month){
  base = "ftp://ftp.ifremer.fr/ifremer/dataref/ww3/resourcecode/HINDCAST/"
  url = paste0(base
               ,year
               ,"/"
               ,month
               ,"/FREQ_NC/RSCD_WW3-RSCD-UG-"
               ,point
               ,"_"
               ,year
               ,month
               ,"_freq.nc"
  )
  temp = tempfile(fileext = ".nc")
  curl::curl_download(url,destfile = temp,mode="wb")

  nc = ncdf4::nc_open(temp)
  on.exit(ncdf4::nc_close(nc))

  freq = nc$dim$frequency$vals

  time = nc$dim$time$vals
  time = as.POSIXct(time*24*3600, origin="1990-01-01Z00:00:00",tz="UTC")

  var_out = list("longitude", "latitude", "frequency1", "frequency2","ef","th1m","th2m","sth1m","sth2m")
  out = lapply(var_out, ncdf4::ncvar_get,nc=nc)
  names(out) = var_out

  out$longitude = out$longitude[1]
  out$latitude = out$latitude[1]
  out$freq = freq

  var_out_1D = list("dpt","wnd","wnddir","cur","curdir","hs","fp","f02","f0m1","th1p","sth1p","dir","spr")
  forcings = lapply(var_out_1D, ncdf4::ncvar_get,nc=nc)
  names(forcings) = var_out_1D

  out$forcings=tibble::tibble(time=time, tibble::as_tibble(forcings))

  out$station=point
  out
}

#' Download the 2D spectrum data from IFREMER ftp
#'
#' @param point the location name (string) requested. Alternatively, the node number. The consistency is checked internally.
#' @param start the starting date (as a string). The consistency is checked internally.
#' @param end the ending date as a string
#'
#' @return A list with the data read from the downloaded netCDF file.
#' @export
#'
#' @examples spec = get_2Dspectrum("SEMREVO",start="1994-01-01",end="1994-02-28")
get_2Dspectrum = function(point,start="1994-01-01",end="1994-02-28"){

  stopifnot(length(point)==1)

  if(is.numeric(point)){point =  resourcecode::rscd_spectral[point,"name"]}

  stopifnot(point %in% resourcecode::rscd_spectral$name)

  if(is.character(start)){start=as.POSIXct(start,tz="UTC")}
  if(is.character(end)){end=as.POSIXct(end,tz="UTC")}

  if(is.numeric(start)){start=as.POSIXct(start,tz="UTC",origin=as.POSIXct("1970-01-01 00:00:00",tz="UTC"))}
  if(is.numeric(end)){end=as.POSIXct(end,tz="UTC",origin=as.POSIXct("1970-01-01 00:00:00",tz="UTC"))}

  stopifnot(format(start,"%Y") >= format(rscd_hindcast_start_date,"%Y"))
  stopifnot(format(end,"%Y") <= format(rscd_hindcast_end_date,"%Y"))
  stopifnot(end>=start)


  dates = seq.POSIXt(from=start,to=end,by = "month")
  years = format(dates,format = "%Y")
  months = format(dates,format = "%m")

  out = get_2Dspectrum_raw(point,years[1],months[1])

  for(m in seq_along(years[-1])){
    temp = get_2Dspectrum_raw(point,years[m+1],months[m+1])
    out$efth = abind::abind(out$efth,temp$efth,along = 3)
    out$forcings = rbind(out$forcings,temp$forcings)
  }

  return(out)
}


#' Download the 1D spectrum data from IFREMER ftp
#'
#' @param point the location name (string) requested. Alternatively, the node number. The consistency is checked internally.
#' @param start the starting date (as a string). The consistency is checked internally.
#' @param end the ending date as a string
#'
#' @return A list with the data read from the downloaded netCDF file.
#' @export
#'
#' @examples
#'   spec = get_1Dspectrum("SEMREVO",start="1994-01-01",end="1994-02-28")
get_1Dspectrum = function(point,start="1994-01-01",end="1994-02-28"){

  stopifnot(length(point)==1)

  if(is.numeric(point)){point =  resourcecode::rscd_spectral[point,"name"]}

  stopifnot(point %in% resourcecode::rscd_spectral$name)

  if(is.character(start)){start=as.POSIXct(start,tz="UTC")}
  if(is.character(end)){end=as.POSIXct(end,tz="UTC")}

  if(is.numeric(start)){start=as.POSIXct(start,tz="UTC",origin=as.POSIXct("1970-01-01",tz="UTC"))}
  if(is.numeric(end)){end=as.POSIXct(end,tz="UTC",origin=as.POSIXct("1970-01-01",tz="UTC"))}

  stopifnot(format(start,"%Y") >= format(rscd_hindcast_start_date,"%Y"))
  stopifnot(format(end,"%Y") <= format(rscd_hindcast_end_date,"%Y"))
  stopifnot(end>=start)

  dates = seq.POSIXt(from=start,to=end,by = "month")
  years = format(dates,format = "%Y")
  months = format(dates,format = "%m")

  out = get_1Dspectrum_raw(point,years[1],months[1])

  for(m in seq_along(years[-1])){
    temp = get_1Dspectrum_raw(point,years[m+1],months[m+1])
    out$ef = abind::abind(out$ef,temp$ef,along = 2)
    out$th1m = abind::abind(out$th1m,temp$th1m,along = 2)
    out$th2m = abind::abind(out$th2m,temp$th2m,along = 2)
    out$sth1m = abind::abind(out$sth1m,temp$sth1m,along = 2)
    out$sth2m = abind::abind(out$sth2m,temp$sth2m,along = 2)
    out$forcings = rbind(out$forcings,temp$forcings)
  }

  return(out)
}
