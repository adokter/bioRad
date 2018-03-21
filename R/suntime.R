# function obtained via Hidde Leijnse, source unknown
#' Calculate sunrise and sunset
#' @param lon longitude in decimal degrees
#' @param lat latitude in decimal degrees
#' @param date date inhereting from class "\code{POSIXt}" or a string interpretable by \link[base]{as.Date}
#' @param elev sun elevation in degrees
#' @param rise whether to output for rising or setting sun
#' @export
#' @return the moment of sunrise or sunset in UTC time
#' @details The angular diameter of the sun is about 0.536 degrees, therefore the moment
#' of sunrise/sunset corresponds to half that elevation at -0.268 degrees.
#'
#' Note that for a given date and location, sunrise time can be after sunset time, depending
#' on the time difference between the local time and the UTC time zone.
#'
#' Approximate astronomical formula are used, therefore the moment of sunrise / sunset may
#' be off by a few minutes
#' @examples
#' # sunrise in the Netherlands
#' suntime(5,53,"2016-01-01")
#' # sunset in the Netherlands
#' suntime(5,53,"2016-01-01",rise=FALSE)
#' # civil twilight in Ithaca, NY, today
#' suntime(-76.5,42.4,Sys.time(),elev=-6)
suntime = function(lon, lat, date, elev=-0.268, rise = TRUE)
{
  dateOnly=as.Date(date)
  #Convert date to julian day
  yyyy = as.numeric(format(dateOnly,"%Y"))
  mm = as.numeric(format(dateOnly,"%m"))
  dd = as.numeric(format(dateOnly,"%d"))
  jy=yyyy

  if (any(jy == 0)) stop("get_time_sun: there is no year zero!")
  jy[jy < 0] = jy[jy < 0] + 1
  jm = mm
  jm[mm > 2] = mm[mm > 2] + 1
  jy[mm <= 2] = jy[mm <= 2] - 1
  jm[mm <= 2] = mm[mm <= 2] + 13
  julday = floor(365.25 * jy) + floor(30.6001 * jm) + dd + 1720995
  julday[(dd + 31 * (mm + 12 * yyyy)) >= (15 + 31 * (10 + 12 * 1582))] = julday[(dd + 31 * (mm + 12 * yyyy)) >= (15 + 31 * (10 + 12 * 1582))] + 2 - floor(0.01 * jy[(dd + 31 * (mm + 12 * yyyy)) >= (15 + 31 * (10 + 12 * 1582))]) + floor(0.25 * floor(0.01 * jy[(dd + 31 * (mm + 12 * yyyy)) >= (15 + 31 * (10 + 12 * 1582))]))
  julday0 = 2451545	#Julian day for 20000101

  #Calculation of eclips coordinates
  MeanLon = 280.460 + 0.9856474 * (julday - julday0)
  MeanAnom = 357.528 + 0.9856003 * (julday - julday0)
  EclipLon = MeanLon + 1.915 * sin(MeanAnom * pi / 180) + 0.020 * sin(2 * MeanAnom * pi / 180)
  EclipLon = EclipLon * pi / 180
  Obliquity = 23.439 - 0.0000004 * (julday - julday0)
  Obliquity = Obliquity * pi / 180

  #Calculation of the celestial coordinates of the sun
  RightAsc = atan2(cos(Obliquity) * sin(EclipLon), cos(EclipLon))
  Declinat = asin(sin(Obliquity) * sin(EclipLon))

  #Calculation of current, local hour angle
  acos_arg = (sin(elev * pi / 180) - sin(Declinat) * sin(lat * pi / 180)) / (cos(Declinat) * cos(lat * pi / 180))
  angleH = seq(1, 1, length.out = length(acos_arg)) * NA
  angleH[abs(acos_arg) <= 1] = acos(acos_arg[abs(acos_arg) <= 1])

  #Determine sign of the derivative to see if the sun is rising or setting
  if (rise) sign_angle = 1
  else sign_angle = -1
  sign_angle = -1 * sign_angle * sign(cos(Declinat) * cos(lat * pi / 180) * sin(angleH))
  sign_angle[sign_angle == 0] = 1

  #Determine time
  GMST = (sign_angle * angleH - lon * pi / 180 + RightAsc) / 15
  hour = GMST * 180 / pi - 6.697375 - 0.0657098242 * (julday - julday0)
  hour = hour - floor(hour / 24) * 24

  output=as.POSIXct(as.POSIXlt(dateOnly,tz='UTC'))+3600*hour
  return(output)
}

#' Calculate whether it is night at a geographic location and time
#' @inheritParams suntime
#' @export
#' @return TRUE when night, FALSE when day
#' @details The angular diameter of the sun is about 0.536 degrees, therefore the moment
#' of sunrise/sunset corresponds to half that elevation at -0.268 degrees.
#'
#' day evaluates to true when the sun has a higher elevation than parameter elev, otherwise to false
#'
#' Approximate astronomical formula are used, therefore the day/night transition may
#' be off by a few minutes
#' @examples
#' # it's day in the Netherlands at UTC noon on January first:
#' night(5,53,"2016-01-01 12:00")
#'
#' @export
night=function(lon,lat,date,elev=-0.268){
  trise=suntime(lon,lat,date,elev,rise=T)
  tset=suntime(lon,lat,date,elev,rise=F)
  output=rep(NA,length(date))
  itsday=(date>trise & date<tset)
  output[trise<tset]=itsday[trise<tset]
  itsday=(date<tset | date>trise)
  output[trise>=tset]=itsday[trise>=tset]
  !output
}

#' Test a bioRad object for night time
#'
#' Test a bioRad object for day time. Dispatches to the logical inverse of \link{night}.
#' @inheritParams night
#' @param x An object of class \code{vp},\code{vplist} or \code{vpts}.
#' @export
#' @return TRUE when day, FALSE when night, NA if unknown (either datetime or geographic location missing). For objects of class vpts an atomic logical vector
#' @examples
#' day(VP)
day <- function (x, elev=-0.268) UseMethod("day", x)

#' @rdname day
#' @export
day.vp <- function(x,elev=-0.268) {
  stopifnot(inherits(x,"vp"))
  !night(x$attributes$where$lon,x$attributes$where$lat,x$datetime,elev=elev)
}

#' @rdname day
#' @export
day.vplist <- function(x,elev=-0.268) {
  stopifnot(inherits(x,"vplist"))
  sapply(x,day.vp,elev=elev)
}

#' @rdname day
#' @export
day.vpts <- function(x,elev=-0.268) {
  stopifnot(inherits(x,"vpts"))
  !night(x$attributes$where$lon,x$attributes$where$lat,x$dates,elev=elev)
}

#' @rdname day
#' @export
day.pvol <- function(x,elev=-0.268) {
  stopifnot(inherits(x,"pvol"))
  !night(x$geo$lon,x$geo$lat,x$datetime,elev=elev)
}
