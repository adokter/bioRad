#' Plot a plan position indicator (PPI)
#'
#' Plots a plan position indicator (PPI) generated with \link{ppi} using \link[ggplot2]{ggplot}
#' @param x an object of class 'ppi'
#' @param param the scan parameter to plot, see details below
#' @param xlim range of x values to plot
#' @param ylim range of y values to plot
#' @param ratio aspect ratio between x and y scale
#' @param zlim the range of parameter values to plot
#' @param ... arguments passed to low level \link[ggplot2]{ggplot} function
#' @export
#' @method plot ppi
#' @examples
#' # load an example scan:
#' data(SCAN)
#' # print to screen the available scan parameters:
#' summary(SCAN)
#' # make ppi for the scan
#' ppi=ppi(SCAN)
#' # plot the first scan parameter, which in this case is "VRADH":
#' plot(ppi)
#' # plot the reflectivity quantity:
#' plot(ppi,param="DBZH")
#' # change the range of reflectivities to plot to -30 to 50 dBZ:
#' plot(ppi,param="DBZH",zlim=c(-30,50))
#' @details
#' Available scan parameters for plotting can by printed to screen by \code{summary(x)}.
#' Commonly available parameters are:
#' \describe{
#'  \item{"\code{DBZH}", "\code{DBZ}"}{(Logged) reflectivity factor [dBZ]}
#'  \item{"\code{VRADH}", "\code{VRAD}"}{Radial velocity [m/s]. Radial velocities towards
#'   the radar are negative, while radial velocities away from the radar are positive}
#'  \item{"\code{RHOHV}"}{Correlation coefficient [unitless]. Correlation between vertically polarized and horizontally polarized reflectivity factor}
#'  \item{"\code{PHIDP}"}{Differential phase [degrees]}
#'  \item{"\code{ZDR}"}{(Logged) differential reflectivity [dB]}
#' }
#' The scan parameters are named according to the OPERA data information model (ODIM), see
#' Table 16 in the \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
plot.ppi=function(x,param,xlim,ylim,zlim=c(-20,20),ratio=1,...){
  stopifnot(inherits(x,"ppi"))
  if(missing(param)){
    if("DBZH" %in% names(x$data)) param="DBZH"
    else param=names(x$data)[1]
  }
  else if(!is.character(param)) stop("'param' should be a character string with a valid scan parameter name")
  if(missing(zlim)) zlim=get_zlim(param)
  colorscale=color_scale_fill(param,zlim)
  # extract the scan parameter
  y=NULL #dummy asignment to suppress devtools check warning
  data=do.call(function(y) x$data[y],list(param))
  # convert to points
  data=data.frame(rasterToPoints(raster(data)))
  # bring z-values within plotting range
  index=which(data[,3]<zlim[1])
  if(length(index)>0) data[index,3]=zlim[1]
  index=which(data[,3]>zlim[2])
  if(length(index)>0) data[index,3]=zlim[2]
  # plot
  if(missing(xlim)) xlim=x$data@bbox[1,]
  if(missing(ylim)) ylim=x$data@bbox[2,]
  bbox = coord_fixed(xlim=xlim,ylim=ylim,ratio=ratio)
  ggplot(data=data,...) + geom_raster(aes(x, y, fill=eval(parse(text=param)))) + colorscale + bbox
}
