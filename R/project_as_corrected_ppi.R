# helper function to calculate expected eta, vectorizing over height for better performance
eta_expected=function(vp,range,elev, antenna=0, beam_angle=1, k=4/3, lat=35, re = 6378, rp = 6357){
  beamshapes=t(sapply(vp$data$HGHT+vp$attributes$where$interval/2,function(x) beam_profile(x,range,elev, antenna=antenna, beam_angle=beam_angle, k=k, lat=lat, re=re, rp=rp)))
  rcs(vp)*colSums(beamshapes*vp$data$dens,na.rm=T)/colSums(beamshapes,na.rm=T)
}

#' calculate a range-bias corrected PPI
#'
#' calculates a PPI that corrects for range-bias effects due to partial beam overlap with
#' the layer of migration (overshooting) at larger distances from the radar
#' @inheritParams beam_height
#' @inheritParams scan_to_raster
#' @param vp a vertical profile of class vp
#' @return a SpatialPointsDataFrame
#'
#' @keywords internal
#'
#' @details to be written
range_correction_scan = function(scan,vp,nx,ny,xlim,ylim,param="DBZH",crs="+proj=longlat +datum=WGS84",res=NA, antenna=NA, beam_angle=1, k=4/3, lat=35, re = 6378, rp = 6357){
  if(!is.scan(scan)) stop("'scan' should be an object of class scan")
  if(!is.vp(vp)) stop("'vp' should be an object of class vp")
  if(!is.numeric(nx)) stop("'nx' should be an integer")
  if(!is.numeric(ny)) stop("ny' should be an integer")
  if(length(xlim)!=2 & !is.numeric(xlim)) stop("'xlim' should be an integer vector of length two")
  if(is.na(xlim[1]) | is.na(xlim[2]) | xlim[1]>xlim[2]) stop("'xlim' should be a vector with two numeric values for upper and lower bound")
  if(length(ylim)!=2 & !is.numeric(ylim)) stop("'ylim' should be an integer vector of length two")
  if(is.na(ylim[1]) | is.na(ylim[2]) | ylim[1]>ylim[2]) stop("'ylim' should be a vector with two numeric values for upper and lower bound")
  if(!(param %in% c("DBZH","DBZV","DBZ","TH","TV"))) stop(paste(x,"not one of DBZH, DBZV, DBZ, TH, TV"))
  if(!(param %in% names(scan$params))) stop(paste(param,"not found in scan"))
  if(is.null(scan$geo$height) && is.na(antenna)) stop("antenna height cannot be found in scan, specify antenna height using 'antenna' argument")
  # check crs argument as in raster::raster()
  crs=CRS(as.character(projection(crs)))
  x=scan_to_raster(scan,nx=nx,ny=ny,xlim=xlim,ylim=ylim,crs=crs,res=res)
  x=as(x,"SpatialGridDataFrame")
  x@data$eta_expected=eta_expected(vp,x@data$range,scan$geo$elangle,antenna=antenna, beam_angle=beam_angle, k=k, lat=lt, re = re, rp = rp)
  x@data$eta=dbz_to_eta(x@data$DBZH,wavelength=vp$attributes$how$wavelength)
  x
}

#' calculate a range-bias corrected PPI
#'
#' calculates a PPI that corrects for range-bias effects due to partial beam overlap with
#' the layer of migration (overshooting) at larger distances from the radar
#' @param vol a polar volume of class pvol
#' @param vp a vertical profile of class vp
#' @param nx number of grid pixels in the longitude dimension
#' @param ny number of grid pixels in the latitude dimension
#' @param xlim longitude range
#' @param ylim latitude range
#' @param quantity one or multiple of 'vir','vid','correction_factor', 'overlap'
#' @return An object of class '\link[=summary.ppi]{ppi}'.
#'
#' @export
#'
#' @details to be written
#'
#' @examples
#' # locate example polar volume file:
#' pvolfile <- system.file("extdata", "volume.h5", package = "bioRad")
#' # load polar volume
#' example_pvol <- read_pvolfile(pvolfile)
#' # load the vertical profile for this polar volume (pre-calculated)
#' data(example_vp)
#' # calculate the range-bias corrected ppi
#' my_ppi <- calculate_range_corrected_ppi(example_pvol,example_vp,nx=100,ny=100,xlim=c(11,15),ylim=c(55,57))
#' TODO: xlim ylim aanpassen naar range
project_as_corrected_ppi = function(vol,vp,nx,ny,xlim,ylim,quantity=c("vir","vid","correction_factor","overlap"),crs="+proj=longlat +datum=WGS84",res=NA){
  if(!is.pvol(vol)) stop("'vol' should be an object of class vol")
  if(!is.vp(vp)) stop("'vp' should be an object of class vp")
  if(!is.numeric(nx)) stop("'nx' should be an integer")
  if(!is.numeric(ny)) stop("ny' should be an integer")
  if(length(xlim)!=2 & !is.numeric(xlim)) stop("'xlim' should be an integer vector of length two")
  if(is.na(xlim[1]) | is.na(xlim[2]) | xlim[1]>xlim[2]) stop("'xlim' should be a vector with two numeric values for upper and lower bound")
  if(length(ylim)!=2 & !is.numeric(ylim)) stop("'ylim' should be an integer vector of length two")
  if(is.na(ylim[1]) | is.na(ylim[2]) | ylim[1]>ylim[2]) stop("'ylim' should be a vector with two numeric values for upper and lower bound")
  if(FALSE %in% (quantity %in% c("vir","vid","eta_sum","eta_expected_sum","azim","range","correction_factor","overlap"))) stop("unknown quantity")
  rasters=lapply(vol$scans,function(x) range_correction_scan(x,vp,nx=nx,ny=ny,xlim=xlim,ylim=ylim,crs=crs,res=res))
  eta_expected_sum=rowSums(do.call(cbind,lapply(1:length(rasters),function(i) (rasters[[i]]$eta_expected))),na.rm=T)
  eta_sum=rowSums(do.call(cbind,lapply(1:length(rasters),function(i) (rasters[[i]]$eta))),na.rm=T)
  output=rasters[[1]]
  output@data$range=output@data$distance
  output@data$eta_sum_expected=eta_expected_sum
  output@data$eta_sum=eta_sum
  output@data$correction_factor=eta_sum/eta_expected_sum
  output@data$vir=integrate_profile(example_vp)$vir*eta_sum/eta_expected_sum
  output@data$vid=integrate_profile(example_vp)$vid*eta_sum/eta_expected_sum
  # calculate the overlap between vp and radiated energy\
  if("overlap" %in% quantity){
    # calculate overlap first for a range grid:
    overlap=beam_profile_overlap(vol, vp, seq(0,max(output@data$range,na.rm=T),length.out=500), ylim=c(0,4000), steps=500, quantity="dens")
    # align our projected pixels with this range grid:
    overlap_index=sapply(output@data$range,function(x) ifelse(is.na(x),NA,which.min(abs(overlap$range - x))))
    # add the overlap data to the output
    output@data$overlap=overlap$overlap[overlap_index]
  }
  # assemble geometry attributes
  geo=vol$geo
  geo$elangle=get_elevation_angles(vol)
  geo$bbox=output@bbox
  rownames(geo$bbox)=c("lon","lat")
  geo$merged=TRUE
  output_ppi=list(radar=vol$radar,datetime=vol$datetime,data=output[quantity],geo=geo)
  class(output_ppi)="ppi"
  output_ppi
}
