#' Analyze and visualize biological signals in weather radar data
#'
#' \pkg{bioRad}
#' @details
#' \subsection{BioRad's class objects}{
#' \pkg{bioRad} uses the following class objects for storing radar data:
#'   \itemize{
#'     \item{\link[=summary.pvol]{pvol}}{, a polar volume: consists typically of a set of polar scans, collected at different elevation angles, that together sample the full aerial volume surrounding the radar}
#'     \item{\link[=summary.scan]{scan}}{, a polar scan: a 360 degree radar scan at a fixed elevation in polar coordinates. One scan typically contains multiple scan parameters.}
#'     \item{\link[=summary.param]{param}}{, a polar scan parameter: one of the observable quantities recorded within a polar scan, such as reflectivity (DBZH) or radial velocity (VRADH).}
#'     \item{\link[=summary.ppi]{ppi}}{, a Cartesian plan position indicator: a projection on a Cartesian grid of a polar scan or a polar scan parameter}
#'     \item{\link[=summary.vp]{vp}}{, a vertical profile: typically biological data extracted from a polar volume by \link{vol2bird}.}
#'     \item{\link[=summary.vplist]{vplist}}{, a list of \link[=summary.vp]{vp} objects.}
#'     \item{\link[=summary.vpts]{vpts}}{, a vertical profile time series: a time-oredered list of \link[=summary.vp]{vp} objects for a single radar.}
#'     \item{\link[=vintegrate]{vivp}}{, vertically integrated vertical profiles.}
#'   }
#' }
#' The common \link[base]{summary}, \link[methods]{is}, \link[base]{dim}, and \link[base]{Extract} methods are available for each of these classes.
#'
#' Plot methods are available for ppi, vp and vpts objects
#' \subsection{Reading radar data}{
#' \pkg{bioRad} can read radar files in
#' \href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM}
#' format, which is the implementation of the OPERA data information model in \href{https://support.hdfgroup.org/HDF5/}{HDF5} format,
#' or a format supported by the \href{http://trmm-fc.gsfc.nasa.gov/trmm_gv/software/rsl/}{RSL library}, such as NEXRAD data.
#' \pkg{bioRad}'s class objects are organised very similar to the OPERA data information model.
#'
#' Raw (level-II) weather radar data is read with the function \link{read.pvol}, which returns a \link[=summary.pvol]{pvol} polar volume object.
#'
#' Use the function \link{rsl2odim} to convert RSL (e.g. NEXRAD) radar data into ODIM HDF5 format.
#' }
#' \subsection{Mapping and projecting radar scans}{
#' Funtion \link{ppi} can be used to project polar scans or polar scan parameters onto a user-defined Cartesian grid
#'
#' Function \link{map} can be used together with \link{basemap} to overlay radar data with all kinds of publicly available map and satellite data.
#' }
#' \subsection{Processing weather radar data into vertical profiles of birds}{
#' \pkg{bioRad} contains an implementation of the \link{vol2bird} algorithm, which
#' processes polar volume data into a vertical profiles of birds (VPB).
#' \link{vol2bird} requires a locally running \href{https://www.docker.com/}{Docker} daemon.
#'
#' \link{vol2bird} outputs a vertical profile object (\link[=summary.vp]{vp}) in R,
#' and can store the vertical profile object in an ODIM-complient hdf5 format on disk.
#' Stored hdf5 profiles can be read from disk with \link{readvp} (for a single file) or with
#' \link{readvp.list} (for a list of files)
#'
#' For users running their own installation of vol2bird outside R and Docker, the function \link{readvp.table}
#' is provided to read vol2bird's stdout (standard output) into R (after piping the stdout to a file).
#' }
#' \subsection{Organizing, analyzing and plotting vertical profile data}{
#' Vertical profiles (\link[=summary.vp]{vp} objects) can be combined with \link{bind.vp} into lists of vertical profiles (\link[=summary.vplist]{vplist} objects).
#'
#' Vertical profile lists (\link[=summary.vplist]{vplist} objects) can be converted into vertical profile time series (\link[=summary.vpts]{vpts} objects) using function \link{vpts}.
#'
#' \link{regularize} can be used to project a \link[=summary.vpts]{vpts} object on a regular time grid.
#' This is typically done before plotting a vertical profile time series.
#'
#' \link{plot.vp} can be used to make a plot of a single vertical profile.
#'
#' \link{plot.vpts} can be used to visually summarize vertical profile time series,
#' both in terms of density and speed, which can be visualized simultaneously (with colors and speed barbs).
#' }
#' \subsection{Conversions into numbers of migrating individuals}{
#' To convert radar reflectivity into densities of individuals, a specific radar
#' cross section per individual needs to be assumed, which is set with \link{rcs}.
#' By default, a radar cross section of 11 cm^2 is used, which is the average value found
#' by Dokter et al. during a full autumn migration season in Europe at C-band.
#'
#' \link{mtr} combines reflectivity and speed into migration traffic rates within user-defined altitude bands
#'
#' \link{mt} calculates migration traffic: it integrates migration traffic rates over time and altitude, to find the total number
#' of individuals passing a radar station in a certain time period
#'
#' \link{cmt} calculates cumulative migration traffic.
#' }
#'
#' \subsection{Conventions}{
#'   \itemize{
#'     \item \code{NA} Maps to 'nodata' in the ODIM convention: value to denote areas void of data (never radiated)
#'     \item \code{NaN} Maps to 'undetect' in the ODIM convention: denote areas below the measurement detection threshold (radiated but nothing detected). The value is also used when there are too few datapoints to calculate a quantity.
#'     \item \code{0} Maps to 0 in the ODIM convention: denote areas where the quantity has a measured value of zero (radiated and value zero detected or inferred).
#'   }
#'   It depends on a radar's detection threshold or signal to noise ratio whether it safe to assume an 'undetect' is equivalent
#'   to zero. When dealing with close range data only (within 35 km), it is typically safe
#'   to assume aerial densities (dens) and reflectivities (eta) are in fact zero in case of undetects.
#' }
#'
#' \subsection{Other useful functionality}{
#'  \itemize{
#'  \item \link{suntime} calculates runrise and sunset times
#'  \item \link{checkDocker} checks whether your local Docker daemon is running correctly
#'  \item \link{elangle} gives the elevation angle(s) of a polar volume or polar scan object
#'  \item \link{beam_height} gives the radar beam height, for a certain elevation and range.
#'  \item \link{beam_width} gives the radar beam width, for a certain range.
#'  }
#' }
#' \subsection{Example datasets}{
#' \itemize{
#' \item \link{SCAN}: example object of class \link[=summary.scan]{scan}.
#' \item \link{VP}: example object of class \link[=summary.vp]{vp} as generated by \link{vol2bird}.
#' \item \link{VPTS}: example object of class \link[=summary.vpts]{vpts}.
#' \item \code{profile.h5}: example hdf5 file containing a vertical profile generated by \link{vol2bird}. Locate this file in your local installation with \code{system.file("extdata", "profile.h5", package="bioRad")}. Read it with \link{readvp}.
#' \item \code{volume.h5}: example hdf5 file containing a polar volume. Locate this file in your local installation with \code{system.file("extdata", "volume.h5", package="bioRad")}. Read it with \link{read.pvol}.
#' \item \code{VPTable.txt}: example standard output of \link{vol2bird} piped to a text file. Locate this file in your local installation with \code{system.file("extdata", "VPtable.txt", package="bioRad")}. Read it with \link{readvp.table}.
#' }
#' }
#'
#' @references
#' \itemize{
#'  \item Bird migration flight altitudes studied by a network of operational weather radars, Dokter et al., J. R. Soc. Interace 8 (54), pp. 30--43, 2011. DOI \href{http://dx.doi.org/10.1098/rsif.2010.0116}{10.1098/rsif.2010.0116}
#' }
#'
#' @import stats
#' @import rhdf5
#' @import fields
#' @import methods
#' @import graphics
#' @import ggplot2
#' @import ggmap
#' @import rgdal
#' @import sp
#' @import utils
#' @importFrom raster rasterToPoints
#' @importFrom raster raster
#' @importFrom curl curl_download
#' @importFrom grDevices col2rgb colorRampPalette rgb
#'
"_PACKAGE"
#> [1] "_PACKAGE"
