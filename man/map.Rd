% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{map}
\alias{map}
\alias{map.ppi}
\title{Map a plan position indicator (\code{ppi}) on a map}
\usage{
map(x, ...)

\method{map}{ppi}(
  x,
  map = "cartolight",
  param,
  alpha = 0.7,
  xlim,
  ylim,
  zlim = c(-20, 20),
  ratio,
  radar_size = 3,
  radar_color = "#202020",
  n_color = 1000,
  palette = NA,
  zoomin = -2,
  cachedir = tools::R_user_dir("bioRad"),
  ...
)
}
\arguments{
\item{x}{A \code{ppi} object.}

\item{...}{Arguments passed to \code{\link[ggplot2:ggplot]{ggplot2::ggplot()}}.}

\item{map}{Basemap to use, one of \code{\link[rosm:deprecated]{rosm::osm.types()}}}

\item{param}{Character. Scan parameter to plot, e.g. \code{DBZH} or \code{VRADH}. See
\code{\link[=summary.param]{summary.param()}} for commonly available parameters.}

\item{alpha}{Numeric. Transparency of the data, value between 0 and 1.}

\item{xlim}{Numeric vector of length 2. Range of x values (degrees longitude)
to plot.}

\item{ylim}{Numeric vector of length 2. Range of y values (degrees latitude)
to plot.}

\item{zlim}{Numeric vector of length 2. The range of values to plot.}

\item{ratio}{Numeric. Aspect ratio between x and y scale, by default
\eqn{1/cos(latitude radar * pi/180)}.}

\item{radar_size}{Numeric. Size of the symbol indicating the radar position.}

\item{radar_color}{Character. Color of the symbol indicating the radar
position.}

\item{n_color}{Numeric. Number of colors (>=1) to use in the palette.}

\item{palette}{Character vector. Hexadecimal color values defining the plot
color scale, e.g. output from \code{\link[viridisLite:viridis]{viridisLite::viridis()}}.}

\item{zoomin}{Numeric. Maps to \code{\link[ggspatial:annotation_map_tile]{ggspatial::annotation_map_tile()}}}

\item{cachedir}{Character. Maps to \code{\link[ggspatial:annotation_map_tile]{ggspatial::annotation_map_tile()}}, defaults to
\code{tools::R_user_dir("bioRad")}}
}
\value{
A ggplot object
}
\description{
Plots a plan position indicator (\code{ppi}) on a base layer
}
\details{
Available scan parameters for mapping can by printed to screen by
\code{summary(x)}. Commonly available parameters are:
\itemize{
\item \code{DBZH}, \code{DBZ}: (Logged) reflectivity factor (dBZ)
\item \code{TH}, \code{T}: (Logged) uncorrected reflectivity factor (dBZ)
\item \code{VRADH}, \code{VRAD}: Radial velocity (m/s). Radial velocities towards the radar
are negative, while radial velocities away from the radar are positive
\item \code{RHOHV}: Correlation coefficient (unitless) Correlation between vertically
polarized and horizontally polarized reflectivity factor
\item \code{PHIDP}: Differential phase (degrees)
\item \code{ZDR}: (Logged) differential reflectivity (dB)
The scan parameters are named according to the OPERA data information
model (ODIM), see Table 16 in the
\href{https://github.com/adokter/vol2bird/blob/master/doc/OPERA2014_O4_ODIM_H5-v2.2.pdf}{ODIM specification}.
}
}
\section{Methods (by class)}{
\itemize{
\item \code{map(ppi)}: Plot a \code{ppi} object on a map.

}}
\examples{
# Project a scan as a ppi
ppi <- project_as_ppi(example_scan)
\donttest{
if (all(sapply(c("ggspatial","prettymapr", "rosm"), requireNamespace, quietly = TRUE))) {
# Choose a basemap
basemap <- rosm::osm.types()[1]

# Map the radial velocity of the ppi onto the basemap
map(ppi, map = basemap, param = "VRADH")

# Extend the plotting range of velocities, from -50 to 50 m/s
map(ppi, map = basemap, param = "VRADH", zlim = c(-50, 50))

# Map the reflectivity
map(ppi, map = basemap, param = "DBZH")

# Change the color palette to Viridis colors
map(ppi, map = basemap, param = "DBZH", palette = viridis::viridis(100), zlim=c(-10,10))

# Give the data more transparency
map(ppi, map = basemap, param = "DBZH", alpha = 0.3)

# Change the appearance of the symbol indicating the radar location
map(ppi, map = basemap, radar_size = 5, radar_color = "blue")

# Crop the map
map(ppi, map = basemap, xlim = c(12.4, 13.2), ylim = c(56, 56.5))
}
}
}
\seealso{
\itemize{
\item \code{\link[=project_as_ppi]{project_as_ppi()}}
}
}
