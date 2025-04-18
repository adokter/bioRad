% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_precip.R
\name{filter_precip}
\alias{filter_precip}
\title{Posthoc precipitation filter}
\usage{
filter_precip(
  x,
  dbz = ifelse(x$attributes$how$wavelength < 7 || is.null(x$attributes$how$wavelength),
    7, 20),
  range = 2500,
  alt_max = 3000,
  drop = FALSE
)
}
\arguments{
\item{x}{A \code{vp} or \code{vpts} object.}

\item{dbz}{The minimum reflectivity factor for precipitation in units dBZ.
Defaults to 7 dBZ for C-band radars and 20 dBZ for S-band radars.}

\item{range}{The minimum altitude range with total reflectivity factor.
\code{DBZH} > \code{dbz} at which the filter is triggered in m.}

\item{alt_max}{Maximum altitude above ground level to consider in m.}

\item{drop}{When \code{TRUE} the profile is removed from the}
}
\value{
A \code{vpts} object or a \code{vp} object, depending on input \code{x}.
}
\description{
The posthoc precipitation filter assesses how much of the altitude column has a high
total reflectivity factor (biology + meteorology) consistent with precipitation,
and removes biological signals when there is evidence for the presence of precipitation.
Applied to vertical profiles ('vp') or time series of vertical profiles ('vpts').
}
\details{
During precipitation events usually a high proportion of the altitude
column will show a high total reflectivity \code{DBZH} (which includes biology +
meteorology), because precipitation usually falls from several kilometers high
to the ground surface. Precipitation events are often obvious in profile plots
of quantity \code{DBZH} as reflectivity signals extending from ground level to high
altitudes far above the typical altitudes where biology is expected. This filter
identifies and removes these cases.

The posthoc precipitation filter examines the total reflectivity factor \code{DBZH}
and calculates the altitude range at which \code{DBZH} is larger than parameter
\code{dbz}. Whenever this altitude range is larger than parameter \code{range} (and \code{drop}
is \code{FALSE}), the biology is removed by setting profile quantities \code{dens} and \code{eta}
to zero and profile quantity \code{dbz} to \code{-Inf}. When parameter \code{drop}
is \code{TRUE}, the profile is removed from the time series altogether.

This posthoc precipitation filter is likely to remove biological scatterers
that co-occur with precipitation, for example biological movements during isolated
convective thunderstorms. It is more aggressive than precipitation filters
applied during vertical profile generation with \code{calculate_vp()} that attempt to
remove precipitation and retain biology. The posthoc precipitation filter is especially
useful for analyses where you want to minimize the risk of precipitation contamination,
at the cost of filtering out some biology during precipitation events.

Lowering the default minimum reflectivity (\code{dbz}) for precipitation
below 7 dBZ is not recommended, as most precipitation has a reflectivity above 7 dBZ.

Parameter \code{range} should be chosen carefully, and should be higher than the
typical altitude where biological scatterers still reach a reflectivity factor equal to \code{dbz}.

Note that at S-band wavelengths bird migration occurs much more frequently in the reflectivity
regime for precipitation than at C-band. Therefore, at C-band lower settings for parameter \code{dbz}
are appropriate than at S-band.
}
\examples{
# rain periods are visible in quantity DBZH as dark vertical lines
# extending above 3 km height:
plot(regularize_vpts(example_vpts), quantity='DBZH')
# Apply posthoc filter to remove profiles during precipitation events:
# (regularization is applied to visualize removed profiles)
my_vpts <- regularize_vpts(filter_precip(example_vpts, drop=TRUE))
# verify that rain events have been removed:
plot(my_vpts, quantity='DBZH')
# the posthoc filter removes some biology during precipitation:
plot(my_vpts, quantity='dens')
# original retains more biology (but at the cost of a higher
# likelihood of occasional precipitation contamination):
plot(regularize_vpts(example_vpts), quantity='dens')
# filter can also be applied to single vp objects:
filter_precip(example_vp)
}
\seealso{
\itemize{
\item \link{eta_to_dbz}
\item \link{dbz_to_eta}
}
}
