% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rcs.R
\name{rcs<-}
\alias{rcs<-}
\alias{rcs<-.vp}
\alias{rcs<-.list}
\alias{rcs<-.vpts}
\alias{rcs<-.vpi}
\title{Set radar cross section}
\usage{
rcs(x) <- value

\method{rcs}{vp}(x) <- value

\method{rcs}{list}(x) <- value

\method{rcs}{vpts}(x) <- value

\method{rcs}{vpi}(x) <- value
}
\arguments{
\item{x}{A \code{vp}, list of \code{vp}, \code{vpts} or \code{vpi} object.}

\item{value}{Numeric. The radar cross section value to assign in cm^2.}
}
\value{
The input object with updated density \code{x$data$dens} and updated radar cross section attribute.
}
\description{
Sets the assumed radar cross section of an object in cm^2. This function also
updates the migration densities in \code{x$data$dens} to \code{eta}/\code{rcs} when above
\code{sd_vvp_threshold} and \code{0} if below.
}
\examples{
# Set the radar cross section for a vp
vp <- example_vp
rcs(vp) <- 11

# Set the radar cross section for a vpts
vpts <- example_vpts
rcs(vpts) <- 11

# Set the radar cross section for a vpi
vpi <- integrate_profile(example_vpts)
rcs(vpi) <- 11
}
\seealso{
\itemize{
\item \code{\link[=rcs]{rcs()}} for getting the radar cross section of an object.
\item \code{\link[=sd_vvp_threshold<-]{sd_vvp_threshold()<-}}
}
}
