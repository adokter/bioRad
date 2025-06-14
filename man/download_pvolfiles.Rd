% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_pvolfiles.R
\name{download_pvolfiles}
\alias{download_pvolfiles}
\title{Download polar volume (\code{pvol}) files from the NEXRAD archive}
\usage{
download_pvolfiles(
  date_min,
  date_max,
  radar,
  directory = ".",
  overwrite = FALSE,
  bucket = "noaa-nexrad-level2",
  directory_tree = TRUE
)
}
\arguments{
\item{date_min}{POSIXct. Start date of file selection. If no timezone are
provided, it will be assumed to be UTC.}

\item{date_max}{POSIXct. End date of file selection.If no timezone are
provided, it will be assumed to be UTC.}

\item{radar}{character (vector). 4-letter radar code(s) (e.g. "KAMA")}

\item{directory}{character. Path to local directory where files should be
downloaded}

\item{overwrite}{logical. TRUE for re-downloading and overwriting previously
downloaded files of the same names.}

\item{bucket}{character. Bucket name to use.}

\item{directory_tree}{logical. Whether to create the yyyy/mm/dd/radar
directory structure. TRUE by default.}
}
\value{
\code{NULL}. The function's primary effect is to download selected polar volume
files from the NEXRAD Level II archive to a specified local directory, and to provide
a message and a progress bar in the console indicating the download status.
}
\description{
Download a selection of polar volume (\code{pvol}) files from the
\href{https://registry.opendata.aws/noaa-nexrad/}{NEXRAD Level II archive data}.
}
\examples{
\donttest{
# create temporary directory
if (requireNamespace("aws.s3", quietly = TRUE)) {
temp_dir <- paste0(tempdir(),"/bioRad_tmp_files")
dir.create(temp_dir)
download_pvolfiles(
  date_min = as.POSIXct("2002-10-01 00:00", tz = "UTC"),
  date_max = as.POSIXct("2002-10-01 00:05", tz = "UTC"),
  radar = "KBRO",
  directory = temp_dir,
  overwrite = TRUE
)
# Clean up
unlink(temp_dir, recursive = TRUE)
}
}
}
