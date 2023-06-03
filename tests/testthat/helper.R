# Define the URLs
urls <- c(
  "https://lw-enram.s3-eu-west-1.amazonaws.com/be/jab/2016/09/19/23/bejab_vp_201609192315.h5",
  "https://lw-enram.s3-eu-west-1.amazonaws.com/be/jab/2016/09/19/23/bejab_vp_201609192320.h5",
  "https://lw-enram.s3-eu-west-1.amazonaws.com/cz/brd/2016/09/19/00/czbrd_vp_20160919T0000Z_0x5.h5",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202303.csv.gz",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202304.csv.gz",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bewid/2023/bewid_vpts_202303.csv.gz"
)


# Define the path to the new temporary directory
temp_dir <- 'temp'

# Create the new directory if not exists
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

# Create the h5 and csv sub-directories
h5_dir <- file.path(temp_dir, "h5")
csv_dir <- file.path(temp_dir, "csv")
if (!dir.exists(h5_dir)) dir.create(h5_dir)
if (!dir.exists(csv_dir)) dir.create(csv_dir)

# Function to download a file
download_test_file <- function(url, dest_dir = temp_dir) {
  # Identify destination subdirectory based on file extension
  ext <- tools::file_ext(url)
  dest_sub_dir <- if (ext == "h5") h5_dir else if (ext == "gz") csv_dir else dest_dir
  
  # Determine destination file name
  file_name <- basename(url)
  dest_file <- file.path(dest_sub_dir, file_name)

  # Download the file if it doesn't already exist
  if (!file.exists(dest_file)) {
    message("Downloading ", url)
    curl::curl_download(url, destfile = dest_file)
  } 
}

sapply(urls, download_test_file)