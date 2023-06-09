# Define the URLs
urls <- c(
  "https://lw-enram.s3-eu-west-1.amazonaws.com/be/jab/2016/09/19/23/bejab_vp_201609192315.h5",
  "https://lw-enram.s3-eu-west-1.amazonaws.com/be/jab/2016/09/19/23/bejab_vp_201609192320.h5",
  "https://lw-enram.s3-eu-west-1.amazonaws.com/cz/brd/2016/09/19/00/czbrd_vp_20160919T0000Z_0x5.h5",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202303.csv.gz",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bejab/2023/bejab_vpts_202304.csv.gz",
  "https://aloft.s3-eu-west-1.amazonaws.com/baltrad/monthly/bewid/2023/bewid_vpts_202303.csv.gz"
)


guess_file_type <- function(file_path, n_lines = 5) {
  # Check if it's an HDF5 or gzip file by looking at the first few bytes
  first_bytes <- readBin(file_path, "raw", n = 10)

  # HDF5 files typically start with the string "\211HDF\r\n\032\n"
  if (identical(first_bytes[1:8], charToRaw("\211HDF\r\n\032\n"))) {
    return("h5")
  }
  # Gzip files typically start with the magic number 1f 8b
  if (identical(first_bytes[1:2], as.raw(c(0x1f, 0x8b)))) {
    return("gz")
  }
  # If it's not an HDF5 or gzip file, check if it's a CSV file
  first_lines <- readLines(file_path, n = n_lines)

  # If any of the lines contains a comma, assume it's a CSV file
  if (any(grepl(",", first_lines))) {
    return("csv")
  } else {
    return("txt")
  }
}


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