
# Function to download a file
download_test_file <- function(url, dest_dir, h5_dir, csv_dir) {
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
