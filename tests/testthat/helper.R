
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

  ## If every line in n_lines contains a comma, assume it's a CSV file
  if (all(sapply(first_lines, function(line) grepl(",", line)))) {
    return("csv")

  } else {
    message('No extension detected; assuming file type .txt')
    return("txt")
  }
}

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
