# Load required libraries



libraries <- c(
    "aws.s3", "lubridate", "tibble",
    "stringi", "aws.signature", "stringr", "janitor",
    "magrittr", "tidyverse",
    "dplyr", "httr","raster","sf"
)

lapply(libraries, library, character.only = TRUE)


# Set AWS credentials (replace with your credentials)
aws.signature::use_credentials(profile = "mefadapt")
Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))

# Define the S3 bucket and file path for the input table
bucket <- "mefadapt"
input_file <- "repository/raw_data/CLIMATE/filelist_isimip.csv"

# Read the table from the S3 bucket
input_table <- s3read_using(
    FUN = read_delim, delim = ";",
    object = input_file,
    bucket = bucket
)

# Construct the URLs based on the table's structure
# Assuming the table has columns 'base_url', 'path', and 'file_name'
path = "https://files.isimip.org/ISIMIP2b/DerivedOutputData/Lange2020/"
table <- input_table %>%
    mutate(url = paste0(
        path, paste(c1, c2, c3, sep = "/"), "/",
        apply(select(., c4:c15), 1, paste, collapse = "_")
    )) %>%
    mutate(file_name = paste0(
        apply(select(., c5:c15), 1, paste, collapse = "_")
    ))

urls <- table %>%
    select(url) %>% unlist %>% as.vector

# Define the local directory to store downloaded files
local_dir <- "C:/Users/RafaelaFlach/Downloads/isimip/"
dir.create(local_dir, showWarnings = FALSE)

# Download the data from the constructed URLs
for (i in seq_along(urls)) {
  url <- urls[i]
  local_file <- file.path(local_dir, table$file_name[i])
  response <- GET(url, write_disk(local_file, overwrite = TRUE))
  
  if (response$status_code == 200) {
    message("Downloaded: ", url)
  } else {
    warning("Failed to download: ", url)
  }
}

# Define the S3 bucket and folder for uploading the files
output_bucket <- bucket
output_folder <- "repository/raw_data/CLIMATE/isimip/raw/"

# Upload the downloaded files to the output S3 bucket
files <- list.files(local_dir, full.names = TRUE)
for (file in files) {
  s3write_using(FUN = writeBin, object = paste0(output_folder, basename(file)), 
                bucket = output_bucket, x = readBin(file, "raw", file.info(file)$size))
  message("Uploaded: ", basename(file))
}

# Define the origin and output folders
origin_folder <- "repository/raw_data/CLIMATE/isimip/raw/"
output_folder <- "repository/raw_data/CLIMATE/isimip/processed/"

# List all files in the origin folder
files <- get_bucket(bucket = bucket, prefix = origin_folder) %>%
    map(~ .x$Key) %>%
    unlist() %>%
    data.frame(Key = .) %>%
    filter(str_detect(Key, "\\.nc4$")) %>%
    pull(Key)


# Process each file to calculate the mean and save to the output folder
for (file in files) {
  # Download the file to a temporary directory
  # Download the file to a local directory
  local_file <- file.path(local_dir, basename(file))
  #save_object(object = file, bucket = bucket, file = local_file)
  
  # Read the file using the raster stack function
  raster_stack <- stack(local_file)
  # Calculate the mean
  raster_mean <- calc(raster_stack, fun = mean, na.rm = TRUE)
  
  # Define the output file path
  output_file <- paste0(output_folder, basename(file), "_mean.tif")
  
  # Save the mean raster to the output folder in S3
  s3write_using(raster_mean, writeRaster, object = output_file, bucket = bucket)
  
  message("Processed and uploaded: ", output_file)
}



# List all .tif files in the S3 folder
tif_files <- get_bucket(
    bucket = s3_bucket,
    prefix = s3_prefix
) %>%
    map(~ .x[["Key"]]) %>%
    unlist() %>%
    data.frame(Key = .) %>%
    filter(grepl("\\.tif", Key)) %>%
    pull(Key)





###################
#
# Select the hazard and time periods for mining hazard study
#
#####################

hazards0 <- c("flood", "storm", "heatwave", "drought")

hazards <- c("ler", "let", "leh", "led")

times <- c("historical", "rcp26", "rcp60")

filenmes <- lapply(hazards, function(i) {
    lapply(times, function(j) {
        data.frame(Key = tif_files) %>% filter(grepl(i, Key) & grepl(j, Key))
    })
}) %>% unlist()%>% unlist()

# Read, stack, and rename raster files
# Create a temporary directory for downloads
tmp_download_dir <- "C:/Users/RafaelaFlach/Downloads/tempdir/"

hazard_stack <- stack(lapply(
  filenmes,
  function(f) {
    local_file <- file.path(tmp_download_dir, basename(f))
    aws.s3::save_object(object = f, bucket = s3_bucket, file = local_file)
    raster(local_file)
  }
))
names(hazard_stack) <- lapply(hazards, function(x) paste(x, times, sep = "_")) %>% unlist()


# Save the hazard_stack raster to S3 as a GeoTIFF
s3write_using(
  hazard_stack,
  writeRaster,
  object = "repository/global_analysis/mining-casestudy/input/hazard_stack.tif",
  bucket = "mefadapt",
  format = "GTiff",
  overwrite = TRUE
)

