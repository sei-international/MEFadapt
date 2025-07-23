# Load required libraries



libraries <- c(
    "aws.s3", "lubridate", "tibble",
    "stringi", "aws.signature", "stringr", "janitor",
    "magrittr", "tidyverse",
    "dplyr", "httr", "raster", "sf"
)

lapply(libraries, library, character.only = TRUE)


# Set AWS credentials (replace with your credentials)
aws.signature::use_credentials(profile = "mefadapt")
Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))

# Define the S3 bucket and file path for the input table
bucket <- "mefadapt"
folder <- "repository/raw_data/CLIMATE/flood_risk/"

# Read the table from the S3 bucket
file_keys <- get_bucket(bucket = bucket, prefix = folder) %>%
    map(~ .$Key) %>%
    keep(~ str_ends(.x, "\\.csv$")) %>%
    keep(~ str_detect(.x, "all_")) %>%
    discard(~ str_detect(.x, "5_v2")) %>%
    keep(~ str_detect(.x, "100yr")) %>%
    discard(~ str_detect(.x, "2050")) %>%
    unlist()

read_edit <- function(key) {
    print(key)
    file <- s3read_using(FUN = read.csv, object = key, bucket = bucket) %>%
        dplyr::select(
            shapeID, metric, scenario, peril, year, average_flood_depth,
            area_inundated_km2, average_depth_Pweighted, area_percent_inundated,
            area_fraction_inundated
        )
}

flood_data_list <- lapply(file_keys, read_edit) %>%
    do.call(rbind, .)

s3write_using(
    FUN = write.csv,
    x = flood_data_list,
    object = "repository/raw_data/CLIMATE/flood_risk/flood_risk_data_all.csv",
    bucket = bucket,
    row.names = FALSE
)
