# ==================================
# Setup
# ==================================

libraries <- c(
    "bslib", "Rcpp", "reticulate",
    "aws.s3", "lubridate", "tibble", "stringi",
    "aws.signature", "ggpubr", "stringr", "janitor",
    "magrittr", "tidyverse", "dplyr", "sf", "raster"
)

lapply(libraries, library, character.only = TRUE)

bucket <- "mefadapt"

aws.signature::use_credentials(profile = "mefadapt")

Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))



# aqueduct from .gdb to gpkg

future_annual <- get_bucket("repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gdb/",
    bucket = bucket
) %>% map(~ .x$Key)

subdir <- "Aq40_Y2023D07M05.gdb/"
dir.create(file.path(tmpdir, subdir), showWarnings = FALSE)

lapply(future_annual[-1], function(x) {
    save_object(object = x, bucket = bucket, file = paste0(tmpdir, subdir, basename(x)))
})

future_annual <- st_read(paste0(tmpdir, subdir), layer = "future_annual")

s3write_using(future_annual, st_write,
    object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gpkg",
    bucket = bucket
)

# isimip from annual to average


let <- save_object(
    object = "repository/raw_data/CLIMATE/isimip/lange2020_ke-tg-meanfield_hadgem2-es_ewembi_rcp60_nosoc_co2_let_landonly_annual_2006_2099.nc4",
    bucket = bucket,
    file = paste0(tmpdir, "let.nc4")
)
let <- stack(paste0(tmpdir, "let.nc4"))

let_avg <- calc(let, fun = mean, na.rm = TRUE)

s3write_using(let_avg, writeRaster,
    object = "repository/raw_data/CLIMATE/isimip/lange2020_ke-tg-meanfield_hadgem2-es_ewembi_rcp60_nosoc_co2_let_landonly_annual_2006_2099_avg.tif",
    bucket = bucket
)
