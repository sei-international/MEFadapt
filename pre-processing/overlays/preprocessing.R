# ==================================
# Setup
# ==================================

libraries <- c(
    "bslib", "Rcpp", "reticulate", "stars",
    "aws.s3", "lubridate", "tibble", "stringi",
    "aws.signature", "ggpubr", "stringr", "janitor",
    "magrittr", "tidyverse", "dplyr", "sf", "raster",
    "terra"
)

lapply(libraries, library, character.only = TRUE)

bucket <- "mefadapt"

aws.signature::use_credentials(profile = "mefadapt")

Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))



# aqueduct

future_annual <- s3read_using(st_read,
    object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/aqueduct_future_annual.gpkg",
    bucket = bucket
)

tmpdir <- tempdir()

test <- future_annual %>% dplyr::select(bau50_ws_x_r, bau50_ws_x_l)

ras <- rast(res = 0.5, vals = 0, ext = ext(future_annual))

f_annual <- terra::rasterize(test, ras, "bau50_ws_x_r", background = NA)

s3write_using(f_annual, terra::writeRaster,
    object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05_bau50_ws_x_r.tif",
    bucket = bucket
)
f_annual <- terra::rasterize(test, ras, "bau50_ws_x_l", background = NA)

s3write_using(f_annual, terra::writeRaster,
    object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05_bau50_ws_x_l.tif",
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
