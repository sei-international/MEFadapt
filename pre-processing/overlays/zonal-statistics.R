# ==================================
# Setup
# ==================================

libraries <- c(
    "bslib", "Rcpp", "reticulate",
    "aws.s3", "lubridate", "tibble", "stringi",
    "aws.signature", "ggpubr", "stringr", "janitor",
    "magrittr", "tidyverse", "dplyr", "sf", "raster",
    "fasterize","terra"
)

lapply(libraries, library, character.only = TRUE)

bucket <- "mefadapt"

aws.signature::use_credentials(profile = "mefadapt")

Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))




# ####### ####################################################
# # WATER
# ####### ####################################################


future_annual <- s3read_using(st_read,
    object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05_future.gpkg",
    bucket = bucket
) %>%
    mutate(across(where(is.numeric), ~ na_if(., -9999)))


baseline_annual <-s3read_using(st_read,
    object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05_baseline.gpkg",
    bucket = bucket
) %>%
    mutate(across(where(is.numeric), ~ na_if(., -9999)))





cats <- baseline_annual %>%
    st_drop_geometry() %>%
    dplyr::select(bws_cat, bws_label) %>% unique



mines_file <- "sp_mining.gpkg"
s3_prefix <- "repository/global_analysis/mining-casestudy/input/"
s3_bucket <- "mefadapt"

mi.shp <- aws.s3::s3read_using(st_read,
    bucket = s3_bucket,
    object = paste0(s3_prefix, mines_file)
) %>% dplyr::select(PROP_ID, geom) %>% st_make_valid()

future_annual <- future_annual %>% st_make_valid()
baseline_annual <- baseline_annual %>% st_make_valid()

sf::sf_use_s2(FALSE)

mi.shp.fut <- st_join(
    mi.shp,
    future_annual %>% dplyr::select(bau80_ws_x_c, pes80_ws_x_c),
    join = st_crosses, largest = TRUE
) %>% st_drop_geometry()

mi.shp.bas <- st_join(mi.shp,
    baseline_annual %>% dplyr::select(bws_cat),
    join = st_crosses, largest = TRUE
) %>%
    st_drop_geometry()


ws <- full_join(mi.shp.fut, mi.shp.bas, by = "PROP_ID")

s3write_using(ws, write.csv,
    object = paste0(s3_prefix, "water_mining.csv"),
    bucket = s3_bucket
)

s3write_using(cats, write.csv,
    object = paste0(s3_prefix, "water_cats.csv"),
    bucket = s3_bucket
)


# ####### ####################################################
# # Climate
# ####### ####################################################
tmpdir <- "C:/Users/RafaelaFlach/Downloads/tempdir/"

hazards <- c("ler", "let", "leh", "led")
times <- c("baseline", "rcp26", "future")

aws.s3::save_object(
    object = paste0(s3_prefix, "hazard_stack.tif"),
    bucket = s3_bucket,
    file = paste0(tmpdir, "hazard_stack.tif")
)
hazard_stack <- stack(paste0(tmpdir, "hazard_stack.tif"))
names(hazard_stack) <- lapply(hazards, function(x) paste(x, times, sep = "_")) %>% unlist()


mines_st <- mi.shp %>%
    st_drop_geometry() %>%
    dplyr::select(PROP_ID)

# Convert sf to SpatVector for terra compatibility
points_vect <- vect(mi.shp)
stck <- rast(hazard_stack)

# Extract raster values at point locations
values_df <- extract(stck, points_vect)

# Drop ID column if needed and bind to original sf object
# terra::extract() returns a data.frame with an ID column by default
values_df <- values_df[, -1] # remove ID column

# Bind as new columns to the original sf object
points_with_values <- cbind(mines_st, values_df)



s3write_using(points_with_values, write.csv,
    object = paste0(s3_prefix, "climate_mining.csv"),
    bucket = s3_bucket
)
 