# Load packages

library(sf)
library(raster)
library(leaflet)
library(plotly)
library(DT)
library(aws.s3)
options(scipen = 999)
library(tidyverse)
library(conflicted)
library(exactextractr)
library(terra)
library(geodata)

# S3 settings
bucket <- "mefadapt"
prefix <- "repository/global_analysis/"

aws.signature::use_credentials(profile = "mefadapt")
Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))

# Ensure dplyr's select is used when there is ambiguity
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(terra::extract)


# download data from GADM
# US, China, Germany, and Turkey
countrylist <- c("USA","CHN","DEU","TUR")
level_list <- c("2","3","3","2")


localfolder <- "C:/Users/RafaelaFlach/Downloads/tempdir/"

download.gadm <- function(x) {
    gadm(countrylist[x],
        level = level_list[x],
        path = paste0(localfolder,"gadm/"),
        overwrite = TRUE
    ) %>% st_as_sf()
}
maps <- lapply(seq_along(countrylist), download.gadm)


##########################################################
# Agriculture data processing
##########################################################

# read data for agriculture

path_ag <- "repository/raw_data/AGRICULTURE/cropgrids/cropgrids/CROPGRIDSv1.08_NC_maps/CROPGRIDSv1.08_NC_maps/"

agriculture <- get_bucket(
    bucket = bucket,
    prefix = path_ag
) %>%
    map(function(x) {
        x$Key
    }) %>%
    unlist() %>%
    data.frame(paths = .) %>%
    filter(!grepl("Countries", paths))

crops <- lapply(seq_along(agriculture$paths), function(x) {
    local_file <- paste0(
        localfolder, "cropgrids/",
        gsub(path_ag, "", agriculture$paths[x])
    )
    # dir.create(dirname(local_file), recursive = TRUE, showWarnings = FALSE)
    # aws.s3::save_object(
    #      bucket = bucket,
    #      object = agriculture$paths[x],
    #      file = local_file
    # )
    terra::rast(local_file)
})

nnames <- agriculture %>%
    mutate(name = gsub(path_ag, "", paths)) %>%
    mutate(name = gsub(".nc", "", name)) %>%
    mutate(name = gsub("CROPGRIDSv1.08_", "", name)) %>%
    pull(name)

ha <- lapply(seq_along(crops), function(i) {
    return(raster(crops[[i]]$harvarea))
}) %>% stack()

ca <- lapply(seq_along(crops), function(i) {
    return(raster(crops[[i]]$croparea))
}) %>% stack()

names(ha) <- nnames
names(ca) <- nnames

writeRaster(ha, paste0(localfolder, "harvarea.tif"), overwrite = TRUE)
writeRaster(ca, paste0(localfolder, "croparea.tif"), overwrite = TRUE)


s3write_using(
    FUN = writeRaster,
    x = ha,
    bucket = bucket,
    object = paste0(prefix, "agriculture/harvarea.tif"),
    overwrite = TRUE
)
s3write_using(
    FUN = writeRaster,
    x = ca,
    bucket = bucket,
    object = paste0(prefix, "agriculture/croparea.tif"),
    overwrite = TRUE
)

hh <- as(ha, "SpatRaster")
ch <- as(ca, "SpatRaster")

# Extract statistics for each crop


for (i in seq_along(countrylist)) {
    # crop hh and ch rasters to the extent of the current country's map
    print(i)
    b <- st_bbox(maps[[i]])
    e <- terra::ext(b["xmin"], b["xmax"], b["ymin"], b["ymax"])
    hhh <- terra::crop(hh, e)
    chh <- terra::crop(ch, e)
    # Replace -1 values with NA in the cropped SpatRaster objects
    hhh[hhh == -1] <- NA
    chh[chh == -1] <- NA
    ha_values <- exact_extract(hhh, maps[[i]],
        fun = "sum"
    )
    ca_values <- exact_extract(chh, maps[[i]],
        fun = "sum"
    )
    harvarea_df <- as.data.frame(ha_values)
    croparea_df <- as.data.frame(ca_values)
    colnames(harvarea_df) <- paste0("harvarea_", names(harvarea_df))
    colnames(croparea_df) <- paste0("croparea_", names(croparea_df))
    df <- cbind(
        maps[[i]] %>% dplyr::select(all_of(c("GID_0", paste0("GID_", level_list[i])))),
        croparea_df, harvarea_df
    ) %>% st_drop_geometry() %>%
        mutate(GID_level = paste0("GID_", level_list[i])) %>%
        setNames(gsub(".sum", "", names(.)))
    colnames(df)[1:2] <- c("GID_0", "GID_code")
    df <- df %>% select(
        GID_0, GID_level, GID_code,
        starts_with("harv"), starts_with("croparea")
    )
    s3write_using(
        FUN = write_delim, delim = ";",
        x = df,
        bucket = bucket,
        object = paste0(prefix, "agriculture/agriculture_stats_", countrylist[i], ".csv"))
}

df <- lapply(seq_along(countrylist), function(i) {
    print(i)
    s3read_using(
        FUN = read_delim,delim = ";",
        bucket = bucket,
        object = paste0(prefix, "agriculture/agriculture_stats_", countrylist[i], ".csv")
    )
}) %>%
    do.call(rbind, .) %>% select(-geometry)

mmaps <- lapply(seq_along(countrylist), function(i) {
    maps[[i]] %>%
        select(all_of(c("GID_0", paste0("GID_", level_list[i])))) %>%
        rename(GID_code = paste0("GID_", level_list[i]))
}) %>% do.call(rbind, .)

dff <- left_join(st_as_sf(mmaps), df, by = c("GID_0", "GID_code"))

numeric_cols <- names(df)[sapply(df, is.numeric)]
cols_to_remove <- numeric_cols[sapply(
    df[numeric_cols],
    function(x) sum(x, na.rm = TRUE)
) == 0]

dff_f <- dff %>%
    dplyr::select(-all_of(cols_to_remove))


# Save the agriculture statistics to S3 as a GeoPackage file

s3write_using(
    FUN = st_write,
    x = dff_f,
    bucket = bucket,
    object = paste0(prefix, "agriculture/agriculture_stats.gpkg"),
    layer = "agriculture_stats",
    driver = "GPKG",
    row.names = FALSE
)


# Save the agriculture statistics to S3 as a CSV file

s3write_using(
    FUN = write.csv,
    x = df,
    bucket = bucket,
    object = paste0(prefix, "agriculture/agriculture_stats.csv"),
    row.names = FALSE
)






##########################################################
# Mining and energy data processing
##########################################################

mines <- s3read_using(
    FUN = read.csv,
    row.names = 1,
    bucket = bucket,
    object = paste0(prefix, "mining/mines_classes.csv")
) %>%
filter(!is.na(LONGITUDE) & !is.na(LATITUDE)) %>%
    mutate(LONGITUDE = as.numeric(LONGITUDE),
           LATITUDE = as.numeric(LATITUDE))

energy <- s3read_using(
    FUN = read.csv,
    row.names = 1,
    bucket = bucket,
    object = paste0(prefix, "energy/sp_energy.csv")
)%>%
filter(!is.na(LONGITUDE) & !is.na(LATITUDE)) %>%
    mutate(LONGITUDE = as.numeric(LONGITUDE),
           LATITUDE = as.numeric(LATITUDE))


# Convert the energy dataset to an sf object assuming the coordinate columns are "lon" and "lat"
energy_sf <- st_as_sf(energy, coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(maps))
mines_sf <- st_as_sf(mines, coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(maps))
# Select only the GID_* columns from the maps object
maps_gid <- maps %>%
    do.call(rbind, .) %>%
    st_as_sf() %>%
    dplyr::select(starts_with("GID_"))

sf::sf_use_s2(FALSE)

# Perform a spatial join to attach the GID_* attributes to the energy data
energy_joined <- st_intersection(energy_sf, maps_gid, left = FALSE)
mines_joined <- st_intersection(mines_sf, maps_gid, left = FALSE)

# Save the updated energy data with GID_* columns
s3write_using(
    FUN = st_write,
    x = energy_joined,
    bucket = bucket,
    object = paste0(prefix, "energy/sp_energy_with_gid.gpkg"),
    row.names = FALSE
)
s3write_using(
    FUN = st_write,
    x = mines_joined,
    bucket = bucket,
    object = paste0(prefix, "mining/mines_with_gid.gpkg"),
    row.names = FALSE
)
