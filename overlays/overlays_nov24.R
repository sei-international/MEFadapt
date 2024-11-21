# Overlay Analysis for Water Stress, Mining, and Energy Data
# Author: Rafaela Flach
# Date: November 24, 2023
# Description: This script performs spatial analysis and visualization of water stress,
# mining, and energy data using various R packages.


# ==================================
# Setup
# ==================================

libraries <- c(
  "bslib", "Rcpp", "reticulate", "exactextractr",
  "aws.s3", "lubridate", "tibble", "stringi",
  "aws.signature", "ggpubr", "stringr", "janitor",
  "magrittr", "tidyverse", "dplyr", "sf", "raster"
)

lapply(libraries, library, character.only = TRUE)

bucket <- "mefadapt"

aws.signature::use_credentials(profile = "mefadapt")

Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))


# ==================================
# input data - general
# ==================================

# get country outlines

countries <- st_read("C:/Users/RafaelaFlach/OneDrive - SEI/Local Files/data/gadm_410-levels.gpkg", layer = "ADM_0")


sf_use_s2(FALSE)

countries_simple <- st_simplify(countries, dTolerance = 0.1)

# ==================================
# input data - commodities
# ==================================

tmpdir <- "C:/Users/RafaelaFlach/Downloads/tempdir/overlays/"

# --------------------- mining --------------------------------

mining <- save_object(
  object = "repository/global_analysis/mining/sp_mining.gpkg",
  bucket = bucket,
  file = paste0(tmpdir, "sp_mining.gpkg")
)
mininggis <- st_read(paste0(tmpdir, "sp_mining.gpkg"))
miningcsv <- s3read_using(
  FUN = read.csv,
  object = "repository/global_analysis/mining/sp_mining.csv",
  bucket = bucket
)
# --------------------- energy --------------------------------


energy <- save_object(
  object = "repository/global_analysis/energy/sp_energy.gpkg",
  bucket = bucket,
  file = paste0(tmpdir, "sp_energy.gpkg")
)
energygis <- st_read(paste0(tmpdir, "sp_energy.gpkg"))

energycsv <- s3read_using(
  FUN = read.csv,
  object = "repository/global_analysis/energy/sp_energy.csv",
  bucket = bucket
)

# --------------------- food --------------------------------

foodgis <- save_object(
  object = "repository/raw_data/AGRICULTURE/gsap/breadbaskets/v1/gis.gpkg",
  bucket = bucket,
  file = paste0(tmpdir, "breadbaskets_gis.gpkg")
)
foodgis <- st_read(paste0(tmpdir, "breadbaskets_gis.gpkg"))

foodcsv <- s3read_using(
  FUN = read.csv,
  object = "repository/raw_data/AGRICULTURE/gsap/breadbaskets/v1/tables.csv",
  bucket = bucket
)

food <- foodgis %>%
  left_join(., foodcsv) %>%
  st_as_sf()

# ==================================
# input data - impacts and resources
# ==================================

# water stress

future_annual <- s3read_using(st_read,
  layer = "future_annual",
  object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gpkg",
  bucket = bucket
)

future_annual <- st_read(paste0(tmpdir, "Aq40_Y2023D07M05.gpkg"), layer = "future_annual")


# tropical storms

let <- save_object(
  object = "repository/raw_data/CLIMATE/isimip/lange2020_ke-tg-meanfield_hadgem2-es_ewembi_rcp60_nosoc_co2_let_landonly_annual_2006_2099_avg.tif",
  bucket = bucket,
  file = paste0(tmpdir, "let.tif")
)
let <- raster(paste0(tmpdir, "let.tif"))

# ==================================
# processing
# ==================================

# mining first

miningindices <- exactextractr::exact_extract(let, mininggis, fun = "mean")















# some statistics on mining and energy

enstats <- energy %>%
  st_drop_geometry() %>%
  dplyr::select(-PLANT_KEY) %>%
  unique() %>%
  filter(FUEL_TYPE != "Energy Storage") %>%
  mutate(
    PLANNED_CAPACITY = as.numeric(PLANNED_CAPACITY),
    OPER_CAPACITY_PLANT = as.numeric(OPER_CAPACITY_PLANT)
  ) %>%
  group_by(OPER_STATUS, FUEL_TYPE) %>%
  summarize(
    planned_capacity = sum(PLANNED_CAPACITY, na.rm = T),
    operating_capacity = sum(OPER_CAPACITY_PLANT, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(grepl("Planned|Operating|Post", OPER_STATUS)) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(planned_capacity:operating_capacity, names_to = "Status", values_to = "Capacity") %>%
  mutate(Capacity = Capacity / 1000) %>%
  arrange(desc(Capacity))

library(ggpubr)
ggbarplot(enstats,
  x = "FUEL_TYPE", y = "Capacity",
  fill = "Status", color = "Status",
  combine = F, palette = c("#6fc8c9", "#196683")
) +
  labs(x = "Fuel type", y = "Capacity (GW)")

minstats <- mining %>%
  st_drop_geometry() %>%
  group_by(DEV_STAGE, PRIMARY_COMMODITY) %>%
  summarize(sumcapacity = sum(MILL_CAPACITY_TONNES_PER_YEAR, na.rm = T)) %>%
  ungroup()





future_annual_simple <- st_simplify(future_annual, dTolerance = 0.1)
