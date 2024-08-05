#################################################################################
#
#   preprocessing of S_P data
#
#################################################################################



libraries <- c(
    "sf", "aws.s3", "dplyr", "stringi", "aws.signature",
    "stringr", "tidyverse", "conflicted", "reshape2", "readxl"
)

lapply(libraries, library, character.only = TRUE)


source("credentials.R")
bucket <- "mefadapt"

conflicts_prefer(dplyr::filter)




## ==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==
# ENERGY
## ==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==

en.files <- get_bucket(
    bucket = "mefadapt",
    prefix = "repository/raw/ENERGY/SP/batch_1/"
) %>%
    map(pluck(1)) %>%
    unlist() %>%
    data.frame() %>%
    setNames("files") %>%
    filter(grepl(".xls", files))

files1 <- en.files %>%
    filter(!grepl("_2.", files)) %>%
    pull(files)
files2 <- en.files %>%
    filter(grepl("_2.", files)) %>%
    pull(files)

readxl <- function(fil, sk) {
    print(fil)
    p <- s3read_using(readxl::read_xls,
        skip = sk, col_names = T, col_types = "text",
        object = fil,
        bucket = bucket
    ) %>% filter(!is.na(PLANT_KEY))
    return(p)
}

f1 <- lapply(files1, function(x) readxl(x, 4)) %>% do.call(rbind, .)

f2 <- lapply(files2, function(x) readxl(x, 4)) %>% do.call(rbind, .)

en.table <- cbind(
    f1 %>% arrange(PLANT_KEY, POWER_PLANT),
    f2 %>% arrange(PLANT_KEY, POWER_PLANT)
) %>% select(-c(6, 7))


en.gis <- en.table %>%
    mutate(lat = as.numeric(LATITUDE), lon = as.numeric(LONGITUDE)) %>%
    filter(!is.na(lat)) %>%
    filter(!is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

s3write_using(
    en.table,
    write.csv,
    object = "repository/processed/energy/sp_energy.csv",
    bucket = bucket
)

s3write_using(
    en.gis,
    st_write,
    object = "repository/processed/energy/sp_energy.gpkg",
    bucket = bucket
)

## ==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==
# MINING
## ==##==##==##==##==##==##==##==##==##==##==##==##==##==##==##==


mi.files <- get_bucket(
    bucket = "mefadapt",
    prefix = "repository/raw/MINERALS/SP/batch_1/"
) %>%
    map(pluck(1)) %>%
    unlist() %>%
    data.frame() %>%
    setNames("files") %>%
    filter(grepl(".xls", files)) %>%
    pull(files)

readxl <- function(fil, sk) {
    print(fil)
    p <- s3read_using(readxl::read_xls,
        skip = sk, col_names = T, col_types = "text",
        object = fil,
        bucket = bucket
    )
    return(p)
}

f1 <- lapply(mi.files, function(x) readxl(x, 4))

mi.table <- cbind(
    f1[[1]],
    f1[[2]] %>% select(-c(PROP_NAME, PROP_ID)),
    f1[[3]] %>% select(-c(PROP_NAME, PROP_ID)),
    f1[[4]] %>% select(-c(PROP_NAME, PROP_ID))
) %>%
    data.frame() %>%
    filter(!is.na(PROP_ID))



mi.gis <- mi.table %>%
    mutate(lat = as.numeric(LATITUDE), lon = as.numeric(LONGITUDE)) %>%
    filter(!is.na(lat)) %>%
    filter(!is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

s3write_using(
    mi.table,
    write.csv,
    object = "repository/processed/mining/sp_mining.csv",
    bucket = bucket
)

s3write_using(
    mi.gis,
    st_write,
    object = "repository/processed/mining/sp_mining.gpkg",
    bucket = bucket
)
