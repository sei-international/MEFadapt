# ==================================
# Setup
# ==================================

libraries <- c(
    "bslib", "Rcpp", "reticulate",
    "aws.s3", "lubridate", "tibble", "stringi",
    "aws.signature", "ggpubr", "stringr", "janitor",
    "magrittr", "tidyverse", "dplyr", "sf", "raster",
    "fasterize","terra","conflicted"
)

lapply(libraries, library, character.only = TRUE)

bucket <- "mefadapt"

aws.signature::use_credentials(profile = "mefadapt")

Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))

# S3 settings
s3_bucket <- "mefadapt"
s3_prefix <- "repository/global_analysis/mining-casestudy/input/"

aws.signature::use_credentials(profile = "mefadapt")
Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))

# Ensure dplyr's select is used when there is ambiguity
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(terra::extract)




# Files and variables
mines_file <- "sp_mining.csv"
class_file <- "class_expanded.csv"

# Load and preprocess
expanded_cl <- s3read_using(read.csv,
    row.names = 1,
    bucket = s3_bucket,
    object = paste0(s3_prefix, class_file)
) %>% unique

mi.table <- aws.s3::s3read_using(read.csv,row.names = 1,
    bucket = s3_bucket,
    object = paste0(s3_prefix, mines_file)
) %>%
    mutate(
        ACTV_STATUS = replace_na(ACTV_STATUS, "Unknown/Other"),
        DEV_STAGE = replace_na(DEV_STAGE, "Unknown/Other")
    )

first <- mi.table %>%
    left_join(., expanded_cl %>% filter(dev_stage != "Any"),
        by = c("ACTV_STATUS" = "actv_status", "DEV_STAGE" = "dev_stage")
    )

second <- first %>%
    filter(is.na(new_class)) %>%
    select(-new_class) %>%
    left_join(., expanded_cl %>% filter(dev_stage == "Any") %>% select(-dev_stage), by = c("ACTV_STATUS" = "actv_status"))

mines <- rbind(first %>% filter(!is.na(new_class)), second) %>%
    select(PROP_ID,DEV_STAGE,ACTV_STATUS,
        new_class
    )


test <- mines %>%
    select(ACTV_STATUS, DEV_STAGE,
        new_class
    ) %>% unique %>% arrange(new_class)

s3write_using(FUN = write.csv,
        test,
        bucket = bucket,
        object = paste0(s3_prefix, "classes_reviewed.csv")
)

s3write_using(FUN = write.csv,
        mines,
        bucket = bucket,
        object = paste0(s3_prefix, "mines_classes.csv")
)
