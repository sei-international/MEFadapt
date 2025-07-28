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

# Set AWS credentials (replace with your credentials)
aws.signature::use_credentials(profile = "mefadapt")
Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))

# Define the S3 bucket and file path for the input table
bucket <- "mefadapt"

conflicts_prefer(dplyr::filter)



# List all .cls files in a specific folder in the S3 bucket
s3_folder <- "repository/global_analysis/mining-casestudy/input/" # adjust this path as needed
files <- get_bucket(bucket = bucket, prefix = s3_folder)

# Filter for .xls files only
xls_files <- sapply(files, function(x) x$Key)
xls_files <- xls_files[grepl("\\.xls$", xls_files, ignore.case = TRUE)]

# Read all .xls files from S3 using s3read_using and readxl::read_excel
excel_list <- lapply(xls_files, function(key) {
    s3read_using(
        FUN = readxl::read_excel,
        object = key,
        bucket = bucket
    )
})

organize <- function(tab) {
    comm <- tab %>%
        slice(3) %>%
        select(-(1:2)) %>%
        unlist() %>%
        unique()
    colnames <- c(
        tab %>% slice(1) %>% select(2),
        tab %>% slice(2) %>% select(-1:-2)
    ) %>% as.vector()
    out <- tab %>%
        slice(-c(1:3)) %>%
        select(-1) %>%
        setNames(colnames) %>%
        mutate(mineral = comm)
    return(out)
}

finaltab <- lapply(excel_list, organize) %>%
    bind_rows() %>%
    pivot_longer(
        cols = -c(mineral, PROP_ID),
        names_to = "year",
        values_to = "value"
    ) %>%
    mutate(year = as.numeric(substr(year, 1, 4))) %>%
    mutate(value = as.numeric(value))


s3write_using(
    FUN = write.csv,
    x = finaltab,
    object = "repository/global_analysis/mining-casestudy/input/shareofworld_all.csv",
    bucket = bucket,
    row.names = FALSE
)


test <- finaltab %>%
    filter(!is.na(value)) %>%
    group_by(mineral, year) %>%
    summarise(value = sum(value, na.rm = TRUE), n = n()) %>%
    ungroup() %>%
    pivot_wider(
        names_from = mineral,
        values_from = value,
        values_fill = 0
    )
