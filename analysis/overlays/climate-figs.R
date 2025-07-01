
# Load packages
library(sf)
library(raster)
library(leaflet)
library(shiny)
library(plotly)
library(DT)
library(ggthemes)
library(RColorBrewer)
library(aws.s3)
options(scipen = 999)
theme_set(theme_minimal())
library(tidyverse)
library(conflicted)
library(exactextractr)
library(terra)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(ggpubr)
library(leaflet)


# S3 settings
bucket <- "mefadapt"
prefix <- "repository/global_analysis/mining-casestudy/input/"

aws.signature::use_credentials(profile = "mefadapt")
Sys.setenv("AWS_DEFAULT_REGION" = get_location("mefadapt"))

# Ensure dplyr's select is used when there is ambiguity
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflicts_prefer(terra::extract)


classes <- s3read_using(
    FUN = read.csv,
    row.names = 1,
    bucket = bucket,
    object = paste0(prefix, "classes_reviewed.csv")
)
# Define critical minerals of interest
critical_minerals <- c("Lithium", "Cobalt", "Nickel", "Graphite", "Copper","Manganese")

mines <- s3read_using(
    FUN = read.csv,
    row.names = 1,
    bucket = bucket,
    object = paste0(prefix, "mines_classes.csv")
) %>%
    filter(!is.na(LONGITUDE)) %>%
    filter(grepl(paste(critical_minerals, collapse = "|"), COMMODITIES_LIST)) %>%
        filter(new_class %in% c("Production", "Development"))


labels.ws <- c("Arid and Low Water Use",
    "Low (<10%)", "Low - Medium (10-20%)",
    "Medium - High (20-40%)", "High (40-80%)",
    "Extremely High (>80%)"
)
labels.cl <- c(
    "Zero", "Low (<10%)", "Low - Medium (10-20%)",
    "Medium - High (20-40%)", "High (40-80%)",
    "Extremely High (>80%)"
)

labels <- c(
    "Zero", "Low (<10%)", "Low - Medium (10-20%)",
    "Medium - High (20-40%)", "High (40-80%)",
    "Extremely High (>80%)", "Arid and Low Water Use"
)

newlevels <- c(0, 1, 1, 1, 1, 1)

levels.ws <- c(-1, 0, 1, 2, 3, 4)
levels.cl <- c(-Inf, 0, 0.1, 0.2, 0.4, 0.8, Inf)

values.ha <- c("ws", "ler", "leh", "let", "led")
labels.ha <- c("Water Stress", "River Flood", "Heatwave", "Tropical Cyclones", "Drought")

values.sc <- c("baseline", "bau", "pes", "rcp26", "rcp60")
labels.sc <- c(
    "Historical", "Future - Business-as-usual", "Future - Pessimistic",
    "Future - Business-as-usual", "Future - Pessimistic"
)



# Color schemes
hazard_colors.ws <- c(
    "Low (<10%)" = "#8ad38a",
    "Low - Medium (10-20%)" = "#ffe137",
    "Medium - High (20-40%)" = "#f99828",
    "High (40-80%)" = "#e3623a",
    "Extremely High (>80%)" = "#901010",
    "Arid and Low Water Use" = "#868686"
)

hazard_colors.cl <- c(
    "Zero" = "#ddebf7",
    "Low (<10%)" = "#8ad38a",
    "Low - Medium (10-20%)" = "#ffe137",
    "Medium - High (20-40%)" = "#f99828",
    "High (40-80%)" = "#e3623a",
    "Extremely High (>80%)" = "#901010"
)



world <- ne_countries(scale = "medium", returnclass = "sf")


# Map Water Stress points (current and future)
future_annual <- s3read_using(st_read,
    object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05_future.gpkg",
    bucket = bucket
) %>%
    dplyr::select(pes80_ws_x_c) %>%
    mutate(value = factor(pes80_ws_x_c, levels = levels.ws, labels = labels.ws)) %>%
    filter(!is.na(value))

baseline_annual <- s3read_using(st_read,
    object = "repository/raw_data/WATER/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05_baseline.gpkg",
    bucket = bucket
) %>%
    dplyr::select(bws_cat) %>%
    mutate(value = factor(bws_cat, levels = levels.ws, labels = labels.ws)) %>%
    filter(!is.na(value))

ws_map_b <- ggplot() +
    # geom_sf(data = world, fill = "gray95", color = "gray50") +
    geom_sf(data = baseline_annual, aes(color = value, fill = value)) +
    scale_color_manual(values = hazard_colors.ws) +
    scale_fill_manual(values = hazard_colors.ws) +
    labs(color = "Water Stress", fill = "Water Stress") +
    theme_minimal()

ws_map_f <- ggplot() +
    # geom_sf(data = world, fill = "gray95", color = "gray50") +
    geom_sf(data = future_annual, aes(color = value, fill = value)) +
    scale_color_manual(values = hazard_colors.ws) +
    scale_fill_manual(values = hazard_colors.ws) +
    labs(color = "Water Stress", fill = "Water Stress") +
    theme_minimal()


# plot both maps into a file at analysis\figures\climate-mining

png("analysis/figures/climate-mining/ws_map.tif", width = 3000, height = 2500,  units = "px", res = 300)
ggarrange(ws_map_b, ws_map_f, ncol = 1,
    common.legend = TRUE,
    legend = "right",
    labels = c("Baseline", "Future")
)
dev.off()





# Map Mining Operations with points colored by primary commodity

# Convert mines data to spatial points using their latitude & longitude
mines_sf <- st_as_sf(mines,
    coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(world)
)

mines_comm_map <- function(mineral) {
    mines1 <- mines_sf %>%
        filter(grepl(mineral, COMMODITIES_LIST)) %>%
        mutate(com = ifelse(PRIMARY_COMMODITY == mineral,
            PRIMARY_COMMODITY, "Other")) %>%
        mutate(PRIMARY_COMMODITY = factor(com,
            levels = c(mineral, "Other"))) %>%
        ggplot() +
        geom_sf(data = world, fill = "gray95", color = "gray50") +
        geom_sf(aes(color = PRIMARY_COMMODITY,
        fill = PRIMARY_COMMODITY, shape = new_class),
        size = 1.2, alpha = 0.8) +
        scale_color_manual(values = c("#33a4c1", "#d46a31")) +
        scale_fill_manual(values = c("#33a4c1", "#d46a31")) +
        labs(
            title = paste0("Mines which produce ", mineral),
            color = "Primary Commodity", fill = "Primary Commodity",
            shape = "Status") +
        guides(color = guide_legend(override.aes = list(size = 5)),
        shape = guide_legend(override.aes = list(size = 5))) +
        theme_minimal()+ theme(legend.position = "bottom")

    png(paste0("analysis/figures/climate-mining/mines_",mineral,"_1.png"),
        width = 2500,
        height = 1500, units = "px", res = 300
    )
    print(mines1)
    dev.off()
}

lapply(critical_minerals, mines_comm_map)



# Climate hazards


tmpdir <- "C:/Users/RafaelaFlach/Downloads/tempdir/"

hazards <- c("ler", "let", "leh", "led")
labels.ha <- c("River Flood",  "Tropical Cyclones","Heatwave", "Drought")

times <- c("baseline", "rcp26", "future")

# aws.s3::save_object(
#     object = paste0(s3_prefix, "hazard_stack.tif"),
#     bucket = s3_bucket,
#     file = paste0(tmpdir, "hazard_stack.tif")
# )
hazard_stack <- stack(paste0(tmpdir, "hazard_stack.tif"))

names(hazard_stack) <- lapply(hazards, function(x) paste(x, times, sep = "_")) %>% unlist()

mask <- as.data.frame(hazard_stack[["ler_baseline"]], xy = TRUE)
names(mask)[3] <- "value"

# TEST
i <- 2
    layer_name <- names(hazard_stack)[which(grepl(hazards[i], names(hazard_stack)))]
    layer_name <- layer_name[-which(grepl("rcp26", layer_name))]
    df1 <- as.data.frame(hazard_stack[[layer_name]][[1]], xy = TRUE)
    df2 <- as.data.frame(hazard_stack[[layer_name]][[2]], xy = TRUE)
    names(df1)[3] <- "bas"
    names(df2)[3] <- "fut"
    df1[which(is.na(mask$bas)), "bas"] <- 0
    df2[which(is.na(mask$fut)), "fut"] <- 0
test <- full_join(df1, df2, by = c("x", "y")) %>%mutate(dif = fut-bas)
 ggplot() +
        geom_raster(data = test, aes(x = x, y = y, fill = dif)) 
# TEST OVER

hazds <- function(i) {
    layer_name <- names(hazard_stack)[which(grepl(hazards[i], names(hazard_stack)))]
    layer_name <- layer_name[-which(grepl("rcp26", layer_name))]
    df1 <- as.data.frame(hazard_stack[[layer_name]][[1]], xy = TRUE)
    df2 <- as.data.frame(hazard_stack[[layer_name]][[2]], xy = TRUE)
    names(df1)[3] <- "value"
    names(df2)[3] <- "value"
    df1[which(is.na(mask$value)), "value"] <- NA
    df2[which(is.na(mask$value)), "value"] <- NA
    output_file <- paste0("analysis/figures/climate-mining/hazard_", labels.ha[i], ".png")

    p <- ggplot() +
        #geom_sf(data = world, fill = "gray95", color = "gray50", size = 2) +
        geom_raster(data = df1, aes(x = x, y = y, fill = value)) +
        scale_fill_gradientn(colors = 
            colorRampPalette(c("white", "#66b466", "#0091ff", "#6d07bb"))(20),
            limits = c(0, 1),
            na.value = "#c7f8f9") +
        labs(x = "", y = "", fill = "Land area fraction \n exposed to hazard") +
        theme_minimal()
    g <- ggplot() +
       # geom_sf(data = world, fill = "gray95", color = "gray50") +
        geom_raster(data = df2, aes(x = x, y = y, fill = value)) +
        scale_fill_gradientn(colors = 
            colorRampPalette(c("white", "#66b466", "#0091ff", "#6d07bb"))(20),
            limits = c(0, 1),
            na.value = "#c7f8f9") +
        labs(x = "", y = "", fill = "Land area fraction \n exposed to hazard") +
            theme_minimal()
    oo <- ggarrange(p, g,
        ncol = 1, common.legend = TRUE,
        legend = "right",
        labels = c("Baseline", "Future")
    )
    oo <- annotate_figure(oo,
        top = text_grob(paste0("Climate hazard: ", labels.ha[i]),
            face = "bold", size = 16
        )
    )
    png(filename = output_file, width = 2500, height = 2500, units = "px", res = 300)
    print(oo)
    dev.off()
}

lapply(1:4, hazds)
