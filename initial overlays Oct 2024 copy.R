
.libPaths() # original libraries
myPaths <- c('C:/Users/ecmh500/rlibrary') # new library
.libPaths(myPaths) # set new library
.libPaths() # check it's worked

library(rgdal)
library(sf)
library(tmap)
library(pryr)
library(dplyr)
library(tidyverse)
library(raster)


# reading in water layers ---------------------


# to see all the layers in the geodatabase
#st_layers("L:/MEFadapt/aqueduct/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gdb")

future_annual <- st_read("L:/MEFadapt/aqueduct/aqueduct-4-0-water-risk-data/Aqueduct40_waterrisk_download_Y2023M07D05/GDB/Aq40_Y2023D07M05.gdb", layer = "future_annual")

# example map
#tm_shape(future_annual[1:1000,]) + tm_fill(col = "bau50_ws_x_l")
#tmap_options(check.and.fix = TRUE)





# reading in mining layers -----------------------
mining <- st_read("C:/Users/RafaelaFlach/Downloads/tempdir/sp_mining.gpkg")

energy <- st_read("C:/Users/RafaelaFlach/Downloads/tempdir/sp_energy.gpkg")



# some statistics on mining and energy

enstats <- energy %>%
  st_drop_geometry() %>%
  dplyr::select(-PLANT_KEY) %>%
  unique() %>%
  filter(FUEL_TYPE != "Energy Storage") %>%
  mutate(PLANNED_CAPACITY = as.numeric(PLANNED_CAPACITY), 
  OPER_CAPACITY_PLANT = as.numeric(OPER_CAPACITY_PLANT)) %>%
  group_by(OPER_STATUS, FUEL_TYPE) %>%
  summarize(
    planned_capacity = sum(PLANNED_CAPACITY, na.rm = T),
    operating_capacity = sum(OPER_CAPACITY_PLANT, na.rm = T)
  ) %>%
  ungroup() %>%
  filter(grepl("Planned|Operating|Post", OPER_STATUS)) %>%
  replace(is.na(.), 0) %>%
  pivot_longer(planned_capacity:operating_capacity, names_to = "Status",values_to = "Capacity") %>%
  mutate(Capacity = Capacity/1000) %>%
  arrange(desc(Capacity))

library(ggpubr)
ggbarplot(enstats,
  x = "FUEL_TYPE", y = "Capacity",
  fill = "Status",color = "Status",
  combine = F, palette= c("#6fc8c9","#196683")
) +
  labs(x = "Fuel type", y = "Capacity (GW)")

minstats <- mining %>%
  st_drop_geometry() %>%
  group_by(DEV_STAGE, PRIMARY_COMMODITY) %>%
  summarize(sumcapacity = sum(MILL_CAPACITY_TONNES_PER_YEAR, na.rm=T)) %>%
  ungroup()






# testing
#tm_shape(mining) + tm_dots()


#names(mining)



#tm_shape(mining[mining$PRIMARY_COMMODITY == "Lithium",]) + tm_dots()

# get country outlines
countries <- st_read("C:/Users/RafaelaFlach/OneDrive - SEI/Local Files/data/gadm_410-levels.gpkg", layer = "ADM_0")


#tm_shape(mining[mining$PRIMARY_COMMODITY == "Lithium",]) + tm_dots() +
#  tm_shape(countries) + tm_borders()






# simplify countries shapefile and water shapefile for visualisations --------------------------------




#countries_simple <- st_simplify(countries, dTolerance = 1000)
#edge x crosses edge x

sf_use_s2(FALSE)

countries_simple <- st_simplify(countries, dTolerance = 0.1)

#object_size(countries)
#object_size(countries_simple)


future_annual_simple <- st_simplify(future_annual, dTolerance = 0.1)

#object_size(future_annual)
#object_size(future_annual_simple)



# mapping water stress -------------------------------

tm_shape(future_annual) + tm_fill(col = "bau50_ws_x_l", palette = c("grey90", "firebrick", "orange", "darkseagreen1", "darkseagreen3", "gold", "grey70")) +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_layout(main.title = "Business as usual, 2050, water stress", frame = FALSE)



# water & mining -------------------------------


tm_shape(future_annual) + tm_fill(col = "bau50_ws_x_l", palette = c("grey90", "firebrick", "orange", "darkseagreen1", "darkseagreen3", "gold", "grey70"), alpha = 0.5) +
  tm_shape(mining) + tm_dots(alpha = 0.1) +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_layout(main.title = "Business as usual, 2050, water stress & mines", frame = FALSE)




# focus commodities & water stress -------------------------------


# lithium, cobalt and graphite mines on bau50 water stress
tm_shape(future_annual) + tm_fill(col = "bau50_ws_x_l", palette = c("grey90", "firebrick", "orange", "darkseagreen1", "darkseagreen3", "gold", "grey70"), alpha = 0.5) +
  tm_shape(mining[mining$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite"),]) + tm_dots(alpha = 0.5) +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_layout(main.title = "Business as usual, 2050, water stress & cobalt, lithium and graphite mines", frame = FALSE)




# focus commodities mines only ------------------------------

# presence of lithium, cobalt and graphite mines (primary commodity)
tm_shape(countries_simple) + tm_borders() +
tm_shape(mining[mining$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite"),]) + tm_dots(col = "PRIMARY_COMMODITY", palette = c("cornflowerblue", "darkseagreen", "salmon"), size = 0.1, alpha = 0.5)




# presence of lithium, cobalt and graphite mines (in commodity list)

tm_shape(countries_simple) + tm_borders() +
  tm_shape(filter(mining, grepl("Cobalt", COMMODITIES_LIST))) + tm_dots(col = "cornflowerblue", size = 0.1, alpha = 0.5) +
  tm_shape(filter(mining, grepl("Graphite", COMMODITIES_LIST))) + tm_dots(col = "darkseagreen", size = 0.1, alpha = 0.5) +
  tm_shape(filter(mining, grepl("Lithium", COMMODITIES_LIST))) + tm_dots(col = "salmon", size = 0.1, alpha = 0.5)



# comparing minerals presence in primary commodities and commodities list

tm_shape(countries_simple) + tm_borders() + tm_shape(filter(mining, grepl("Lithium", COMMODITIES_LIST))) + tm_dots(col = "cornflowerblue")
#973 mines with Lithium as part of the commodities list

tm_shape(countries_simple) + tm_borders() + tm_shape(mining[mining$PRIMARY_COMMODITY == "Lithium",]) + tm_dots(col = "cornflowerblue")
#676 mines with Lithium as primary commodity




tm_shape(countries_simple) + tm_borders() + tm_shape(filter(mining, grepl("Cobalt", COMMODITIES_LIST))) + tm_dots(col = "cornflowerblue")
#1561 mines with Lithium as part of the commodities list

tm_shape(countries_simple) + tm_borders() + tm_shape(mining[mining$PRIMARY_COMMODITY == "Cobalt",]) + tm_dots(col = "cornflowerblue")
#150 mines with Lithium as primary commodity





tm_shape(countries_simple) + tm_borders() + tm_shape(filter(mining, grepl("Graphite", COMMODITIES_LIST))) + tm_dots(col = "cornflowerblue")
#380 mines with Lithium as part of the commodities list

tm_shape(countries_simple) + tm_borders() + tm_shape(mining[mining$PRIMARY_COMMODITY == "Graphite",]) + tm_dots(col = "cornflowerblue")
#150 mines with Lithium as primary commodity



# check crs of all shapefiles -----------------------

#st_crs(countries)
# wgs 84

#st_crs(future_annual)
# wgs 84

#st_crs(mining)
# wgs 84


# counting points in the future_annual shapefile ----------------------

# perform a spatial join using st_join, and join = st_within

mining_future_annual <- st_join(mining, future_annual, join = st_within)
# takes ~5 mins to run

# counting the number of mines within each water stress category
#mines_per_bau50_ws_x_l <- aggregate(mining_future_annual$PROP_ID, by = list(mining_future_annual$bau50_ws_x_l), FUN = length)

# counting the number of mines in each water stress category AND by primary commodity
#pc_mines_per_bau50_ws_x_l <- aggregate(mining_future_annual$PROP_ID, by = list(mining_future_annual$bau50_ws_x_l, mining_future_annual$PRIMARY_COMMODITY), FUN = length)


# plot of number of mines per commodity per water stress category
#ggplot(pc_mines_per_bau50_ws_x_l[pc_mines_per_bau50_ws_x_l$Group.2 %in% c("Lithium", "Cobalt", "Graphite"),], aes(x = Group.1, y = x)) + geom_col() + facet_wrap(~Group.2)


# counting the number of mines in each water stress category AND by primary commodity (category number labels instead of text labels)
pc_mines_per_bau50_ws_x_c <- aggregate(mining_future_annual$PROP_ID, by = list(mining_future_annual$bau50_ws_x_c, mining_future_annual$PRIMARY_COMMODITY), FUN = length)

# plotting cobalt, graphite and lithium per water stress category
ggplot(pc_mines_per_bau50_ws_x_c[pc_mines_per_bau50_ws_x_c$Group.2 %in% c("Lithium", "Cobalt", "Graphite"),], aes(x = Group.1, y = x)) + 
  geom_col() + 
  facet_wrap(~Group.2) +
  labs(caption = "-1 = Arid and low water use; 0 = Low; 1 = Low-medium; 2 = Medium-high; 3 = High; 4 = Extremely high", title = "Number of mines per water stress category, based on primary commodity", x = "Water Stress category", y = "Number of mines") +
  theme_minimal()


# plotting cobalt, graphite and lithium per water stress category (stacked proportional bar chart)

ggplot(pc_mines_per_bau50_ws_x_c[pc_mines_per_bau50_ws_x_c$Group.2 %in% c("Lithium", "Cobalt", "Graphite"),], aes(x = Group.2, y = x)) + 
  geom_col(aes(fill = Group.1), position = "fill") + 
  scale_fill_gradient(low = "pink", high = "firebrick") + 
  labs(caption = "-1 = Arid and low water use; 0 = Low; 1 = Low-medium; 2 = Medium-high; 3 = High; 4 = Extremely high", title = "Number of mines per water stress category, based on primary commodity", x = "Commodity type", y = "Number of mines") +
  theme_minimal()



# mapping where lithium, cobalt and graphite mines coincide with extremely high water stress ----------------------------------------------------------------------

tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(future_annual) + tm_fill(col = "bau50_ws_x_l", palette = c("white", "firebrick", "white", "white", "white", "white", "white"), alpha = 0.5) +
  tm_shape(mining_future_annual[mining_future_annual$PRIMARY_COMMODITY == "Lithium" & mining_future_annual$bau50_ws_x_c == "4",]) + tm_dots() +
  tm_shape(mining_future_annual[mining_future_annual$PRIMARY_COMMODITY == "Cobalt" & mining_future_annual$bau50_ws_x_c == "4",]) + tm_dots() +
  tm_shape(mining_future_annual[mining_future_annual$PRIMARY_COMMODITY == "Graphite" & mining_future_annual$bau50_ws_x_c == "4",]) + tm_dots() +
    tm_layout(main.title = "Mines (based on primary commodity) which are located in extremely high water stress areas", frame = FALSE)







# limiting these maps to only active mines
# active mines coinciding with extremely high water stress
tm_shape(countries_simple) + tm_borders(col = "grey90") +
  tm_shape(mining_future_annual[mining_future_annual$ACTV_STATUS == "Active" & mining_future_annual$bau50_ws_x_c == "4",]) + tm_bubbles(size = 0.05, alpha = 0.5, col = "firebrick", border.alpha = 0) +
  tm_layout(title = "Active mines which are located in extremely high water stress areas", frame = FALSE)



# active lithium, cobalt and graphite mines coinciding with extremely high water stress
tm_shape(countries_simple) + tm_borders(col = "grey90") +
  tm_shape(mining_future_annual[mining_future_annual$ACTV_STATUS == "Active" & mining_future_annual$bau50_ws_x_c == "4" & mining_future_annual$PRIMARY_COMMODITY %in% c("Lithium", "Graphite", "Cobalt"),]) + tm_bubbles(size = 0.1, alpha = 0.5, col = "firebrick", border.alpha = 0) +
  tm_layout(title = "Active lithium, cobalt and graphite (Primary Commodity) mines which are located in extremely high water stress areas", frame = FALSE)



# read in energy data -------------------------------------------------


energy <- st_read("L:/MEFadapt/energy (from s3 sep 2024)/sp_energy.gpkg")

# make operating capacity numeric

energy$OPER_CAPACITY_PLANT <- as.numeric(energy$OPER_CAPACITY_PLANT)



# water & energy -------------------------------

tm_shape(energy) + tm_dots() + tm_facets(by = "TECH_TYPE")
tm_shape(energy[energy$OPER_STATUS == "Operating" | energy$OPER_STATUS == "Operating & Planned",]) + tm_dots() + tm_facets(by = "TECH_TYPE")



tm_shape(future_annual) + tm_fill(col = "bau50_ws_x_l", palette = c("grey90", "firebrick", "orange", "darkseagreen1", "darkseagreen3", "gold", "grey70"), alpha = 0.5) +
  tm_shape(energy[(energy$OPER_STATUS == "Operating" | energy$OPER_STATUS == "Operating & Planned") & energy$TECH_TYPE == "Hydraulic Turbine",]) + tm_dots(size = "OPER_CAPACITY_PLANT", alpha = 0.5) +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_layout(main.title = "Business as usual, 2050, water stress & hydraulic turbines (operating or operating & planned only)", frame = FALSE)




# perform a spatial join using st_join, and join = st_within


energy_future_annual <- st_join(energy, future_annual, join = st_within)
# takes ~5 mins to run



energy_tech_type_per_bau50_ws_x_c <- aggregate(energy_future_annual$TECH_TYPE, by = list(energy_future_annual$bau50_ws_x_c, energy_future_annual$TECH_TYPE), FUN = length)





# plotting energy sites per water stress category, per tech type
ggplot(energy_tech_type_per_bau50_ws_x_c, aes(x = Group.1, y = x)) + 
  geom_col() + 
  facet_wrap(~Group.2, scales = "free_y") +
  labs(caption = "-1 = Arid and low water use; 0 = Low; 1 = Low-medium; 2 = Medium-high; 3 = High; 4 = Extremely high", title = "Number of energy sites per water stress category, per tech_type", x = "Water Stress category", y = "Number of energy sites") +
  theme_minimal()


# plotting energy sites per water stress category, per tech type
ggplot(energy_tech_type_per_bau50_ws_x_c, aes(x = Group.2, y = x)) + 
  geom_col(aes(fill = Group.1), position = "fill") +
  scale_fill_gradient(low = "pink", high = "firebrick") + 
  labs(caption = "-1 = Arid and low water use; 0 = Low; 1 = Low-medium; 2 = Medium-high; 3 = High; 4 = Extremely high", title = "Number of energy sites per water stress category, per tech type", x = "Type of Energy site", y = "Proportion of sites") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))



# extremely high water stress & hydraulic turbines

tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(energy_future_annual[(energy_future_annual$OPER_STATUS == "Operating" | energy_future_annual$OPER_STATUS == "Operating & Planned") & energy_future_annual$TECH_TYPE == "Hydraulic Turbine" & energy_future_annual$bau50_ws_x_c == "4",]) + tm_bubbles(size = "OPER_CAPACITY_PLANT", alpha = 0.5, col = "firebrick", border.alpha = 0) +
  tm_layout(main.title = "Hydraulic turbines (operating or operating & planned only) in areas of extremely high water stress, bau50", frame = FALSE)





# extremely high water stress & nuclear

tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(energy_future_annual[(energy_future_annual$OPER_STATUS == "Operating" | energy_future_annual$OPER_STATUS == "Operating & Planned") & energy_future_annual$TECH_TYPE == "Nuclear" & energy_future_annual$bau50_ws_x_c == "4",]) + tm_bubbles(size = "OPER_CAPACITY_PLANT", alpha = 0.5, col = "firebrick", border.alpha = 0) +
  tm_layout(main.title = "Nuclear sites (operating or operating & planned only) in areas of extremely high water stress, bau50", frame = FALSE)





# mining & tropical storms -------------------------------------------------------------------------------


# read in storms raster
let <- stack("L:/MEFadapt/isimip-download-63492043fc4532aaf33ff1b761d213b0aacc9918/lange2020_ke-tg-meanfield_hadgem2-es_ewembi_rcp60_nosoc_co2_let_landonly_annual_2006_2099.nc4")


# getting 20-year average values around 2050 
av_50 <- mean(let[[36:54]])


# extract raster values to mining points data
mining_50 <- raster::extract(av_50, mining, sp = TRUE)

# convert to sf
mining_50 <- st_as_sf(mining_50)

# remove cells with 0 value from rasters
av_50[av_50 == 0] <- NA


# mapping

# simple overlay of raster & mines, 2050
tm_shape(av_50) + tm_raster(palette = "Reds") +
  tm_shape(mining[mining$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel"),]) + tm_dots(alpha = 0.5) +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_layout(main.title = "Land area exposed to tropical storms, 2050 & selected mines", frame = FALSE) +
  tm_credits("Cobalt, lithium, graphite, copper and nickel mines", position = c("LEFT", "BOTTOM"))

# extracted overlay of raster & mines, 2050, active mines only
tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(mining_50[mining_50$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel") & mining_50$layer > 0 & mining_50$ACTV_STATUS == "Active",]) + tm_bubbles(size = 0.05, alpha = 0.5, col = "firebrick", border.alpha = 0) +
  tm_layout(main.title = "Selected active mines exposed to tropical storms, 2050", frame = FALSE) +
  tm_credits("Cobalt, lithium, graphite, copper and nickel mines", position = c("LEFT", "BOTTOM"))



# create categories of land area exposed
mining_50$cat <- cut(mining_50$layer, breaks = c(0, 0.2, 0.4, 0.6), labels = c("low", "medium", "high"))
# add a "zero" level
new_levels <- c("zero", "low", "medium", "high")
mining_50$cat <- factor(mining_50$cat, levels = new_levels)
mining_50$cat[is.na(mining_50$cat)] <- "zero"



# counting the number of mines in each category AND by primary commodity (category number labels instead of text labels)
mines_per_cat_50 <- aggregate(mining_50$PROP_ID[mining_50$ACTV_STATUS == "Active"], by = list(mining_50$cat[mining_50$ACTV_STATUS == "Active"], mining_50$PRIMARY_COMMODITY[mining_50$ACTV_STATUS == "Active"]), FUN = length)

# plotting cobalt, graphite and lithium per category
ggplot(mines_per_cat_50[mines_per_cat_50$Group.2 %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel"),], aes(x = Group.1, y = x)) + 
  geom_col() + 
  facet_wrap(~Group.2) +
  labs(title = "Number of mines per land exposed to tropical storms, based on primary commodity, 2050", x = "Land exposed to tropical storms category", y = "Number of mines") +
  theme_minimal()

# plotting cobalt, graphite and lithium per category (stacked proportional bar chart) 
ggplot(mines_per_cat_50[mines_per_cat_50$Group.2 %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel"),], aes(x = Group.2, y = x)) + 
  geom_col(aes(fill = Group.1), position = "fill") + 
  scale_fill_manual(values = c("seashell2", "pink", "salmon", "firebrick")) + 
  labs(title = "Number of mines per land exposed to tropical storms, based on primary commodity, 2050", x = "Commodity type", y = "Number of mines") +
  theme_minimal()




# extracted overlay of raster & mines, 2050, active mines only - mines colour coded by category
tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(mining_50[mining_50$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel") & mining_50$layer > 0 & mining_50$ACTV_STATUS == "Active",]) + tm_bubbles(size = 0.1, alpha = 0.5, col = "cat", border.alpha = 0.1, palette = c("seashell2", "pink", "salmon", "firebrick", "grey")) +
  tm_layout(main.title = "Selected active mines exposed to tropical storms, 2050", frame = FALSE) +
  tm_credits("Cobalt, lithium, graphite, copper and nickel mines", position = c("LEFT", "BOTTOM"))




# river flooding & mining -----------------------------------------------------------------

# seven different impact models

ler1 <- stack("L:/MEFadapt/isimip-download-63492043fc4532aaf33ff1b761d213b0aacc9918/lange2020_jules-w1_hadgem2-es_ewembi_rcp60_nosoc_co2_ler_landonly_annual_2006_2099.nc4")
ler2 <- stack("L:/MEFadapt/isimip-download-63492043fc4532aaf33ff1b761d213b0aacc9918/lange2020_clm45_hadgem2-es_ewembi_rcp60_2005soc_co2_ler_landonly_annual_2006_2099.nc4")
ler3 <- stack("L:/MEFadapt/isimip-download-63492043fc4532aaf33ff1b761d213b0aacc9918/lange2020_h08_hadgem2-es_ewembi_rcp60_2005soc_co2_ler_landonly_annual_2006_2099.nc4")
ler4 <- stack("L:/MEFadapt/isimip-download-63492043fc4532aaf33ff1b761d213b0aacc9918/lange2020_lpjml_hadgem2-es_ewembi_rcp60_2005soc_co2_ler_landonly_annual_2006_2099.nc4")
ler5 <- stack("L:/MEFadapt/isimip-download-63492043fc4532aaf33ff1b761d213b0aacc9918/lange2020_orchidee_hadgem2-es_ewembi_rcp60_nosoc_co2_ler_landonly_annual_2006_2099.nc4")
ler6 <- stack("L:/MEFadapt/isimip-download-63492043fc4532aaf33ff1b761d213b0aacc9918/lange2020_pcr-globwb_hadgem2-es_ewembi_rcp60_2005soc_co2_ler_landonly_annual_2006_2099.nc4")
ler7 <- stack("L:/MEFadapt/isimip-download-63492043fc4532aaf33ff1b761d213b0aacc9918/lange2020_watergap2_hadgem2-es_ewembi_rcp60_2005soc_co2_ler_landonly_annual_2006_2099.nc4")


av_50_ler <- mean(mean(ler1[[36:54]]), mean(ler2[[36:54]]), mean(ler3[[36:54]]), mean(ler4[[36:54]]), mean(ler5[[36:54]]), mean(ler6[[36:54]]), mean(ler7[[36:54]]))


# extract raster values to mining points data
mining_50_ler <- raster::extract(av_50_ler, mining, sp = TRUE)

# convert to sf
mining_50_ler <- st_as_sf(mining_50_ler)

# remove cells with 0 value from rasters
av_50_ler[av_50_ler == 0] <- NA


# mapping

# simple overlay of raster & mines, 2050
tm_shape(av_50_ler) + tm_raster(palette = "Blues") +
  tm_shape(mining[mining$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel"),]) + tm_dots(alpha = 0.5) +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_layout(main.title = "Land area exposed to river flooding, 2050 & selected mines", frame = FALSE) +
  tm_credits("Cobalt, lithium, graphite, copper and nickel mines", position = c("LEFT", "BOTTOM"))


# extracted overlay of raster & mines, 2050, active mines only
tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(mining_50_ler[mining_50_ler$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel") & mining_50_ler$layer > 0 & mining_50_ler$ACTV_STATUS == "Active",]) + tm_bubbles(size = 0.05, alpha = 0.5, col = "royalblue4", border.alpha = 0) +
  tm_layout(main.title = "Selected active mines exposed to river floods, 2050", frame = FALSE) +
  tm_credits("Cobalt, lithium, graphite, copper and nickel mines", position = c("LEFT", "BOTTOM"))




# create categories of land area exposed
mining_50_ler$cat <- cut(mining_50_ler$layer, breaks = c(0, 0.1, 0.2, 0.3), labels = c("low", "medium", "high"))
# add a "zero" level
new_levels <- c("zero", "low", "medium", "high")
mining_50_ler$cat <- factor(mining_50_ler$cat, levels = new_levels)
mining_50_ler$cat[is.na(mining_50_ler$cat)] <- "zero"



# counting the number of mines in each category AND by primary commodity (category number labels instead of text labels)
mines_per_cat_50_ler <- aggregate(mining_50_ler$PROP_ID[mining_50_ler$ACTV_STATUS == "Active"], by = list(mining_50_ler$cat[mining_50_ler$ACTV_STATUS == "Active"], mining_50_ler$PRIMARY_COMMODITY[mining_50_ler$ACTV_STATUS == "Active"]), FUN = length)

# plotting cobalt, graphite and lithium per category
ggplot(mines_per_cat_50_ler[mines_per_cat_50_ler$Group.2 %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel"),], aes(x = Group.1, y = x)) + 
  geom_col() + 
  facet_wrap(~Group.2) +
  labs(title = "Number of mines per land exposed to river flooding, based on primary commodity, 2050", x = "Land exposed to tropical storms category", y = "Number of mines") +
  theme_minimal()

# plotting cobalt, graphite and lithium per category (stacked proportional bar chart) 
ggplot(mines_per_cat_50_ler[mines_per_cat_50_ler$Group.2 %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel"),], aes(x = Group.2, y = x)) + 
  geom_col(aes(fill = Group.1), position = "fill") + 
  scale_fill_manual(values = c("seashell2", "pink", "salmon", "firebrick")) + 
  labs(title = "Number of mines per land exposed to river flooding, based on primary commodity, 2050", x = "Commodity type", y = "Number of mines") +
  theme_minimal()




# extracted overlay of raster & mines, 2050, active mines only - mines colour coded by category
tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_shape(mining_50_ler[mining_50_ler$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel") & mining_50_ler$layer > 0 & mining_50_ler$ACTV_STATUS == "Active",]) + tm_bubbles(size = 0.1, alpha = 0.5, col = "cat", border.alpha = 0.1, palette = c("seashell2", "pink", "salmon", "firebrick", "grey")) +
  tm_layout(main.title = "Selected active mines exposed to river flooding, 2050", frame = FALSE) +
  tm_credits("Cobalt, lithium, graphite, copper and nickel mines", position = c("LEFT", "BOTTOM"))




# water stress & mines (lithium, cobalt, graphite, copper and nickel) -------------------------------


# focus commodities & water stress 

# lithium, cobalt and graphite mines on bau50 water stress
tm_shape(future_annual) + tm_fill(col = "bau50_ws_x_l", palette = c("grey90", "firebrick", "orange", "darkseagreen1", "darkseagreen3", "gold", "grey70"), alpha = 0.5) +
  tm_shape(mining[mining$PRIMARY_COMMODITY %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel"),]) + tm_dots(alpha = 0.5) +
  tm_shape(countries_simple) + tm_borders(col = "grey80") +
  tm_layout(main.title = "Business as usual, 2050, water stress & selected mines", frame = FALSE)




# plotting cobalt, graphite, lithium, copper and nickel per water stress category (stacked proportional bar chart)

ggplot(pc_mines_per_bau50_ws_x_c[pc_mines_per_bau50_ws_x_c$Group.2 %in% c("Lithium", "Cobalt", "Graphite", "Copper", "Nickel"),], aes(x = Group.2, y = x)) + 
  geom_col(aes(fill = Group.1), position = "fill") + 
  scale_fill_gradient(low = "pink", high = "firebrick") + 
  labs(caption = "-1 = Arid and low water use; 0 = Low; 1 = Low-medium; 2 = Medium-high; 3 = High; 4 = Extremely high", title = "Number of mines per water stress category, based on primary commodity", x = "Commodity type", y = "Number of mines") +
  theme_minimal()




# active lithium, cobalt, graphite, copper and nickel mines coinciding with extremely high water stress
tm_shape(countries_simple) + tm_borders(col = "grey90") +
  tm_shape(mining_future_annual[mining_future_annual$ACTV_STATUS == "Active" & mining_future_annual$bau50_ws_x_c == "4" & mining_future_annual$PRIMARY_COMMODITY %in% c("Lithium", "Graphite", "Cobalt", "Copper", "Nickel"),]) + tm_bubbles(size = 0.1, alpha = 0.5, col = "firebrick", border.alpha = 0) +
  tm_layout(title = "Active mines (selected commodities) which are located in extremely high water stress areas", frame = FALSE)
