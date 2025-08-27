
# Chargement de packages 
rm(list=ls())
packages <- c("sp","terra", "RColorBrewer", "classInt", "ggplot2", 
              "ggmap", "sf", "spdep", "tmap", "purrr", "tidyr",
              "dplyr", "data.table", "stargazer", "openxlsx","readr",
              "patchwork","gridExtra","haven","units","readr", "stringr","rvest","tibble")

install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

dfgeo <- st_read("C:/Users/libre.DJA-1041-ZORN6/OneDrive/Mobile money and gender inequality/shps/ne_50m_admin_0_countries.shp")


# Importation du fichier shp des communes france métropolaire geographie 2020(34816 communes)
dfgeo <- dfgeo %>% rename(countrycode=ADM0_A3)
dfgeo$Lat <- map_dbl(dfgeo$geometry, ~st_centroid(.x)[[1]])
dfgeo$Long <- map_dbl(dfgeo$geometry, ~st_centroid(.x)[[2]])


# Extration des coordonnées géographiques

dfgeo<-dfgeo%>%select(countrycode, Lat,Long)

## EXtraire les don

# 1. Méthode la plus directe : st_drop_geometry()
df_attr <- st_drop_geometry(dfgeo)

# Enregistrer 

write_dta(df_attr, "C:/Users/libre.DJA-1041-ZORN6/OneDrive/Mobile money and gender inequality/Data/sf_pays.dta")