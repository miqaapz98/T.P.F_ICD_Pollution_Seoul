# REGIONES
library(leaflet)
library(ggmap)
library(ggplot2)
library(dplyr)
library(sp)

station
# Definir una paleta de colores para las regiones
region_colors <- colorFactor(c("black", "brown", "orange", "purple"), levels = c("Noreste", "Noroeste", "Sureste", "Suroeste"))

# Crear el mapa con leaflet

mapa <- leaflet(data = station_info) %>%
  addTiles() %>%
  addCircleMarkers(~Longitude, ~Latitude, color = ~region_colors(region), radius = 8, popup = ~station_name) %>%
  addMarkers(data = station_info, lng = ~Longitude, 
             lat = ~Latitude, label = ~station_name) %>%
  addLegend(
    position = "bottomright", 
    pal = region_colors, 
    values = ~region, 
    title = "Regiones",
    opacity = 1
  )

tabla_reg <- station_info %>%select(region,station_name)



