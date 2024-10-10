## librerias-----------------------
library(tidyverse)
install.packages("gt")
install.packages("webshot2")
webshot2::install_phantomjs()

#### dataset -----------------------
#cambio nombre de columnas asi evito problemas! 
gases_col <- Measurement_info

gases_col<-rename(Measurement_info,
             'Fecha_hora' = `Measurement date`,
             'estacion_id' = `Station code` ,
             'sust_quimica_id' = `Item code` ,
             'Emisiones' = `Average value`  ,
             'estado_instrumento' = `Instrument status` 
)


quimicos_info<-rename(Measurement_item_info,
                      'sust_quimica_id' = `Item code` ,
                      'Nombre_componente' = `Item name`  ,
                      'unidad' = `Unit of measurement`,
                      "Bueno(azul)"=`Good(Blue)`,
                      "Normal(Verde)" =  `Normal(Green)`,
                      "Malo(Amarrillo)" =  `Bad(Yellow)`,
                      "Muy malo(Rojo)" =  `Very bad(Red)`
)

estacion <- rename(Measurement_station_info,
                   'estacion_id' = `Station code`,
                   'estacion_nombre' = `Station name(district)`,
                   'direccion' = `Address`,
                   'Latitud' = `Latitude`,
                   'Longitud' = `Longitude`)
#cambiar
estacion <- estacion %>%
  mutate(estacion_id = as.integer(estacion_id))

quimicos_info <- quimicos_info %>%
  mutate(sust_quimica_id = as.integer(sust_quimica_id))

# CAMBIAR FECHA Y HORA 
gases_col <- gases_col %>%
  mutate(date = lubridate::date(Fecha_hora), hrs_min_seg = format(Fecha_hora,"%H:%M:%S")) %>%
  select(-Fecha_hora)

gases_col <- gases_col %>%
  mutate(year = lubridate::year(Fecha_hora),month = lubridate::month(Fecha_hora),
         day = lubridate::day(Fecha_hora),
         hrs = format(Fecha_hora,"%H"), 
         sust_quimica_id= as.integer(sust_quimica_id),
         estacion_id = as.integer(estacion_id),
         estado_instrumento = as.integer(estado_instrumento)) %>%
  select(-Fecha_hora)

### crear variables que me podrian funcionar------------
# # COLUMNA DE SEASONS

get_season <- function(month, day) {
  if ((month == 12 && day >= 21) || (month %in% c(1, 2)) || (month == 3 && day < 21)) {
    return('Invierno')
  } else if ((month == 3 && day >= 21) || (month %in% c(4, 5)) || (month == 6 && day < 21)) {
    return('Primavera')
  } else if ((month == 6 && day >= 21) || (month %in% c(7, 8)) || (month == 9 && day < 23)) {
    return('Verano')
  } else {
    return('Otoño')
  }
}

# Agregar la columna de estaciones
info2 <- info2 %>%
  mutate(estaciones = mapply(get_season, month, day))


### regiones

center_lat <- 37.5665
center_lon <- 126.9780

get_region <- function(Latitude, Longitude, center_lat, center_lon) {
  if (Latitude > center_lat & Longitude > center_lon) {
    return("Noreste")
  } else if (Latitude > center_lat & Longitude < center_lon) {
    return("Noroeste")
  } else if (Latitude < center_lat & Longitude > center_lon) {
    return("Sureste")
  } else {
    return("Suroeste")
  }
}

station_info <- station_info %>% mutate(region = mapply(get_region, Latitude , Longitude, MoreArgs = list(center_lat, center_lon)))


#### unir datasets y crear uno con todo lo que necesito-------------------

gases_col <- full_join(gasesg_col , estacion , by='estacion_id')

#### poner como columnas a los gases, quiero sus valores average en columnas-----------------------
gases_col <- gases%>% select(-item_code) %>%
  pivot_wider(names_from =item_name , values_from = average)

gases_col<- rename(gases_col,
                     'SO2' = `1` ,'NO2' = `3`,'CO' = `5`,
                     'O3' = `6` ,'PM10' = `8`,'PM2.5' = `9`)

#### cambiar su tipo de datos para graficar y modelar-----------------
glimpse(gases_col)

gases_col$region <- as.factor(gases_col$region)
gases_col$season <- as.factor(gases_col$season)
gases_col$day <- as.integer(gases_col$day)
gases_col$year <- as.factor(gases_col$year)
gases_col$month <- as.integer(gases_col$month)
gases_col$hour <- as.integer(gases_col$hour)
gases_col$station_code <- as.integer(gases_col$station_code)


#14) filtras solo gases, son dos unidades diferentes, me quedo con gases (ppm)--------------------------------
gases_col <- select(gases_col, -PM2.5, -PM10)

# Eliminar valores NA y negativos de las columnas CO y NO2 ---------------
gases_col <- gases_col %>%
  filter(!is.na(CO) & !is.na(NO2) & CO > 0 & NO2 > 0)

# Verificar si hay valores NA o negativos en CO y NO2-----------------------
summary(gases_col$CO)
summary(gases_col$NO2)


#### graficos relacion no2vsCO --------------------------
titulos <- labs(
  title = "Gráfico NO2 vs CO",
  subtitle = "Se representan las mediciones en las estaciones de ambos gases entre 2017 y 2019",
  x = "CO [ppm]",
  y = "NO2 [ppm]",
  color = "Estaciones"
)


tamaños <- 
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )


NO2vsCO <- ggplot(gases_col) +
  geom_point(aes(x = CO, y = NO2), position = position_jitter(width = 0.1, height = 0.01), alpha = 0.5) +
   titulos + tamaños
  

NO2vsCO_años <- ggplot(gases_col) +
  geom_point(aes(x = CO, y = NO2, color = year), position = position_jitter(width = 0.1, height = 0.01), alpha = 0.5) +
  facet_wrap(~ year) + titulos + tamaños

NO2vsCO_estaciones <- ggplot(gases_col) +
  geom_point(aes(x = CO, y = NO2, color = season), position = position_jitter(width = 0.1, height = 0.01), alpha = 0.5) +
  facet_grid(region ~ season) + titulos + tamaños

#### guardar graficos -----------------------
guardar <- ggsave("NO2vsCO_RelaEstaciones.png", NO2vsCO_estaciones, width = 10, height = 8)



