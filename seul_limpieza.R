library(tidyverse)
library(dplyr)

unique(Measurement_info)

info <- read_csv("archivo.csv")
item_info <-  read_csv("Measurement_item_info.csv")
station_info <-  read_csv("Measurement_station_info.csv")

# 1) RENOMBRAR COLUMNAS -------------------------------------- 
info <- read_csv("archivo.csv")

item_info<-rename(item_info,
             'item_code' = `Item code` ,
             'item_name' = `Item name`  ,
             'unit' = `Unit of measurement`,
             "Good(Blue)"=`Good(Blue)`,
             "Normal(Green)" =  `Normal(Green)`,
             "Bad(Yellow)" =  `Bad(Yellow)`,
             "VeryBad(Red)" =  `Very bad(Red)`)

station_info <- rename(station_info,
                'station_code' = `Station code`,
                'station_name' = `Station name(district)`,
                'address' = `Address`,
                'Latitude' = `Latitude`,
                'Longitude' = `Longitude`)

#2) Cambio tipo de variable -----------------------

station_info <- station_info %>%mutate(station_code = as.factor(station_code))

item_info <- item_info %>%mutate(item_code = as.factor(item_code))

info[c("item_code","instrument_status")] <- lapply(info[c("item_code","instrument_status")], as.factor)

# 3) Filtro por instrumento ---------------------
inst_barra <- ggplot ()


histo_instrument= ggplot(info ,aes(x=average , fill=instrument_status)) +
  geom_histogram(binwidth=1000,alpha=0.5,position='identity') +
  labs(y='cantidad', x='Instrumento',title="Histograma")

info2 <- info2 %>% filter(estado_instrumento==0)


info <-select(info,-estado_instrumento) # saca columna estado_insturmento==0

# 4) CAMBIAR FECHA Y HORA -----------------------

info2 <- info %>%
  mutate(year = lubridate::year(fecha_hora_formateada),month = lubridate::month(fecha_hora_formateada),
         day = lubridate::day(fecha_hora_formateada),
         hour = format(fecha_hora_formateada,"%H"), 
         item_code= as.integer(item_code),
         station_code = as.integer(station_code)) %>%
  select(-fecha_hora_formateada)

info2[c("year","month","day","hour")] <- lapply(info2[c("year","month","day","hour")], as.factor)


# 5) COLUMNA DE SEASONS ----------------------------------
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

info2 <- info2 %>% mutate(season = mapply(get_season, month, day))

# 6) AGREGAR FRANJA HORARIA -------------------------------
get_horario <- function(hrs) {
  if (hour >= 00 && hour < 06) {
    return("Madrugada")
  } else if (hour >= 06 && hour < 12) {
    return('Mañana')
  } else if (hour >= 12 && hour < 18) {
    return('Tarde')
  } else {
    return('Vesperino')
  }
}

info2<- info2 %>% mutate(time_day = mapply(get_horario,hrs))

# 7) AGREGAR COLUMNA NOMBRE QUIMICOS -----------------

quimico_seleccion <- item_info %>% select(item_code, item_name)
info2 <- inner_join(info2, quimico_seleccion , by='item_code')

#8)veo na  --------------------
info2 %>% summarise(cant_quimicos = n(),
                    na_promedio = sum(is.na(average))
)

#9) veo numero =<0
conteo_info <- info %>%filter(average=<0)%>% 
select(item_code, average)%>%
group_by(item_code, average) %>%
  summarise(Cantidad = n())  # Unknown : (3) 102 | (5) : 3 | (6)  :377

#10) filtro na y average =<0 
info <- filter (info,average =<0)

#10 ) Agrego columna : regiones en station --------
# Coordenadas del centro de Seúl (aproximadas) 
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

#11) AGREGAR station 
info2 <- full_join(info2, estacion , by='estacion_id')

#12) Filtro gases 
gases <- info2 %>% filter(item_code %in % c("SO2","CO","NO2","O3"))


#13) Quimicos a columnas 
gases_col <- gases%>% select(-item_code) %>%
  pivot_wider(names_from =item_name , values_from = average)

#14) filtras gases para modelado 
gas_no2_co <- select(gases_col, -SO2,-O3)

#descargar
ruta_archivo <- "C://Users//mikpz//Documents//Data//tp_final//info_final.csv"
write.csv(info2, file = ruta_archivo, row.names = FALSE, quote = FALSE)




# Definir función para clasificar los valores según los criterios dados
#classify_value <- function(Item_code, Average_value) {
#  if (Item_code == 1) {
#    if (Average_value < 0.05 && Average_value >= 0.02) {
#      return("Good")
#    }else if ( Average_value>0 && Average_value < 0.02){
#      return("VeryGood")
#    } 
#    } 
#    else if (Average_value < 0.15&& Average_value >= 0.05) {
#      return("Normal")
#    } else if (Average_value < 1.0 && Average_value >= 0.15) {
#      return("Bad")
#    } else if ( Average_value >= 1.0){
#      return("Verybad")
#    } else { 
#      return("Unknown")}
#    
#  } else if (Item_code == 3) {
#    if (Average_value < 0.06  && Average_value >= 0.03) {
#      return("Good")
#    } else if ( Average_value>0 && Average_value < 0.03){
#      return("VeryGood")
#    } else if (Average_value < 0.2 && Average_value >= 0.06) {
#      return("Normal")
#    } else if (Average_value < 2 && Average_value >= 0.20) {
#      return("Bad")
#    } else if ( Average_value >= 2.0){
#      return("Verybad")
#    } else { 
#      return("Unknown")}
#  } else if (Item_code == 5) {
#    if (Average_value < 9.00 && Average_value >= 2.00) {
#      return("Good")
#    } else if ( Average_value>0 && Average_value < 9.00){
#      return("VeryGood")
#    }  else if (Average_value < 15.00 && Average_value >= 9.00) {
#      return("Normal")
#    } else if (Average_value < 50.0 && Average_value >= 15.00) {
#      return("Bad")
#    }else if ( Average_value >= 50.0){
#      return("Verybad")
#    } else { 
#      return("Unknown")}
#    
#  } else if (Item_code == 6) {
#    if (Average_value < 0.09 && Average_value >= 0.03) {
#      return("Good")
#    } else if ( Average_value>0 && Average_value < 0.03){
#      return("VeryGood")
#    } else if (Average_value < 0.15 && Average_value >= 0.09) {
#      return("Normal")
#    } else if (Average_value < 0.5 && Average_value >= 0.15) {
#      return("Bad")
#    } else if ( Average_value >= 0.5){
#      return("Verybad")
#    } else { 
#      return("Unknown")}
#    
#  } else if (Item_code == 8) {
#    if (Average_value < 80.00 && Average_value >= 30.00) {
#      return("Good")
#    } else if ( Average_value>0 && Average_value < 30.00){
#      return("VeryGood")
#    } else if (Average_value < 150.00 && Average_value >= 80.00) {
#      return("Normal")
#    } else if (Average_value < 600.0 && Average_value >= 150.0) {
#      return("Bad")
#    } else if ( Average_value >= 600.0){
#      return("Verybad")
#    } else { 
#      return("Unknown")}
#  } else if (Item_code == 9) {
#    if (Average_value < 35.00 && Average_value >= 15.00) {
#      return("Good")
#    } else if ( Average_value>0 && Average_value < 15.00){
#      return("VeryGood")
#    }  else if (Average_value < 75.00 && Average_value >= 35.00) {
#      return("Normal")
#    } else if (Average_value < 500.0 && Average_value >= 75.00) {
#      return("Bad")
#    } else if ( Average_value >= 500.0){
#      return("Verybad")
#    } else { 
#      return("Unknown")}
#  } else {
#    return("Unknown")
#  }
#}

# Aplicar la función a cada fila de la tabla
#info <- info %>% 
#  mutate(Classification = mapply(classify_value, Item_code, Average_value))