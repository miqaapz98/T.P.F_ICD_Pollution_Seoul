library(tidyverse)
library(modelr)
library(gt)
library(webshot2)

#### analisis de mod5_log y mod---------------------------

###### data frame-----------------------
gases <- gases_sincero_19

gases_col <- gases%>% select(-item_code) %>%
  pivot_wider(names_from =item_name , values_from = average)

# Verificar que no haya valores faltantes
gases_col <- gases_col %>%
  filter(!is.na(CO) & !is.na(NO2))

## chequeo que todo este ok
unique(gases_col$season)
glimpse(gases_col)

## cambio el tipo de datos de las variables categoricas a factor para los modelos 
gases_col$region <- as.factor(gases_col$region)
gases_col$season <- factor(gases_col$season, levels = c( "Primavera", "Verano", "Otoño", "Invierno"))
gases_col$day <- as.integer(gases_col$day)
gases_col$year <- as.factor(gases_col$year)
gases_col$month <- as.integer(gases_col$month)
gases_col$hour <- as.integer(gases_col$hour)
gases_col$station_code <- as.integer(gases_col$station_code)



#### modelos simples -----------------

mod1 <- lm(formula = NO2 ~ CO , data = gases_col)
mod2 <- lm(formula = NO2 ~ CO + region , data = gases_col)
mod3 <- lm(formula = NO2 ~ CO + region + season , data = gases_col)
mod4 <- lm(formula = NO2 ~ CO*region + season  , data = gases_col)
mod5 <- lm(formula = NO2 ~ CO*region*season , data = gases_col)

anova(mod1, mod2, mod3, mod4, mod5)


summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
summary(mod5)


grilla <- data_grid(gases_col, NO2, CO, region, season)  %>%
  add_predictions(mod5, var = "pred_lineal") %>%
  add_residuals(mod5, var = "res_lineal")
plot(mod5)



# modelos con log ----------------------------------
mod1_log <- lm(formula = NO2 ~ log(CO), data = gases_col)
mod2_log <- lm(formula = NO2 ~ log(CO) + region, data = gases_col)
mod3_log <- lm(formula = NO2 ~ log(CO) + region + season, data = gases_col)
mod4_log <- lm(formula = NO2 ~ log(CO)*region + season, data = gases_col)
mod5_log <- lm(formula = NO2 ~ log(CO)*region*season, data = gases_col)

summary(mod1_log)
summary(mod2_log)
summary(mod3_log)
summary(mod4_log)
summary(mod5_log)




anova(mod1_log,mod2_log, mod3_log,mod4_log, mod5_log)

grilla_log5 <- data_grid(gases_col,NO2, CO, region, season) %>%
  add_predictions(mod5_log, var = "pred_log5") %>%
  add_residuals(mod5_log, var= "res_log5")

### graficos ------------------

NO2vsCO_mod5 <- ggplot(gases_col) + 
  geom_point(aes(x = CO , y = NO2)) +
  geom_line(data = grilla, aes(x = CO, y = pred_lineal, color = season, linetype = modelo), size = 1.2) +
  
  facet_grid(region ~ season) + 
  labs(
    title = " Gráfico de emisiones de NO2 vs de log(CO) en las regiones de Seúl ", 
    subtitle = "Las emisiones fueron medidas entre los años 2017 y 2019 durante las estaciones", 
    x = "CO [ppm]", 
    y = "NO2 [ppm]",
    linetype = "Modelo", 
    color = "Estaciones"
  ) +  
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Tamaño del título del gráfico
    plot.subtitle = element_text(size = 14),  # Tamaño del subtítulo del gráfico
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje X
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje Y
    legend.title = element_text(size = 14),  # Tamaño del título de la leyenda
    legend.text = element_text(size = 12),  # Tamaño del texto de la leyenda
    legend.position = "top"  # Colocar la leyenda en la parte superior
  )+  
  scale_y_continuous(expand = c(0, 0))






# Gráfico para el modelo logarítmico
NO2vsCO_mod_log <- ggplot(gases_col) + 
  geom_point(aes(x = CO, y = NO2)) +
  geom_line(data = grilla_log5, aes(x = CO, y = pred_log5, color = season), size = 1.2) +
  facet_grid(region ~ season) + 
  labs(
    title = "Gráfico de emisiones de NO2 vs CO en las regiones de Seúl", 
    subtitle = "Las emisiones fueron medidas entre los años 2017 y 2019 durante las estaciones", 
    x = "CO [ppm]", 
    y = "NO2 [ppm]",
    color = "Estaciones"
  ) +  
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Tamaño del título del gráfico
    plot.subtitle = element_text(size = 14),  # Tamaño del subtítulo del gráfico
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje X
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje Y
    legend.title = element_text(size = 14),  # Tamaño del título de la leyenda
    legend.text = element_text(size = 12),  # Tamaño del texto de la leyenda
    legend.position = "top"  # Colocar la leyenda en la parte superior
  )+  
  scale_y_continuous(expand = c(0, 0))

#### analizo los residuos de ambos modelos, quiero ver en que distancia estan los puntos en la relacion entre no2 y co, es decir segun la cc de ambos---------------
residuos_mod5 <- ggplot(grilla) + geom_boxplot(aes( x= CO, y = res_lineal, color= season))+
  facet_wrap( ~ region) + labs(title = "Boxplot residuos vs CO", subtitle = "Se observa la ubicación de los datos en funcion de CO", x="
                               CO [ppm]", y = "Residuos", color="Estaciones") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Tamaño del título del gráfico
    plot.subtitle = element_text(size = 14),  # Tamaño del subtítulo del gráfico
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje X
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje Y
    legend.title = element_text(size = 14),  # Tamaño del título de la leyenda
    legend.text = element_text(size = 12),  # Tamaño del texto de la leyenda
    legend.position = "top"  # Colocar la leyenda en la parte superior
  )


residuosbox_mod5_log<- ggplot(grilla_log5) + geom_boxplot(aes( x= CO, y = res_log5, color= season))+
  facet_wrap( ~ region) + labs(title = "Boxplot residuos vs CO, mod5_log", subtitle = "Se observa la ubicación de los datos en funcion de CO", x="
                               CO [ppm]", y = "Residuos", color="Estaciones") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),  # Tamaño del título del gráfico
    plot.subtitle = element_text(size = 14),  # Tamaño del subtítulo del gráfico
    axis.title.x = element_text(size = 14),  # Tamaño del título del eje X
    axis.title.y = element_text(size = 14),  # Tamaño del título del eje Y
    legend.title = element_text(size = 14),  # Tamaño del título de la leyenda
    legend.text = element_text(size = 12),  # Tamaño del texto de la leyenda
    legend.position = "top"  # Colocar la leyenda en la parte superior
  )
#### guardar graficos (mejor que exportarlos directo de rstudio!)-----------------------
ggsave("residuosbox_mod5_log.png", residuosbox_mod5_log, width = 10, height = 8)

