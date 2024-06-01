library(tidyverse)
library(modelr)
library(ggplot2)
library(geomtextpath)

#### modelos ------------------------------------
gases <- read_csv("C:/Users/mikpz/Documents/Data/tp_final/dataset/gases_sincero_19.csv")
particulas <- particulas19
glimpse(gases)

gases[c("day", "month","year")] <- lapply(gases[c("day", "month","year")], as.factor)
gases[c("station_code", "hour","item_code","season","region")] <- lapply(gases[c("station_code", "hour","item_code","season","region")], as.factor)

## gases -----------------
## en estos modelos consideramos separar la variable target average, ya que por su composicion
## los gases tienen diferente unidad y distribucion que las particulas PM10 y PM2.5
## Notamos que la funcion de regresion lineal sin item_code daba un valor r_squared bastante bajo, por lo que no ajustaba a la mayoria de los valores
## Probamos tambien la variacion polinmica ](grado 2) y el valor de ajuste era simplemente el mismo
## Tambien hicimos con variables independientes hora, dia mes año y no ajustaba, solo con item_code o item_name como factor ajusta el modelo

mod_gases <- lm(average ~ hour +month + year +day +season +item_code , gases)
summary(mod_gases_m)

mod_gases_h <- lm(average ~ hour  + item_code , gases)
mod_gases_d <- lm(average ~ day  + item_code , gases)
mod_gases_m <- lm(average ~ month  + item_code , gases)

ggplot(gases, aes(y=average, x=hour))+ geom_boxplot()+ facet_grid( ~ item_code)
ggplot(gases_O3, aes(y=average, x=hour))+ geom_boxplot() + facet_wrap(~season)
ggplot(gases_CO, aes(y=average, x=hour))+ geom_boxplot() + facet_wrap(~season)
ggplot(gases_CO, aes(y=average, x=hour))+ geom_boxplot()
ggplot(gases_demas, aes(y=average, x=hour))+ geom_boxplot() + facet_wrap(~season)
ggplot(gases_demas, aes(y=average, x=hour))+ geom_boxplot()
ggplot(gases_SO2, aes(y=average, x=hour))+ geom_boxplot()


mod_gases_h <- lm(average ~ poly(as.integer(hour),3)*season, gases_demas)

gases_demas <- gases_demas %>% add_predictions(mod_gases_h)

ggplot(gases_demas, aes(y=average, x=hour))+ 
  geom_boxplot() +
  geom_point(aes(y=pred))+
  geom_line(aes(x=hour, y=pred),color="red")+ facet_wrap( ~ season)

#FILTROS
gases_col <- gases%>% select(-item_code) %>%
  pivot_wider(names_from =item_name , values_from = average)

gas_no2_co <- select(gases_col, -SO2,-O3)

# GRAFICOS RELACIONADOS CON LOS GASES 
# so2 y o3
xso2_yo3<-ggplot(gases_col)+ 
  geom_point(aes(x=SO2, y =O3)) +
  labs(title = "Relación entre las emisiones de SO2 y O3",
       subtitle = "Durante los años 2017,2018 y 2019",
       y = "Emisiones de ozono (O3) [ppm]",
       x ="Emisiones de dioxido de sulfuro(SO2) [ppm]")

yso2_xyo3<-ggplot(gases_col)+ 
  geom_point(aes(y=SO2, x =O3)) +
  labs(title = "Relación entre las emisiones de SO2 y O3",
       subtitle = "Durante los años 2017,2018 y 2019",
       x = "Emisiones de O3 [ppm]",
       y ="Emisiones de SO2 [ppm]")

#no2 y o3
xno2_yo3<-ggplot(gases_col)+ 
  geom_point(aes(x = NO2, y = O3)) +
  labs(title = "Relación entre las emisiones de NO2 y O3",
       subtitle = "Durante los años 2017,2018 y 2019",
       y = "Emisiones de O3 [ppm]",
       x ="Emisiones de NO2 [ppm]")

yno2_xo3<-ggplot(gases_col)+ 
  geom_point(aes(y = NO2, x = O3)) +
  labs(title = "Relación entre las emisiones de NO2 y O3",
       subtitle = "Durante los años 2017,2018 y 2019",
       x = "Emisiones de O3 [ppm]",
       y ="Emisiones de NO2 [ppm]")

#SO2 y NO2 
xso2_yNO2<-ggplot(gases_col)+ 
  geom_point(aes(x=SO2, y =NO2))+
  labs(title = "Relación entre las emisiones de SO2 y NO2",
       subtitle = "Durante los años 2017,2018 y 2019",
       y = "Emisiones de NO2 [ppm]",
       x ="Emisiones de SO2 [ppm]")

yso2_xNO2<-ggplot(gases_col)+ 
  geom_point(aes(y = SO2, x =NO2))+
  labs(title = "Relación entre las emisiones de SO2 y NO2",
       subtitle = "Durante los años 2017,2018 y 2019",
       x = "Emisiones de NO2 [ppm]",
       y ="Emisiones de SO2 [ppm]")

#SO2 y CO
xso2_yco<-ggplot(gases_col)+ 
  geom_jitter(aes(x=SO2, y=CO),shape=20)+
  labs(title = "Relación entre las emisiones de SO2 y CO",
       subtitle = "Durante los años 2017,2018 y 2019",
       x = "Emisiones de diocido de sulfuro (SO2) [ppm]",
       y ="Emisiones de monoximo de carbono (CO) [ppm]")

yso2_xco<-ggplot(gases_col)+ 
  geom_jitter(aes(x=SO2, y=CO),shape=20)+
  labs(title = "Relación entre las emisiones de SO2 y CO",
       subtitle = "Durante los años 2017,2018 y 2019",
       x = "Emisiones de diocido de sulfuro (SO2) [ppm]",
       y ="Emisiones de monoximo de carbono (CO) [ppm]")

# O3 y CO
xo3_yco <- ggplot(gases_col)+
  geom_jitter(aes(x=O3, y=CO),shape=20)+
    labs(title = "Relación entre las emisiones de O3 y CO",
         subtitle = "Durante los años 2017,2018 y 2019",
         x = "Emisiones de O3 [ppm]",
         y ="Emisiones de CO [ppm]")

yO3_xCO <- ggplot(gases_col) +
  geom_jitter(aes(y=O3, x=CO),shape=20)+
  labs(title = "Relación entre las emisiones de O3 y CO",
       subtitle = "Durante los años 2017,2018 y 2019",
       y = "Emisiones de O3 [ppm]",
       x ="Emisiones de CO [ppm]")

#NO2 y CO
xno2_yco <- ggplot(gases_col)+
  geom_jitter(aes(x=NO2, y=CO),shape=20)+
  labs(title = "Relación entre las emisiones de NO2 y CO",
       subtitle = "Durante los años 2017,2018 y 2019",
       x = "Emisiones de NO2 [ppm]",
       y ="Emisiones de CO [ppm]")

yno2_xco <- ggplot(gases_col)+
  geom_jitter(aes(y=NO2, x=CO),shape=20)+
  labs(title = "Relación entre las emisiones de NO2 y CO",
       subtitle = "Durante los años 2017,2018 y 2019",
       y = "Emisiones de NO2 [ppm]",
       x ="Emisiones de CO [ppm]")


#FIN DE RELACIONAR GASES
# GRAFICOS DEL MODELADO ------------------------------

graf_jitter1 <- ggplot(gases_col)+
  geom_jitter(aes(y=NO2, x=CO),shape=20)+
  labs(title = "Relación entre las emisiones de NO2 y CO según temporada",
      subtitle = "Durante los años 2017, 2018 y 2019",
      y = "Emisiones de NO2 [ppm]",
      x ="Emisiones de CO [ppm]")

graf_jitter2 <- ggplot(gases_col)+
  geom_jitter(aes(y=NO2, x=CO),shape=20)+
  labs(title = "Relación entre las emisiones de NO2 y CO según temporada",
      subtitle = "Durante los años 2017, 2018 y 2019",
      y = "Emisiones de NO2 [ppm]",
      x ="Emisiones de CO [ppm]") +
  facet_wrap(season ~ region)

NO2_CO_t<-ggplot(gases_col )+
  geom_jitter(aes(x=NO2, y=CO),shape=20)+
  labs(title = "Relación entre las emisiones de NO2 y CO según temporada",
       subtitle = "Durante los años 2017, 2018 y 2019",
       x = "Emisiones de NO2 [ppm]",
       y ="Emisiones de CO [ppm]")+
  facet_wrap(~ season)

NO2_CO_d<-ggplot(gases_col )+
  geom_jitter(aes(x=NO2, y=CO),shape=20)+
  labs(title = "Relación entre las emisiones de NO2 y CO según dia del mes",
       subtitle = "Durante los años 2017, 2018 y 2019",
       x = "Emisiones de NO2 [ppm]",
       y ="Emisiones de CO [ppm]")+
  facet_wrap(~ factor(day))

NO2_CO_d2<-ggplot(gases_col )+
  geom_jitter(aes(y=NO2, x=CO),shape=20)+
  labs(title = "Relación entre las emisiones de NO2 y CO según dia del mes",
       subtitle = "Durante los años 2017,2018 y 2019",
       y = "Emisiones de NO2 [ppm]",
       x ="Emisiones de CO [ppm]")+
  facet_wrap(~ factor(day))

NO2_CO_region_f<-ggplot(gases_col )+
  geom_jitter(aes(x=NO2, y=CO),shape=20)+
  labs(title = "Relación entre las emisiones de NO2 y CO por region",
       subtitle = "Durante las franjas horarias de los años 2017,2018 y 2019",
       x = "Emisiones de dioxido de nitrogeno (NO2) [ppm]",
       y ="Emisiones de monoximo de carbono (CO) [ppm]")+
  facet_grid( factor(time_day) ~ factor(region))

# MODELADOS 

mod1 <- lm(NO2 ~ CO ,gas_no2_co)
mod2 <- lm(NO2 ~ CO + region ,gas_no2_co)
mod3 <- lm(NO2 ~ CO * region ,gas_no2_co)
mod4 <- lm(NO2 ~ CO + season + region ,gas_no2_co)
mod5 <- lm(NO2 ~ CO *region * season ,gas_no2_co)
mod6 <- lm(NO2 ~ CO *season + region ,gas_no2_co)

mod7 <- lm(NO2 ~ CO *year ,gas_no2_co)
summary(mod_NO2_CO_y)

#ANOVA ------------------------
no2_co_anova <- anova( mod1, mod2, mod3, mod4, mod5,mod6)

#SUMMARY PARA COMPARAR
summary(mod3)
summary(mod4)

# Grafico con gas---------------------------------------

grilla_mod4 <- gas_no2_co %>% 
  data_grid(NO2,CO,season,region) %>% 
  add_predictions(model = mod4)

gas_no2_co <- gas_no2_co %>% add_predictions(model=mod1, var="mod0") 


mod4_graf<- ggplot(gas_no2_co) +
  geom_jitter(aes(y = NO2, x = CO), shape = 20, alpha=0.3) +
  geom_line(data = grilla_mod4, aes(y = pred, x = CO, color = season, linetype = "Modelo 4"),alpha=0.7, size = 1) +
  geom_line(aes(y = mod0, x = CO, color = season, linetype = "Modelo 1"), size = 1.5) +
  labs(
    title = "Comparacion de modelados de las emisiones de NO2 y CO por región",
    subtitle = "Durante las temporadas de los años 2017, 2018 y 2019",
    y = "Emisiones de NO2 [ppm]",
    x = "Emisiones de CO [ppm]",
    color = "Temporada",
    linetype = "Tipo de Línea"
  ) +
  facet_grid(region ~ season, scales = "free_y") +
  scale_linetype_manual(values = c("Modelo 4" = "solid", "Modelo 1" = "dashed")) +
  scale_size_manual(values = c("Modelo 4" = 1, "Modelo 1" = 1.5)) +
  guides(
    linetype = guide_legend(title = "Tipo de Línea"),
    size = guide_legend(title = "Tipo de Línea")
  ) +
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.position = "top"
  )
  theme_minimal()

#GRAFICOS DE MAS 

NO2_CO_year <- ggplot(gas_no2_co) +
  geom_jitter(aes(y = NO2, x = CO,color=factor(year)), shape = 10,alpha=0.4) +
  labs(
    title = "Relación entre las emisiones de NO2 y CO ",
    subtitle = "Durante los años 2017, 2018 y 2019",
    y = "Emisiones de NO2 [ppm]",
    x = "Emisiones de CO [ppm]") +
  facet_grid(~year)+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    legend.position = NULL 
  )


NO2_CO_box <- ggplot(gas_no2_co) +
  geom_boxplot(aes(x = NO2, y = CO)) +
  #geom_line(data = grilla_mod9, aes(x = NO2, y = pred, color = region)) +
  labs(
    title = "Relación entre las emisiones de NO2 y CO por región",
    subtitle = "Durante las franjas horarias de los años 2017, 2018 y 2019",
    x = "Emisiones de dióxido de nitrógeno (NO2) [ppm]",
    y = "Emisiones de monóxido de carbono (CO) [ppm]") +
  facet_grid(~region)

NO2_CO_vio <- ggplot(gas_no2_co) +
  geom_violin(aes(x = NO2, y = CO)) +
  #geom_line(data = grilla_mod9, aes(x = NO2, y = pred, color = region)) +
  labs(
    title = "Relación entre las emisiones de NO2 y CO por región",
    subtitle = "Durante las franjas horarias de los años 2017, 2018 y 2019",
    x = "Emisiones de dióxido de nitrógeno (NO2) [ppm]",
    y = "Emisiones de monóxido de carbono (CO) [ppm]") +
  facet_grid(~region)

NO2_CO_timeline <- ggplot(gas_no2_co, aes(x = fecha)) +
  geom_point(aes(y = NO2, color = "NO2")) +
  geom_point(aes(y = CO, color = "CO")) +
  labs(
    title = "Línea de tiempo de las emisiones de NO2 y CO",
    subtitle = "Durante las franjas horarias de los años 2017, 2018 y 2019",
    x = "Fecha",
    y = "Concentración de gases [ppm]"
  ) +
  facet_grid(~region) +
  scale_color_manual(name = "Gases", values = c("NO2" = "blue", "CO" = "red")) +
  theme_minimal()


