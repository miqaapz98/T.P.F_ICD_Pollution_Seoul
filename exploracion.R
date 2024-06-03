library(tidyverse)
library(ggridges)
library(geomtextpath)

glimpse(gases)


gases <- select(gases,-Latitude,-Longitude)


# VIOLINES Y BOXPLOR POR TEMPORADA Y AÑO SIN DINSTINTION DE GASES
box_graf<-ggplot(gases, aes(x=season, y= average, fill=factor(year))) + 
geom_boxplot()+
labs(title="Emisiones s de los gases CO, NO, O3 ,SO2",
subtitle= "Según temporada y año " ,x="Temporada" , y="Emisiones [ppm]")+
scale_fill_discrete(name="Año")+ coord_cartesian(ylim=c(0,0.125))


#FILTRAR GASES --------------------
gases_demas <- filter(gases, item_name %in% c( "NO2","CO"))

gases_demas2 <- filter(gases, item_name %in% c( "SO2","CO"))
gases_SO2 <- filter(gases, item_name %in% c( "SO2"))
gases_CO <- filter(gases, item_name %in% c( "CO"))
gases_NO2 <- filter(gases, item_name %in% c( "NO2"))

gases_NO2 <- filter(gases, item_name %in% c( "CO"))

#VIOLINES Y BOXPLOT DE GASES x=region
demas_vio<-ggplot(gases_demas, aes(x = region, y = average ,fill = region)) +
  geom_violin(draw_quantiles =  c(0.25, 0.5, 0.75))+
  facet_grid(item_name~ year , scales = "free_y") +
  labs(title = "Distribución del Emisiones  de NO2 y CO" ,
  subtitle="por Región y Año",
  x = "Región",
  y ="Emisiones  [ppm]")+
  scale_fill_discrete(name="Region")+
  theme_light() 

#CO SEASON YEAR REGION BOX 
box_co_no2<-ggplot(gas_no2_co, aes(x = region, y = CO , fill = region)) +
  geom_boxplot()+
  facet_grid(season ~ year , scales = "free_y") +
  labs(title = "Distribución de emisiones de CO" ,
       subtitle="Según región , temporada y año",
       x = "Región",
       y ="Emisiones de CO [ppm]")+
  scale_fill_discrete(name ="Región")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
    )

# co region year region
box2_co<-ggplot(gas_no2_co, aes(x = region, y = CO , fill = region)) +
  geom_boxplot()+
  facet_wrap(~ year , scales = "free_y") +
  labs(title = "Distribuciones de emisiones de CO" ,
       subtitle="Según región y año",
       x = "Región",
       y ="Emisiones de CO [ppm]")+
  scale_fill_discrete(name ="Región")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

box2_NO2<-ggplot(gas_no2_co, aes(x = region, y = NO2 , fill = region)) +
  geom_boxplot()+
  facet_wrap(~ year , scales = "free_y") +
  labs(title = "Distribuciones de emisiones de NO2" ,
       subtitle="Según región y año",
       x = "Región",
       y ="Emisiones de NO2 [ppm]")+
  scale_fill_discrete(name ="Región")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

# co season year season
box2_co<-ggplot(gas_no2_co, aes(x = region, y = CO , fill = factor(year))) +
  geom_boxplot()+
  facet_wrap(~ season , scales = "free_y") +
  labs(title = "Distribuciónes de emisiones de CO por región" ,
       subtitle="Según temporada y año",
       x = "Región",
       y ="Emisiones de CO [ppm]",
       fill="Año")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


#estaciones x year
box3_co<-ggplot(gas_no2_co, aes(x = season, y = CO , fill = factor(year))) +
  geom_boxplot()+
  labs(title = "Distribuciones de emisiones de CO por año" ,
       subtitle="Según estaciones del año ",
       x = "Estaciones",
       y ="Emisiones de CO [ppm]",
       fill="Año")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


box_no2<-ggplot(gas_no2_co, aes(x = season, y = NO2 , fill = factor(year))) +
  geom_boxplot()+
  labs(title = "Distribuciones de emisiones de NO2 por año" ,
       subtitle="Según estaciones del año ",
       x = "Estaciones",
       y ="Emisiones de NO2 [ppm]",
       fill="Año")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


vi_no2<-ggplot(gas_no2_co, aes(x = region, y = NO2 , fill = region)) +
  geom_violin(draw_quantiles =  c(0.25, 0.5, 0.75))+
  facet_wrap(~ year , scales = "free_y") +
  labs(title = "Distribución de emisiones de NO2" ,
       subtitle="Según región y año",
       x = "Región",
       y ="Emisiones de NO2 [ppm]")+
  scale_fill_discrete(name ="Región")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

vi2_NO2<-ggplot(gas_no2_co, aes(x = season, y = NO2 , fill = factor(year))) +
  geom_violin(draw_quantiles =  c(0.25, 0.5, 0.75))+
  labs(title = "Distribución de emisiones de NO2 por año" ,
       subtitle="Según estaciones del año ",
       x = "Estaciones",
       y ="Emisiones de NO2 [ppm]",
       fill="Año")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

box2_SO2<-ggplot(gases_col, aes(x = region, y =SO2 , fill = region)) +
  geom_boxplot()+
  facet_wrap(~ year , scales = "free_y") +
  labs(title = "Distribuciones de emisiones de SO2" ,
       subtitle="Según región y año",
       x = "Región",
       y ="Emisiones de SO2 [ppm]")+
  scale_fill_discrete(name ="Región")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

box_O3<-ggplot(gases_col, aes(x = region, y =O3 , fill = region)) +
  geom_boxplot()+
  facet_wrap(~ year , scales = "free_y") +
  labs(title = "Distribuciones de emisiones de O3" ,
       subtitle="Según región y año",
       x = "Región",
       y ="Emisiones de O3 [ppm]")+
  scale_fill_discrete(name ="Región")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

box_so2<-ggplot(gases_col, aes(x = season, y = SO2 , fill = factor(year))) +
  geom_boxplot()+
  labs(title = "Distribuciones de emisiones de SO2 por año" ,
       subtitle="Según estaciones del año ",
       x = "Estaciones",
       y ="Emisiones de SO2 [ppm]",
       fill="Año")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

box_03<-ggplot(gases_col, aes(x = season, y = O3 , fill = factor(year))) +
  geom_boxplot()+
  labs(title = "Distribuciones de emisiones de O3 por año" ,
       subtitle="Según estaciones del año ",
       x = "Estaciones",
       y ="Emisiones de O3 [ppm]",
       fill="Año")+
  theme_light()+
  theme(
    plot.title = element_text(size = 20,face="bold"),
    plot.subtitle = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    strip.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

demas_box<-ggplot(gases_co_no2, aes(x = region, y = average ,fill=region)) +
  geom_boxplot()+
  facet_grid(item_name~ year , scales = "free_y") +
  labs(title = "Distribución del Emisiones  de NO2 y O3" ,
       subtitle="por Región y Año",
       x = "Región",
       y ="Emisiones  [ppm]")+
  scale_fill_discrete(name="Region")+
  theme_light()

v_CO<-ggplot(gases_CO, aes(x = region, y = average ,fill=region)) +
  geom_boxplot()+
  facet_grid(~ year,scales="free_y") +
  labs(title = "Distribución del Emisiones  de CO",
  subtitle="por Región y Año",
  x = "Región",
  y ="Emisiones  [ppm]")+
  scale_fill_discrete(name="Region")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vi_CO<-ggplot(gases_CO, aes(x = region, y = average ,fill=region)) +
  geom_violin(draw_quantiles =  c(0.25, 0.5, 0.75))+
  facet_grid(~ year,scales="free_y") +
  labs(title = "Distribución del Emisiones  de CO",
       subtitle="por Región y Año",
       x = "Región",
       y ="Emisiones  [ppm]")+
  scale_fill_discrete(name="Region")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


v_SO2<-ggplot(gases_SO2, aes(x = region, y = average ,fill=region)) +
  geom_boxplot()+
  facet_grid(~ year,scales="free_y") +
  labs(title = "Distribución del emisiones de SO2",
  subtitle="Por Región y Año",
  x = "Región",
  y ="Emisiones de SO2 [ppm]")+
  scale_fill_discrete(name="Region")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#season 
v_SO2<-ggplot(gases_SO2, aes(x = season, y = average ,fill=factor(year))) +
  geom_boxplot()+
  facet_grid(~ item_name,scales="free_y") +
  labs(title = "Distribución de las emisiones de SO2",
       subtitle="Según temporada y año",
       x = "Temporada",
       y ="Emisiones de SO2 [ppm]")+
  scale_fill_discrete(name="Año")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

v_CO<-ggplot(gases_CO, aes(x = season, y = average ,fill=factor(year))) +
  geom_boxplot()+
  facet_grid(~ item_name,scales="free_y") +
  labs(title = "Distribución de las emisiones de CO",
       subtitle="Según temporada y año",
       x = "Temporada",
       y ="Emisiones de CO [ppm]")+
  scale_fill_discrete(name="Año")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vi_demas<-ggplot(gases_demas, aes(x = season, y = average ,fill=factor(year))) +
  geom_violin(draw_quantiles =  c(0.25, 0.5, 0.75))+
  facet_wrap(~ item_name,scales="free_y",ncol=1) +
  labs(title = "Distribución del Emisiones  de NO2 y O3",
       subtitle="Según temporada y año",
       x = "Temporada",
       y ="Emisiones [ppm]")+
  scale_fill_discrete(name="Año")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vi_demas2<-ggplot(gases_demas2, aes(x = season, y = average ,fill=factor(year))) +
  geom_boxplot(draw_quantiles =  c(0.25, 0.5, 0.75))+
  facet_wrap(~ item_name,scales="free_y",ncol=1) +
  labs(title = "Distribución del Emisiones  de SO2 y CO",
       subtitle="Según temporada y año",
       x = "Temporada",
       y ="Emisiones [ppm]")+
  scale_fill_discrete(name="Año")+
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size=10),
        strip.text = element_text(size = 14))

# hora - gases ----------------------------

#scatter gases
scatter_gases <- ggplot(gases , aes(x=hour, y= average)) + geom_point() +
facet_grid(~item_name,scales="free_y")+
labs(title="Emisiones  de las emisiones gases (SO2,NO2,O3,CO) por hora " ,x="Hora" , 
y="Emisiones [microgram/m3]")


scatter_gases <- ggplot(gases , aes(x=hour, y= average)) + geom_point() +
  facet_grid(~item_name,scales="free_y")+
  labs(title="Emisiones  de las emisiones gases (SO2,NO2,O3,CO) por hora " ,x="Hora" , 
       y="Emisiones [microgram/m3]")

#densidad
density_gases <- ggplot(gases , aes(x=hour)) +
 geom_point() +
  facet_grid(~item_name,scales="free_y")+
  labs(title="Emisiones  de las emisiones gases (SO2,NO2,O3,CO) por hora " ,x="Hora" , 
       y="Emisiones [microgram/m3]")

