library(tidyverse)
library(RColorBrewer)

colnames(inst)

data <- read_csv("C:/Users/mikpz/Documents/Data/tp_final/dataset/instrumento.csv")

conteo2 <- data %>%
  mutate(instrument_status_grouped = ifelse(instrument_status %in% c(2, 4,8, 1, 9), "Grouped", "0")) %>%
  group_by(año, instrument_status_grouped) %>%
  summarise(Cantidad = sum(n()))


totales <- data %>%group_by(año) %>%summarise(Totales = n()) %>% ungroup()

porc<- conteo2 %>%
  left_join(totales, by = c("año")) %>%
  mutate(porcentaje = round((Cantidad / Totales) * 100,2))


bar2 <- ggplot(porc, aes(x = factor(año), y = porcentaje, fill = factor(instrument_status_grouped))) +
  geom_bar(stat = "identity",width=0.7) +
  labs(title = "Porcentaje del estado del instrumento por año",
      x = "Año",
      y = "Porcentaje [%]",
      fill = "Estado del Instrumento")+
  theme_minimal()+
  geom_text(aes(label = porcentaje), vjust = -0.5, colour = "black")+
  scale_fill_manual(values = c( "#EED3D9", "#41B7C4"),
  labels=c("0: Normal ",
  "Otros: Falta calibrar, en reparacion ...")
  )
