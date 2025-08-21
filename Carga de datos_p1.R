#* Función integradora
pvica <- function(x) {
  a1 <- openxlsx::read.xlsx(x, startRow = 2, na.strings = "null")
  b1 <- openxlsx::read.xlsx(x, startRow = 2, na.strings = "null", colNames = TRUE)
  
  nombres <- names(a1)[37:length(names(a1))]
  nombres <- make.unique(nombres, sep = "_")
  nombres <- gsub("_(\\d+)$", "_", nombres)
  fijos <- c(
    "UBIGEO", "CCPP", "DEP", "PROV", "DIST", "AMBT", "POB_TOTAL", "POB_SERV", "QUINTIL",
    "DD", "DA", "CODIGO_RED", "NOMBRE_RED", "DIRESA", "RED_SALUD", "MICRO_RED", "ID_SISTEMA",
    "TIPO_SISTEMA", "NOMBRE_SISTEMA", "ID_PROVEEDOR", "TIPO_PROVEEDOR", "NOMBRE_PROVEEDOR",
    "NRO_MUESTREO", "NRO_MUESTRA", "ESTE", "NORTE", "HUSO", "ALTURA", "ESTADO_MUESTREO",
    "FECHA_MUESTREO", "FECHA_FINALIZADO", "ID_MUESTREO", "UBICACION_MUESTREO", "NOMBRE_MUESTREO",
    "HORAS_DIA", "DIAS_SEMANA"
  )
  ubicacion <- b1 %>%
    set_names(c(fijos, nombres)) %>%
    slice(-1) %>% mutate(NRO_MUESTRA = as.numeric(NRO_MUESTRA),
                  NRO_MUESTREO = as.numeric(NRO_MUESTREO),
                DD = as.numeric(DD))
  ubicacion
}

library(tidyverse)
library(openxlsx)
tablas_datos <- list.files(path = "datasets", pattern = "*.xlsx", full.names = TRUE)
tabla <- map(tablas_datos, pvica) %>% bind_rows() %>%
  mutate(organismos_de_vida_libre = rowSums(select(., starts_with("Organismos")), na.rm = TRUE),
        Color_ = rowSums(select(., starts_with("Color.")), na.rm = TRUE)) %>% select(-starts_with("Color.")) %>%
  select(-starts_with("Organismos."))

#* Dividiendo la data:
ambiental <- tabla %>% select(c(
    "UBIGEO", "CCPP", "DEP", "PROV", "DIST", "AMBT", "POB_TOTAL", "POB_SERV", "QUINTIL",
    "DD", "DA", "CODIGO_RED", "NOMBRE_RED", "DIRESA", "RED_SALUD", "MICRO_RED", "ID_SISTEMA",
    "TIPO_SISTEMA", "NOMBRE_SISTEMA", "ID_PROVEEDOR", "TIPO_PROVEEDOR", "NOMBRE_PROVEEDOR",
    "NRO_MUESTREO", "NRO_MUESTRA", "ESTE", "NORTE", "HUSO", "ALTURA", "ESTADO_MUESTREO",
    "FECHA_MUESTREO", "FECHA_FINALIZADO", "ID_MUESTREO", "UBICACION_MUESTREO", "NOMBRE_MUESTREO",
    "HORAS_DIA", "DIAS_SEMANA"
  ), ends_with("_")) %>% filter(UBICACION_MUESTREO == "Fuente de captación")

salud <- tabla %>% select(c(
    "UBIGEO", "CCPP", "DEP", "PROV", "DIST", "AMBT", "POB_TOTAL", "POB_SERV", "QUINTIL",
    "DD", "DA", "CODIGO_RED", "NOMBRE_RED", "DIRESA", "RED_SALUD", "MICRO_RED", "ID_SISTEMA",
    "TIPO_SISTEMA", "NOMBRE_SISTEMA", "ID_PROVEEDOR", "TIPO_PROVEEDOR", "NOMBRE_PROVEEDOR",
    "NRO_MUESTREO", "NRO_MUESTRA", "ESTE", "NORTE", "HUSO", "ALTURA", "ESTADO_MUESTREO",
    "FECHA_MUESTREO", "FECHA_FINALIZADO", "ID_MUESTREO", "UBICACION_MUESTREO", "NOMBRE_MUESTREO",
    "HORAS_DIA", "DIAS_SEMANA"
  ), !ends_with("_")) %>% filter(UBICACION_MUESTREO != "Fuente de captación")

openxlsx::write.xlsx(lst(salud, ambiental), "dataset_diresa_tacna.xlsx")

#* Generando data espacial:
library(sf)
library(geoidep)

xd <- geoidep::get_departaments()
tacna <- xd %>% filter(nombdep == "TACNA") %>% st_transform(crs = 32719)
puntos <- tabla %>% select(1:35) %>%
  distinct() %>% 
  st_as_sf(coords = c("ESTE", "NORTE"),
           crs = 32719, remove = FALSE)
# Puntos fuera y dentro de tacna:
puntos_fuera <- puntos[!st_intersects(puntos, tacna, sparse = FALSE), ]
puntos_dentro <- puntos[st_intersects(puntos, tacna, sparse = FALSE), ]
st_write(puntos_dentro, "monitoreo_diresa2.gpkg")

fechados <- puntos_dentro %>% 
  mutate(FECHA_MUESTREO = as.Date(FECHA_MUESTREO, format = "%d-%m-%Y"),
          year = format(FECHA_MUESTREO, format = "%Y"))

#! Graficando xd
#* Points per year:
graph1 <- ggplot() +
  geom_sf(data = tacna) +
  geom_sf(data = fechados) + 
  theme_bw() + scale_fill_brewer(palette = "Blues") +
  labs(color = "Horas/día", 
       title = "Ubicación de los puntos de monitoreo de la DIRESA - Tacna", 
       subtitle = "Calidad de agua - Según ubicación de sus muestreos") +
  theme(legend.position = "bottom", 
        plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20)) +
  facet_grid(year~UBICACION_MUESTREO)

ggsave("mapa_diresa.pdf", graph1, width = 210, height = 297, units = "mm")
View(puntos_fuera)


write.xlsx(puntos_fuera %>% 
  st_drop_geometry(), "reporte_puntos_fuera.xlsx")



ggplot() +
  geom_sf(data = tacna, fill = "transparent", lwd = 0.3) +
  geom_hex(data = fechados, aes(x = ESTE, y = NORTE), bins = 10, alpha = 0.9, color = "white", lwd = 0.3) + 
  theme_bw() + scale_fill_viridis_c() +
  labs(color = "Horas/día", 
       title = "Ubicación de los puntos de monitoreo de la DIRESA - Tacna", 
       subtitle = "Calidad de agua - Según ubicación de sus muestreos",
      fill = "Monitoreos") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
        plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 12),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 20),
        legend.position = "top", axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 7, angle = 90, , hjust = 0.5),
        axis.title = element_blank(), strip.background = element_rect(fill = "white"),
        legend.title = element_text(hjust = 0.7, vjust = 0.7, face = "bold")) +
  facet_grid(year~UBICACION_MUESTREO) +
  guides(fill = guide_colourbar(barwidth = 30,
                                barheight = 1)) -> graph2

ggsave("mapa_diresa2.pdf", graph2, width = 210, height = 297, units = "mm")
