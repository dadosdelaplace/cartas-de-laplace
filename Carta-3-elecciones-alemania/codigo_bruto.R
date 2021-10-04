rm(list = ls()) # borramos entorno
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # fijamos directorio

# Librerías y paquetes
library(tidyverse) # Manejo de datos
library(lubridate) # Manejo de fechas
library(stringi) # Manejo de cadenas de texto
library(purrr) # Operaciones con listas
library(grDevices) # Paletas de colores y fuentes
library(patchwork) # Componer gráficas
library(cowplot) # Componer gráficas
library(showtext) # Fuentes
library(here) # Rutas y archivos
library(png) # lectura de png
library(grid) # raster de imágenes
library(glue) # pegar cómodo
library(ggbump) # curvas sigmoides
library(ggtext) # textos (y en html)
library(Rfast) # Cálculo de máximos/mínimos eficientes
library(rvest) # leer datos web
library(biscale) # Escala bivariante
font_add_google("Lobster Two", "lobstertwo")
font_add_google("Poppins", "poppins")
font_add_google("Titillium Web", "titillium")
showtext_auto() 

# #########################################
# CARGA DE DATOS ELECTORALES Y LIMPIEZA
# #########################################

fechas <- c("2021-09-26", "2017-09-24", "2013-09-22", "2009-09-27")

# Cargamos datos, filtramos columnas,
# filtramos solo primeras votaciones, pasamos votos a números,
# unimos CDU + CSU, calculamos el OTROS, editamos nombres
datos_por_fecha <- list()
for (i in 1:length(fechas)) {
  
  # brutos
  datos_brutos <-
    read_delim(file = glue("./DATOS/{year(fechas[i])}.csv"),
               delim = ";")
  
  if (year(fechas[i]) %in% c(2013, 2009)) {
    
    # Filtramos datos
    datos_filtrados <- datos_brutos %>%
      # Seleccionamos columnas
      select(starts_with(c("Nr", "gehört", "Gebiet", "Gültige",
                           "CDU", "SPD", "FDP", "DIE LINKE", 
                           "GRÜNE", "CSU")))
    datos_filtrados$AfD <- 0
    datos_filtrados <- datos_filtrados %>%
      relocate(AfD, .before = 7)
    
  } else {
    
    # Filtramos datos
    datos_filtrados <- datos_brutos %>%
      # Seleccionamos columnas
      select(starts_with(c("Nr", "gehört", "Gebiet", "Gültige",
                           "Christlich Demokratische",
                           "Sozialdemokratische",
                           "Alternative für Deutschland",
                           "Freie Demokratische", "DIE LINKE", 
                           "BÜNDNIS 90/DIE GRÜNEN",
                           "Christlich-Soziale")))
  }
  
  datos_filtrados <- datos_filtrados %>%
    # Quitamos la fila primera de NA
    slice(-(1:2)) %>%
    # Pasamos variables de votos a números
    mutate_at(vars(!contains(c("Gebiet"))), as.numeric)
    
  # nombres variables
  names(datos_filtrados) <- 
    c("id", "id2", "distritos",  "validos", "CDU", "SPD", "AfD",
      "FDP", "LINKE", "GRÜNE", "CSU")
  
  # Unimos CDU + CSU y calculamos el OTROS
  datos_filtrados <- datos_filtrados %>% 
    mutate("CDU_CSU" = datos_filtrados %>% 
             select(c("CDU", "CSU")) %>%
             replace(is.na(.), 0) %>%
             rowSums) %>%
    # Eliminamos CDU/CSU antiguas
    select(-c("CDU", "CSU")) %>%
    # Cambiamos orden de columna
    relocate(CDU_CSU, .before = 5) %>%
    # Sumamos en OTROS los votos de todos esos partidos
    mutate("OTROS" = rowSums(select(., -(1:4))))
  
  # Añadimos OTROS partidos
  datos_filtrados$OTROS <- datos_filtrados$validos -
    datos_filtrados$OTROS
  
  # Eliminamos filas vacías o agregadas
  datos_filtrados <- datos_filtrados %>% filter(!is.na(id)) %>%
    filter(id2 != 99) %>% filter(validos != 0)
  
  # Guardamos
  datos_por_fecha[[as.character(fechas[i])]] <-
    datos_filtrados
}

# Guardamos nombres partidos 
partidos <- names(datos_por_fecha$`2017-09-24`)[-(1:4)]

# ##################
# ESTADÍSTICAS
# ##################

# Reemplazamos NA
datos_por_fecha <- 
  map(datos_por_fecha,
      function(x) { x %>%
          mutate_all(~replace(., is.na(.), 0)) })



# Calculamos votos en % respecto a votos válidos
datos_por_fecha <-
  map(datos_por_fecha,
      function(x) { x %>% mutate_at(partidos,
                                    funs("porc" = 100 * (./validos))) %>%
          rowwise() %>%
          mutate("primero" =
                   partidos[which.max(c_across(CDU_CSU:OTROS))],
                 "primer.partido" = max(c_across(CDU_CSU_porc:OTROS_porc)),
                 "segundo" = partidos[nth(c_across(CDU_CSU:OTROS), k = 2,
                                          descending = TRUE,
                                          index.return = TRUE)],
                 
                 "segundo.partido" = nth(c_across(CDU_CSU_porc:OTROS_porc), k = 2,
                                         descending = TRUE,
                                         index.return = FALSE))} )

# Datos globales de Alemania en cada elección
datos1 <- datos_por_fecha$`2021-09-26` %>% 
  select_if(is.numeric) %>%
  select(-contains(c("id", "_porc", "validos", "primer", "segundo"))) %>% 
  pivot_longer(everything()) %>% group_by(name) %>%
  summarize(sum(value, na.rm = TRUE)) %>% mutate("eleccion" = 2021)
datos2 <- datos_por_fecha$`2017-09-24` %>% 
  select_if(is.numeric) %>%
  select(-contains(c("id", "_porc", "validos", "primer", "segundo"))) %>% 
  pivot_longer(everything()) %>% group_by(name) %>%
  summarize(sum(value, na.rm = TRUE)) %>% mutate("eleccion" = 2017)
datos3 <- datos_por_fecha$`2013-09-22` %>% 
  select_if(is.numeric) %>%
  select(-contains(c("id", "_porc", "validos", "primer", "segundo"))) %>% 
  pivot_longer(everything()) %>% group_by(name) %>%
  summarize(sum(value, na.rm = TRUE)) %>% mutate("eleccion" = 2013)
datos4 <- datos_por_fecha$`2009-09-27` %>% 
  select_if(is.numeric) %>%
  select(-contains(c("id", "_porc", "validos", "primer", "segundo"))) %>% 
  pivot_longer(everything()) %>% group_by(name) %>%
  summarize(sum(value, na.rm = TRUE)) %>% mutate("eleccion" = 2009)
# Globales uniendo todas las elecciones
datos_global_alemania <- rbind(datos1, datos2, datos3, datos4)
names(datos_global_alemania) <- c("name", "value", "eleccion")



# ##################
# SHAPES (MAPAS)
# ##################

# Distritos electorales: cargamos shp
mapa <-
  sf::read_sf("./DATOS/shapes/Geometrie_Wahlkreise_20DBT_geo.shp")

# Cruce
mapa_sf_datos <- map(datos_por_fecha, 
                      function(x, y) { left_join(y, x, by = c("WKR_NR" = "id")) },
                     mapa)


# ########################
# FUNCIONES Y PARÁMETROS
# ########################

# Paletas de colores
# partidos: "CDU_CSU", "SPD", ""DIE_LINKE", GRUNE", "FDP", "AfD", "OTROS"
colores <- c("Grays", "Reds", "Blues", "Yellows",
             "Burg", "Greens3", "Purples")

# Títulos escala
titulos_escala <-
  c("% votos CDU/CSU", "% votos SPD", 
    "% votos AfD (ultraderecha)", "% votos FDP (liberales)",
    "% votos DIE LINKE (izquierda)", "% votos GRÜNE (verdes)",
    "% votos otros partidos")

# Saltos para definir la escala (100 = inf, -1)
saltos <-
  list(c(5, 10, 15, 20, 25, 30, 35, Inf),
       c(5, 10, 15, 20, 25, 30, 35, Inf),
       c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 100),
       c(0, 2, 4, 6, 8, 10, 12, 100),
       c(0, 3, 6, 9, 12, 15, 18, 21, 100),
       c(0, 2, 4, 6, 8, 10, 12, 14, 16, 100),
       c(0, 2, 4, 6, 8, 10, 12, 14, 16, 100))

# Mapa % de votos
mapa_porc_votos_fecha <-
  function(mapa_sf_datos, datos, partido = "CDU_CSU", fecha = "2017-09-24",
           titulo = "% de votos de la CDU/CSU", colores = "Grays",
           saltos = c(10, 15, 20, 25, 30, 35, 40, 50, 75),
           paleta = rev(hcl.colors(length(saltos), colores)),
           labs_plot = prettyNum(saltos, big.mark = " "),
           tipo_var = "_porc", keywidth = 2.5, keyheight = 1) {
    
    # CDU_CSU, SPD, GRUNE, FDP, DIE_LINKE, AfD, OTROS
    variable <- paste0(partido, tipo_var)
    
    # Rangos
    datos <- mapa_sf_datos[[fecha]]
    datos$rangos <- cut(datos[[variable]], saltos,
                        include.lowest = TRUE, dig.lab = 3)
    
    # Calculamos las fronteras
    fronteras <- datos %>% sf::st_cast("MULTILINESTRING")
    
    # ggplot2
    grafica <-
      ggplot(datos) +
      geom_sf(aes(fill = rangos), size = 0.02, color = "black", alpha = 1) +
      geom_sf(data = fronteras, col = "black", size = 0.25) +
      scale_fill_manual(name = titulo,
                        values = paleta,
                        labels = glue("{labs_plot}%"), drop = FALSE,
                        guide =
                          guide_legend(direction = "horizontal",
                                       keyheight = keyheight,
                                       keywidth = keywidth,
                                       title.position = "top",
                                       title.hjust = 0.5, label.hjust = .5,
                                       nrow = 1, byrow = TRUE, reverse = FALSE,
                                       label.position = "bottom")) +
      theme(
        # título
        plot.title = element_text(size = 13, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
        # subtítulo
        plot.subtitle = element_text(size = 5, color = "grey20",
                                     family = "titillium", hjust = 0.5),
        legend.title = element_text(color = rev(paleta)[1]))
    
    # Output
    return(grafica)
    
  }


# Mapa de ganadores
mapa_ganadores_fecha <-
  function(mapa_sf_datos, datos, fecha = "2017-09-24",
           titulo = "Primer partido en votos", colores = "Grays",
           partidos = c("CDU/CSU", "SPD", "GRÜNE", "FDP",
                        "DIE LINKE", "AfD", "OTROS"),
           paleta = c("#7e9fce", "#494949", "#b8637a", "#f0c553",
                      "#5ba06a", "#877ab5", "#e35565"),
           labs_plot = c("AfD", "CDU", "DIE LINKE", "FDP",
                         "GRÜNE",  "OTROS", "SPD"),
           keywidth = 2, keyheight = 0.8) {
 
    # Rangos
    datos <- mapa_sf_datos[[fecha]]

    # Calculamos las fronteras
    fronteras <- datos %>% sf::st_cast("MULTILINESTRING")
    
    # ggplot2
    grafica <-
      ggplot(datos) +
      geom_sf(aes(fill = primero), size = 0.02, color = "black", alpha = 1) +
      geom_sf(data = fronteras, col = "black", size = 0.25)  +
      scale_colour_manual(name = glue("{titulo}"),
                          aesthetics = c("colour", "fill"),
                          values = c("AfD" = "#7e9fce", "CDU_CSU" = "#494949",
                                     "LINKE" = "#b8637a", "FDP" = "#f0c553",
                                     "GRÜNE" = "#5ba06a", "OTROS" = "#877ab5",
                                     "SPD" = "#e35565"),
                          labels = c("AfD", "CDU/CSU", "LINKE", "FDP",
                                     "DIE LINKE", "OTROS", "SPD"),
                          drop = FALSE,
                          guide =
                            guide_legend(direction = "horizontal",
                                         keyheight = keyheight,
                                         keywidth = keywidth,
                                         title.position = "top",
                                         title.hjust = 0.5, label.hjust = .5,
                                         nrow = 1, byrow = TRUE, reverse = FALSE,
                                         label.position = "bottom")) +
      theme(
        # título
        plot.title = element_text(size = 13, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
        # subtítulo
        plot.subtitle = element_text(size = 5, color = "grey20",
                                     family = "titillium", hjust = 0.5),
        legend.title = element_text(color = "grey20", face = "bold"))
    
    # Output
    return(grafica)
    
  }

# Mapa de segundos
mapa_segundos_fecha <-
  function(mapa_sf_datos, datos, fecha = "2017-09-24",
           titulo = "Segundo partido en votos", colores = "Grays",
           partidos = c("CDU/CSU", "SPD", "GRÜNE", "FDP",
                        "DIE LINKE", "AfD", "OTROS"),
           paleta = c("#7e9fce", "#494949", "#b8637a", "#f0c553",
                      "#5ba06a", "#877ab5", "#e35565"),
           labs_plot = c("AfD", "CDU", "DIE LINKE", "FDP",
                         "GRÜNE",  "OTROS", "SPD"),
           keywidth = 2, keyheight = 0.8) {
    
    # Rangos
    datos <- mapa_sf_datos[[fecha]]
    
    # Calculamos las fronteras
    fronteras <- datos %>% sf::st_cast("MULTILINESTRING")
    
    # ggplot2
    grafica <-
      ggplot(datos) +
      geom_sf(aes(fill = segundo), size = 0.02, color = "black", alpha = 1) +
      geom_sf(data = fronteras, col = "black", size = 0.25) +
      scale_colour_manual(name = glue("{titulo}"),
                          aesthetics = c("colour", "fill"),
                          values = c("AfD" = "#7e9fce", "CDU_CSU" = "#494949",
                                     "LINKE" = "#b8637a", "FDP" = "#f0c553",
                                     "GRÜNE" = "#5ba06a", "OTROS" = "#877ab5",
                                     "SPD" = "#e35565"),
                          labels = c("AfD", "CDU/CSU", "LINKE", "FDP",
                                     "GRÜNE", "OTROS", "SPD"),
                          drop = FALSE,
                          guide =
                            guide_legend(direction = "horizontal",
                                         keyheight = keyheight,
                                         keywidth = keywidth,
                                         title.position = "top",
                                         title.hjust = 0.5, label.hjust = .5,
                                         nrow = 1, byrow = TRUE, reverse = FALSE,
                                         label.position = "bottom")) +
      theme(
        # título
        plot.title = element_text(size = 13, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
        # subtítulo
        plot.subtitle = element_text(size = 5, color = "grey20",
                                     family = "titillium", hjust = 0.5),
        legend.title = element_text(color = "grey20", face = "bold"))
    
    # Output
    return(grafica)
    
  }

mapa_diferencia_fecha <-
  function(mapa_sf_datos, datos, fecha = "2017-09-24",
           titulo = "Diferencia en votos entre primero y segundo",
           colores = "BurgYl",
           saltos = c(0, 5, 10, 15, 20, 25, 30, 35, 40, Inf),
           paleta = rev(hcl.colors(length(saltos), colores)),
           labs_plot = prettyNum(saltos, big.mark = " "),
           keywidth = 2, keyheight = 0.8) {
    
    # Rangos
    datos <- mapa_sf_datos[[fecha]]
    datos$rangos <- cut(datos$primer.partido - datos$segundo.partido,
                        saltos, include.lowest = TRUE, dig.lab = 3)
    
    # Calculamos las fronteras
    fronteras <- datos %>% sf::st_cast("MULTILINESTRING")
    
    # ggplot2
    grafica <-
      ggplot(datos) +
      geom_sf(aes(fill = rangos), size = 0.02, color = "black", alpha = 1) +
      geom_sf(data = fronteras, col = "black", size = 0.25) +
      scale_fill_manual(name = titulo,
                        values = paleta,
                        labels = glue("{labs_plot}%"), drop = FALSE,
                        guide =
                          guide_legend(direction = "horizontal",
                                       keyheight = keyheight,
                                       keywidth = keywidth,
                                       title.position = "top",
                                       title.hjust = 0.5, label.hjust = .5,
                                       nrow = 1, byrow = TRUE, reverse = FALSE,
                                       label.position = "bottom")) +
      theme(
        # título
        plot.title = element_text(size = 13, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
        # subtítulo
        plot.subtitle = element_text(size = 5, color = "grey20",
                                     family = "titillium", hjust = 0.5),
        legend.title = element_text(color = "grey20", face = "bold"))
    
    # Output
    return(grafica)
    
  }

mapas_bivariantes <-
  function(nuts, datos, fecha = "2017-09-24",
           titulo = "Segundo partido en votos",
           partido_1 = "primer.partido", partido_2 =  "segundo.partido",
           paleta = "Brown", xmin = 2, xmax = 6.5,
           ymin = 52.8, ymax = 57.3) {
    
    # Rangos
    datos <- mapa_sf_datos[[fecha]]
    
    # Calculamos las fronteras
    fronteras <- datos %>% sf::st_cast("MULTILINESTRING")
    
    # Creamos clase bivariante
    maximo_1 <- max(datos[[partido_1]])
    maximo_2 <- max(datos[[partido_2]])
    datos_bivar <-
      bi_class(datos, x = .data[[partido_1]],
               y = .data[[partido_2]],
               style = "quantile", dim = 3)
    
    # Leyenda bivariante
    leyenda_bivar <- bi_legend(pal = paleta, dim = 3, xlab = partido_1,
                               ylab = partido_2, size = 9)
    
    # ggplot2
    grafica <-
      ggplot(datos_bivar) +
      geom_sf(aes(fill = bi_class), size = 0.02,
              color = "black", alpha = 1, show.legend = FALSE) +
      geom_sf(data = fronteras, col = "black", size = 0.25)  +
      bi_scale_fill(pal = paleta, dim = 3, na.value = "grey90") +
      annotation_custom(ggplotGrob(leyenda_bivar), 
                        xmin = xmin, xmax = xmax,
                        ymin = ymin, ymax = ymax) +
      labs(title = "Mapa choropleth bivariante") +
      theme(
        # título
        plot.title = element_text(size = 13, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
        # subtítulo
        plot.subtitle = element_text(size = 5, color = "grey20",
                                     family = "titillium", hjust = 0.5),
        legend.title = element_text(color = "grey20", face = "bold"))
    
    # Output
    return(grafica)
    
  }
# #############################
# DATAVIZ
# #############################

# Ajustes comunes de las gráficas
theme_set(theme_void())
theme_update(
  # leyenda
  legend.text = element_text(size = 6, color = "grey20",
                             family = "titillium"),
  legend.title = element_text(face = "bold", size = 8, color = "grey20",
                              family = "titillium"),
  legend.position = "bottom",
  # ejes/grid
  panel.background = element_rect(fill = "white", color = "white"),
  # márgenes
  plot.margin = margin(t = 0.2, b = 0.2, r = 0.1, l = 0.1, "cm"))



# ##################################
# MAPAS % votos (escala adaptada)
# ##################################

grafica_escala_adaptada <- list()
for (p in 1:length(partidos)) {
  
  grafica_escala_adaptada[[partidos[p]]] <- list()
  for (i in 1:length(fechas)) {
    
    if (colores[p] == "Yellows") {
      
      grafica_escala_adaptada[[partidos[p]]][[i]] <-
        mapa_porc_votos_fecha(mapa_sf_datos, datos_por_fecha, partido = partidos[p],
                              fecha = fechas[i], colores = colores[p],
                              titulo = titulos_escala[p],
                              saltos = saltos[[p]],
                              paleta = colorRampPalette(c("#FDFDF8", "#E5B125"))(length(saltos[[p]])))
      
    } else {
      
      grafica_escala_adaptada[[partidos[p]]][[i]] <-
        mapa_porc_votos_fecha(mapa_sf_datos, datos_por_fecha, partido = partidos[p],
                              fecha = fechas[i], colores = colores[p],
                              titulo = titulos_escala[p],
                              saltos = saltos[[p]])
    }
  }
  names(grafica_escala_adaptada[[partidos[p]]]) <- fechas
}

# ##################################
# MAPAS % votos (escala unificada)
# ##################################

grafica_escala_unificada <- list()
saltos_unificados <-
  c(0, 5, 10, 15, 20, 25, 30, 35, 100)
for (p in 1:length(partidos)) {
  
  grafica_escala_unificada[[partidos[p]]] <- list()
  for (i in 1:length(fechas)) {
    
    if (colores[p] == "Yellows") {
      
      grafica_escala_unificada[[partidos[p]]][[i]] <-
        mapa_porc_votos_fecha(mapa_sf_datos, datos_por_fecha, partido = partidos[p],
                              fecha = fechas[i], colores = colores[p],
                              titulo = titulos_escala[p],
                              saltos = saltos_unificados,
                              colorRampPalette(c("#FDFDF8", "#E5B125"))(length(saltos_unificados)),
                              keywidth = 0.8, keyheight = 0.4)
      
    } else {
      
      grafica_escala_unificada[[partidos[p]]][[i]] <-
        mapa_porc_votos_fecha(mapa_sf_datos, datos_por_fecha, partido = partidos[p],
                              fecha = fechas[i], colores = colores[p],
                              titulo = titulos_escala[p],
                              saltos = saltos_unificados,
                              keywidth = 0.75, keyheight = 0.4)
    }
  }
  names(grafica_escala_unificada[[partidos[p]]]) <- fechas
}

# ##################################
# BARRAS con la distribución de votos por elección
# ##################################

distrib_barras <-
  ggplot(datos_global_alemania,
         aes(y = value, x = as.factor(eleccion), fill = name)) +
  # position = "fill" para que sean de igual tamaño las barras
  geom_bar(position = "fill", stat = "identity", width = 0.8) +
  # Barras horizontales
  coord_flip() +
  # Escala de colores (orden alfabético de los partidos)
  scale_fill_manual(values = c("#7e9fce", "#494949", "#f0c553",
                               "#5ba06a", "#b8637a", "#877ab5",
                               "#e35565"),
                    labels = c("AfD", "CDU/CSU", "FDP", "GRÜNE", 
                               "LINKE", "OTROS", "SPD")) +
  # Eje y con el año de la elección
  theme(axis.text.y = element_text(size = 11)) +
  # Leyenda, título (colores bandera alemania) y subtítulo
  labs(fill = "Partidos",
       title = paste0("<span style = 'color:#000000'>ELECCIONES</span> ",
                      "<span style = 'color:#dd1f00'>FEDERALES</span> ",
                      "<span style = 'color:#ffce01'>DE ALEMANIA</span>"),
       subtitle = paste0("Metodología: datos de primeras votaciones desagregados en 299\n",
                         "distritos electorales (Wahlkreise) (se omiten segundas votaciones).\n",
                         "% de voto con ESCALA RELATIVA en colores.")) +
  theme(
    # título con element_markdown para el html con colores
    plot.title = element_markdown(size = 16, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
    # subtítulo
    plot.subtitle = element_text(size = 9, color = "grey20",
                                 family = "titillium", hjust = 0.5))

# ##################################
# CURVA SIGMOIDE+NODOS con evolución de voto
# ##################################

evol_voto <-
  ggplot(datos_global_alemania,
         aes(x = eleccion, y = value, color = name)) +
  # Curvas sigmoides
  geom_bump(smooth = 8, size = 2) +
  # Escala de colores (orden alfabético de los partidos)
  scale_color_manual(values = c("#7e9fce", "#494949", "#f0c553",
                                "#5ba06a", "#b8637a", "#877ab5",
                                "#e35565")) +
  geom_segment(aes(x = 2009, xend = 2009,
                   y = min(value), yend = max(value)),
               color = "grey20", size = 0.8) +
  geom_segment(aes(x = 2013, xend = 2013,
                   y = min(value), yend = max(value)),
               color = "grey20", size = 0.2) +
  geom_segment(aes(x = 2017, xend = 2017,
                   y = min(value), yend = max(value)),
               color = "grey20", size = 0.2) +
  # Puntos/nodos por elección
  geom_point(data = datos_global_alemania %>%
               filter(eleccion == 2009), size = 4) +
  geom_point(data = datos_global_alemania %>%
               filter(eleccion == 2013), size = 2.5) +
  geom_point(data = datos_global_alemania %>%
              filter(eleccion == 2017), size = 2.5) +
  geom_point(data = datos_global_alemania %>%
               filter(eleccion == 2021), 
             aes(fill = name), size = 4, shape = 21,
             color = "grey20", stroke = 1) + 
  # Escala de colores del relleno (orden alfabético de los partidos)
  scale_fill_manual(values = c("#7e9fce", "#494949", "#f0c553",
                               "#5ba06a", "#b8637a", "#877ab5",
                               "#e35565")) +
  # Para que no se limite a los límites de la gráfica
  coord_cartesian(clip = "off") +
  # Eje x con etiquetas
  scale_x_continuous(expand = c(0.001, 0.001),
                     limits = c(2008, 2022),
                     breaks = c(2009, 2013, 2017, 2021),
                     labels = as.character(format(rev(as.Date(fechas)),
                                                  "%d-%m-%Y")),
                     # Duplicamos eje para usar solo el de arriba
                     sec.axis = dup_axis()) +
  # Texto al final (como en una columna extra vertical) 
  # con el % de voto
  geom_text(data = datos_global_alemania %>% filter(eleccion == 2021) %>%
              mutate("porc" = 100 * value / sum(value)),
            aes(x = 2021.2, label = glue("{round(porc, 2)}%"),
                color = name),
            family = "titillium",
            size = 3, hjust = 0) +
  labs(fill = "partidos") +
  guides(color = FALSE) +
  # Tema: eje x en la parte superior
  theme(axis.text.x.top = element_text(size = 9, color = "grey20"),
        axis.title.x.top = element_text(color = "black", size = 1))


# ##################################
# MAPAS (escala adaptada) + [barras + evolucion]
# 3 elecciones con escala adaptada
# Fichas de partido
# ##################################


# BARRAS + EVOLUCIÓN (2 filas, más grande la primera,
# segunda sin leyenda)
graficas <-
  plot_grid(distrib_barras, evol_voto + theme(legend.position = "none"),
            nrow = 2, rel_heights = c(1, 1.3))

# MAPAS (escala adaptada) + [barras + evolucion]
# 3 elecciones con escala adaptada
# Fichas de partido
mapas <- ficha_partido <- list()
for (i in 1:length(grafica_escala_adaptada)) {
  
  mapas[[names(grafica_escala_adaptada)[i]]] <- 
    ((grafica_escala_adaptada[[i]]$`2013-09-22` +
        labs(title = "22/09/2013")) +
       (grafica_escala_adaptada[[i]]$`2017-09-24` +
          labs(title = "24/09/2017")) +
       (grafica_escala_adaptada[[i]]$`2021-09-26` +
          labs(title = "26/09/2021"))) +
    plot_layout(nrow = 1, guides = "collect")
  
  # Mapas + gráficas
  ficha_partido[[names(grafica_escala_adaptada)[i]]] <- 
    plot_grid(mapas[[i]], graficas, nrow = 1, rel_widths = c(2.5, 2)) +
    # Añadimos caption
    plot_annotation(theme =
                      theme(plot.caption =
                              element_text(size = 9, color = "grey60",
                                           family = "titillium", hjust = 0.5)),
                    caption =
                      paste0("cartasdelaplace.substack.com • Javier Álvarez Liébana (@dadosdelaplace) • ",
                             "Inspirado en mapas de Álvaro Merino, Dominic Royé, Diego Hernán y Milos Popovic\n",
                             "Datos electorales y shapes: The Federal Returning Officer ",
                             "(https://www.bundeswahlleiter.de) • Datos encuestas: Wikipedia")) 
  
}

# Mapas con escala fija: cada columna un partido
columna <- list()
for (i in 1:6) {
  
  columna[[i]] <-
    grafica_escala_unificada[[i]]$`2017-09-24` +
    labs(title = glue("24/09/2017")) +
    grafica_escala_unificada[[i]]$`2021-09-26` +
    labs(title = glue("26/09/2021")) +
    plot_layout(guides = "collect", ncol = 1) 
  
}
mapas_escala_fija <- 
  plot_grid(columna[[1]], columna[[2]], columna[[3]],
            columna[[4]], columna[[5]], columna[[6]], nrow = 1) +
  # Añadimos caption
  plot_annotation(title = paste0("<span style = 'color:#000000'>ELECCIONES</span> ",
                                 "<span style = 'color:#dd1f00'>FEDERALES</span> ",
                                 "<span style = 'color:#ffce01'>DE ALEMANIA</span>"),
                  subtitle =paste0("Metodología: datos de primeras votaciones visualizados en base a una ",
                                   "desagregación territorial en 299 distritos electorales (Wahlkreise)\n",
                                   "(se omiten datos de segundas votaciones). ",
                                   "% de voto con ESCALA FIJA en colores."),
                  caption =
                    paste0("cartasdelaplace.substack.com • Javier Álvarez Liébana (@dadosdelaplace) • ",
                           "Inspirado en mapas de Álvaro Merino, Dominic Royé, Diego Hernán y Milos Popovic\n",
                           "Datos electorales y shapes: The Federal Returning Officer ",
                           "(https://www.bundeswahlleiter.de) • Datos encuestas: Wikipedia"),
                  theme =
                    theme(plot.caption =
                            element_text(size = 9, color = "grey60",
                                         family = "titillium", hjust = 0.5),
                          # título con element_markdown para el html con colores
                          plot.title =
                            element_markdown(size = 21, face = "bold", color = "black",
                                             family = "titillium", hjust = 0.5),
                          # subtítulo
                          plot.subtitle = element_text(size = 9, color = "grey20",
                                                       family = "titillium", hjust = 0.5)))


# ##################################
# MAPAS primero vs segundo
# ##################################

mapas_ganadores <- mapas_segundos <- list()
for (i in 1:length(fechas)) {
  
  mapas_ganadores[[fechas[i]]] <-
    mapa_ganadores_fecha(mapa_sf_datos, datos_por_fecha, fecha = fechas[i],
                         titulo = "Partidos")
  mapas_segundos[[fechas[i]]] <-
    mapa_segundos_fecha(mapa_sf_datos, datos_por_fecha, fecha = fechas[i],
                        titulo = "Partidos")
}

mapa_primero_segundo <- 
  (mapas_ganadores$`2021-09-26` +
     labs(title = "1º partido (2021)")) +
  (mapas_segundos$`2021-09-26` +
     labs(title = "2º partido (2021)")) +
  (mapas_ganadores$`2017-09-24` +
     labs(title = "1º partido (2017)")) +
  (mapas_segundos$`2017-09-24` +
     labs(title = "2º partido (2017)")) +
  plot_layout(nrow = 1, guides = "collect")  +
  # Añadimos caption
  plot_annotation(title = paste0("<span style = 'color:#000000'>ELECCIONES</span> ",
                                 "<span style = 'color:#dd1f00'>FEDERALES</span> ",
                                 "<span style = 'color:#ffce01'>DE ALEMANIA</span>"),
                  subtitle =
                    paste0("Metodología: desagregación territorial basada en los 299 distritos electorales.\n",
                           "Se muestran los dos partidos con mayor % de votos"),
                  theme =
                    theme(plot.caption =
                            element_text(size = 9, color = "grey60",
                                         family = "titillium", hjust = 0.5),
                          # título con element_markdown para el html con colores
                          plot.title =
                            element_markdown(size = 21, face = "bold", color = "black",
                                             family = "titillium", hjust = 0.5),
                          # subtítulo
                          plot.subtitle = element_text(size = 9, color = "grey20",
                                                       family = "titillium", hjust = 0.5)))


# ##################################
# MAPAS BIVARIANTES
# ##################################
mapa_bivariante <- 
  mapas_bivariantes(mapa_sf_datos, datos_por_fecha, fecha = fechas[1],
                    paleta = "Brown") +
  # Añadimos subtítulo
  labs(subtitle = paste0("Más <b><span style='color::#64acbe'>AZUL</span></b> ",
                         "mayor voto del <b><span style='color:#64acbe'>partido ganador,</span></b><br>",
                         "más <b><span style='color:#c85a5a'>ROJO</span></b> ",
                         "mayor voto del <b><span style='color:#c85a5a'>segundo partido,</span></b><br>",
                         "en <b><span style='color:#574249'>MARRÓN</span></b> ",
                         "mayor dominio de <b><span style='color:#574249'>bipartidismo</span></b>.")) +
  theme(plot.subtitle =
          element_markdown(size = 7, hjust = 0.5))

# ##################################
# MAPAS PRIMERO-SEGUNDO + bivariante
# ##################################
mapa_primero_segundo_bivariante <-
  plot_grid(mapa_primero_segundo) + mapa_bivariante +
  plot_annotation(caption =
                    paste0("Newsletter: cartasdelaplace.substack.com • ",
                           "Visualización: Javier Álvarez Liébana (@dadosdelaplace) • ",
                           "Inspirado en los mapas de Álvaro Merino, Dominic Royé, Diego Hernán y Milos Popovic\n",
                           "Datos electorales: The Federal Returning Officer ",
                           "(https://www.bundeswahlleiter.de/en/bundeswahlleiter.html) • ",
                           "Datos encuestas: Wikipedia"),
                  theme =
                    theme(plot.caption =
                            element_text(size = 9, color = "grey60",
                                         family = "titillium", hjust = 0.5)))



# ##################################
# MAPAS DIFERENCIAS + bivariante
# ##################################

mapas_diferencias <- 
  (mapa_diferencia_fecha(mapa_sf_datos, datos_por_fecha, fecha = fechas[3]) +
     labs(title = "Diferencial 2013")) +
  (mapa_diferencia_fecha(mapa_sf_datos, datos_por_fecha, fecha = fechas[2]) +
     labs(title = "Diferencial 2017")) +
  (mapa_diferencia_fecha(mapa_sf_datos, datos_por_fecha, fecha = fechas[1]) +
     labs(title = "Diferencial 2021")) +
  plot_layout(nrow = 1, guides = "collect") +
  # Añadimos caption
  plot_annotation(title = paste0("<span style = 'color:#000000'>ELECCIONES</span> ",
                                 "<span style = 'color:#dd1f00'>FEDERALES</span> ",
                                 "<span style = 'color:#ffce01'>DE ALEMANIA</span>"),
                  subtitle =
                    paste0("Metodología: desagregación territorial basada en NUTS (Eurostat, paquete {giscoR}) con nivel 3 de ",
                           "profundidad\n(se omiten subdivisiones de distritos como Berlín). Se muestra el partido con mayor % votos"),
                  theme =
                    theme(plot.caption =
                            element_text(size = 9, color = "grey60",
                                         family = "titillium", hjust = 0.5),
                          # título con element_markdown para el html con colores
                          plot.title =
                            element_markdown(size = 21, face = "bold", color = "black",
                                             family = "titillium", hjust = 0.5),
                          # subtítulo
                          plot.subtitle = element_text(size = 9, color = "grey20",
                                                       family = "titillium", hjust = 0.5)))


mapa_diferencias_bivariante <-
  plot_grid(mapas_diferencias) + mapa_bivariante +
  plot_annotation(caption =
                    paste0("Newsletter: cartasdelaplace.substack.com • ",
                           "Visualización: Javier Álvarez Liébana (@dadosdelaplace) • ",
                           "Inspirado en los mapas de Álvaro Merino, Dominic Royé, Diego Hernán y Milos Popovic\n",
                           "Datos electorales: The Federal Returning Officer ",
                           "(https://www.bundeswahlleiter.de/en/bundeswahlleiter.html) • ",
                           "Datos encuestas: Wikipedia"),
                  theme =
                    theme(plot.caption =
                            element_text(size = 9, color = "grey60",
                                         family = "titillium", hjust = 0.5)))



# ##################################
# ENCUESTAS
# ##################################    

# Link encuestas (¡son % de voto, no de escaños!)
wiki <- paste0("https://es.wikipedia.org/wiki/",
              "Elecciones_federales_de_Alemania_de_2021")
html<- read_html(wiki)
datos <- html_nodes(html,".wikitable")
tabla_encuestas_bruto <- html_table(datos[[5]],header = FALSE)
write_csv(tabla_encuestas_bruto, "./DATOS/EXPORTADO/tabla_encuestas_bruto.csv")
# Nombres en las variables
names(tabla_encuestas_bruto) <- tabla_encuestas_bruto[1, ]

# convertimos char a número
tabla_encuestas <- 
  tabla_encuestas_bruto %>%
  # Eliminamos filas que no nos sirven
  filter(!(Encuesta %in% c("Encuesta", "Última elección"))) %>%
  # Quitamos "," en números
  mutate_at(vars(contains(c("Muestra"))),
            function(x) { gsub(",", "", x) }) %>%
  # Convertimos a números
  mutate_at(vars(!contains(c("Encuesta", "Fecha"))),
            as.numeric) %>% 
  # Convertimos a fecha
  mutate_at(vars(contains(c("Fecha"))),
            function(x) { 
              dmy(map_chr(str_split(
                map_chr(str_split(x, " "),
                        function (y) { paste0(y, collapse = " ") }), "–"),
                function(z) { z[-1] })) }) %>%
  # Limpiamos nombre encuestas
  mutate_at(vars(contains(c("Encuesta"))),
            function(x) { toupper(map_chr(str_split(x, "\\["),
                                          function (y) { y[1] } )) })
# Guardamos en local
write_csv(tabla_encuestas, "./DATOS/EXPORTADO/tabla_encuestas_depurada.csv")

# Reformateamos para el dataviz
tabla_encuestas <- tabla_encuestas %>%
  select(-c("Fecha", "Encuesta", "Muestra")) %>%
  pivot_longer(everything()) %>%
  mutate("Fecha" = rep(tabla_encuestas$Fecha, each = 7),
         "Muestra" = rep(tabla_encuestas$Muestra, each = 7),
         "Casa" = rep(tabla_encuestas$Encuesta, each = 7))

# Solo encuestas de 2021 y con tamaño muestral
tabla_encuestas <- 
  tabla_encuestas %>% filter(Fecha > "2021-01-01" & !is.na(Muestra)) 
names(tabla_encuestas) <-
  gsub("name", "partidos", names(tabla_encuestas))

# Ponderamos por fecha,
# ponderamos encuestas por tamaño muestral,
# eliminamos lo que no llegue al 5%, 
# recalculamos % sobre el total --> al ser un sistema
# muy proporcional por la segunda votación, lo traducimos a escaños
media_encuesta <-
  # Filtramos últimos 2 meses
  tabla_encuestas %>% filter(Fecha > as.Date("2021-09-01")) %>%
  group_by(partidos) %>%
  summarize("media_ponderada"= 
           weighted.mean(value, (as.numeric(Fecha) /
                                   sum(unique(as.numeric(Fecha)))) *
                           (Muestra / sum(Muestra)))) %>%
  # desagrupamos
  ungroup() %>% filter(media_ponderada > 5 & partidos != "Otros") %>%
  mutate("media_ponderada" = media_ponderada / sum(media_ponderada))
media_encuesta$partidos <-
  gsub("AFD", "AfD", toupper(gsub("CDU/CSU", "CDU_CSU",
                                  media_encuesta$partidos)))

# Cruzamos encuestas con últimas elecciones
media_encuesta <- 
  left_join(media_encuesta,
          datos_global_alemania %>% filter(eleccion == 2021) %>%
            mutate("porc" = value / sum(value)),
          by = c("partidos" = "name")) %>%
  # Añadimos % de escaños en lugar de votos
  mutate("porc" = c(83, 151 + 45, 92, 118, 39, 206) / 735) %>%
  # Calculamos escaños conseguidos - escaños encuestas
  mutate("dif_asientos" = floor(735 * (porc - media_ponderada)))

# Calculamos error medio
error_medio_abs <- mean(abs(media_encuesta$media_ponderada -
                              media_encuesta$porc))
# dumbbell fallo de encuestas
dumbbell_encuestas <-
  ggplot(media_encuesta, aes(y = partidos, x = media_ponderada,
                           xend = porc)) +
  ggalt::geom_dumbbell(aes(color = partidos),
                       colour_x = "#EE7523", colour_xend = "#6D3A5D",
                       size = 3, dot_guide = TRUE) +
  scale_color_manual(values = c("#7e9fce", "#494949", "#f0c553",
                                "#5ba06a", "#b8637a", "#e35565"),
                     labels = c("AfD", "CDU_CSU", "FDP", "GRÜNE",
                                "LINKE", "SPD")) +
  scale_y_discrete(expand = c(0.09, 0)) + 
  labs(x = "% de voto vs promedio de encuestas", y = "partidos") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01,
                                                     suffix = "%"),
                     breaks = c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
                     limits = c(0.05, 0.3),
                     expand = c(0.02, 0.02)) +
  theme(axis.text.y = element_text(size = 9, color = "grey20",
                                   family = "titillium"),
        axis.text.x = element_text(size = 9, color = "grey20",
                                   family = "titillium")) +
geom_text(data = media_encuesta %>% filter(partidos == "SPD"),
          aes(x = media_ponderada, y = partidos),
          label = "Media ponderada de encuestas",
          color = "#EE7523", size = 3, vjust = -1.5, hjust = 1.1,
          fontface = "bold") +
  geom_text(data = media_encuesta %>% filter(partidos == "SPD"),
            aes(x = media_ponderada, y = partidos),
            label = "% escaños",
            color = "#6D3A5D", size = 3, vjust = -1.5, hjust = -0.07,
            fontface = "bold") +
  geom_text(aes(x = media_ponderada, y = partidos,
                label = glue("{round(100 * media_ponderada, 2)}%"),
                hjust = ifelse(sign(media_ponderada - porc) == 1, -0.3, -1) *
                  sign(media_ponderada - porc + 0.003)),
            vjust = 2.2, color = "#EE7523", size = 3)  +
  geom_text(aes(x = porc, y = partidos,
                label = glue("{round(100 * porc, 2)}%"),
                hjust = ifelse(sign(media_ponderada - porc) == 1, 1.2, 0.3) *
                  sign(media_ponderada - porc + 0.003)),
            vjust = 2.2, color = "#6D3A5D", size = 3) +
  theme(plot.caption =
         element_text(size = 9, color = "grey60",
                      family = "titillium", hjust = 0.5)) +
  labs(caption =
          glue("Error medio absoluto (de las media ponderadas): {round(100 * error_medio_abs, 2)}%"))


asientos_fallo <-
  ggplot(data = media_encuesta %>% group_by(partidos) %>%
         summarize("asientos" =
                     sign(dif_asientos) *
                     seq(1, abs(dif_asientos), l = abs(dif_asientos))) %>%
         ungroup(),
       aes(x = asientos, y = partidos,
           color = partidos, fill = partidos, group = partidos)) +
  geom_point(size = 4) + 
  geom_segment(aes(x = 0, xend = 0, y = min(partidos), yend = max(partidos)),
               color = "grey20", size = 0.8) +
  geom_segment(aes(x = -0.5, xend = -6, y = max(partidos),
                   yend = max(partidos)),
               color = "grey20", size = 1.1,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(aes(x = -9, y = max(partidos),
                label = "Encuestas sobrestimaron"),
            color = "grey20", vjust = -1.5, size = 3.5) +
  geom_segment(aes(x = 0.5, xend = 6, y = min(partidos),
                   yend = min(partidos)),
               color = "grey20", size = 1.1,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(aes(x = 9, y = min(partidos),
                label = "Encuestas infraestimaron"),
            color = "grey20", vjust = 1.7, size = 3.5) +
  scale_color_manual(values = c("#7e9fce", "#494949", "#f0c553",
                                "#5ba06a", "#b8637a", "#e35565"),
                     labels = c("AfD", "CDU_CSU", "FDP", "GRÜNE",
                                "LINKE", "SPD")) + 
  scale_fill_manual(values = c("#7e9fce", "#494949", "#f0c553",
                                "#5ba06a", "#b8637a", "#e35565"),
                     labels = c("AfD", "CDU_CSU", "FDP", "GRÜNE",
                                "LINKE", "SPD")) +
  geom_text(data = media_encuesta,
            aes(x = dif_asientos + sign(dif_asientos) * 5, y = partidos,
                label = paste0(glue("{dif_asientos} escaños ({round(100 * (media_encuesta$porc - media_encuesta$media_ponderada), 1)}%"),
                               ifelse(partidos == 'SPD', " error)", ")"))),
            vjust = -1.2, size = 3) +
  scale_x_continuous(expand = c(0.15, 0.15))
  
encuestas_fallo <-
  plot_grid(dumbbell_encuestas, asientos_fallo,
          rel_widths = c(1.2, 1)) +
  labs(title = paste0("<span style = 'color:#000000'>ELECCIONES</span> ",
                      "<span style = 'color:#dd1f00'>FEDERALES</span> ",
                      "<span style = 'color:#ffce01'>DE ALEMANIA</span>"),
       subtitle = paste0("¿Acertaron las encuestas? Encuestas recopiladas ",
                         "en Wikipedia desde el ",
                         format(min(tabla_encuestas$Fecha), "%d-%m-%Y"),
                         " sobre estimación directa de voto. La estimación de escaños ",
                         "se ha realizado promediando doblemente las encuestas de voto\n",
                         "tamaño muestral y fecha. Tras descartar partidos que no superen ",
                         "el 5%, se ha recalculado el % directo de escaños asumiendo ",
                         "que las segundas votaciones hagan un sistema totalmente proporcional."),
       caption =
         paste0("Newsletter: cartasdelaplace.substack.com • ",
                "Visualización: Javier Álvarez Liébana (@dadosdelaplace) • ",
                "Inspirado en los mapas de Álvaro Merino, Dominic Royé, Diego Hernán y Milos Popovic\n",
                "Datos electorales: The Federal Returning Officer ",
                "(https://www.bundeswahlleiter.de/en/bundeswahlleiter.html) • ",
                "Datos encuestas: Wikipedia")) +
  theme(# título con element_markdown para el html con colores
    plot.title = element_markdown(size = 16, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
    # subtítulo
    plot.subtitle = element_text(size = 9, color = "grey20",
                                 family = "titillium", hjust = 0.5),
    plot.caption =
      element_text(size = 9, color = "grey60",
                   family = "titillium", hjust = 0.5))





puntos_encuestas <- 
  ggplot(tabla_encuestas, aes(x = Fecha, y = value, color = partidos)) +
  geom_point(size = 3, alpha = .4) +
  scale_color_manual(values = c("#7e9fce", "#494949", "#b8637a",
                                "#f0c553", "#5ba06a", "#877ab5",
                                "#e35565"),
                     labels = c("AfD", "CDU/CSU", "Linke", "FDP",
                                "Grüne", "Otros", "SPD")) +
  scale_y_continuous(breaks = seq(5, 35, by = 5),
                     labels = function(x) paste0(x, "%")) +
  scale_x_date() +
  theme(axis.text.y = element_text(size = 11, color = "grey20",
                                   family = "titillium"),
        axis.text.x = element_text(size = 11, color = "grey20",
                                   family = "titillium")) +
  xlab("Fechas") + ylab("% de voto estimado")

raincloud_encuestas <-
  ggplot(tabla_encuestas,
             aes(y = value, x = partidos, color = partidos,
                 fill = partidos, size = Fecha)) +
  geom_point(alpha = 0.2) +
  ggdist::stat_halfeye(adjust = 1, position = position_nudge(x = .23)) +
  scale_color_manual(values = c("#7e9fce", "#494949", "#b8637a",
                                "#f0c553", "#5ba06a", "#877ab5",
                                "#e35565"),
                     labels = c("AfD", "CDU/CSU", "Linke", "FDP",
                                "Grüne", "Otros", "SPD")) +
  scale_fill_manual(values = c("#7e9fce", "#494949", "#b8637a",
                               "#f0c553", "#5ba06a", "#877ab5",
                               "#e35565"),
                    labels = c("AfD", "CDU/CSU", "Linke", "FDP",
                               "Grüne", "Otros", "SPD")) +
  scale_y_continuous(breaks = seq(5, 35, by = 5),
                     labels = function(x) paste0(x, "%")) +
  xlab("Partidos") + ylab("% de voto estimado de encuestas")

encuestas <- 
  plot_grid(puntos_encuestas, raincloud_encuestas,
            rel_widths = c(1.5, 1)) +
  labs(title = paste0("<span style = 'color:#000000'>ELECCIONES</span> ",
                      "<span style = 'color:#dd1f00'>FEDERALES</span> ",
                      "<span style = 'color:#ffce01'>DE ALEMANIA</span>"),
       subtitle = paste0("¿Acertaron las encuestas? Encuestas recopiladas ",
                         "en Wikipedia desde el ",
                         format(min(tabla_encuestas$Fecha), "%d-%m-%Y"),
                         " sobre estimación directa de voto"),
       caption =
         paste0("Newsletter: cartasdelaplace.substack.com • ",
                "Visualización: Javier Álvarez Liébana (@dadosdelaplace) • ",
                "Inspirado en los mapas de Álvaro Merino, Dominic Royé, Diego Hernán y Milos Popovic\n",
                "Datos electorales: The Federal Returning Officer ",
                "(https://www.bundeswahlleiter.de/en/bundeswahlleiter.html) • ",
                "Datos encuestas: Wikipedia")) +
  theme(# título con element_markdown para el html con colores
    plot.title = element_markdown(size = 16, face = "bold", color = "black",
                                  family = "titillium", hjust = 0.5),
    # subtítulo
    plot.subtitle = element_text(size = 9, color = "grey20",
                                 family = "titillium", hjust = 0.5),
    plot.caption =
      element_text(size = 9, color = "grey60",
                   family = "titillium", hjust = 0.5))




# datos 2021
lista_graficas <- list(distrib_barras, evol_voto, grafica_escala_adaptada,
                       grafica_escala_unificada, mapas_escala_fija,
                       mapas, ficha_partido, mapas_ganadores, 
                       mapas_segundos, mapa_primero_segundo,
                       mapa_bivariante, mapa_primero_segundo_bivariante,
                       puntos_encuestas, raincloud_encuestas, encuestas)

