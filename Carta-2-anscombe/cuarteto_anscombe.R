
rm(list = ls()) # borramos entorno
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # fijamos directorio
# Instalamos paquetes si fueran necesarios.
repos <- "http://cran.us.r-project.org"
if(!require(datasets)) install.packages("datasets", repos = repos)
if(!require(ggplot2)) install.packages("ggplot2", repos = repos)
if(!require(latex2exp)) install.packages("latex2exp", repos = repos)
if(!require(tidyverse)) install.packages("tidyverse", repos = repos)
if(!require(ggthemes)) install.packages("ggthemes", repos = repos)
if(!require(patchwork)) install.packages("patchwork", repos = repos)
if(!require(ggdist)) install.packages("ggdist", repos = repos)
if(!require(gghalves)) devtools::install_github("erocoar/gghalves")
if(!require(colorspace)) install.packages("colorspace", repos = repos)

# Pares de variables (X, Y)
datos_anscombe <-
  as_tibble(data.frame("x" = unlist(anscombe[, 1:4]),
                       "y" = unlist(anscombe[, 5:8]),
                       "par" = as.factor(rep(1:4, each = dim(anscombe)[1]))))
   
# Tema gráfico base (theme_light para quitar importancia a los ejes)
theme_set(theme_light(base_size = 15, base_family = "Poppins"))

# Variables X
plot_X <- ggplot(datos_anscombe, aes(x = x, y = par, color = par)) + # datos
  geom_boxplot(size = 1.3) + # Boxplots sin nada más
  scale_x_continuous(limits = c(0, 19)) + #fijamos escala (para incluir al 0)
  scale_color_tableau() + # Escala de colores de tableu
  labs(x = "Variable X",
       y = TeX("Par del cuarteto", bold = TRUE), # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe en «Graphs in Statistical Analysis», ",
                "Amer. Statis. 27, 1973")) +
  annotate(geom = "curve", x = 16.5, xend = 18.9, y = 4.35, yend = 4.1,
           color = "black", curvature = -0.5, size = 1.1,
           arrow = arrow(length = unit(3.5, "mm"))) +
  annotate(geom = "text", x = 16.1, y = 4.35,
           label = "Dato atípico del par 4",
           hjust = "right", family = "Poppins")  +
  annotate(geom = "curve", x = 14, xend = 11.6, y = 3.7, yend = 3.3,
           color = "black", curvature = 0.3, size = 1.1,
           arrow = arrow(length = unit(3.5, "mm"))) +
  annotate(geom = "text", x = 14.1, y = 3.7,
           label = "Percentil 75 (P75) o cuartil 3 (Q3)",
           hjust = "left", family = "Poppins") +
  annotate(geom = "curve", x = 14,
           xend = median((datos_anscombe %>% filter(par == 1))$x),
           y = 1.5, yend = 1.3,
           color = "black", curvature = -0.5, size = 1.1,
           arrow = arrow(length = unit(3.5, "mm"))) +
  annotate(geom = "text", x = 13.9, y = 1.6,
           label = "Percentil 50 (P50) o MEDIANA",
           hjust = "left", family = "Poppins")  +
  annotate(geom = "curve", x = 4.5,
           xend = quantile((datos_anscombe %>% filter(par == 2))$x)[2],
           y = 2.5, yend = 2.3,
           color = "black", curvature = -0.4, size = 1.1,
           arrow = arrow(length = unit(3.5, "mm"))) +
  annotate(geom = "text", x = 4.5, y = 2.6,
           label = "Percentil 25 (P25) o cuartil 1 (Q1)",
           hjust = "right", family = "Poppins") +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: gráfico de cajas y bigotes") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 17, face = "bold"), 
        plot.subtitle = element_text(size = 9), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.7, b = 0.7, r = 1.5, l = 1.5, "cm")
  ) 

# Variable X con puntos
plot_X <- ggplot(datos_anscombe, aes(x = x, y = par, color = par)) + # datos
  geom_boxplot(size = 0.9, outlier.alpha = 0.5) + # Boxplots sin nada más
  geom_point(size = 6, alpha = 0.25) +  # puntos
  scale_x_continuous(limits = c(0, 19)) + #fijamos escala (para incluir al 0)
  scale_color_tableau() + # Escala de colores de tableu
  labs(x = "Variable X",
       y = TeX("Par del cuarteto", bold = TRUE), # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe en «Graphs in Statistical Analysis», ",
                "Amer. Statis. 27, 1973")) +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: gráfico de cajas y bigotes") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 17, face = "bold"), 
        plot.subtitle = element_text(size = 9), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.7, b = 0.7, r = 1.5, l = 1.5, "cm")
  ) 


# Variable X con puntos + densidad
plot_X <- ggplot(datos_anscombe,
                 aes(x = x, y = as.numeric(par) - 0.25,
                     color = par)) + # datos
  geom_boxplot(size = 0.9, outlier.alpha = 0.5) + # Boxplots sin nada más
  geom_point(size = 6, alpha = 0.25) +  # puntos
  stat_halfeye(aes(y = as.numeric(par), color = par,
                   fill = after_scale(lighten(color, .5))),
               shape = 18, point_size = 3, interval_size = 1.8,
               adjust = .5, .width = c(0, 1)) +
  scale_color_tableau() + # Escala de colores de tableu
  labs(x = "Variable X",
       y = TeX("Par del cuarteto", bold = TRUE), # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe en «Graphs in Statistical Analysis», ",
                "Amer. Statis. 27, 1973")) +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: gráfico de cajas y bigotes") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 17, face = "bold"), 
        plot.subtitle = element_text(size = 9), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.4, b = 0.4, r = 1.5, l = 1.5, "cm")
  ) 


  
  

# Variables Y
plot_Y <- ggplot(datos_anscombe, aes(x = y, y = par, color = par)) + # datos
  geom_boxplot(size = 1.3) + # Boxplots sin nada más
  scale_x_continuous(limits = c(0, 13)) + #fijamos escala (para incluir al 0)
  scale_color_tableau() + # Escala de colores de tableu
  labs(x = "Variable Y",
       y = TeX("Par del cuarteto", bold = TRUE), # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe en «Graphs in Statistical Analysis», ",
                "Amer. Statis. 27, 1973")) +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: gráfico de cajas y bigotes") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 17, face = "bold"), 
        plot.subtitle = element_text(size = 9), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.7, b = 0.7, r = 1.5, l = 1.5, "cm")
  ) 


# Variable Y con puntos + densidad
plot_Y <- ggplot(datos_anscombe,
                 aes(x = y, y = as.numeric(par) - 0.25,
                     color = par)) + # datos
  geom_boxplot(size = 0.9, outlier.alpha = 0.5) + # Boxplots sin nada más
  geom_point(size = 6, alpha = 0.25) +  # puntos
  stat_halfeye(aes(y = as.numeric(par), color = par,
                   fill = after_scale(adjust_transparency(lighten(color, .6),
                                                          alpha = 0.5))),
               shape = 18, point_size = 3, interval_size = 1.8,
               adjust = .5, .width = c(0, 1)) +
  scale_color_tableau() + # Escala de colores de tableu
  labs(x = "Variable Y",
       y = TeX("Par del cuarteto", bold = TRUE), # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe en «Graphs in Statistical Analysis», ",
                "Amer. Statis. 27, 1973")) +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: gráfico de cajas y bigotes") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 17, face = "bold"), 
        plot.subtitle = element_text(size = 9), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.4, b = 0.4, r = 1.5, l = 1.5, "cm")
  ) 


# Todos juntos: cruz de errores
datos_anscombe <- datos_anscombe %>% group_by(par) %>%
  mutate(mediana_x = median(x)) %>%
  mutate(mediana_y = median(y)) %>%
  mutate(sd_x = sd(x)) %>% mutate(sd_y = sd(y))
pal <- c("#4E79A7", "#F28E2B", "#E15759",  "#76B7B2")

# PAR 1
datos_anscombe1 <- datos_anscombe %>% filter(par == 1)
plot_1 <- 
  ggplot(datos_anscombe1, aes(x, y)) +
  geom_errorbar(data = datos_anscombe1,
    aes(x = mediana_x, ymin = mediana_y - sd_y,
        ymax = mediana_y + sd_y, color = pal[1],
        color = after_scale(darken(color, .2, space = "combined"))),
    inherit.aes = F, width = .8, size = .8) +
  geom_errorbar(data = datos_anscombe1,
                aes(y = mediana_y, xmin = mediana_x - sd_x,
                    xmax = mediana_x + sd_x, color = pal[1],
                    color = after_scale(darken(color, .2,
                                               space = "combined"))),
                inherit.aes = F, width = .5, size = .8) +
  geom_point(aes(fill = pal[1], size = 7), shape = 21,
             color = "transparent", alpha = .7) +
  geom_point(aes(size = 7), shape = 21, color = "white",
             fill = "transparent") +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed",
              color = "gold3") +
  annotate(geom = "text", x = 13.5, y = 10,
           label =
             paste0("Recta de regresión y = ",
                    round(lm(datos_anscombe1$y ~
                               datos_anscombe1$x)$coefficients[1], 1),
                    " + ",
                    round(lm(datos_anscombe1$y ~
                               datos_anscombe1$x)$coefficients[2], 1),
                    " * x (R2 = ",
                    round(summary(lm(datos_anscombe1$y ~
                                       datos_anscombe1$x))$r.squared, 3),
                    ")"),
           hjust = "right", family = "Poppins", color = "gold3") +
  scale_color_tableau() + scale_fill_tableau() + # Escala de colores de tableu
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(0, 16), breaks = seq(0, 15, by = 2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(4, 12), breaks = seq(4, 12, by = 2),
                     expand = c(0, 0)) +
  labs(x = "Variable X", y = "Variable Y", # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe")) +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: PAR 1") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 15, face = "bold"), 
        plot.subtitle = element_text(size = 8), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.3, b = 0.3, r = 1, l = 1, "cm")
  ) 

# PAR 2
datos_anscombe2 <- datos_anscombe %>% filter(par == 2)
plot_2 <- 
  ggplot(datos_anscombe2, aes(x, y)) +
  geom_errorbar(data = datos_anscombe2,
                aes(x = mediana_x, ymin = mediana_y - sd_y,
                    ymax = mediana_y + sd_y), color = pal[2],
                inherit.aes = F, width = .8, size = .8) +
  geom_errorbar(data = datos_anscombe2,
                aes(y = mediana_y, xmin = mediana_x - sd_x,
                    xmax = mediana_x + sd_x), color = pal[2],
                inherit.aes = F, width = .5, size = .8) +
  geom_point(aes(size = 7), shape = 21, fill = pal[2],
             color = "transparent", alpha = .7) +
  geom_point(aes(size = 7), shape = 21, color = "white",
             fill = "transparent") +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed",
              color = "gold3") +
  annotate(geom = "text", x = 6, y = 11.5,
           label =
             paste0("Recta de regresión y = ",
                    round(lm(datos_anscombe2$y ~
                               datos_anscombe2$x)$coefficients[1], 1),
                    " + ",
                    round(lm(datos_anscombe2$y ~
                               datos_anscombe2$x)$coefficients[2], 1),
                    " * x (R2 = ",
                    round(summary(lm(datos_anscombe2$y ~
                                       datos_anscombe2$x))$r.squared, 3),
                    ")"),
           hjust = "left", family = "Poppins", color = "gold3") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(2, 20), breaks = seq(2, 20, by = 2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 14, by = 2),
                     expand = c(0, 0)) +
  labs(x = "Variable X", y = "Variable Y", # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe")) +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: PAR 2") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 15, face = "bold"), 
        plot.subtitle = element_text(size = 8), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.3, b = 0.3, r = 1, l = 1, "cm")
  ) 

# PAR 3
datos_anscombe3 <- datos_anscombe %>% filter(par == 3)
plot_3 <- 
  ggplot(datos_anscombe3, aes(x, y)) +
  geom_errorbar(data = datos_anscombe3,
                aes(x = mediana_x, ymin = mediana_y - sd_y,
                    ymax = mediana_y + sd_y), color = pal[3],
                inherit.aes = F, width = .8, size = .8) +
  geom_errorbar(data = datos_anscombe3,
                aes(y = mediana_y, xmin = mediana_x - sd_x,
                    xmax = mediana_x + sd_x), color = pal[3],
                inherit.aes = F, width = .5, size = .8) +
  geom_point(aes(size = 7), shape = 21, fill = pal[3],
             color = "transparent", alpha = .7) +
  geom_point(aes(size = 7), shape = 21, color = "white",
             fill = "transparent") +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed",
              color = "gold3") +
  annotate(geom = "text", x = 16.5, y = 11,
           label =
             paste0("Recta de regresión y = ",
                    round(lm(datos_anscombe3$y ~
                               datos_anscombe3$x)$coefficients[1], 1),
                    " + ",
                    round(lm(datos_anscombe3$y ~
                               datos_anscombe3$x)$coefficients[2], 1),
                    " * x (R2 = ",
                    round(summary(lm(datos_anscombe3$y ~
                                       datos_anscombe3$x))$r.squared, 3),
                    ")"),
           hjust = "right", family = "Poppins", color = "gold3") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(2, 20), breaks = seq(2, 20, by = 2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 14, by = 2),
                     expand = c(0, 0)) +
  labs(x = "Variable X", y = "Variable Y", # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe")) +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: PAR 3") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 15, face = "bold"), 
        plot.subtitle = element_text(size = 8), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.3, b = 0.3, r = 1, l = 1, "cm")
  ) 


# PAR 4
datos_anscombe4 <- datos_anscombe %>% filter(par == 4)
plot_4 <- 
  ggplot(datos_anscombe4, aes(x, y)) +
  geom_errorbar(data = datos_anscombe4,
                aes(x = mediana_x, ymin = mediana_y - sd_y,
                    ymax = mediana_y + sd_y), color = pal[4],
                inherit.aes = F, width = .8, size = .8) +
  geom_errorbar(data = datos_anscombe4,
                aes(y = mediana_y, xmin = mediana_x - sd_x,
                    xmax = mediana_x + sd_x), color = pal[4],
                inherit.aes = F, width = .5, size = .8) +
  geom_point(aes(size = 7), shape = 21, fill = pal[4],
             color = "transparent", alpha = .7) +
  geom_point(aes(size = 7), shape = 21, color = "white",
             fill = "transparent") +
  geom_smooth(method = lm, se = FALSE, linetype = "dashed",
              color = "gold3") +
  annotate(geom = "text", x = 16.5, y = 12.5,
           label =
             paste0("Recta de regresión y = ",
                    round(lm(datos_anscombe4$y ~
                               datos_anscombe4$x)$coefficients[1], 1),
                    " + ",
                    round(lm(datos_anscombe4$y ~
                               datos_anscombe4$x)$coefficients[2], 1),
                    " * x (R2 = ",
                    round(summary(lm(datos_anscombe4$y ~
                                       datos_anscombe4$x))$r.squared, 3),
                    ")"),
           hjust = "right", family = "Poppins", color = "gold3") +
  coord_cartesian(clip = "off") +
  scale_x_continuous(limits = c(2, 20), breaks = seq(2, 20, by = 2),
                     expand = c(0, 0)) +
  scale_y_continuous(limits = c(4, 14), breaks = seq(4, 14, by = 2),
                     expand = c(0, 0)) +
  labs(x = "Variable X", y = "Variable Y", # Etiquetas de los ejes
       subtitle =
         paste0("Gráfico: Javier Álvarez Liébana (@DadosDeLaplace) | ",
                "Datos: F.J. Anscombe")) +
  # Título y subtítulo
  ggtitle("CUARTETO DE ANSCOMBE: PAR 4") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 15, face = "bold"), 
        plot.subtitle = element_text(size = 8), 
        # fuente título ejes
        axis.title = element_text(size = 13, face = "bold"), 
        axis.text.x = # fuente X
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        axis.text.y = # fuente Y
          element_text(family = "Poppins", size = 9, face = "bold",
                       color = "black"), 
        panel.grid = element_blank(), # sin grid de fondo
        plot.margin = # márgenes
          margin(t = 0.3, b = 0.3, r = 1, l = 1, "cm")
  ) 
