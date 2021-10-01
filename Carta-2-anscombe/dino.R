
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
if(!require(colorspace)) install.packages("colorspace", repos = repos)
if(!require(datasauRus)) install.packages("datasauRus", repos = repos)
if(!require(gganimate)) install.packages("gganimate", repos = repos)
if(!require(grid)) install.packages("grid", repos = repos)

anotaciones <- datasaurus_dozen %>% group_by(dataset) %>%
  summarize(MEDIA_X = mean(x), MEDIA_Y = mean(y),
            VARIANZA_X = var(x), VARIANZA_Y = var(y),
            CORRELACION_XY = cor(x, y))
theme_set(theme_light(base_size = 17, base_family = "Poppins"))
g <- ggplot(datasaurus_dozen,
            aes(x = x, y = y, color = as.numeric(as.factor(dataset)))) +
  geom_point(size = 5, alpha = 0.5, show.legend = FALSE) +
  scale_color_gradient2_tableau("Red-Blue Diverging") +
  transition_states(dataset, 3, 1) + enter_fade() + exit_fade() +
  ggtitle("THE DATASAURUS DOZEN\n") +
  labs(subtitle =
         paste0("Gráficos: J. Álvarez Liébana | ",
                "Datos: Alberto Cairo")) +
  geom_text(data = anotaciones,
            aes(x = 50, y = Inf,
                label = paste0("Media x = ", round(MEDIA_X, 1),
                               ", Media y = ", round(MEDIA_Y, 1),
                               "\nDesv. típica x = ",
                               round(sqrt(VARIANZA_X), 1),
                               ", Desv. típica y = ",
                               round(sqrt(VARIANZA_Y), 1),
                               "\nCorrelación (X, Y) = ",
                               round(CORRELACION_XY, 1)),
                color = as.numeric(as.factor(dataset))),
            vjust = -1.45, hjust = 0, size = 4.5) +
  coord_cartesian(clip = "off") +
  theme(legend.position = "none", # sin leyenda
        # Fuente título
        plot.title = element_text(size = 30, face = "bold"), 
        plot.subtitle = element_text(size = 9), 
        text = element_text(size = 15, face = "bold"),
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
          margin(t = 3.5, b = 0.5, r = 0.5, l = 0.5, "cm")
  )
# 
# g <- ggplot(datasaurus_dozen, aes(x = x, y = y, color = dataset)) + # datos
#   geom_point(size = 6, alpha = 0.5, show.legend = FALSE) +  # puntos
#   
#   ggtitle("THE DATASAURUS DOZEN\n") +
#   theme(legend.position = "none", # sin leyenda
#         # Fuente título
#         plot.title = element_text(size = 19, face = "bold"), 
#         plot.subtitle = element_text(size = 13), 
#         # fuente título ejes
#         axis.title = element_text(size = 13, face = "bold"),
#         axis.text.x = # fuente X
#           element_text(family = "Poppins", size = 9, face = "bold",
#                        color = "black"), 
#         axis.text.y = # fuente Y
#           element_text(family = "Poppins", size = 9, face = "bold",
#                        color = "black"), 
#         panel.grid = element_blank(), # sin grid de fondo
#         plot.margin = # márgenes
#           margin(t = 4, b = 0.7, r = 1.5, l = 1.5, "cm")
#   ) +
#   geom_text(data = anotaciones,
#             "Year: {frame_time}")aes(0, y = Inf,
#                 label =
#                   paste0("MEDIA DE X = ",
#                          round(`MEDIA DE X`, 2),
#                          ", MEDIA DE Y = ",
#                          round(`MEDIA DE Y`, 2),
#                          "\nVARIANZA DE X = ",
#                          round(`VARIANZA DE X`, 2),
#                          ", VARIANZA DE Y = ",
#                          round(`VARIANZA DE Y`, 2),
#                          "\nCORRELACIÓN (X, Y) = ",
#                          round(`CORRELACIÓN (X, Y)`, 2))),
#             vjust = -1.2, hjust = 0, color = "firebrick") +
#   coord_cartesian(clip = "off") +
#   transition_states(states = as.numeric(as.factor(dataset)),
#                     transition_length = 12, state_length = 12) +
#   ease_aes("linear") + enter_fade() + exit_fade()
# 
# animate(g, nframes = 100)
# 
# 
