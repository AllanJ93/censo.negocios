## code to prepare `resultados` dataset goes here

library(dplyr)
library(ggplot2)

datos_censo |>
  glimpse()

# P5 Tipos de cocina --------------------------------------------------------------------------

bd_tipos_cocina <-
  datos_censo |>
  as_tibble() |>
  count(P5_O1, sort = TRUE) |>
  rename(respuesta = P5_O1)

p_tipos_cocina <-
  "¿Qué tipo de cocina ofrece este establecimiento?"

g_tipos_cocina <-
  bd_tipos_cocina |>
  left_join(datos_censo |>
              distinct(P5_O1, color),
             by = c("respuesta" = "P5_O1")) |>
  ggplot(aes(x = reorder(respuesta, n),
             y = n,
             fill = color)) +
  geom_col() +
  geom_text(aes(label = n), nudge_y = 0.6, size = 4) +
  coord_flip() +
  labs(x = NULL, y = NULL, caption = p_tipos_cocina) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 45)) +
  scale_y_continuous(limits = c(0, 40)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(text = element_text(family = "Poppins"),
        legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 16),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"))

# P8 Musica en vivo ---------------------------------------------------------------------------

bd_musica_vivo <-
  datos_censo |>
  as_tibble() |>
  count(P8, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P8)

p_musica_vivo <-
  "¿Hay música en vivo?"

g_musica_vivo <-
  graficar_gauge(bd = bd_musica_vivo |>
                               filter(respuesta == "Sí"),
                             color_principal = "blue",
                             color_secundario = "gray70",
                             escala = c(0, 1),
                             size_text_pct = 12) +
  labs(title = p_musica_vivo,
       caption = 'Establecimientos que cuentan\ncon música en vivo') +
  theme(plot.caption = element_text(size = 14),
        plot.title = element_text(size = 16))

bd_musica_tiposCocina <-
  datos_censo |>
  as_tibble() |>
  filter(P8 == "Sí") |>
  count(P5_O1, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P5_O1) |>
  left_join(datos_censo |>
              distinct(P5_O1, color),
            by = c("respuesta" = "P5_O1"))

g_musica_tiposCocina <-
  bd_musica_tiposCocina |>
  ggplot(aes(x = reorder(respuesta, n),
             y = n,
             fill = color)) +
  geom_col() +
  geom_text(aes(label = n), nudge_y = 0.7, size = 10) +
  coord_flip() +
  labs(x = NULL, y = NULL, caption = 'Establecimientos que cuentan\ncon música en vivo') +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 30)) +
  scale_y_continuous(limits = c(0, 10), breaks = scales::breaks_pretty(n = 3)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(text = element_text(family = "Poppins"),
        legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 18),
        plot.caption = element_text(size = 14),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
        plot.title = element_text(size = 16))

# P9 Menus de temporadas ----------------------------------------------------------------------

bd_menu_temporada <-
  datos_censo |>
  as_tibble() |>
  count(P9, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P9)

p_menu_temporada <-
  "¿Ofrecen menús de temporada\nque cambian regularmente?"

g_menu_temporada <-
  graficar_gauge(bd = bd_menu_temporada |>
                               filter(respuesta == "Sí"),
                             color_principal = "blue",
                             color_secundario = "gray70",
                             escala = c(0, 1),
                             size_text_pct = 12) +
  labs(title = p_menu_temporada,
       caption = 'Establecimientos que ofrecen\nmenús de temporada') +
  theme(plot.caption = element_text(size = 14),
        plot.title = element_text(size = 16))

bd_menu_tiposCocina <-
  datos_censo |>
  as_tibble() |>
  filter(P9 == "Sí") |>
  count(P5_O1, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P5_O1) |>
  left_join(datos_censo |>
              distinct(P5_O1, color),
            by = c("respuesta" = "P5_O1"))

g_menu_tiposCocina <-
  bd_menu_tiposCocina |>
  ggplot(aes(x = reorder(respuesta, n),
             y = n,
             fill = color)) +
  geom_col() +
  geom_text(aes(label = n), nudge_y = 0.7, size = 8) +
  coord_flip() +
  labs(x = NULL, y = NULL, caption = 'Establecimientos que ofrecen\nmenús de temporada') +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 30)) +
  scale_y_continuous(limits = c(0, 15), breaks = scales::breaks_pretty(n = 5)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(text = element_text(family = "Poppins"),
        legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
        plot.title = element_text(size = 16))

# P10 Promociones especiales ------------------------------------------------------------------

bd_promociones <-
  datos_censo |>
  as_tibble() |>
  count(P10, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P10)

p_promociones <-
  "¿Realizan promociones especiales?"

g_promociones <-
  graficar_gauge(bd = bd_promociones |>
                               filter(respuesta == "Sí"),
                             color_principal = "blue",
                             color_secundario = "gray70",
                             escala = c(0, 1),
                             size_text_pct = 12) +
  labs(title = p_promociones,
       caption = 'Establecimientos que realizan\npromociones especiales') +
  theme(plot.caption = element_text(size = 14),
        plot.title = element_text(size = 16))

bd_promociones_tiposCocina <-
  datos_censo |>
  as_tibble() |>
  filter(P10 == "Sí") |>
  count(P5_O1, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P5_O1) |>
  left_join(datos_censo |>
              distinct(P5_O1, color),
            by = c("respuesta" = "P5_O1"))

g_promociones_tiposCocina <-
  bd_promociones_tiposCocina |>
  ggplot(aes(x = reorder(respuesta, n),
             y = n,
             fill = color)) +
  geom_col() +
  geom_text(aes(label = n), nudge_y = 0.8, size = 8) +
  coord_flip() +
  labs(x = NULL, y = NULL, caption = 'Establecimientos que realizan\npromociones especiales') +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 30)) +
  scale_y_continuous(limits = c(0, 20), breaks = scales::breaks_pretty(n = 4)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(text = element_text(family = "Poppins"),
        legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
        plot.title = element_text(size = 16))

# P11 Pedidos en linea ------------------------------------------------------------------------

bd_pedidosLinea <-
  datos_censo |>
  as_tibble() |>
  count(P11, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P11)

p_pedidosLinea <-
  "¿Ofrecen la opción de pedidos en línea\no reservas a través de una aplicación?"

caption_pedidosLinea <-
  'Establecimientos que ofrecen\nel servicio de pedidos en línea'

g_pedidosLinea <-
  graficar_gauge(bd = bd_pedidosLinea |>
                               filter(respuesta == "Sí"),
                             color_principal = "blue",
                             color_secundario = "gray70",
                             escala = c(0, 1),
                             size_text_pct = 12) +
  labs(title = p_pedidosLinea,
       caption = caption_pedidosLinea) +
  theme(plot.caption = element_text(size = 14),
        plot.title = element_text(size = 16))

bd_pedidosLinea_tiposCocina <-
  datos_censo |>
  as_tibble() |>
  filter(P11 == "Sí") |>
  count(P5_O1, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P5_O1) |>
  left_join(datos_censo |>
              distinct(P5_O1, color),
            by = c("respuesta" = "P5_O1"))

g_pedidosLinea_tiposCocina <-
  bd_pedidosLinea_tiposCocina |>
  ggplot(aes(x = reorder(respuesta, n),
             y = n,
             fill = color)) +
  geom_col() +
  geom_text(aes(label = n), nudge_y = 0.7, size = 8) +
  coord_flip() +
  labs(x = NULL, y = NULL, caption = caption_pedidosLinea) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 30)) +
  scale_y_continuous(limits = c(0, 25), breaks = scales::breaks_pretty(n = 5)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(text = element_text(family = "Poppins"),
        legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
        plot.title = element_text(size = 16))

# P12 Terrazas --------------------------------------------------------------------------------

bd_terrazas <-
  datos_censo |>
  as_tibble() |>
  count(P12, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P12)

p_terrazas <-
  "¿Hay áreas al aire libre o terrazas disponibles?"

caption_terrazas <-
  'Establecimientos que cuentan con\náreas al aire libre o terrazas'

g_terrazas <-
  graficar_gauge(bd = bd_terrazas |>
                               filter(respuesta == "Sí"),
                             color_principal = "blue",
                             color_secundario = "gray70",
                             escala = c(0, 1),
                             size_text_pct = 12) +
  labs(title = p_terrazas,
       caption = caption_terrazas) +
  theme(plot.caption = element_text(size = 14),
        plot.title = element_text(size = 16))

bd_terrazas_tiposCocina <-
  datos_censo |>
  as_tibble() |>
  filter(P12 == "Sí") |>
  count(P5_O1, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P5_O1) |>
  left_join(datos_censo |>
              distinct(P5_O1, color),
            by = c("respuesta" = "P5_O1"))

g_terrazas_tiposCocina <-
  bd_terrazas_tiposCocina |>
  ggplot(aes(x = reorder(respuesta, n),
             y = n,
             fill = color)) +
  geom_col() +
  geom_text(aes(label = n), nudge_y = 0.7, size = 8) +
  coord_flip() +
  labs(x = NULL, y = NULL, caption = caption_terrazas) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 30)) +
  scale_y_continuous(limits = c(0, 20), breaks = scales::breaks_pretty(n = 4)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(text = element_text(family = "Poppins"),
        legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
        plot.title = element_text(size = 16))

# P13 Juegos ninos ----------------------------------------------------------------------------

bd_ninos <-
  datos_censo |>
  as_tibble() |>
  count(P13, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P13)

p_ninos <-
  "¿Tienen un área de juegos para niños?"

caption_ninos <-
  'Establecimientos que cuentan con\nárea de juego para niños'

g_ninos <-
  graficar_gauge(bd = bd_ninos |>
                               filter(respuesta == "Sí"),
                             color_principal = "blue",
                             color_secundario = "gray70",
                             escala = c(0, 1),
                             size_text_pct = 12) +
  labs(title = p_ninos,
       caption = caption_ninos) +
  theme(plot.caption = element_text(size = 14),
        plot.title = element_text(size = 16))

bd_ninos_tiposCocina <-
  datos_censo |>
  as_tibble() |>
  filter(P13 == "Sí") |>
  count(P5_O1, sort = TRUE) |>
  mutate(media = n/sum(n)) |>
  rename(respuesta = P5_O1) |>
  left_join(datos_censo |>
              distinct(P5_O1, color),
            by = c("respuesta" = "P5_O1"))

g_ninos_tiposCocina <-
  bd_ninos_tiposCocina |>
  ggplot(aes(x = reorder(respuesta, n),
             y = n,
             fill = color)) +
  geom_col() +
  geom_text(aes(label = n), nudge_y = 0.7, size = 8) +
  coord_flip() +
  labs(x = NULL, y = NULL, caption = caption_ninos) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 30)) +
  scale_y_continuous(limits = c(0, 10), breaks = scales::breaks_pretty(n = 3)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(text = element_text(family = "Poppins"),
        legend.position = "none",
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 14),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
        plot.title = element_text(size = 16))

# Exportar ------------------------------------------------------------------------------------

library(officer)

pptx <-
  officer::read_pptx(path = "./data-raw/plantilla_vladimir.pptx")

add_slide(pptx, layout = "portada", master = "Office Theme") |>
  ph_with(value = "Censo de Negocios", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = "Plaza Galerías Hipódromo", location = ph_location_label(ph_label = "subtitulo"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "Total de negocios por tipo de cocina", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_tipos_cocina, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "dos_graficas_equitativas", master = "Office Theme") %>%
  ph_with(value = "Establecimientos con música en vivo", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_musica_vivo, location = ph_location_label(ph_label = "grafica_uno")) |>
  ph_with(value = g_musica_tiposCocina, location = ph_location_label(ph_label = "grafica_dos"))

add_slide(pptx, layout = "dos_graficas_equitativas", master = "Office Theme") %>%
  ph_with(value = "Establecimientos con diferentes menús por temporada", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_menu_temporada, location = ph_location_label(ph_label = "grafica_uno")) |>
  ph_with(value = g_menu_tiposCocina, location = ph_location_label(ph_label = "grafica_dos"))

add_slide(pptx, layout = "dos_graficas_equitativas", master = "Office Theme") %>%
  ph_with(value = "Establecimientos con promociones especiales", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_promociones, location = ph_location_label(ph_label = "grafica_uno")) |>
  ph_with(value = g_promociones_tiposCocina, location = ph_location_label(ph_label = "grafica_dos"))

add_slide(pptx, layout = "dos_graficas_equitativas", master = "Office Theme") %>%
  ph_with(value = "Establecimientos con servicio en línea", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_pedidosLinea, location = ph_location_label(ph_label = "grafica_uno")) |>
  ph_with(value = g_pedidosLinea_tiposCocina, location = ph_location_label(ph_label = "grafica_dos"))

add_slide(pptx, layout = "dos_graficas_equitativas", master = "Office Theme") %>%
  ph_with(value = "Establecimientos con áreas al aire libre o terrazas", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_terrazas, location = ph_location_label(ph_label = "grafica_uno")) |>
  ph_with(value = g_terrazas_tiposCocina, location = ph_location_label(ph_label = "grafica_dos"))

add_slide(pptx, layout = "dos_graficas_equitativas", master = "Office Theme") %>%
  ph_with(value = "Establecimientos con áreas de juegos para niños", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_ninos, location = ph_location_label(ph_label = "grafica_uno")) |>
  ph_with(value = g_ninos_tiposCocina, location = ph_location_label(ph_label = "grafica_dos"))

print(pptx, "./data-raw/press.pptx")
