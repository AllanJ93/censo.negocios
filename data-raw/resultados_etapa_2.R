
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)


# Insumos -------------------------------------------------------------------------------------

bd_etapa_2_raw <-
  readxl::read_xlsx(path = "./data-raw/41 ESTAURANTE-nov_-24_2024_10_11_21_03.xlsx", na = "-1") |>
  janitor::clean_names() |>
  mutate(rango_edad = as.character(cut(as.integer(edad),c(17, 24, 59, 200),
                                       c("18A24","25A59","60YMAS"))),
         sexo = if_else(q_23 == "Mujer", "F", "M"))

bd_etapa_2_raw |>
  glimpse()

colores_tipoCocina <-
  datos_censo |>
  as_tibble() |>
  distinct(tipos_cocina, color)

shp_etapa_2 <-
  bd_etapa_2_raw |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = 4326)


# Funciones -----------------------------------------------------------------------------------

calcular_proporciones <- function(bd, variable) {

  bd |>
    count(!!rlang::sym(variable), sort = TRUE) |>
    na.omit() |>
    rename(respuesta = !!rlang::sym(variable)) |>
    mutate(pct = n/sum(n, na.rm = TRUE))

}

calcular_proporciones_multirespuesta <- function(bd, prefijo, aspectos) {

  bd |>
    select(sbj_num, all_of(paste(prefijo, aspectos, sep = "_"))) |>
    tidyr::pivot_longer(cols = !sbj_num,
                        names_to = "pregunta",
                        values_to = "respuesta") |>
    count(respuesta, sort = TRUE) |>
    na.omit() |>
    mutate(pct = n/nrow(bd))

}

graficar_barras <- function(bd, x_wrap = 30){

  bd |>
    ggplot(aes(x = reorder(respuesta, n),
               y = n,
               fill = respuesta)) +
    geom_col() +
    geom_text(aes(label = n), nudge_y = 0.7, size = 6, colour = "gray30") +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = x_wrap)) +
    scale_y_continuous(limits = c(0, round(max(bd$n)*1.1)),
                       breaks = scales::breaks_pretty(n = 10)) +
    # scale_fill_identity() +
    theme_minimal() +
    theme(text = element_text(family = "Poppins"),
          legend.position = "none",
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 12),
          plot.caption = element_text(size = 14),
          panel.grid = element_blank(),
          panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
          plot.title = element_text(size = 12))

}

graficar_gauge <-  function (bd, color_principal, color_secundario = "gray80", escala,
                             size_text_pct)
{
  g <- bd %>% ggplot() + geom_rect(aes(xmin = 2, xmax = 3,
                                       ymin = 0, ymax = media), fill = color_principal, color = "white",
                                   alpha = 0.95) + geom_rect(aes(xmin = 2, xmax = 3, ymin = media,
                                                                 ymax = escala[2]), fill = color_secundario, color = "white")
  if (escala[2] == 1) {
    g <- g + geom_text(aes(x = 0, y = 0.5, label = scales::percent(x = media,
                                                                   accuracy = 1)), size = size_text_pct, family = "Poppins",
                       nudge_y = 0.25)
  }
  else {
    g <- g + geom_text(aes(x = 0, y = 0.5, label = scales::comma(x = media,
                                                                 accuracy = 1.1)), size = size_text_pct, family = "Poppins",
                       nudge_y = 0.25)
  }
  g <- g + scale_fill_manual(values = c("#1DCDBC", "#38C6F4")) +
    scale_x_continuous(limits = c(0, NA)) + scale_y_continuous(limits = c(0,
                                                                          escala[2])) + xlab("") + ylab("") + coord_polar(theta = "y") +
    theme_void() + theme(legend.position = "none", axis.text = element_blank(),
                         text = element_text(size = 15, family = "Poppins"))
  return(g)
}

# Resultados ----------------------------------------------------------------------------------

bd_etapa_2_raw |>
  glimpse()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_5") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones_multirespuesta(prefijo = "q_6", aspectos = c("o1", "o2", "o3")) |>
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_7") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_7") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_8") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_9") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_10") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_11") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_12") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_13") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_14") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "q_15") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()


bd_etapa_2_raw |>
  calcular_proporciones(variable = "t_q_16_1") |>
  filter(respuesta == 'Sí') |>
  rename(media = pct) |>
  graficar_gauge(color_principal = "#bb3e03",
                 color_secundario = "gray80",
                 escala = c(0, 1),
                 size_text_pct = 12)

bd_etapa_2_raw |>
  calcular_proporciones(variable = "t_q_16_2") |>
  filter(respuesta == 'Sí') |>
  rename(media = pct) |>
  graficar_gauge(color_principal = "#bb3e03",
                 color_secundario = "gray80",
                 escala = c(0, 1),
                 size_text_pct = 12)

bd_etapa_2_raw |>
  calcular_proporciones(variable = "t_q_16_3") |>
  filter(respuesta == 'Sí') |>
  rename(media = pct) |>
  graficar_gauge(color_principal = "#bb3e03",
                 color_secundario = "gray80",
                 escala = c(0, 1),
                 size_text_pct = 12)

bd_etapa_2_raw |>
  calcular_proporciones(variable = "t_q_16_4") |>
  filter(respuesta == 'Sí') |>
  rename(media = pct) |>
  graficar_gauge(color_principal = "#bb3e03",
                 color_secundario = "gray80",
                 escala = c(0, 1),
                 size_text_pct = 12)

bd_etapa_2_raw |>
  calcular_proporciones(variable = "t_q_16_5") |>
  filter(respuesta == 'Sí') |>
  rename(media = pct) |>
  graficar_gauge(color_principal = "#bb3e03",
                 color_secundario = "gray80",
                 escala = c(0, 1),
                 size_text_pct = 12)

bd_etapa_2_raw |>
  calcular_proporciones(variable = "bistro_q_18") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "bistro_q_19") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "bistro_q_20") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  glimpse()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "gastro_pub_q_18") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "gastro_pub_q_19") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "gastro_pub_q_20") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  glimpse()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "cocina_de_barrio_q_18") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "cocina_de_barrio_q_19") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "cocina_de_barrio_q_20") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  glimpse()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "trattoria_q_18") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "trattoria_q_19") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "trattoria_q_20") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  glimpse()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "asian_street_food_fusion_q_18") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "asian_street_food_fusion_q_19") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "asian_street_food_fusion_q_20") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()

bd_etapa_2_raw |>
  glimpse()

bd_etapa_2_raw |>
  calcular_proporciones_multirespuesta(prefijo = "q_21", aspectos = c("o1", "o2", "o3", "o4", "o5")) |>
  graficar_barras()

bd_etapa_2_raw |>
  calcular_proporciones_multirespuesta(prefijo = "q_21", aspectos = c("o1", "o2", "o3", "o4", "o5")) |>
  graficar_barras()

bd_etapa_2_raw |>
  glimpse()

bd_etapa_2_raw |>
  calcular_proporciones(variable = "asian_street_food_fusion_q_20") |>
  # left_join(colores_tipoCocina, by = c("respuesta" = "tipos_cocina"))
  graficar_barras()
