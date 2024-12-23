
# Preambulo -----------------------------------------------------------------------------------

library(dplyr)
devtools::load_all(path = "../encuestar/")

# Insumos -------------------------------------------------------------------------------------
eliminados <-
  c(211447999L, 211448001L, 211456177L, 211467551L, 211460475L,
    211512570L, 211527775L, 211525172L, 211527222L, 211578777L,
    211578776L, 211608436L, 211683190L, 211685478L, 211686152L,
    211827566L)

bd_etapa_2_snapshot <-
readxl::read_xlsx(path = "./data-raw/41 ESTAURANTE-nov_-24_2024_15_11_00_47.xlsx", na = "-1") |>
  janitor::clean_names() |>
  filter(!sbj_num %in% eliminados) |>
  # rename(filtro_b = sexo) |>
  mutate(rango_edad = as.character(cut(as.integer(edad),c(17, 24, 59, 200),
                                       c("18A24","25A59","60YMAS"))),
         # sexo = if_else(sexo == "Mujer", "F", "M"),
         across(.cols = c(q_9, q_10, q_11, q_12), .fns = ~ gsub(pattern = " \\(No leer)", replacement = "", x = .x)),
         across(.cols = ends_with("q_20"), .fns = ~ gsub(pattern = " \\(No leer)", replacement = "", x = .x)),
         across(.cols = matches("q_18|q_19"), .fns = ~ gsub(pattern = "^\\d+ = ", replacement = "", x = .x))
  ) |>
  mutate(amai_jefegrado=case_when(jefe_grado %in% c("No estudió","No contesta")~0,
                                  jefe_grado=="Primaria incompleta"~6,
                                  jefe_grado=="Primaria completa"~11,
                                  jefe_grado=="Secundaria incompleta"~12,
                                  jefe_grado=="Secundaria completa"~18,
                                  jefe_grado=="Preparatoria incompleta"~23,
                                  jefe_grado=="Preparatoria completa"~27,
                                  jefe_grado=="Licenciatura incompleta"~36,
                                  jefe_grado=="Licenciatura completa"~59,
                                  jefe_grado=="Diplomado o maestría"~85,
                                  jefe_grado=="Diplomado o maestría"~85,
                                  jefe_grado=="Doctorado"~85,.default = NA),
         amai_cantidadwc=case_when(cantidad_wc=="0"~0,
                                   cantidad_wc=="1"~24,
                                   cantidad_wc=="2 o más"~47,
                                   .default = NA),
         amai_cantidadautos=case_when(cantidad_autos=="0"~0,
                                      cantidad_autos=="1"~22,
                                      cantidad_autos=="2 o más"~43,
                                      .default = NA),
         amai_internet=case_when(internet=="No tiene"~0,
                                 internet=="Sí tiene"~32,.default = NA),
         amai_trabajo=case_when(trabajo=="0"~0,
                                trabajo=="1"~15,
                                trabajo=="2"~31,
                                trabajo=="3"~46,
                                trabajo=="4 o más"~61,
                                .default = NA
         ),
         amai_cantidadcuartos=case_when(cantidad_cuartos=="0"~0,
                                        cantidad_cuartos=="1"~8,
                                        cantidad_cuartos=="2"~16,
                                        cantidad_cuartos=="3"~24,
                                        cantidad_cuartos=="4 o más"~32,
                                        .default = NA)) %>%
  mutate(suma_amai = rowSums(select(., contains("amai_")), na.rm = TRUE),
         nivel_socioec=case_when(
           (suma_amai>=0 & suma_amai<=47)~"E",
           (suma_amai>=48 & suma_amai<=94)~"D",
           (suma_amai>=95 & suma_amai<=115)~"D_mas",
           (suma_amai>=116 & suma_amai<=140)~"C_menos",
           (suma_amai>=141 & suma_amai<=167)~"C",
           (suma_amai>=168 & suma_amai<=201)~"C_mas",
           suma_amai>=202~"A_B",.default = NA))

bd_etapa_2_raw <-
  readxl::read_xlsx(path = "./data-raw/bd_etapa_2_efectivos_kathe.xlsx", na = "-1") |>
  janitor::clean_names() |>
  filter(!sbj_num %in% eliminados) |>
  # rename(filtro_b = sexo) |>
  mutate(rango_edad = as.character(cut(as.integer(edad),c(17, 24, 59, 200),
                                       c("18A24","25A59","60YMAS"))),
         # sexo = if_else(sexo == "Mujer", "F", "M"),
         across(.cols = c(q_9, q_10, q_11, q_12), .fns = ~ gsub(pattern = " \\(No leer)", replacement = "", x = .x)),
         across(.cols = ends_with("q_20"), .fns = ~ gsub(pattern = " \\(No leer)", replacement = "", x = .x)),
         across(.cols = matches("q_18|q_19"), .fns = ~ gsub(pattern = "^\\d+ = ", replacement = "", x = .x))
         ) |>
  mutate(amai_jefegrado=case_when(jefe_grado %in% c("No estudió","No contesta")~0,
                                  jefe_grado=="Primaria incompleta"~6,
                                  jefe_grado=="Primaria completa"~11,
                                  jefe_grado=="Secundaria incompleta"~12,
                                  jefe_grado=="Secundaria completa"~18,
                                  jefe_grado=="Preparatoria incompleta"~23,
                                  jefe_grado=="Preparatoria completa"~27,
                                  jefe_grado=="Licenciatura incompleta"~36,
                                  jefe_grado=="Licenciatura completa"~59,
                                  jefe_grado=="Diplomado o maestría"~85,
                                  jefe_grado=="Diplomado o maestría"~85,
                                  jefe_grado=="Doctorado"~85,.default = NA),
         amai_cantidadwc=case_when(cantidad_wc=="0"~0,
                                   cantidad_wc=="1"~24,
                                   cantidad_wc=="2 o más"~47,
                                   .default = NA),
         amai_cantidadautos=case_when(cantidad_autos=="0"~0,
                                      cantidad_autos=="1"~22,
                                      cantidad_autos=="2 o más"~43,
                                      .default = NA),
         amai_internet=case_when(internet=="No tiene"~0,
                                 internet=="Sí tiene"~32,.default = NA),
         amai_trabajo=case_when(trabajo=="0"~0,
                                trabajo=="1"~15,
                                trabajo=="2"~31,
                                trabajo=="3"~46,
                                trabajo=="4 o más"~61,
                                .default = NA
         ),
         amai_cantidadcuartos=case_when(cantidad_cuartos=="0"~0,
                                        cantidad_cuartos=="1"~8,
                                        cantidad_cuartos=="2"~16,
                                        cantidad_cuartos=="3"~24,
                                        cantidad_cuartos=="4 o más"~32,
                                        .default = NA)) %>%
  mutate(suma_amai = rowSums(select(., contains("amai_")), na.rm = TRUE),
         nivel_socioec=case_when(
           (suma_amai>=0 & suma_amai<=47)~"E",
           (suma_amai>=48 & suma_amai<=94)~"D",
           (suma_amai>=95 & suma_amai<=115)~"D_mas",
           (suma_amai>=116 & suma_amai<=140)~"C_menos",
           (suma_amai>=141 & suma_amai<=167)~"C",
           (suma_amai>=168 & suma_amai<=201)~"C_mas",
           suma_amai>=202~"A_B",.default = NA)) |>
  mutate(q_15 = q_15_correcion)
# rename(atractivo_bistro = bistro_q_18,
#        atractivo_pub = gastro_pub_q_18,
#        atractivo_barrio = cocina_de_barrio_q_18,
#        atractivo_trattoria = trattoria_q_18,
#        atractivo_asian = asian_street_food_fusion_q_18,
#        innovador_bistro = bistro_q_19,
#        innovador_pub = gastro_pub_q_19,
#        innovador_barrio = cocina_de_barrio_q_19,
#        innovador_trattoria = trattoria_q_19,
#        innovador_asian = asian_street_food_fusion_q_19,
#        plato_bistro = bistro_q_20,
#        plato_pub = gastro_pub_q_20,
#        plato_barrio = cocina_de_barrio_q_20,
#        plato_trattoria = trattoria_q_20,
#        plato_asian = asian_street_food_fusion_q_20)

# bd_etapa_2_snapshot |>
#   anti_join(bd_etapa_2_raw, by = "sbj_num") |>
#   openxlsx2::write_xlsx(file = "./data-raw/bd_etapa_2_efectivos_280.xlsx")

bd_etapa_2_diferencias_280 <-
readxl::read_xlsx(path = "./data-raw/bd_etapa_2_efectivos_280_kathe.xlsx", na = "-1") |>
  janitor::clean_names() |>
  filter(!sbj_num %in% eliminados) |>
  # rename(filtro_b = sexo) |>
  mutate(rango_edad = as.character(cut(as.integer(edad),c(17, 24, 59, 200),
                                       c("18A24","25A59","60YMAS"))),
         # sexo = if_else(sexo == "Mujer", "F", "M"),
         across(.cols = c(q_9, q_10, q_11, q_12), .fns = ~ gsub(pattern = " \\(No leer)", replacement = "", x = .x)),
         across(.cols = ends_with("q_20"), .fns = ~ gsub(pattern = " \\(No leer)", replacement = "", x = .x)),
         across(.cols = matches("q_18|q_19"), .fns = ~ gsub(pattern = "^\\d+ = ", replacement = "", x = .x))
  ) |>
  mutate(amai_jefegrado=case_when(jefe_grado %in% c("No estudió","No contesta")~0,
                                  jefe_grado=="Primaria incompleta"~6,
                                  jefe_grado=="Primaria completa"~11,
                                  jefe_grado=="Secundaria incompleta"~12,
                                  jefe_grado=="Secundaria completa"~18,
                                  jefe_grado=="Preparatoria incompleta"~23,
                                  jefe_grado=="Preparatoria completa"~27,
                                  jefe_grado=="Licenciatura incompleta"~36,
                                  jefe_grado=="Licenciatura completa"~59,
                                  jefe_grado=="Diplomado o maestría"~85,
                                  jefe_grado=="Diplomado o maestría"~85,
                                  jefe_grado=="Doctorado"~85,.default = NA),
         amai_cantidadwc=case_when(cantidad_wc=="0"~0,
                                   cantidad_wc=="1"~24,
                                   cantidad_wc=="2 o más"~47,
                                   .default = NA),
         amai_cantidadautos=case_when(cantidad_autos=="0"~0,
                                      cantidad_autos=="1"~22,
                                      cantidad_autos=="2 o más"~43,
                                      .default = NA),
         amai_internet=case_when(internet=="No tiene"~0,
                                 internet=="Sí tiene"~32,.default = NA),
         amai_trabajo=case_when(trabajo=="0"~0,
                                trabajo=="1"~15,
                                trabajo=="2"~31,
                                trabajo=="3"~46,
                                trabajo=="4 o más"~61,
                                .default = NA
         ),
         amai_cantidadcuartos=case_when(cantidad_cuartos=="0"~0,
                                        cantidad_cuartos=="1"~8,
                                        cantidad_cuartos=="2"~16,
                                        cantidad_cuartos=="3"~24,
                                        cantidad_cuartos=="4 o más"~32,
                                        .default = NA)) %>%
  mutate(suma_amai = rowSums(select(., contains("amai_")), na.rm = TRUE),
         nivel_socioec=case_when(
           (suma_amai>=0 & suma_amai<=47)~"E",
           (suma_amai>=48 & suma_amai<=94)~"D",
           (suma_amai>=95 & suma_amai<=115)~"D_mas",
           (suma_amai>=116 & suma_amai<=140)~"C_menos",
           (suma_amai>=141 & suma_amai<=167)~"C",
           (suma_amai>=168 & suma_amai<=201)~"C_mas",
           suma_amai>=202~"A_B",.default = NA)) |>
  mutate(q_15 = q_15_correcion)

bd_etapa_2 <-
bd_etapa_2_raw |>
  bind_rows(bd_etapa_2_diferencias_280)

diccionario_segunda_etapa <-
  readxl::read_xlsx(path = "./data-raw/diccionario_segunda_etapa.xlsx") |>
  mutate(respuestas = strsplit(respuestas, "_"))

diseno_censo <-
  survey::svydesign(
    ids=~1,
    data = bd_etapa_2)

resultados_censo <-
  encuestar:::Resultados$new(encuesta = NULL,
                             diseno = diseno_censo,
                             diccionario = diccionario_segunda_etapa,
                             tema = encuestar:::tema_morant())

# bd_etapa_2_raw |>
#   openxlsx2::write_xlsx(file = "./data-raw/bd_etapa_2_efectivos.xlsx")

colores_tipoCocina <-
  datos_censo |>
  as_tibble() |>
  distinct(tipos_cocina, color)

# rm(shp_etapa_2) <-
#   bd_etapa_2 |>
#   sf::st_as_sf(coords = c("longitude", "latitude"),
#                crs = 4326)

colores_tipoCocina_etapa2 <-
  c("Mexicana / Antojitos mexicanos" = "blue",
    "Carne / Asados" = "#F9844A",
    "Japonesa" = "#f49cbb",
    "Italiana" = "#AAA1C8",
    "Comida rápida o urbana" = '#4D908E',
    "Pescado y mariscos" = "#302F4D",
    "China" = "#4A1942",
    "Taquería" = "purple",
    "Otro:" = "#F9844A",
    "Vegena / Ensaladas / Orgánico" = "#4D908E",
    "Barbacoa" = "#893168",
    "Birria" = "#F8961E",
    "Burritos" = "#ef476f",
    "Española" = "#ef476f",
    "Ns/Nc" = "gray70")

bd_etapa_2 |>
  left_join(bd_etapa_2_snapshot |>
              select(sbj_num, sexo_sn = q_23), by = "sbj_num") |>
  select(!sexo) |>
  rename(sexo = sexo_sn) |>
  relocate(sexo, .after = edad) |>
  select(!c(bistro_q_18:q_23, q_15_correcion)) |>
  openxlsx2::write_xlsx(file = "./data-raw/bd_censo_negocios_segunda_etapa_20241122.xlsx")


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

graficar_barras_pct <- function(bd, x_wrap = 30){

  bd |>
    ggplot(aes(x = reorder(respuesta, pct),
               y = pct,
               fill = respuesta)) +
    geom_col() +
    geom_text(aes(label = scales::percent(pct, accuracy = 1.0)), nudge_y = 0.005, size = 6, colour = "gray30") +
    coord_flip() +
    labs(x = NULL, y = NULL) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = x_wrap)) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 10),
                       labels = scales::percent) +
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

p_consumido_ultimoMes_tit <-
  "¿En el último mes ha consumido alimentos en algún restaurante de este centro comercial?"

g_consumido_ultimoMes <-
  bd_etapa_2 |>
  calcular_proporciones(variable = "filtro_b") |>
  filter(respuesta == 'Sí') |>
  rename(media = pct) |>
  graficar_gauge(color_principal = "#bb3e03",
                 color_secundario = "gray80",
                 escala = c(0, 1),
                 size_text_pct = 12) +
  labs(caption = "Personas que contesttaron que Sí")

p_comida_favorita_tit <-
  "¿Cuál es tu tipo de comida favorita?"

g_comida_favorita <-
  bd_etapa_2 |>
  calcular_proporciones(variable = "q_5") |>
  rename(media = pct) |>
  encuestar:::graficar_barras(salto = 35) +
  scale_fill_manual(values = colores_tipoCocina_etapa2) +
  encuestar:::tema_morant() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

p_comida_fueraCasa_tit <-
  "¿Cuáles son los tres tipos de comidas que más consumes cuando comes fuera de casa?"

g_comida_fueraCasa <-
  bd_etapa_2 |>
  calcular_proporciones_multirespuesta(prefijo = "q_6", aspectos = c("o1", "o2", "o3")) |>
  rename(media = pct) |>
  encuestar:::graficar_barras(salto = 35) +
  scale_fill_manual(values = colores_tipoCocina_etapa2) +
  encuestar:::tema_morant() +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

p_aspectoImportante_tit <-
  "¿Qué aspecto consideras más importante cuando eliges un restaurante?"

g_aspectoImportante <-
  bd_etapa_2 |>
  calcular_proporciones(variable = "q_7") |>
  rename(media = pct) |>
  encuestar:::graficar_barras(salto = 35) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))


p_restaurante_mes_tit <-
  "¿Cuántas veces al mes sueles comer en un restaurante?"

g_restaurante_mes <-
  bd_etapa_2 |>
  count(q_8) |>
  tidyr::complete(q_8 = c("0", "Entre 1 y 3 veces", "Entre 4 y 6 veces", "Entre 7 y 9 veces", "Entre 10 y 12 veces", "Más de 12 veces", "Ns/Nc"),
                  fill = list(n = 0)) |>
  mutate(pct = n/sum(n)) |>
  rename(respuesta = q_8,
         media = pct) |>
  encuestar:::graficar_barras(orden_respuestas = c("0", "Entre 1 y 3 veces", "Entre 4 y 6 veces", "Entre 7 y 9 veces", "Entre 10 y 12 veces", "Más de 12 veces", "Ns/Nc")) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))

# ggplot(aes(x = factor(respuesta, levels = rev(c("0", "Entre 1 y 3 veces", "Entre 4 y 6 veces", "Entre 7 y 9 veces", "Entre 10 y 12 veces", "Más de 12 veces", "Ns/Nc"))),
#            y = pct,
#            fill = respuesta)) +
# geom_col() +
# geom_text(aes(label = scales::percent(pct, accuracy = 1.0)), nudge_y = 0.005, size = 6, colour = "gray30") +
# coord_flip() +
# labs(x = NULL, y = NULL) +
# scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 30)) +
# scale_y_continuous(breaks = scales::breaks_pretty(n = 10),
#                    labels = scales::percent) +
# # scale_fill_identity() +
# theme_minimal() +
# theme(text = element_text(family = "Poppins"),
#       legend.position = "none",
#       axis.text.x = element_text(size = 16),
#       axis.text.y = element_text(size = 12),
#       plot.caption = element_text(size = 14),
#       panel.grid = element_blank(),
#       panel.grid.major.x = element_line(colour = "#C5C5C5", linetype = "dotted"),
#       plot.title = element_text(size = 12))

p_precio_platillo_tit <-
  "En promedio, ¿cuánto sueles gastar individualmente en un plantillo en un restaurante?"

g_precio_platillo <-
  bd_etapa_2 |>
  count(q_9) |>
  tidyr::complete(q_9 = c("Menos de 150", "Entre $150 y $250", "Entre $251 y $500", "Entre $501 y $750", "Entre $751 y $1,000", "Más de $1,000", "Ns/Nc"),
                  fill = list(n = 0)) |>
  mutate(pct = n/sum(n)) |>
  rename(respuesta = q_9,
         media = pct) |>
  encuestar:::graficar_barras(orden_respuestas = c("Menos de 150", "Entre $150 y $250", "Entre $251 y $500", "Entre $501 y $750", "Entre $751 y $1,000", "Más de $1,000", "Ns/Nc")) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))

p_delivery_tit <-
  "¿Cuántas veces al mes sueles comprar comida en un servicio de delivery como Uber Eats o DiDiFood?"

g_delivery <-
  bd_etapa_2 |>
  count(q_10) |>
  tidyr::complete(q_10 = c("0", "Entre 1 y 3 veces", "Entre 4 y 6 veces", "Entre 7 y 9 veces", "Entre 10 y 12 veces", "Más de 12 veces", "Ns/Nc"),
                  fill = list(n = 0)) |>
  mutate(pct = n/sum(n)) |>
  rename(respuesta = q_10,
         media = pct) |>
  encuestar:::graficar_barras(orden_respuestas = c("0", "Entre 1 y 3 veces", "Entre 4 y 6 veces", "Entre 7 y 9 veces", "Entre 10 y 12 veces", "Más de 12 veces", "Ns/Nc")) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))

p_costo_delivery_tit <-
  "En promedio, ¿cuánto sueles gastar individualmente en un pedido para llevar a domicilio?"

g_costo_delivery <-
  bd_etapa_2 |>
  filter(q_10 != 0) |>
  count(q_11) |>
  tidyr::complete(q_11 = c("Menos de 150", "Entre $150 y $250", "Entre $251 y $500", "Entre $501 y $750", "Entre $751 y $1,000", "Más de $1,000", "Ns/Nc"),
                  fill = list(n = 0)) |>
  mutate(pct = n/sum(n)) |>
  rename(respuesta = q_11,
         media = pct) |>
  encuestar:::graficar_barras(orden_respuestas = c("Menos de 150", "Entre $150 y $250", "Entre $251 y $500", "Entre $501 y $750", "Entre $751 y $1,000", "Más de $1,000", "Ns/Nc")) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))

p_frecuencia_visitarPlaza_tit <-
  "Pensando específicamente en los restaurantes de este centro comercial, ¿con qué frecuencia sueles venir a comer aquí cada mes?"

g_frecuencia_visitarPlaza <-
  bd_etapa_2 |>
  count(q_12) |>
  tidyr::complete(q_12 = c("0", "Entre 1 y 3 veces", "Entre 4 y 6 veces", "Entre 7 y 9 veces", "Entre 10 y 12 veces", "Más de 12 veces", "Ns/Nc"),
                  fill = list(n = 0)) |>
  mutate(pct = n/sum(n)) |>
  rename(respuesta = q_12,
         media = pct) |>
  encuestar:::graficar_barras(orden_respuestas = c("0", "Entre 1 y 3 veces", "Entre 4 y 6 veces", "Entre 7 y 9 veces", "Entre 10 y 12 veces", "Más de 12 veces", "Ns/Nc")) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 16))

g_frecuencia_restaurante <-
  bd_etapa_2 |>
  count(q_13) |>
  mutate(pct = n/sum(n)) |>
  transmute(respuesta = stringr::str_to_title(string = q_13),
            media = pct) |>
  mutate(respuesta = forcats::fct_lump_min(f = respuesta, min = 0.01, w = media, other_level = "Otros"),
         respuesta = dplyr::if_else(condition = respuesta == "Otro", true = "Otros", false = respuesta)) %>%
  group_by(respuesta) |>
  summarise(media = sum(media)) |>
  encuestar:::graficar_barras(salto = 35) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 16))

p_preferencia_menu_tit <-
  "¿Qué prefieres generalmente: un restaurante con un menú muy especializado en un tipo de cocina o uno que ofrezca una gran variedad de opciones?"

g_preferencia_menu <-
  bd_etapa_2 |>
  count(q_14) |>
  tidyr::complete(q_14 = c("Menú con variedad", "Menú especializado", "Ns/Nc"),
                  fill = list(n = 0)) |>
  mutate(pct = n/sum(n)) |>
  rename(respuesta = q_14,
         media = pct) |>
  encuestar:::graficar_barras(orden_respuestas = rev(c("Menú con variedad", "Menú especializado", "Ns/Nc"))) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = c("Menú con variedad" ="#f94144",
                               "Menú especializado" = "#f3722c",
                               "Ns/Nc" = "gray70"))
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))

g_buenaOpcion <-
  bd_etapa_2 |>
  count(q_15) |>
  mutate(pct = n/sum(n)) |>
  transmute(respuesta = q_15,
            media = pct) |>
  encuestar:::graficar_barras(salto = 35) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = colores_tipoCocina_etapa2) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))

top_ideas <-
bd_etapa_2 |>
  count(q_15) |>
  filter(q_15 != "Otro") |>
  top_n(n = 5, wt = n) |>
  pull(q_15)

# g_buenaOpcion <-
  bd_etapa_2 |>
  count(q_15, q_9) |>
    filter(q_15 %in% top_ideas) |>
    group_by(q_15) |>
  mutate(pct = n/sum(n)) |>
    ungroup() |>
  rename(respuesta = q_9,
            media = pct,
         tema = q_15) |>
    ggplot(aes(x = factor(respuesta,
                          levels = c("Menos de 150", "Entre $150 y $250", "Entre $251 y $500", "Entre $501 y $750", "Entre $751 y $1,000", "Más de $1,000", "Ns/Nc")),
               y =  media,
               group = tema,
               color = tema)) +
    geom_line(linewidth = 1,
              show.legend = FALSE) +
    geom_point(size = 4) +
    scale_color_manual(values = colores_restaurantes2,
                       labels = function(x) stringr::str_wrap(string = x, width = 15)) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 10)) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 0.8)) +
    encuestar:::tema_morant() +
    labs(color = "") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 16),
          legend.text = element_text(size = 14))



p_conocimiento_restaurantes_tit <-
  "A continuación, te diré cinco tipos de restaurantes, me dirás si los conoces o has escuchado hablar de ellos"

colores_restaurantes <-
  c("Bistro" = "#218380",
    "Gastro Pub" = "#133c55",
    "Cocina de barrio" = "#d81159",
    "Trattoria" = "#613dc1",
    "Asian Street, Food Fusion" = "#8f2d56")

g_conocimiento_restaurantes <-
  paste0("t_q_16_", seq.int(from = 1, to = 5, by = 1)) %>%
  purrr::map_df(.x = .,
                .f = ~ bd_etapa_2 |>
                  calcular_proporciones(variable = .x) |>
                  mutate(var = .x)) |>
  filter(respuesta == "Sí") |>
  rename(value = respuesta) |>
  mutate(respuesta = dplyr::case_when(var == "t_q_16_1" ~ "Bistro",
                                      var == "t_q_16_2" ~ "Gastro Pub",
                                      var == "t_q_16_3" ~ "Cocina de barrio",
                                      var == "t_q_16_4" ~ "Trattoria",
                                      var == "t_q_16_5" ~ "Asian Street, Food Fusion")) |>
  rename(media = pct) |>
  encuestar:::graficar_barras() +
  encuestar:::tema_morant() +
  scale_fill_manual(values = colores_restaurantes) +
  labs(caption = "El porcentaje corresponde a los que contestaron Sí") +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        plot.caption = element_text(size = 14))

p_atractivo_tit <-
  "De acuerdo con tus gustos y en una escala del 0 al 5 ¿qué tan atractivo te resulta este tipo de restaurante?"

bd_atractivo <-
  encuestar:::analizar_frecuencias_aspectos(diseno = diseno_censo,
                                          diccionario = diccionario_segunda_etapa,
                                          patron_pregunta = "atractivo",
                                          aspectos =  c("bistro", "pub", "barrio", "trattoria", "asian")) |>
  mutate(respuesta = factor(respuesta,
                            levels = rev(c("No me atrae mucho, pero podría ir por otras personas",
                                       "No me atrae en absoluto, no lo visitaría",
                                       "Me parece interesante, estaría dispuesto a probarlo",
                                       "Me atrae bastante, podría visitarlo de vez en cuando",
                                       "Me agrada, lo elegiría como una opción regular",
                                       "Me encanta la idea, sería uno de mis restaurantes favoritos")))) |>
  left_join(diccionario_segunda_etapa |>
              distinct(llaves, tema), by = c("aspecto" = "llaves")) |>
  transmute(tema, respuesta, pct = media) |>
  # filter(tema == "Bistró") |>
  group_by(tema) |>
  arrange(tema, respuesta) |>
  mutate(sup = cumsum(pct),
         inf = lag(sup, default = 0),
         label = (inf + sup)/2) |>
  ungroup()

orden_atractivo <-
  bd_atractivo |>
  filter(respuesta == "Me encanta la idea, sería uno de mis restaurantes favoritos") |>
  arrange(pct) |>
  pull(tema)

g_atractivo <-
bd_atractivo |>
  ggplot(aes(x = factor(tema, levels = orden_atractivo),
             fill = respuesta)) +
  ggchicklet::geom_chicklet(aes(y = pct),
                            radius = grid::unit(3, "pt"),
                            color = "transparent", alpha = 0.8, width = 0.7,
                            show.legend = TRUE) +
  ggfittext::geom_bar_text(aes(y = label, label = scales::percent(pct, accuracy = 1.))) +
  coord_flip() +
  tema_morant() +
  scale_fill_manual(values = c("No me atrae mucho, pero podría ir por otras personas" = "#4A4E69",
                               "No me atrae en absoluto, no lo visitaría" = "#606299",
                               "Me parece interesante, estaría dispuesto a probarlo"= "#D5B9B2",
                               "Me atrae bastante, podría visitarlo de vez en cuando" = '#61CF74',
                               "Me agrada, lo elegiría como una opción regular" = "#52B788",
                               "Me encanta la idea, sería uno de mis restaurantes favoritos" = "#2D6A4F"),
                    labels = c("No me atrae mucho, pero podría ir por otras personas" = "0 = No me atrae mucho,\npero podría ir por otras personas",
                               "No me atrae en absoluto, no lo visitaría" = "1 = No me atrae en absoluto,\nno lo visitaría",
                               "Me parece interesante, estaría dispuesto a probarlo"= "2 = Me parece interesante,\nestaría dispuesto a probarlo",
                               "Me atrae bastante, podría visitarlo de vez en cuando" = "3 = Me atrae bastante,\npodría visitarlo de vez en cuando",
                               "Me agrada, lo elegiría como una opción regular" = "4 = Me agrada, lo elegiría\ncomo una opción regular",
                               "Me encanta la idea, sería uno de mis restaurantes favoritos" = "5 = Me encanta la idea,\nsería uno de mis restaurantes favoritos")
                    # labels = function(x) stringr::str_wrap(string = x, width = 25)
                    ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 15)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = "",
       title = "",
       fill = "") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 12),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12))

# g_atractivo <-
#   resultados_censo$Especial$candidatoOpinion(patron_inicial = "atractivo",
#                                              aspectos = c("bistro", "pub", "barrio", "trattoria", "asian"),
#                                              ns_nc = "",
#                                              regular = "",
#                                              llave_burbuja = NA,
#                                              filtro_burbuja = NULL,
#                                              size_burbuja = 8,
#                                              caption_opinion = "",
#                                              caption_nsnc = "Ns/Nc",
#                                              caption_burbuja = "",
#                                              size_caption_opinion = 14,
#                                              size_caption_nsnc = 14,
#                                              size_caption_burbuja = 12,
#                                              size_text_cat = 16,
#                                              size_pct = 14,
#                                              grupo_negativo = c("No me atrae mucho, pero podría ir por otras personas",
#                                                                 "No me atrae en absoluto, no lo visitaría"),
#                                              grupo_positivo = c("Me parece interesante, estaría dispuesto a probarlo",
#                                                                 "Me atrae bastante, podría visitarlo de vez en cuando",
#                                                                 "Me agrada, lo elegiría como una opción regular",
#                                                                 "Me encanta la idea, sería uno de mis restaurantes favoritos"),
                                             # colores_opinion = c("No me atrae mucho, pero podría ir por otras personas" = "#4A4E69",
                                             #                     "No me atrae en absoluto, no lo visitaría" = "#606299",
                                             #                     "Me parece interesante, estaría dispuesto a probarlo"= "#D5B9B2",
                                             #                     "Me atrae bastante, podría visitarlo de vez en cuando" = '#61CF74',
                                             #                     "Me agrada, lo elegiría como una opción regular" = "#52B788",
                                             #                     "Me encanta la idea, sería uno de mis restaurantes favoritos" = "#2D6A4F"),
                                             # orden_resp = c("No me atrae mucho, pero podría ir por otras personas",
                                             #                "No me atrae en absoluto, no lo visitaría",
                                             #                "Me parece interesante, estaría dispuesto a probarlo",
                                             #                "Me atrae bastante, podría visitarlo de vez en cuando",
                                             #                "Me agrada, lo elegiría como una opción regular",
                                             #                "Me encanta la idea, sería uno de mis restaurantes favoritos"),
#                                              salto = 15,
#                                              mostrar_nsnc = F,
#                                              salto_respuestas = 35,
#                                              orden_cat = NULL)



p_innovacion_tit <-
  "Según tu opinión y en una escala del 0 al 5, ¿qué tan innovador consideras que sería incorporar este tipo de restaurante en esta zona comercial?"






bd_innovador <-
  encuestar:::analizar_frecuencias_aspectos(diseno = diseno_censo,
                                            diccionario = diccionario_segunda_etapa,
                                            patron_pregunta = "innovador",
                                            aspectos =  c("bistro", "pub", "barrio", "trattoria", "asian")) |>
  mutate(respuesta = factor(respuesta,
                            levels = rev(c("Nada, hay restaurantes iguales en la zona",
                                           "Es muy similar a algunos restaurantes",
                                           "Es una propuesta interesante, pero no se aleja de lo que ya existe",
                                           "Tiene algunas ideas innovadoras",
                                           "Destacaría en la zona, llamaría la atención",
                                           "Sería una propuesta única y nueva en la zona")))) |>
  left_join(diccionario_segunda_etapa |>
              distinct(llaves, tema), by = c("aspecto" = "llaves")) |>
  transmute(tema, respuesta, pct = media) |>
  # filter(tema == "Bistró") |>
  group_by(tema) |>
  arrange(tema, respuesta) |>
  mutate(sup = cumsum(pct),
         inf = lag(sup, default = 0),
         label = (inf + sup)/2) |>
  ungroup()

orden_innovador <-
  bd_innovador |>
  filter(respuesta == "Sería una propuesta única y nueva en la zona") |>
  arrange(pct) |>
  pull(tema)


g_innovador <-
  bd_innovador |>
  ggplot(aes(x = factor(tema, levels = orden_innovador),
             fill = respuesta)) +
  ggchicklet::geom_chicklet(aes(y = pct),
                            radius = grid::unit(3, "pt"),
                            color = "transparent", alpha = 0.8, width = 0.7,
                            show.legend = TRUE) +
  ggfittext::geom_bar_text(aes(y = label, label = scales::percent(pct, accuracy = 1.))) +
  coord_flip() +
  tema_morant() +
  scale_fill_manual(values = c("Nada, hay restaurantes iguales en la zona" = "#4A4E69",
                               "Es muy similar a algunos restaurantes" = "#606299",
                               "Es una propuesta interesante, pero no se aleja de lo que ya existe" = "#D5B9B2",
                               "Tiene algunas ideas innovadoras" = '#61CF74',
                               "Destacaría en la zona, llamaría la atención" = "#52B788",
                               "Sería una propuesta única y nueva en la zona" = "#2D6A4F"),
                    labels = c("Nada, hay restaurantes iguales en la zona" = "0 = Nada, hay restaurantes\niguales en la zona",
                               "Es muy similar a algunos restaurantes" = "1 = Es muy similar a\nalgunos restaurantes",
                               "Es una propuesta interesante, pero no se aleja de lo que ya existe" = "2 = Es una propuesta interesante,\npero no se aleja de lo que ya existe",
                               "Tiene algunas ideas innovadoras" = "3 = Tiene algunas\nideas innovadoras",
                               "Destacaría en la zona, llamaría la atención" = "4 = Destacaría en la zona,\nllamaría la atención",
                               "Sería una propuesta única y nueva en la zona" = "5 = Sería una propuesta\núnica y nueva en la zona"),
                    # labels = function(x) stringr::str_wrap(string = x, width = 25)
                    ) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 15)) +
  scale_y_continuous(labels = scales::percent) +
  labs(caption = "",
       title = "",
       fill = "") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        plot.caption = element_text(size = 12),
        plot.title = element_text(size = 12),
        legend.text = element_text(size = 12))














# g_innovador <-
#   resultados_censo$Especial$candidatoOpinion(patron_inicial = "innovador",
#                                              aspectos = c("bistro", "pub", "barrio", "trattoria", "asian"),
#                                              ns_nc = "",
#                                              regular = "",
#                                              llave_burbuja = NA,
#                                              filtro_burbuja = NULL,
#                                              size_burbuja = 8,
#                                              caption_opinion = "",
#                                              caption_nsnc = "Ns/Nc",
#                                              caption_burbuja = "",
#                                              size_caption_opinion = 14,
#                                              size_caption_nsnc = 14,
#                                              size_caption_burbuja = 12,
#                                              size_text_cat = 18,
#                                              size_pct = 14,
#                                              grupo_negativo = c("Nada, hay restaurantes iguales en la zona",
#                                                                 "Es muy similar a algunos restaurantes"),
#                                              grupo_positivo = c("Es una propuesta interesante, pero no se aleja de lo que ya existe",
#                                                                 "Tiene algunas ideas innovadoras",
#                                                                 "Destacaría en la zona, llamaría la atención",
#                                                                 "Sería una propuesta única y nueva en la zona"),
                                             # colores_opinion = c("Nada, hay restaurantes iguales en la zona" = "#4A4E69",
                                             #                     "Es muy similar a algunos restaurantes" = "#606299",
                                             #                     "Es una propuesta interesante, pero no se aleja de lo que ya existe" = "#D5B9B2",
                                             #                     "Tiene algunas ideas innovadoras" = '#61CF74',
                                             #                     "Destacaría en la zona, llamaría la atención" = "#52B788",
                                             #                     "Sería una propuesta única y nueva en la zona" = "#2D6A4F"),
#                                              orden_resp = c("Nada, hay restaurantes iguales en la zona",
#                                                             "Es muy similar a algunos restaurantes",
#                                                             "Es una propuesta interesante, pero no se aleja de lo que ya existe",
#                                                             "Tiene algunas ideas innovadoras",
#                                                             "Destacaría en la zona, llamaría la atención",
#                                                             "Sería una propuesta única y nueva en la zona"),
#                                              salto = 15,
#                                              mostrar_nsnc = F,
#                                              salto_respuestas = 35,
#                                              orden_cat = NULL)

p_plato_fuerte_tit <-
  "¿Cuál sería el monto máximo que estarías dispuesto a pagar por un plato fuerte en este tipo de restaurante?"

bd_plato_fuerte <-
  encuestar:::analizar_frecuencias_aspectos(diseno = diseno_censo,
                                            diccionario = diccionario_segunda_etapa,
                                            patron_pregunta = "plato",
                                            aspectos = c("bistro", "pub", "barrio", "trattoria", "asian")) |>
  left_join(diccionario_segunda_etapa |>
              distinct(llaves, tema), by = c("aspecto" = "llaves"))

colores_restaurantes2 <-
  c("Bistró" = "#218380",
    "Gastro Pub" = "#133c55",
    "Cocina de barrio" = "#d81159",
    "Trattoria" = "#613dc1",
    "Asian Street, Food Fusion" = "#8f2d56")

g_plato_fuerte <-
  bd_plato_fuerte |>
  ggplot(aes(x = factor(respuesta,
                        levels = c("Menos de 150", "Entre $150 y $250", "Entre $251 y $500", "Entre $501 y $750", "Entre $751 y $1,000", "Más de $1,000", "Ns/Nc")),
             y =  media,
             group = tema,
             color = tema)) +
  geom_line(linewidth = 1,
            show.legend = FALSE) +
  geom_point(size = 4) +
  scale_color_manual(values = colores_restaurantes2,
                     labels = function(x) stringr::str_wrap(string = x, width = 15)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(string = x, width = 10)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0, 0.8)) +
  encuestar:::tema_morant() +
  labs(color = "") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 14))


p_prioridad_tit <-
  "Por favor, ordena los restaurantes según tus preferencias personales, donde 1 representa el tipo de restaurante que más te atrae, y 5 el que menos te atrae. Considera aspectos como el estilo de comida, ambiente, y la experiencia general al elegir tu orden de preferencia"

bd_prioridad <-
  paste0("q_21_o", seq.int(from = 1, to = 5, by = 1)) %>%
  purrr::map_df(.x = .,
                .f = ~ encuestar:::analizar_frecuencias(diseno = diseno_censo,
                                                        pregunta = .x) |>
                  mutate(preferencia = as.factor(gsub(pattern = "q_21_o",
                                                      replacement = "",
                                                      x = .x))))

orden_prioridad <-
  bd_prioridad |>
  filter(preferencia == "1") |>
  arrange(desc(media)) |>
  pull(respuesta)

g_prioridad <-
  bd_prioridad |>
  rename(var_x = respuesta,
         var_y = preferencia) |>
  ggplot(aes(x = factor(var_x, levels = orden_prioridad),
             y = factor(var_y, levels = rev(as.character(seq.int(from = 1, to = 5, by = 1)))),
             fill = media,
             label = scales::percent(media, 1.))) +
  geom_tile(color = "white") +
  ggfittext::geom_fit_text(contrast = T,
                           family = "Poppins",
                           size = 18) +
  scale_fill_gradient(low = "white",
                      high = "#bb3e03",
                      labels = scales::percent, na.value = "gray70") +
  scale_x_discrete(position = "top",
                   labels =  function(x) stringr::str_wrap(string = x, width = 15)) +
  scale_y_discrete(labels =  function(x) stringr::str_wrap(string = x, width = 30)) +
  labs(x = "",
       y = "",
       caption = "") +
  theme_minimal() +
  tema_transparente() +
  theme(legend.position = "none",
        text = element_text(family = "Poppins"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.caption = element_text(size = 16))


# CRUCE
hola <-
readxl::read_xlsx(path = "./data-raw/41 ESTAURANTE-nov_-24_2024_15_11_00_47.xlsx", na = "-1") |>
  janitor::clean_names() |>
  filter(!sbj_num %in% eliminados) |>
  mutate(rango_edad = as.character(cut(as.integer(edad),c(17, 24, 59, 200),
                                       c("18A24","25A59","60YMAS"))),
         generacion = case_when(edad >= 18 & edad <= 25 ~ "Generación Z (18 a 25 años)",
                                edad >= 26 & edad <= 40 ~ "Millenials (26 a 40 años)",
                                edad >= 41 & edad <= 55 ~ "Generación X (41 a 55 años)",
                                edad >= 56  ~ "Baby Boomers  (56 años o más)"),
         generacion = factor(generacion, levels = c("Generación Z (18 a 25 años)",
                                                    "Millenials (26 a 40 años)",
                                                    "Generación X (41 a 55 años)",
                                                    "Baby Boomers  (56 años o más)")))

# diseno_censo_2 <-
#   survey::svydesign(
#     ids=~1,
#     data = hola)

g_crruce_generacion_prioridad <-
  hola |>
  filter(q_21_o1 == "Cocina de barrio") |>
  count(generacion) |>
  mutate(respuesta = generacion, media = n/sum(n)) |>
  encuestar:::graficar_barras(orden_respuestas = c("Generación Z (18 a 25 años)",
                                                   "Millenials (26 a 40 años)",
                                                   "Generación X (41 a 55 años)",
                                                   "Baby Boomers  (56 años o más)")) +
  encuestar:::tema_morant() +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))

g_crruce_sexo_prioridad <-
  hola |>
  filter(q_21_o1 == "Cocina de barrio") |>
  count(q_23) |>
  mutate(respuesta = q_23, media = n/sum(n)) |>
  encuestar:::graficar_barras() +
  encuestar:::tema_morant() +
  scale_fill_manual(values = c("#bb3e03", color_opinion_mala)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))





# EVALUACION CENTRO COMERCIAL


p_evaluacion_01_tit <-
  "En una escala del 0 al 5, donde 0 es “pésimo” y 5 “excelente”, ¿Cómo calificarías la ..."

temas_evaluacion_01 <-
  diccionario_segunda_etapa |>
  filter(llaves %in% paste0("t_q_22_", seq.int(from = 1, to = 5, by = 1))) |>
  distinct(tema) |>
  pull()

bd_evaluacion_01 <-
  paste0("t_q_22_", seq.int(from = 1, to = 5, by = 1)) %>%
  purrr::map_df(.x = .,
                .f = ~ encuestar:::analizar_frecuencias(diseno = diseno_censo,
                                                        pregunta = .x)
  ) |>
  left_join(diccionario_segunda_etapa |>
              distinct(llaves, tema), by = c("pregunta" = "llaves")) |>
  mutate(respuesta = as.character(respuesta)) |>
  tidyr::complete(tema, respuesta,
                  fill = list(media = 0))

orden_evaluacion_01 <-
  bd_evaluacion_01 |>
  filter(respuesta == "5") |>
  arrange(desc(media)) |>
  pull(tema)

g_evaluacion_01 <-
  bd_evaluacion_01 |>
  rename(var_x = tema,
         var_y = respuesta) |>
  ggplot(aes(x = factor(var_x, levels = orden_evaluacion_01),
             y = factor(var_y, levels = as.character(seq.int(from = 0, to = 5, by = 1))),
             fill = media,
             label = scales::percent(media, 1.))) +
  geom_tile(color = "white") +
  ggfittext::geom_fit_text(contrast = T,
                           family = "Poppins",
                           size = 18) +
  scale_fill_gradient(low = "white",
                      high = "#bb3e03",
                      labels = scales::percent, na.value = "gray70") +
  scale_x_discrete(position = "top",
                   labels =  function(x) stringr::str_wrap(string = x, width = 25)) +
  scale_y_discrete(labels =  function(x) stringr::str_wrap(string = x, width = 30)) +
  labs(x = "",
       y = "",
       caption = "") +
  theme_minimal() +
  tema_transparente() +
  theme(legend.position = "none",
        text = element_text(family = "Poppins"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16),
        plot.caption = element_text(size = 16))




temas_evaluacion_02 <-
  diccionario_segunda_etapa |>
  filter(llaves %in% paste0("t_q_22_", seq.int(from = 6, to = 10, by = 1))) |>
  distinct(tema) |>
  pull()

bd_evaluacion_02 <-
  paste0("t_q_22_", seq.int(from = 6, to = 10, by = 1)) %>%
  purrr::map_df(.x = .,
                .f = ~ encuestar:::analizar_frecuencias(diseno = diseno_censo,
                                                        pregunta = .x)
  ) |>
  left_join(diccionario_segunda_etapa |>
              distinct(llaves, tema), by = c("pregunta" = "llaves")) |>
  mutate(respuesta = as.character(respuesta)) |>
  tidyr::complete(tema, respuesta,
                  fill = list(media = 0))

orden_evaluacion_02 <-
  bd_evaluacion_02 |>
  filter(respuesta == "5") |>
  arrange(desc(media)) |>
  pull(tema)

g_evaluacion_02 <-
  bd_evaluacion_02 |>
  rename(var_x = tema,
         var_y = respuesta) |>
  ggplot(aes(x = factor(var_x, levels = orden_evaluacion_02),
             y = factor(var_y, levels = as.character(seq.int(from = 0, to = 5, by = 1))),
             fill = media,
             label = scales::percent(media, 1.))) +
  geom_tile(color = "white") +
  ggfittext::geom_fit_text(contrast = T,
                           family = "Poppins",
                           size = 18) +
  scale_fill_gradient(low = "white",
                      high = "#bb3e03",
                      labels = scales::percent, na.value = "gray70") +
  scale_x_discrete(position = "top",
                   labels =  function(x) stringr::str_wrap(string = x, width = 25)) +
  scale_y_discrete(labels =  function(x) stringr::str_wrap(string = x, width = 30)) +
  labs(x = "",
       y = "",
       caption = "") +
  theme_minimal() +
  tema_transparente() +
  theme(legend.position = "none",
        text = element_text(family = "Poppins"),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 16),
        plot.caption = element_text(size = 16))


# SOCIODEMOGRAFICOS

p9.1_graf <-
  srvyr::as_survey_design(diseno_censo) %>%
  mutate(edad = as.integer(edad)) |>
  filter(edad < 100,  edad >= 18) %>%
  mutate(sexo = case_when(sexo == "F" ~ "Mujer",
                          sexo == "M" ~ "Hombre")) %>%
  mutate(edad = ifelse(test = edad >= 18 & edad <= 20, yes = "18 a 20", no = (5*ceiling(edad/5)))) %>%
  group_by(sexo,edad) %>%
  summarise(srvyr::survey_total(na.rm=T, vartype=NULL)) %>%
  ungroup() %>%
  mutate(tot=sum(coef, na.rm=T),
         coef=coef/tot) %>%
  mutate(coef = case_when(sexo == "Hombre" ~ -coef,
                          T  ~ coef)) %>%
  mutate(rango = case_when(edad == 25 ~ "21-25",
                           edad == 30 ~ "26-30",
                           edad == 35 ~ "31-35",
                           edad == 40 ~ "36-40",
                           edad == 45 ~ "41-45",
                           edad == 50 ~ "46-50",
                           edad == 55 ~ "51-55",
                           edad == 60 ~ "56-60",
                           edad == 65 ~ "61-65",
                           edad == 70 ~ "66-70",
                           edad == 75 ~ "71-75",
                           edad == 80 ~ "76-80",
                           edad == 85 ~ "81-85",
                           edad == 90 ~ "86-90",
                           edad == 95 ~ "91-95",
                           T ~ edad)) %>%
  ggplot(aes(x = rango, y = coef, fill = sexo, label = scales::percent(abs(coef), 1))) +
  ggchicklet::geom_chicklet(alpha = 0.9) +
  coord_flip() +
  scale_fill_manual(values = c("#bb3e03", color_opinion_mala)) +
  ggfittext::geom_bar_text(contrast = TRUE) +
  lemon::scale_y_symmetric(labels = function(x) scales::percent(abs(x), accuracy = 1)) +
  tema_morant() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.title.y = element_text(family = font_family, colour = font_color, size = 18),
        legend.text = element_text(size = 16),
        plot.background = element_rect(color = "transparent",fill = "transparent"),
        panel.background = element_rect(color = "transparent",fill = "transparent")) +
  labs(x = "Rango de edad",y=NULL)


p9.2_bd <-
  encuestar:::analizar_frecuencias(diseno = diseno_censo,
                                   pregunta = "q_24") |>
  select(respuesta, media)

p9.2_graf <-
  p9.2_bd |>
  ggplot(aes(x = factor(respuesta, levels = c("No estudió",
                                              "Primaria incompleta",
                                              "Primaria completa",
                                              "Secundaria incompleta",
                                              "Secundaria completa",
                                              "Preparatoria incompleta",
                                              "Preparatoria completa",
                                              "Licenciatura incompleta",
                                              "Licenciatura completa",
                                              "Posgrado",
                                              "No contesta")),
             y = media,
             fill = respuesta)) +
  ggchicklet::geom_chicklet(width = 0.6, alpha = 0.9) +
  ggfittext::geom_bar_text(aes(label = scales::percent(media, accuracy = 1.)), contrast = T) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  # scale_fill_manual(values = colores_estudios) +
  tema_morant() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        plot.caption = element_text(family = font_family, colour = font_color, size = 14),
        axis.text.x = element_text(family = font_family, colour = font_color, size = 14),
        axis.text.y = element_text(family = font_family, colour = font_color, size = 16),
        plot.background = element_rect(color = "transparent",fill = "transparent"),
        panel.background = element_rect(color = "transparent",fill = "transparent"))


p8.3_graf <-
  encuestar:::analizar_frecuencias(diseno = diseno_censo,
                                   pregunta = "q_25") |>
  encuestar:::graficar_barras(salto = 35) +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  tema_morant()+
  theme(legend.position = "none",
        plot.title = element_blank(),
        plot.caption = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        plot.background = element_rect(color = "transparent",fill = "transparent"),
        panel.background = element_rect(color = "transparent",fill = "transparent"))

p8.4_bd <-
  encuestar:::analizar_frecuencias(diseno = diseno_censo,
                                   pregunta = "q_26") |>
  select(respuesta, media)

p8.4_graf <-
  p8.4_bd |>
  ggplot(aes(x = factor(respuesta, levels = c("De 0 a 3,000",
                                              "De 3,001 a 6,000",
                                              "De 6,001 a 9,000",
                                              "De 9,001 a 12,000",
                                              "De 12,001 a 15,000",
                                              "De 15,001 a 18,000",
                                              "Más de 18,001",
                                              "No contesta")),
             y = media,
             fill = respuesta)) +
  ggchicklet::geom_chicklet(width = 0.6, alpha = 0.9) +
  ggfittext::geom_bar_text(aes(label = scales::percent(media, accuracy = 1.)), contrast = T) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  tema_morant() +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none",
        plot.caption = element_text(family = font_family, colour = font_color, size = 14),
        axis.text.x = element_text(family = font_family, colour = font_color, size = 14),
        axis.text.y = element_text(family = font_family, colour = font_color, size = 14),
        plot.background = element_rect(color = "transparent",fill = "transparent"),
        panel.background = element_rect(color = "transparent",fill = "transparent"))

g_amai <-
  encuestar:::analizar_frecuencias(diseno = diseno_censo,
                                   pregunta = "nivel_socioec") |>  encuestar:::graficar_barras(orden_respuestas = rev(c("A_B", "C_mas", "C", "C_menos", "D_mas", "D"))) +
  encuestar:::tema_morant() +
  scale_x_discrete(labels = c("A_B" = "A-B",
                              "C_mas" = "C+",
                              "C" = "C",
                              "C_menos" = "C-",
                              "D_mas" = "D+",
                              "D" = "D")) +
  scale_fill_manual(values = rep("#bb3e03", 15)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 16))


