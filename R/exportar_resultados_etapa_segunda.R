# Exportar ------------------------------------------------------------------------------------

# library(officer)

pptx <-
  officer::read_pptx(path = "./data-raw/plantilla_vladimir.pptx")

add_slide(pptx, layout = "portada", master = "Office Theme") |>
  ph_with(value = "Censo de Negocios", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = "Plaza Galerías Hipódromo\nSegunda Etapa", location = ph_location_label(ph_label = "subtitulo"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_consumido_ultimoMes_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_consumido_ultimoMes, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_comida_favorita_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_comida_favorita, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_comida_fueraCasa_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_comida_fueraCasa, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_aspectoImportante_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_aspectoImportante, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_restaurante_mes_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_restaurante_mes, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_precio_platillo_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_precio_platillo, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_delivery_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_delivery, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_costo_delivery_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_costo_delivery, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_frecuencia_visitarPlaza_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_frecuencia_visitarPlaza, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "¿Cuál es el restaurante o establecimiento de comida que más frecuentas en este centro comercial?", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_frecuencia_restaurante, location = ph_location_label(ph_label = "imagen_principal"))

# add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
#   ph_with(value = p_frecuencia_visitarPlaza_tit, location = ph_location_label(ph_label = "titulo")) |>
#   ph_with(value = g_frecuencia_visitarPlaza, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "¿Qué tipo de restaurante o comida crees que sería una buena opción para agregar a este centro comercial?", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_buenaOpcion, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_preferencia_menu_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_preferencia_menu, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_conocimiento_restaurantes_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_conocimiento_restaurantes, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_atractivo_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_atractivo, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_innovacion_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_innovador, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_plato_fuerte_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_plato_fuerte, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_prioridad_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_prioridad, location = ph_location_label(ph_label = "imagen_principal"))



add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "Personas que eligieron a 'Comida de barrio' como la opción número uno", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_crruce_sexo_prioridad, location = ph_location_label(ph_label = "imagen_principal"))





add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_evaluacion_01_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_evaluacion_01, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = p_evaluacion_01_tit, location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_evaluacion_02, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "Edad y Sexo", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = p9.1_graf, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "¿Cuál es su último grado de estudios?", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = p9.2_graf, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "¿Cuál es su principal ocupación?", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = p8.3_graf, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "¿Cuánto gana usted por este trabajo al mes?", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = p8.4_graf, location = ph_location_label(ph_label = "imagen_principal"))

add_slide(pptx, layout = "una_grafica_sencilla", master = "Office Theme") %>%
  ph_with(value = "Índice AMAI", location = ph_location_label(ph_label = "titulo")) |>
  ph_with(value = g_amai, location = ph_location_label(ph_label = "imagen_principal"))

print(pptx, "./data-raw/cruce_segunda_etapa.pptx")
