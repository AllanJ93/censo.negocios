## code to prepare `datos_censo` dataset goes here

library(dplyr)
library(sf)

bd_censo_raw <-
  readxl::read_xlsx(path = "./data-raw/38 CENSO_KATH.xlsx", na = "-1") |>
  filter(!SbjNum %in% c(210983135, 210983136)) |>
  mutate(P5_O1 = P5_correguida) |>
  filter(!is.na(P5_O1))

bd_tipo_cocina <-
  bd_censo_raw |>
  select(SbjNum, starts_with("P5_")) |>
  tidyr::pivot_longer(cols = !SbjNum,
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  na.omit() |>
  group_by(SbjNum) |>
  mutate(n_cocina = n(),
         multiple = dplyr::if_else(condition = n_cocina > 1,
                                   true = TRUE,
                                   false = FALSE)) |>
  ungroup() |>
  group_by(SbjNum, n_cocina) |>
  summarise(tipos_cocina = paste(respuesta, collapse = ", "),
            .groups = "drop")

datos_censo <-
  bd_censo_raw |>
  left_join(bd_tipo_cocina, by = 'SbjNum') |>
  mutate(P2 = stringr::str_to_title(P2)) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4326)

bd_colores_raw <-
  readxl::read_xlsx(path = "./data-raw/colores_censo.xlsx") |>
  mutate(colores = paste0("#", colores)) |>
  bind_rows(tibble(colores = c("red", "blue", "yellow", "black"))) |>
  rename(color = colores)

bd_colores <-
  bd_censo_raw |>
  select(SbjNum, starts_with("P5_")) |>
  tidyr::pivot_longer(cols = !SbjNum,
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  distinct(respuesta) |>
  na.omit() |>
  bind_cols(bd_colores_raw)

datos_censo <-
  datos_censo |>
  left_join(bd_colores, by = c("P5_O1" = "respuesta"))

usethis::use_data(datos_censo, overwrite = TRUE)

tipos_cocina <-
  datos_censo |>
  as_tibble() |>
  select(SbjNum, starts_with("P5_")) |>
  tidyr::pivot_longer(cols = !SbjNum,
                      names_to = "pregunta",
                      values_to = "respuesta") |>
  na.omit() |>
  distinct(respuesta)

# tipos_cocina <-
#   tipos_cocina |>
#   bind_cols(color = topo.colors(n_distinct(tipos_cocina$respuesta)))

usethis::use_data(tipos_cocina, overwrite = TRUE)

