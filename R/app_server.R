#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny dplyr shinydashboard
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  output$menu <- shinydashboard::renderMenu({
    menus <- tibble(tipo = c("Mapa principal"),
                    nombre = c("mapa_principal"),
                    icono = c("map")) %>%
      purrr::pmap(function(tipo, nombre, icono){
        menuItem(tipo,
                 selected = F,
                 tabName = nombre,
                 icon = icon(icono)
        )
      })
    sidebarMenu(.list = menus)
  })

  mod_mapa_principal_server("mapa_principal_1")

}
