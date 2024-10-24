#' mapa_principal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import ggplot2 sf leaflet
#' @importFrom shiny NS tagList
mod_mapa_principal_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      h1("Plaza Galerías Hipódromo"),
      fluidRow(
        selectInput(inputId = ns("giro_negocio"),
                    label = h2("Tipo de negocio"),
                    choices = c("Todos", unique(datos_censo$giro))),
        column(
          width = 6,
          leafletOutput(outputId = ns("mapa_principal_negocio"),
                        height = "700px")
        )
      )
    )
  )
}

#' mapa_principal Server Functions
#'
#' @noRd
mod_mapa_principal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    datos_censo_reactive <- eventReactive(input$giro_negocio, {
      datos_censo %>%
        {
          if(input$giro_negocio != "Todos"){
            filter(.data = ., giro == input$giro_negocio)
          } else {
            .
          }
        }
    })

    output$mapa_principal_negocio <- renderLeaflet({
      shp_galeriasHipodromo |>
        st_bbox(obj = shp_galeriasHipodromo) |>
        st_as_sfc() |>
        leaflet(options = leafletOptions(zoomControl = FALSE)) |>
        addProviderTiles(providers$Esri.WorldStreetMap) |>
        addMarkers(lng = -116.9925142,
                   lat = 32.5083855) |>
        addPolygons(data = circunferencia_1km,
                    stroke = TRUE,
                    weight = 2,
                    fillOpacity = 0.2) |>
        addMarkers(data = datos_censo_reactive())
    })

  })
}

## To be copied in the UI
# mod_mapa_principal_ui("mapa_principal_1")

## To be copied in the server
# mod_mapa_principal_server("mapa_principal_1")
