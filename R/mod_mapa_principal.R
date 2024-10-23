#' mapa_principal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import ggplot2
#' @importFrom shiny NS tagList
mod_mapa_principal_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      h1("Página principal"),
      hr(),
      fluidRow(
        column(
          width = 12,
          plotOutput(outputId = ns("mapa_principal_negocio"))
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

    output$mapa_principal_negocio <- renderPlot({

      tibble::tibble("respuesta" = LETTERS[1:5],
                     media = seq.int(from = 1, to = 5, by = 1)) |>
        ggplot(aes(x = respuesta, y = media)) +
        geom_col() +
        coord_flip()

    })

  })
}

## To be copied in the UI
# mod_mapa_principal_ui("mapa_principal_1")

## To be copied in the server
# mod_mapa_principal_server("mapa_principal_1")
