#' mapa_principal UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import ggplot2 sf leaflet highcharter
#' @importFrom shiny NS tagList
mod_mapa_principal_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$style(HTML("
    .selectize-input { font-size: 20px; }
    .selectize-dropdown .option { font-size: 20px; }
                      ")),
      h1("Plaza Galerías Hipódromo"),
      fluidRow(
        selectInput(inputId = ns("tipo_cocina"),
                    label = h2("Tipo de cocina"),
                    choices = c("Todos", sort(unique(datos_censo$P5_O1))), width = "40%"),
        column(
          width = 6,
          leafletOutput(outputId = ns("mapa_principal_negocio"),
                        height = "600px")
        ),
        column(
          width = 6,
          highchartOutput(outputId = ns("barras_giro"),
                          height = "600px")
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

    datos_censo_reactive <- eventReactive(input$tipo_cocina, {
      datos_censo %>%
        left_join(tipos_cocina, by = c("P5_O1" = "respuesta")) %>%
        {
          if(input$tipo_cocina != "Todos"){
            filter(.data = .,
                   P5_O1 == input$tipo_cocina)
            # filter(.data = .,
            #        if_any(.cols = starts_with("P5_"), ~ . == input$tipo_cocina))
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
        addTiles(urlTemplate = "https://mt1.google.com/vt/lyrs=r&x={x}&y={y}&z={z}",
                 attribution = '© Google') |>
        addMarkers(lng = -116.9925142,
                   lat = 32.5083855) |>
        addPolygons(data = circunferencia_1km,
                    stroke = TRUE,
                    weight = 2,
                    fillOpacity = 0.2,
                    group = "1 Km a la redonda") |>
        addPolygons(data = shp_poligono,
                    stroke = TRUE,
                    weight = 5,
                    fillOpacity = 0.1,
                    group = "Área de interés") |>
        addCircleMarkers(data = datos_censo_reactive(),
                         opacity = 1,
                         radius = 10,
                         fillOpacity = 1,
                         stroke = F,
                         # fillColor = ~pal_tipo_negocios(P5_O1),
                         fillColor = ~color,
                         # color = ~pal_tipo_negocios(P5_O1),
                         color = ~color,
                         # clusterOptions = markerClusterOptions(),
                         popup = ~ glue::glue("<span style='font-size:20px;'>Nombre: {P2} <br>
                                              Tipo de cocina: {tipos_cocina} <br>
                                              Horario de apertura: {P6} <br>
                                              Horario de cierre: {P7} <br>
                                              Música en vivo: {P8} <br>
                                              Menús de temporada: {P9} <br>
                                              Promociones especiales: {P10} <br>
                                              Pedidos en línea o en app: {P11} <br>
                                              Áreas al aire libre o terrazas: {P12} <br>
                                              Área de juego para niños: {P13}</span>"),
                         group = "Negocios") |>
        addLayersControl(baseGroups = c("Negocios"),
                         overlayGroups = c("1 Km a la redonda", "Área de interés"),
                         options = layersControlOptions(),
                         position = "bottomright") |>
        hideGroup(c("1 Km a la redonda"))
    })

    output$barras_giro <- renderHighchart({
      # browser()
      bd_tipos_cocina <-
        datos_censo_reactive() |>
        as_tibble() |>
        select(SbjNum, respuesta = P5_O1) |>
        count(respuesta, sort = TRUE) |>
        mutate(pct = n/sum(n)) |>
        # select(SbjNum, starts_with("P5_")) |>
        # tidyr::pivot_longer(cols = !c(SbjNum),
        #                     names_to = "pregunta",
        #                     values_to = "respuesta") |>
        # na.omit() %>%
        # count(respuesta) |>
        # mutate(pct = n/nrow(datos_censo_reactive())) |>
        # arrange(desc(pct)) |>
        # select(respuesta, n, pct) |>
        left_join(tipos_cocina, by = c("respuesta"))

      total <-
        datos_censo_reactive() |>
        as_tibble() |>
        distinct(SbjNum) |>
        nrow()

      legend <-
        if(input$tipo_cocina == "Todos") {
          "todos los tipos de cocina: "
        } else {
          paste("tipo de cocina '", input$tipo_cocina, "': ", sep = "")
        }

      g <-
        highchart() |>
        hc_title(text = glue::glue("Total de establecimientos por ", legend, "{total}"), style = list(fontSize = "30px")) |>
        hc_xAxis(categories = bd_tipos_cocina$respuesta, labels = list(style = list(fontSize = "18px"))) |>
        hc_yAxis(labels = list(style = list(fontSize = "18px")), tickInterval = 1) |>
        hc_add_series(data = bd_tipos_cocina$n, type = "bar", showInLegend = FALSE, colorByPoint = TRUE, colors = bd_tipos_cocina$color) |>
        hc_plotOptions(series = list(dataLabels = list(enabled = TRUE, format = "{point.y}", style = list(fontSize = "24px")))) |>
        hc_legend(itemStyle = list(fontSize = "24px")) |>
        hc_credits(
          enabled = TRUE,
          text = "Los establecimientos pueden tener más de un tipo de cocina",
          href = NULL,
          style = list(fontSize = "24px",  textAlign = "right")
        )

      return(g)

    })

  })
}

## To be copied in the UI
# mod_mapa_principal_ui("mapa_principal_1")

## To be copied in the server
# mod_mapa_principal_server("mapa_principal_1")
