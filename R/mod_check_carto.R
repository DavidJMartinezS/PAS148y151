#' check_carto UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList req tags observeEvent
#' @importFrom shinyWidgets pickerInput actionBttn
#' @importFrom shinyjs enable disable
mod_check_carto_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "color: #0461b8; font-size: 16px",
      icon("circle-info"),
      tags$span(
        "Lee un shapefile de la cartografia digital, y chequea si tiene los
        campos y el orden de éstos de acuerdo con los requerimientos técnicos de CONAF."
      )
    ),
    tags$div(style = "margin-top: 10px"),
    mod_leer_sf_ui(ns("sf_check"),"Ingrese Shapefile"),
    tags$div(style = "margin-top: -10px"),
    shinyWidgets::pickerInput(
      inputId = ns("select_sf_check"),
      label = "Seleccione la capa a la que corresponde el shapefile",
      choices = c(
        "Áreas",
        "Caminos",
        "Curvas de nivel",
        "Hidrografía",
        "Límite predial",
        "Parcelas",
        "Puntos de referencia",
        "Rangos de pendiente",
        "Rodales",
        "Suelos"
      ),
      options = list(title = "Selecciona una opción")
    ),
    shinyWidgets::actionBttn(
      inputId = ns("check_carto"),
      label = "Chequear",
      style = "unite",
      size = "sm",
      color = "success"
    )
  )
}

#' check_carto Server Functions
#'
#' @noRd
mod_check_carto_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    shp_check <- mod_leer_sf_server("sf_check")

    shinyjs::disable("check_carto")
    observe({
      req(shp_check(), input$select_sf_check)
      shinyjs::enable("check_carto")
    })

    observeEvent(input$check_carto,{
      req(shp_check(), input$select_sf_check)
      check_carto(x = shp_check(), id = input$select_sf_check, shiny = T)
    })
  })
}

## To be copied in the UI
# mod_check_carto_ui("check_carto_1")

## To be copied in the server
# mod_check_carto_server("check_carto_1")
