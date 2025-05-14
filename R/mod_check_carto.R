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
    tags$h3("Chequeo de cartografía"),
    mod_leer_sf_ui(ns("sf_check"),"Ingrese Shapefile"),
    tags$div(style = "margin-top: -10px"),
    shinyWidgets::pickerInput(
      inputId = "select_sf_check",
      label = "Seleccione la capa a la que corresponde el shapefile",
      choices = c(
        "Área",
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
      inputId = "check_carto",
      label = "Chequear",
      style = "unite",
      size = "sm",
      color = "success"
    ),
    tags$hr(style="height:2px;border-width:0;color:gray;background-color:gray")
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
      check_carto(x = shp_check(), id = input$select_sf_check)
    })
  })
}

## To be copied in the UI
# mod_check_carto_ui("check_carto_1")

## To be copied in the server
# mod_check_carto_server("check_carto_1")
