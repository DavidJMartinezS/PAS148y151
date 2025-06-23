#' st_order UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom bsplus bs_embed_tooltip
#' @importFrom dplyr group_by mutate syms
#' @importFrom shiny NS tagList eventReactive observe req observeEvent tags
#' @importFrom shinyWidgets pickerInput actionBttn
#' @importFrom shinyjs enable disable
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner notify_success
#'
mod_st_order_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "color: #0461b8; font-size: 16px",
      icon("circle-info"),
      tags$span("Lee un shapefile y crea un campo 'ID_ord' con el orden espacial que indique.")
    ),
    tags$div(style = "margin-top: 10px"),
    mod_leer_sf_ui(ns("sf_order"), "Ingrese Shapefile que desea ordenar"),
    tags$div(style = "margin-top: -10px"),
    shinyWidgets::pickerInput(
      inputId = ns("orden"),
      label = "Ordenar de:",
      selected = "NS-OE",
      choices = c("NS-OE","NS-EO","SN-EO","SN-OE","EO-NS","EO-SN","OE-NS","OE-SN")
    ),
    tags$div(style = "margin-top: -10px"),
    shinyWidgets::pickerInput(
      inputId = ns("select_field_order"),
      label = "Agrupar por (Opcional):",
      multiple = T,
      choices = c(NULL),
      options = list(title = "Selecciona una o más opciones")
    ),
    tags$div(
      id = "flex",
      shinyWidgets::actionBttn(
        inputId = ns("apply_order"),
        label = "Ordenar capa",
        style = "unite",
        size = "sm",
        color = "success"
      ) %>%
        bsplus::bs_embed_tooltip(title = "Crea campo 'ID_ord' con el orden"),
      mod_downfiles_ui(ns("down_sf_ordered"))
    )
  )
}

#' st_order Server Functions
#'
#' @noRd
mod_st_order_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    shp_to_order <- mod_leer_sf_server("sf_order")
    shp_to_order_name <- mod_leer_sf_server("sf_order", path = T)

    observeEvent(shp_to_order(),{
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "select_field_order",
        choices = names(shp_to_order())
      )
    })

    shinyjs::disable("apply_order")
    observe({
      req(shp_to_order())
      shinyjs::enable("apply_order")
    })

    shp_ordered <- eventReactive(input$apply_order,{
      req(shp_to_order())
      shp_to_order() %>%
        {if(!is.null(input$select_field_order)) dplyr::group_by(., !!!dplyr::syms(input$select_field_order)) else .} %>%
        dplyr::mutate(ID_ord = st_order(geometry, order = input$orden))
    })

    observeEvent(input$apply_order,{
      req(shp_to_order())
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#35978F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando campo 'ID_ord' con la numeración.",
            tags$br(),
            " Por favor espere, esto puede tardar un poco"
          )
        )
      )
      req(shp_ordered())
      gc(reset = T)
      shinybusy::remove_modal_spinner()
      shinybusy::notify_success("Shapefile ordenado!", timeout = 3000, position = "right-bottom")
      mod_downfiles_server(id = "down_sf_ordered", x = shp_ordered(), name_save = paste0(shp_to_order_name(),"_ord"))
    })
  })
}

## To be copied in the UI
# mod_st_order_ui("st_order_1")

## To be copied in the server
# mod_st_order_server("st_order_1")
