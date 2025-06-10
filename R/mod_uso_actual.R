#' uso_actual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_uso_actual_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(style = "margin-top: -10px"),
    tags$h3("Uso actual"),
    mod_leer_sf_ui(ns("predios"), "Ingresar capa de predios") %>%
      add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio'"),
    mod_leer_sf_ui(ns("catastro"), "Ingresar capa de catastro de CONAF") %>%
      add_help_text(title = "Campos minimos requeridos:\n'USO', 'SUBUSO', 'ESTRUCTURA'"),
    mod_leer_sf_ui(ns("suelos"), "Ingresar capa de suelos de CIREN") %>%
      add_help_text(title = "Campos minimos requeridos:\n'TEXTCAUSo o Clase_Uso'"),
    tags$div(
      id = "flex",
      shinyWidgets::actionBttn(
        inputId = ns("get_uso_actual"),
        label = "Generar capa",
        style = "unite",
        size = "sm",
        color = "success"
      ),
      mod_downfiles_ui(ns("down_uso_actual"))
    )
  )
}

#' uso_actual Server Functions
#'
#' @noRd
mod_uso_actual_server <- function(id, crs, dec_sup){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    predios <- mod_leer_sf_server(
      id = "predios",
      crs = crs
    )
    catastro <- mod_leer_sf_server(
      id = "catastro",
      crs = crs,
      fx = function(x){
        x %>%
          dplyr::rename_all(
            ~ ifelse(
              . == "geometry",
              .,
              stringi::stri_trans_toupper(stringi::stri_trans_general(.,"Latin-ASCII"))
            )
          )
      },
      wkt_filter = sf::st_as_text(sf::st_geometry(sf::st_union(predios())))
    )
    suelos <- mod_leer_sf_server(
      id = "suelos",
      crs = crs,
      fx = function(x){
        x %>%
          dplyr::rename_if(
            names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T),
            ~ "Clase_Uso"
          )
      },
      wkt_filter = sf::st_as_text(sf::st_geometry(sf::st_union(predios())))
    )

    uso_actual <- eventReactive(input$get_uso_actual, {
      cart_uso_actual(
        catastro = catastro(),
        predios = predios(),
        suelos = suelos(),
        dec_sup = dec_sup
      )
    })

    observeEvent(input$get_uso_actual, {
      req(c(predios(), catastro(), suelos()))
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#35978F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando capa de uso actual.",
            tags$br(),
            "Por favor espere, esto puede tardar un poco"
          )
        )
      )
      req(uso_actual())
      gc(reset = T)
      shinybusy::remove_modal_spinner()
      shinybusy::notify_success("Listo el uso actual!", timeout = 3000, position = "right-bottom")
      mod_downfiles_server(id = "down_uso_actual", x = uso_actual(), name_save = "Uso_actual")
    })
  })
}

## To be copied in the UI
# mod_uso_actual_ui("uso_actual_1")

## To be copied in the server
# mod_uso_actual_server("uso_actual_1")
