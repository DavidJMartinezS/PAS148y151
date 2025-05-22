#' add_attr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags uiOutput fileInput renderUI observeEvent observe isTruthy eventReactive reactive
#' @importFrom shinyWidgets materialSwitch actionBttn pickerInput updatePickerInput
#' @importFrom shinyjs enable disable
#' @importFrom sf st_crs st_drop_geometry st_geometry st_distance st_nearest_feature
#' @importFrom dplyr mutate case_when bind_cols syms relocate last_col
#' @importFrom purrr map_dbl
#' @importFrom units drop_units
#' @importFrom janitor round_half_up
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#'
mod_add_attr_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3("Agregar Atributos"),
    mod_leer_sf_ui(ns("sf_to_attr"),"Ingrese Shapefile al cual añadir los atributos"),
    tags$div(style = "margin-top: -10px"),
    shinyWidgets::materialSwitch(
      inputId = ns("add_pend_info"),
      label = "¿Agregar pendiente media y rangos de pendientes?",
      status = "success"
    ) %>%
      add_help_text("Crea o actualiza los campos 'Pend_media' y 'Ran_Pend'"),
    uiOutput(ns("add_pend_info_ui")),
    shinyWidgets::materialSwitch(
      inputId = ns("add_hidro_info"),
      label = "¿Agregar distancia a los cursos de agua?",
      status = "success"
    ) %>%
      add_help_text("Crea o actualiza los campos 'Distancia' e incluye campos a elección"),
    uiOutput(ns("add_hidro_info_ui")),
    tags$div(
      id = "flex",
      shinyWidgets::actionBttn(
        inputId = ns("add_attr"),
        label = "Atributar",
        style = "unite",
        size = "sm",
        color = "success"
      ),
      mod_downfiles_ui(id = ns("down_sf"), style = "material-flat", label = "Shapefile"),
      mod_downfiles_ui(id = ns("down_xlsx"), style = "material-flat", label = "Excel"),
    )
  )
}

#' add_attr Server Functions
#'
#' @noRd
mod_add_attr_server <- function(id, PAS){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    shp <- mod_leer_sf_server("sf_to_attr")
    shp_name <- mod_leer_sf_server("sf_to_attr", path = T)

    # rangos pendiene
    observeEvent(input$add_pend_info,{
      output$add_pend_info_ui <- renderUI({
        if(input$add_pend_info){
          tags$div(
            fileInput(
              inputId = ns("dem_help"),
              label = "Ingresar DEM",
              multiple = F,
              accept = c(".tif",".jp2"),
              buttonLabel = "Seleccionar",
              placeholder = "Archivo no seleccionado"
            ) %>%
              add_help_text("Por favor utilizar DEM acotado al área de estudio"),
            div(style = "margin-top: -10px")
          )
        }
      })
    })

    # Hidrografia
    observeEvent(input$add_hidro_info,{
      output$add_hidro_info_ui <- renderUI({
        if(input$add_hidro_info){
          tags$div(
            id = "flex",
            mod_leer_sf_ui(ns("hidro"), "Ingresar capa hidrográfica", width = "50%"),
            tags$div(style = "margin-left: 10px"),
            shinyWidgets::pickerInput(
              inputId = ns("campos"),
              label = "Campos que añadir",
              choices = c(NULL),
              multiple = T,
              options = list(title = "Selecciona una o más opciones")
            ),
            tags$div(style = "margin-top: -10px")
          )
        }
      })
    })
    hidro <- mod_leer_sf_server(id = "hidro", crs = sf::st_crs(shp()))
    observeEvent(hidro(),{
      shinyWidgets:updatePickerInput(
        session = session,
        inputId = "campos",
        choices = hidro() %>% sf::st_drop_geometry() %>% names()
      )
    })

    shinyjs::disable("add_attr")
    observe({
      if ((input$add_pend_info & isTruthy(input$dem_help$datapath) & isTruthy(shp())) ||
          (input$add_hidro_info & isTruthy(hidro()) & isTruthy(shp()))) {
        shinyjs::enable("add_attr")
      }
    })

    shp_2 <- eventReactive(input$add_attr,{
      req(shp())
      shp() %>%
        {if (input$add_pend_info) {
          .[] %>%
            dplyr::mutate(Pend_media = get_slope(dem = input$dem_help$datapath, x = shp())) %>%
            {if(PAS == 148){
              .[] %>%
                dplyr::mutate(
                  Ran_Pend = dplyr::case_when(
                    Pend_media >= 0 & Pend_media < 30 ~ "0% - 30%",
                    Pend_media >= 30 & Pend_media < 45 ~ "30% - 45%",
                    Pend_media >= 45 & Pend_media < 60 ~ "45% - 60%",
                    Pend_media >= 60  ~ "60% y más"
                  )
                )
            } else {
              .[] %>%
                dplyr::mutate(
                  Ran_pend = dplyr::case_when(
                    Pend_media >= 0 & Pend_media < 10 ~ "0% - 10%",
                    Pend_media >= 10 & Pend_media < 30 ~ "10% - 30%",
                    Pend_media >= 30 & Pend_media < 45 ~ "30% - 45%",
                    Pend_media >= 45 & Pend_media < 60 ~ "45% - 60%",
                    Pend_media >= 60  ~ "60% y más"
                  )
                )
            }}
        } else .} %>%
        {if (input$add_hidro_info) {
          .[] %>%
            dplyr::mutate(
              Distancia = c(1:nrow(shp())) %>%
                purrr::map_dbl(function(x) {
                  sf::st_distance(shp()[x,], hidro()[sf::st_nearest_feature(shp()[x,], sf::st_geometry(hidro())),]) %>%
                    units::drop_units() %>%
                    janitor::round_half_up()
                })
            ) %>%
            dplyr::bind_cols(
              hidro()[sf::st_nearest_feature(shp(), sf::st_geometry(hidro())),] %>%
                select(!!!dplyr::syms(input$campos)) %>%
                sf::st_drop_geometry()
            )
        } else .} %>%
        dplyr::relocate(geometry, .after = dplyr::last_col())
    })

    observeEvent(input$add_attr,{
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#35978F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Añadiendo atributos",
            tags$br(),
            "Por favor espere un poco"
          )
        )
      )
      req(shp_2())
      gc(reset = T)
      shinybusy::remove_modal_spinner()
    })
    observeEvent(shp_2(),{
      mod_downfiles_server(id = "down_sf", x = shp_2(), name_save = shp_name())
      mod_downfiles_server(id = "down_xlsx", x = sf::st_drop_geometry(shp_2()), name_save = shp_name())
    })
  })
}

## To be copied in the UI
# mod_add_attr_ui("add_attr_1")

## To be copied in the server
# mod_add_attr_server("add_attr_1")
