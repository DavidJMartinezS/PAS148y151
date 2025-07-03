#' uso_actual UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_uso_actual_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      style = "color: #0461b8; font-size: 16px",
      icon("circle-info"),
      tags$span(
        "Crear capa de la cartografía digital a partir de la capa de predios,
        catastro de CONAF y clase de usos de CIREN"
      )
    ),
    tags$div(style = "margin-top: 10px"),
    mod_leer_sf_ui(ns("predios"), "Ingresar capa de predios") %>%
      add_help_text(title = "Campos minimos requeridos:\n'N_Predio', Nom_Predio'"),
    tags$div(style = "margin-top: -10px"),
    mod_leer_sf_ui(ns("catastro"), "Ingresar capa de catastro de CONAF") %>%
      add_help_text(title = "Campos minimos requeridos:\n'USO', 'SUBUSO', 'ESTRUCTURA'"),
    tags$div(style = "margin-top: -10px"),
    mod_leer_sf_ui(ns("suelos"), "Ingresar capa de suelos de CIREN") %>%
      add_help_text(title = "Campos minimos requeridos:\n'TEXTCAUSo o Clase_Uso'"),
    tags$div(style = "margin-top: -10px"),
    tags$div(
      id = "flex",
      shinyWidgets::actionBttn(
        inputId = ns("get_uso_actual"),
        label = "Generar capa",
        style = "unite",
        size = "sm",
        color = "success"
      ),
      mod_downfiles_ui(ns("down_uso_actual")),
      mod_downfiles_ui(ns("down_tbl_uso_actual"), label = "Tabla", style = "material-flat", icon = "file-excel")
    )
  )
}

#' uso_actual Server Functions
#'
#' @noRd
#' @importFrom shiny eventReactive moduleServer observeEvent req
#' @importFrom terra crs
mod_uso_actual_server <- function(id, crs, dec_sup){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    predios <- mod_leer_sf_server(
      id = "predios",
      crs = crs
    )
    catastro <- eventReactive(predios(),{
      mod_leer_sf_server(
        id = "catastro",
        crs = crs(),
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
    })

    observeEvent(catastro(),{
      req(shiny::isTruthy(catastro()))
      check_input(
        x = catastro(),
        names_req = c('USO', 'SUBUSO', 'ESTRUCTURA'),
        id = "catastro-sf_file"
      )
    })

    suelos <- eventReactive(predios(), {
      mod_leer_sf_server(
        id = "suelos",
        crs = crs(),
        fx = function(x){
          x %>%
            dplyr::rename_if(
              names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T),
              ~ "Clase_Uso"
            )
        },
        wkt_filter = sf::st_as_text(sf::st_geometry(sf::st_union(predios())))
      )
    })

    observeEvent(suelos(),{
      req(shiny::isTruthy(suelos()))
      check_input(
        x = suelos(),
        names_req = c('Clase_Uso'),
        id = "suelos-sf_file"
      )
    })

    uso_actual <- eventReactive(input$get_uso_actual, {
      req(c(predios(), catastro(), suelos()))
      cart_uso_actual(
        catastro = catastro(),
        predios = predios(),
        suelos = suelos(),
        dec_sup = dec_sup
      )
    })

    wb_uso_actual <- eventReactive(uso_actual(),{
      req(c(uso_actual(), predios()))

      tbl_uso_actual <- uso_actual() %>%
        sf::st_join(predios() %>% dplyr::select(N_Predio), largest = T) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(N_Predio, Uso_Actual, Sup_ha) %>%
        dplyr::mutate(
          Uso_Actual = dplyr::case_when(
            Uso_Actual %>% stringi::stri_detect_regex("Bosque") ~ paste0("Bosques_", Uso_Actual),
            Uso_Actual %>% stringi::stri_detect_regex("Uso agrícola") ~ Uso_Actual %>% stringi::stri_replace_all_regex(" \\(", "_") %>% stringi::stri_replace_all_regex("\\)", ""),
            Uso_Actual %>% stringi::stri_detect_regex("Otros usos") ~ "Otros usos",
            .default = Uso_Actual
          )
        ) %>%
        dplyr::group_by(N_Predio, Uso_Actual) %>%
        dplyr::summarise(Sup_ha = sum(Sup_ha), .groups = "drop") %>%
        tidyr::pivot_wider(
          names_from = Uso_Actual, values_from = Sup_ha
        ) %>%
        dplyr::mutate(Total = rowSums(select(., -c(1)), na.rm = TRUE)) %>%
        dplyr::select(
          N_Predio,
          dplyr::matches("Bosques.*Adulto"),
          dplyr::matches("Bosques.*Renoval"),
          dplyr::matches("Bosques.*Mixto"),
          dplyr::matches("Uso.*I-IV"),
          dplyr::matches("Uso.*V-VIII"),
          dplyr::matches("Uso.*N.C."),
          dplyr::starts_with("Áreas"),
          dplyr::starts_with("Otros"),
          Total
        ) %>%
        dplyr::rename("N° Predio" = N_Predio) %>%
        flextable::flextable() %>%
        flextable::separate_header(split = "_") %>%
        flextable::autofit() %>%
        flextable::theme_box() %>%
        flextable::valign(part = "header", valign = "center") %>%
        flextable::align(part = "header", align = "center") %>%
        flextable::bg(bg = "#bcc5d4", part = "header")

      wb <- openxlsx2::wb_workbook(theme = "Integral") %>%
        openxlsx2::wb_add_worksheet("Uso_Actual", grid_lines = T) %>%
        flexlsx::wb_add_flextable(sheet = "Uso_Actual", ft = tbl_uso_actual, start_col = 1, start_row = 1)

      return(wb)
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
      req(c(uso_actual(), wb_uso_actual()))
      gc(reset = T)
      shinybusy::remove_modal_spinner()
      shinybusy::notify_success("Listo el uso actual!", timeout = 3000, position = "right-bottom")
    })

    observeEvent(uso_actual(), {
      mod_downfiles_server(id = "down_uso_actual", x = uso_actual(), name_save = "Uso_actual")
    })
    observeEvent(wb_uso_actual(),{
      mod_downfiles_server(id = "down_tbl_uso_actual", x = wb_uso_actual(), name_save = "Tabla_Uso_actual")
    })
  })
}

## To be copied in the UI
# mod_uso_actual_ui("uso_actual_1")

## To be copied in the server
# mod_uso_actual_server("uso_actual_1")
