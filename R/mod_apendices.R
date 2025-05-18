#' apendices UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom dplyr rename_all rename_at rename_if mutate_at contains starts_with vars matches
#' @importFrom janitor clean_names
#' @importFrom shiny NS tagList tags fileInput uiOutput observeEvent renderUI reactive req observe eventReactive isTruthy
#' @importFrom shinyWidgets virtualSelectInput materialSwitch actionBttn
#' @importFrom shinyjs enable disable
#' @importFrom openxlsx2 read_xlsx
#' @importFrom sf st_distance
#' @importFrom stringi stri_trans_totitle stri_opts_brkiter stri_cmp_equiv stri_detect_regex stri_trans_general stri_trans_toupper stri_replace_all_regex
mod_apendices_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(style = "margin-top: -10x"),
    tags$h3("Apéndices", style = "font-weight: bold;"),
    tags$div(
      id = "inline",
      shinyWidgets::virtualSelectInput(
        inputId = ns("portada"),
        label = "Seleccionar portada :",
        choices = c("default", "KIMAL"),
        selected = "default",
        width = "200px",
        dropboxWrapper = "body"
      ),
      style = "margin-bottom: 10px"
    ),
    tags$h4("Apéndices 2 y 3 (Densidad de especies y ubicación de parcela)", style = "font-weight: bold;"),
    shinyWidgets::materialSwitch(
      inputId = ns("add_bd_pcob"),
      label = "¿Desea incluir parcelas de cobertura?",
      status = "success"
    ),
    uiOutput(ns("add_bd_pcob_ui")),
    tags$div(style = "margin-top: -5px"),
    tags$div(
      id = "flex",
      shinyWidgets::actionBttn(
        inputId = ns("get_apendices_2y3_btn"),
        label = "Obtener Apéndices 2 y 3",
        style = "unite",
        size = "sm",
        color = "success"
      ),
      mod_downfiles_ui(ns("down_apendices_2"), label = "Apéndice 2", style = "material-flat"),
      mod_downfiles_ui(ns("down_apendices_3"), label = "Apéndice 3", style = "material-flat")
    ),
    tags$br(),
    tags$h4("Apéndice 5 - Tablas formulario CONAF", style = "font-weight: bold;"),
    div(style = "margin-top: 10px"),
    tags$div(
      style = "margin-left: -15px",
      mod_downfiles_ui(
        ns("tabla_attr_rodal_0"),
        label = "Tabla atributación de rodales",
        style = "material-flat",
        icon = "file-excel"
      )
    ),
    tags$div(style = "margin-top: 15px"),
    fileInput(
      inputId = ns("tabla_attr_rodal"),
      label = "Ingresar tabla con los atributos de rodal definitiva y revisada",
      multiple = F,
      accept = c(".xlsx"),
      buttonLabel = "Seleccionar",
      placeholder = "Archivo no seleccionado"
    ) %>%
      add_help_text(title = "Cargar con los mismos atributos de como se descargó"),
    tags$div(style = "margin-top: -5px"),
    mod_leer_sf_ui(ns("obras_ap5"), "Ingresar obras (opcional)") %>%
      add_help_text(title = "Campos minimos requeridos:\n'Tipo', 'Obra'"),
    tags$div(style = "margin-top: -5px"),
    uiOutput(ns("bd_fauna_ui")),
    tags$div(style = "margin-top: -10px"),
    tags$div(
      id = "flex",
      shinyWidgets::actionBttn(
        inputId = ns("get_apendice_5_btn"),
        label = "Obtener tablas formulario",
        style = "unite",
        size = "sm",
        color = "success"
      ),
      mod_downfiles_ui(ns("down_apendice_5"), label = "Apéndice 5", style = "material-flat")
    )
  )
}

#' apendices Server Functions
#'
#' @noRd
mod_apendices_server <- function(id, PAS, provincia, huso, crs, inputs, carto){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # APÉNDICES 2 Y 3 ----
    observeEvent(input$add_bd_pcob,{
      output$add_bd_pcob_ui <- renderUI({
        if(input$add_bd_pcob){
          div(
            fileInput(
              inputId = ns("bd_pcob"),
              label = "Ingresar BD  de parcelas de cobertura",
              multiple = F,
              accept = c(".xlsx"),
              buttonLabel = "Seleccionar",
              placeholder = "Archivo no seleccionado"
            ) %>%
              add_help_text(
                title = "Campos minimos requeridos:\n
              'Parcela', 'Especie', 'Copa_NS', 'Copa_EO'"
              ),
            div(style = "margin-top: -10px")
          )
        }
      })
    })

    bd_pcob <- reactive({
      if (input$add_bd_pcob) {
        req(input$bd_pcob$datapath)
        read_xlsx(input$bd_pcob$datapath) %>%
          janitor::clean_names() %>%
          dplyr::rename_all( ~ if_else(
            . == "geometry",.,
            stringi::stri_trans_totitle(
              stringi::stri_trans_general(., "Latin-ASCII"),
              opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")
            )
          )) %>%
          dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("copa_ns", strength = 1), ~ "Copa_NS") %>%
          dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("copa_eo", strength = 1), ~ "Copa_EO") %>%
          dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("diametro.*1", case_insensitive = T), ~ "Copa_NS") %>%
          dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("diametro.*1", case_insensitive = T), ~ "Copa_EO") %>%
          dplyr::rename_at(dplyr::vars(dplyr::contains("UTM")), stringi::stri_trans_toupper) %>%
          dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Copa")), stringi::stri_replace_all_regex, "\\,", "\\.") %>%
          dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Copa")), as.numeric)
      } else {
        NULL
      }
    })

    # observeEvent(bd_pcob(),{
    #   check_bd_pcob(bd_pcob())
    # })

    shinyjs::disable("get_apendices_2y3_btn")
    observe({
      req(c(inputs$bd_flora, inputs$rodales_def, inputs$predios_def))
      shinyjs::enable("get_apendices_2y3_btn")
    })

    apendices_2y3 <- eventReactive(input$get_apendices_2y3_btn,{
      req(c(inputs$bd_flora, inputs$rodales_def, inputs$predios_def))
      apendice_2_3(
        PAS = PAS,
        bd_flora = inputs$bd_flora,
        bd_pcob = bd_pcob(),
        rodales = inputs$rodales_def,
        predios = inputs$predios_def,
        portada = input$portada,
        provincia = provincia,
        huso = huso
      )
    })

    observeEvent(input$get_apendices_2y3_btn,{
      req(inputs$bd_flora, inputs$rodales_def, inputs$predios_def)
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#35978F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando apéndices 2 y 3.",
            tags$br(),
            "Por favor espere, esto puede tardar unos minutos"
          )
        )
      )
      req(apendices_2y3())
      gc()
      shinybusy::remove_modal_spinner()
    })

    mod_downfiles_server(
      id = "down_apendices_2",
      x = apendices_2y3()[[1]],
      name_save = c("APÉNDICE 2. Densiadad de especies")
    )
    mod_downfiles_server(
      id = "down_apendices_3",
      x = apendices_2y3()[[2]],
      name_save = c("APÉNDICE 3. Coordenadas ubicación de parcelas")
    )

    # ATRIBUTOS DE RODAL ----
    tabla_attr_rodal_0 <- reactive({
      req(c(isTruthy(inputs$bd_flora), isTruthy(inputs$rodales_def)), cancelOutput = T)
      get_tabla_attr_rodal(
        PAS = PAS,
        parcelas_rodales = inputs$bd_flora,
        rodales_def = inputs$rodales_def
      )
    })
    tabla_attr_rodal <- reactive({
      req(input$tabla_attr_rodal)
      openxlsx2::read_xlsx(input$tabla_attr_rodal$datapath)
    })

    mod_downfiles_server(
      id = "tabla_attr_rodal_0",
      x = tabla_attr_rodal_0(),
      name_save = c("Tabla atributacion de rodales")
    )

    # APÉNDICE 5 ----
    shinyjs::disable("get_apendice_5_btn")
    observe({
      req(c(
        isTruthy(inputs$bd_flora),
        isTruthy(inputs$rodales_def),
        isTruthy(carto$tabla_predios),
        isTruthy(carto$tabla_areas),
        isTruthy(tabla_attr_rodal())
      ))
      shinyjs::enable("get_apendice_5_btn")
    })

    obras_ap5 <- mod_leer_sf_server(id = "obras_ap5", crs = crs, fx = function(x){
      x %>%
        dplyr::rename_all( ~ ifelse(
          . == "geometry",
          .,
          stringi::stri_trans_totitle(
            stringi::stri_trans_general(., "Latin-ASCII"),
            opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")
          )
        ))
    })
    observeEvent(obras_ap5(),{
      if(!all(c('Tipo', 'Obra') %in% names(obras_ap5()))){
        shinyalerta(names_act = names(obras_ap5()), names_req = c('Tipo', 'Obra'))
      }
    })

    observeEvent(PAS,{
      output$bd_fauna_ui <- renderUI({
        if (PAS == 148) {
          fileInput(
            inputId = ns("bd_fauna"),
            label = "Ingresar BD de fauna (opcional)",
            multiple = F,
            accept = c(".xlsx"),
            buttonLabel = "Seleccionar",
            placeholder = "Archivo no seleccionado"
          ) %>%
            add_help_text(
              title = "Campos minimos requeridos:\n'Nombre_cientifico', 'UTM_E', 'UTM_N', 'Categoria', 'Decreto'"
            )
        }
      })
    })

    bd_fauna <- reactive({
      req(input$bd_fauna$datapath)
      openxlsx2::read_xlsx(input$bd_fauna$datapath) %>%
        janitor::clean_names() %>%
        dplyr::rename_all(
          ~ stringi::stri_trans_totitle(
            stringi::stri_trans_general(., "Latin-ASCII"),
            opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")
          )
        ) %>%
        dplyr::rename_at(dplyr::vars(dplyr::contains("utm")), stringi::stri_trans_toupper) %>%
        dplyr::select(dplyr::matches('Nombre_cientifico|UTM_E|UTM_N|Categoria|Decreto'))
    })
    observeEvent(bd_fauna(),{
      check_input(
        x = bd_fauna(),
        names_req = c('Nombre_cientifico', 'UTM_E', 'UTM_N', 'Categoria', 'Decreto'),
        id = "bd_fauna"
      )
    })

    apendice_5 <- eventReactive(input$get_apendice_5_btn,{
      req(c(
        isTruthy(inputs$bd_flora),
        isTruthy(inputs$rodales_def),
        isTruthy(carto$tabla_predios),
        isTruthy(carto$tabla_areas),
        isTruthy(tabla_attr_rodal())
      ))
      if (input$PAS == 148) {
        apendice_5_PAS148(
          bd_flora = inputs$bd_flora,
          rodales = inputs$rodales_def,
          tabla_predios = carto$tabla_predios,
          tabla_areas = carto$tabla_areas,
          tabla_attr_rodal = tabla_attr_rodal(),
          portada = input$portada,
          provincia = provincia,
          carto_uso_actual = carto$uso_actual,
          obras = obras_ap5(),
          bd_fauna = bd_fauna()
        )
      } else {
        apendice_5_PAS148(
          bd_flora = inputs$bd_flora,
          rodales = inputs$rodales_def,
          tabla_predios = carto$tabla_predios,
          tabla_areas = carto$tabla_areas,
          tabla_attr_rodal = tabla_attr_rodal(),
          portada = input$portada,
          provincia = provincia,
          obras = obras_ap5()
        )
      }
    })

    observeEvent(input$get_apendice_5_btn,{
      req(c(
        isTruthy(inputs$bd_flora),
        isTruthy(inputs$areas_def),
        isTruthy(inputs$rodales_def),
        isTruthy(inputs$predios_def),
        isTruthy(carto$tabla_predios),
        isTruthy(carto$tabla_areas)
      ))
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#6FB58F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando compilado de tablas.",
            tags$br(),
            " Por favor espere, esto puede tardar unos minutos"
          )
        )
      )
      req(apendice_5())
      gc()
      shinybusy::remove_modal_spinner()
    })

    mod_downfiles_server(
      id = "down_apendice_5",
      x = apendice_5(),
      name_save = c("APÉNDICE 5. Tablas formulario CONAF")
    )
  })
}

## To be copied in the UI
# mod_apendices_ui("apendices_1")

## To be copied in the server
# mod_apendices_server("apendices_1")
