#' PredRodArea UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList uiOutput
mod_PredRodArea_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_leer_sf_ui(ns("linea_base"), "Ingrese cartografía de linea base") %>%
      add_help_text(title = "Campos minimos requeridos:\n'PID', 'Tipo_fores', 'Subtipo_fo', 'Tipo_veg', 'Regulacion'"),
    mod_leer_sf_ui(ns("obras"), "Ingrese shp de obras"),
    mod_leer_sf_ui(ns("predios"), "Ingrese shp de predios") %>%
      add_help_text(title = "Campos minimos requeridos:\n'N_Predio','Nom_Predio', 'Rol', 'Propietari'"),
    mod_leer_sf_ui(ns("suelos"), "Ingrese shp de suelos") %>%
      add_help_text(title = "Campos minimos requeridos:\n'TEXTCAUS'"),
    shinyWidgets::pickerInput(
      inputId = ns("group_by_LB"),
      label = "Agrupar por (Opcional):",
      choices = c(NULL),
      multiple = T,
      options = list(title = "Selecciona una o más opciones")
    ),
    shinyWidgets::materialSwitch(
      inputId = ns("sep_by_soil"),
      label = "¿Separar geometrías por CUS?",
      value = T,
      status = "success"
    ),
    shinyWidgets::materialSwitch(
      inputId = ns("group_by_dist"),
      label = "¿Agrupar áreas por distancia?",
      status = "success"
    ),
    uiOutput(ns("distanceUI")),
    shinyWidgets::materialSwitch(
      inputId = ns("cut_by_prov"),
      label = "¿Cortar áreas por una provincia?",
      status = "success"
    ),
    uiOutput(ns("select_prov_UI")),
    tags$div(
      shinyWidgets::materialSwitch(
        inputId = ns("ord_rodales"),
        label = "¿Ordenar rodales espacialmente?",
        status = "success"
      ),
      uiOutput(ns("ord_rodales_UI"))
    ),
    tags$div(
      id = "flex",
      shinyWidgets::actionBttn(
        inputId = ns("get_area"),
        label = "Generar capas",
        style = "unite",
        size = "sm",
        color = "success"
      ),
      mod_downfiles_ui(ns("down_areas"))
    )
  )
}

#' PredRodArea Server Functions
#'
#' @noRd
#' @importFrom shiny eventReactive icon moduleServer observe observeEvent reactive renderUI req
mod_PredRodArea_server <- function(id, crs, dec_sup, provincia, PAS, huso){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # GENERAR ÁREAS DE CORTA ----
    LB <- mod_leer_sf_server(id = "linea_base", crs = crs, fx = function(x){
      x %>%
        dplyr::rename_all( ~ ifelse(. == "geometry",.,
          stringi::stri_trans_totitle(
            stringi::stri_trans_general(., "Latin-ASCII"),
            opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")
          )
        )) %>%
        dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("pid", strength = 1), ~ "PID") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^tipo.*for", case_insensitive = T), ~ "Tipo_fores") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^tipo.*veg", case_insensitive = T), ~ "Tipo_veg") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^sub.*tipo.*fo", case_insensitive = T), ~ "Subtipo_fo") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ley.*20283", case_insensitive = T), ~ "Regulacion")
    })
    observeEvent(LB(),{
      check_input(
        x = LB(),
        names_req = c('Tipo_fores', 'Subtipo_fo', 'Tipo_veg', 'Regulacion'),
        huso = huso,
        id = "linea_base-sf_file"
      )
      if (!'PID' %in% names(LB())){
        shinybusy::notify_warning("Campo 'PID' será creado", timeout = 3000, position = "right-bottom")
      }
      shinyWidgets::updatePickerInput(
        session = session,
        inputId = "group_by_LB",
        choices = names(LB())[!names(LB()) == "geometry"]
      )
    })

    obras <- mod_leer_sf_server(id = "obras", crs = crs)
    observeEvent(obras(),{
      check_input(
        x = obras(),
        names_req = NULL,
        huso = huso,
        id = "obras-sf_file"
      )
    })

    predios <- mod_leer_sf_server(id = "predios", crs = crs, fx = function(x){
      x %>%
        dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("n_predio", strength = 1), ~ "N_Predio") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("nom_predio", strength = 1), ~ "Nom_Predio") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("rol", strength = 1), ~ "Rol") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^prop", case_insensitive = T), ~ "Propietari")
    })
    observeEvent(predios(),{
      check_input(
        x = predios(),
        names_req = c('N_Predio', 'Nom_Predio', 'Rol', 'Propietari'),
        huso = huso,
        id = "predios-sf_file"
      )
    })

    suelos <- mod_leer_sf_server(id = "suelos", crs = crs, fx = function(x){
      x %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_Uso") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("desceros|cat_erosio|clase_eros", case_insensitive = T), ~ "Clase_Eros")
    })
    observeEvent(suelos(),{
      if(PAS == 148){
        check_input(
          x = suelos(),
          names_req = c('Clase_Uso'),
          huso = huso,
          id = "suelos-sf_file"
        )
      } else {
        check_input(
          x = suelos(),
          names_req = ('Clase_Eros'),
          huso = huso,
          id = "suelos-sf_file"
        )
      }
    })

    observeEvent(input$group_by_dist,{
      output$distanceUI <- renderUI({
        if (input$group_by_dist) {
          shinyWidgets::numericInputIcon(
            inputId = ns("distance"),
            label = "Distancia",
            value = 50,
            step = 5,
            icon = icon("ruler-horizontal")
          )
        }
      })
    })
    distance <- reactive({
      req(input$group_by_dist)
      if (input$group_by_dist) {
        input$distance
      } else {
        NULL
      }
    })

    observeEvent(input$ord_rodales,{
      output$ord_rodales_UI <- renderUI({
        if (input$ord_rodales) {
          shinyWidgets::pickerInput(
            inputId = ns("orden_rodales"),
            label = "Ordenar de:",
            choices = c("NS-EO","NS-OE","SN-EO","SN-OE","EO-NS","EO-SN","OE-NS","OE-SN"),
            selected = "NS-OE"
          )
        }
      })
    })

    shinyjs::disable("get_area")
    observe({
      req(c(LB(), obras(), predios(), suelos()))
      shinyjs::enable("get_area")
    })

    areas_prop <- eventReactive(input$get_area,{
      req(LB(), obras(), predios(), suelos())
      get_pred_rod_area(
        PAS = PAS,
        LB = LB(),
        obras = obras(),
        predios = predios(),
        suelos = suelos(),
        group_by_LB = input$group_by_LB,
        sep_by_soil = input$sep_by_soil,
        group_by_dist = input$group_by_dist,
        distance_max = distance(),
        cut_by_prov = input$cut_by_prov,
        provincia = provincia,
        n_rodal_ord = input$ord_rodales,
        orden_rodal = input$orden_rodales,
        dec_sup = dec_sup
      )
    })

    observeEvent(input$get_area,{
      req(LB(), obras(), predios(), suelos())
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#35978F",
        text = tags$div(
          tags$br(),
          tags$p(
            "Generando capa de predios, rodales y áreas de corta.",
            tags$br(),
            " Puede ir a prepararse un café, esto tardará unos minutos"
          )
        )
      )
      req(areas_prop())
      gc(reset = T)
      shinybusy::remove_modal_spinner()
      shinybusy::notify_success(text = "Listo!", timeout = 3000, position = "right-bottom")
    })

    observe({
      req(areas_prop())
      if (any(areas_prop()$Rodales %>% dplyr::group_by(N_Rodal) %>% dplyr::summarise_at("Sup_ha", sum) %>% .$Sup_ha < 0.5) & PAS == 148) {
        shinybusy::report_warning(
          title = "OJO!. Rodales de bosque menores a 0,5 ha",
          text = paste0(
            "Los siguientes rodales de BN presentan una superficie inferior a 0,5 ha:\n",
            areas_prop()$Rodales %>%
              dplyr::group_by(N_Rodal) %>%
              dplyr::summarise_at("Sup_ha", sum) %>%
              dplyr::filter(Sup_ha < 0.5) %>%
              .$N_Rodal %>%
              shQuote() %>%
              paste0(collapse = ", ")
          )
        )
      }
      if (any(areas_prop()$Rodales %>% dplyr::group_by(N_Rodal) %>% dplyr::summarise_at("Sup_ha", sum) %>% .$N_Rodal < 1) & PAS == 151) {
        shinybusy::report_warning(
          title = "OJO!. Rodales de FX menores a 1 ha",
          text = paste0(
            "Los siguientes rodales presentan una superficie inferior a 1 ha:\n",
            areas_prop()$Rodales %>%
              dplyr::group_by(N_Rodal) %>%
              dplyr::summarise_at("Sup_ha", sum) %>%
              dplyr::filter(Sup_ha < 1) %>%
              .$N_Rodal %>%
              shQuote() %>%
              paste0(collapse = ", ")
          )
        )
      }
      if (nrow(areas_prop()$Rodales %>% dplyr::count(N_Rodal)) >
          nrow(areas_prop()$Rodales %>% dplyr::count(N_Rodal) %>% .[areas_prop()$Areas, ])) {
        shinybusy::report_warning(
          title = "Rodales sin áreas",
          text = paste0(
            "Los siguientes rodales sobran:\n",
            setdiff(
              areas_prop()$Rodales %>% dplyr::count(N_Rodal) %>% .$N_Rodal,
              areas_prop()$Rodales %>% dplyr::count(N_Rodal) %>% .[areas_prop()$Areas, ] %>% .$N_Rodal
            ) %>%
              shQuote() %>%
              paste(collapse = ", ")
          )
        )
      }
    })

    mod_downfiles_server(id = "down_areas", x = areas_prop(), name_save = c("Rodales_propuestos","Areas_propuestas","Predios_propuestos"))
  })
}

## To be copied in the UI
# mod_PredRodArea_ui("PredRodArea_1")

## To be copied in the server
# mod_PredRodArea_server("PredRodArea_1")
