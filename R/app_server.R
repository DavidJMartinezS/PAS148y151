#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 4000 * 1024 ^ 2, timeout = 600, shiny.sanitize.errors = TRUE)

  crs <- reactive({ifelse(input$huso == "18S", 32718, 32719)})

  flextable::set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  ) %>% suppressWarnings()

  # Outputs ----
  rv <- reactiveValues(
    crs = NULL,
    distance = NULL,
    orden_rodales = NULL,
    areas_prop = NULL,
    carto_digital = NULL,
    wb_planos = NULL,
    tabla_attr_rodal_0 = NULL,
    tabla_attr_rodal = NULL,
    apendices_2y3 = NULL,
    bd_flora_2 = NULL,
    obras_ap5 = NULL,
    apendice_5 = NULL
  )

  observeEvent(input$distance, {
    rv$distance <- input$distance
  })
  observeEvent(crs(), {
    rv$crs <- crs()
  })

  # AYUDAS ----
  ## Pred Rod Area ----
  LB <- mod_read_sf_server(id = "linea_base", crs = crs(), fx = function(x){
    x %>%
      dplyr::rename_all(~ ifelse(
        . == "geometry",
        .,
        stringi::stri_trans_totitle(
          stringi::stri_trans_general(., "Latin-ASCII"),
          type = "sentence"
        )
      )) %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("pid", strength = 1), ~ "PID") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^tipo.*for", case_insensitive = T), ~ "Tipo_fores") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^tipo.*veg", case_insensitive = T), ~ "Tipo_veg") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^sub.*tipo.*fo", case_insensitive = T), ~ "Subtipo_fo") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ley.*20283", case_insensitive = T), ~ "F_ley20283")
  })
  observeEvent(LB(),{
    check_input(
      x = LB(),
      names_req = c('Tipo_fores', 'Subtipo_fo', 'Tipo_veg', 'F_ley20283'),
      huso = input$huso,
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

  obras <- mod_read_sf_server(id = "obras", crs = crs())
  observeEvent(obras(),{
    check_input(
      x = obras(),
      names_req = NULL,
      huso = input$huso,
      id = "obras-sf_file"
    )
  })

  predios <- mod_read_sf_server(id = "predios", crs = crs(), fx = function(x){
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
      huso = input$huso,
      id = "predios-sf_file"
    )
  })

  suelos <- mod_read_sf_server(id = "suelos", crs = crs(), fx = function(x){
    x %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_Uso") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("desceros|cat_erosio|clase_eros", case_insensitive = T), ~ "Clase_Eros")
  })
  observeEvent(suelos(),{
    if(input$PAS == 148){
      check_input(
        x = suelos(),
        names_req = c('Clase_Uso'),
        huso = input$huso,
        id = "suelos-sf_file"
      )
    } else {
      check_input(
        x = suelos(),
        names_req = ('Clase_Eros'),
        huso = input$huso,
        id = "suelos-sf_file"
      )
    }
  })

  observeEvent(input$group_by_dist,{
    output$distanceUI <- renderUI({
      if (input$group_by_dist) {
        shinyWidgets::numericInputIcon(
          inputId = "distance",
          label = "Distancia",
          value = 50,
          step = 5,
          icon = icon("ruler-horizontal")
        )
      }
    })
  })

  observeEvent(input$ord_rodales,{
    output$ord_rodales_UI <- renderUI({
      if (input$ord_rodales) {
        shinyWidgets::pickerInput(
          inputId = "orden_rodales",
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

  observeEvent(input$get_area,{
    req(LB(), obras(), predios(), suelos())
    shinybusy::show_modal_spinner(
      spin = "flower",
      color = "#6FB58F",
      text = tags$div(
        tags$br(),
        tags$p(
          "Generando capa de predios, rodales y áreas de corta.",
          tags$br(),
          " Puede ir a prepararse un café, esto tardará unos minutos"
        )
      )
    )
    on.exit({
      shinybusy::remove_modal_spinner()
    }, add = TRUE)

    rv$areas_prop <- tryCatch({
      get_pred_rod_area(
        PAS = input$PAS,
        LB = LB(),
        obras = obras(),
        predios = predios(),
        suelos = suelos(),
        group_by_LB = input$group_by_LB,
        sep_by_soil = input$sep_by_soil,
        group_by_dist = input$group_by_dist,
        distance_max = rv$distance,
        cut_by_prov = input$cut_by_prov,
        provincia = input$provincia,
        n_rodal_ord = input$ord_rodales,
        orden_rodal = if(input$ord_rodales) input$orden_rodales else "NS-OE",
        dec_sup = input$n_dec
      )
    }, error = function(e) {
      print(e)
      shinyalert::shinyalert(
        title = "Error al generar la capas preliminares!",
        text = as.character(e$message),
        html = TRUE,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
      return(NULL)
    })

    if (!is.null(rv$areas_prop)) {
      shinybusy::notify_success(
        text = "¡Listo! Cartografía digital generada.",
        timeout = 3000, position = "right-bottom"
      )
    }
    
  })

  mod_downfiles_server(
    id = "down_areas",
    x = reactive(rv$areas_prop),
    name_save = list(capas_preliminares = c("Rodales_propuestos", "Areas_propuestas", "Predios_propuestos"))
  )
  
  observeEvent(rv$areas_prop, {
    req(rv$areas_prop)
    if (any(rv$areas_prop$Rodales %>% dplyr::group_by(N_Rodal) %>% dplyr::summarise_at("Sup_ha", sum) %>% .$Sup_ha < 0.5) & input$PAS == 148) {
      shinybusy::report_warning(
        title = "OJO!. Rodales de bosque menores a 0,5 ha",
        text = paste0(
          "Los siguientes rodales de BN presentan una superficie inferior a 0,5 ha:\n",
          rv$areas_prop$Rodales %>%
            dplyr::group_by(N_Rodal) %>%
            dplyr::summarise_at("Sup_ha", sum) %>%
            dplyr::filter(Sup_ha < 0.5) %>%
            .$N_Rodal %>%
            shQuote() %>%
            paste0(collapse = ", ")
        )
      )
    }
    if (any(rv$areas_prop$Rodales %>% dplyr::group_by(N_Rodal) %>% dplyr::summarise_at("Sup_ha", sum) %>% .$N_Rodal < 1) & input$PAS == 151) {
      shinybusy::report_warning(
        title = "OJO!. Rodales de FX menores a 1 ha",
        text = paste0(
          "Los siguientes rodales presentan una superficie inferior a 1 ha:\n",
          rv$areas_prop$Rodales %>%
            dplyr::group_by(N_Rodal) %>%
            dplyr::summarise_at("Sup_ha", sum) %>%
            dplyr::filter(Sup_ha < 1) %>%
            .$N_Rodal %>%
            shQuote() %>%
            paste0(collapse = ", ")
        )
      )
    }
    if (nrow(rv$areas_prop$Rodales %>% dplyr::count(N_Rodal)) >
        nrow(rv$areas_prop$Rodales %>% dplyr::count(N_Rodal) %>% .[rv$areas_prop$Areas, ])) {
      shinybusy::report_warning(
        title = "Rodales sin áreas",
        text = paste0(
          "Los siguientes rodales sobran:\n",
          setdiff(
            rv$areas_prop$Rodales %>% dplyr::count(N_Rodal) %>% .$N_Rodal,
            rv$areas_prop$Rodales %>% dplyr::count(N_Rodal) %>% .[rv$areas_prop$Areas, ] %>% .$N_Rodal
          ) %>%
            shQuote() %>%
            paste(collapse = ", ")
        )
      )
    }
  })

  ## Ordenar shp ----
  mod_st_order_server("st_order")

  ## Chequear carto ----
  mod_check_carto_server("check_carto")

  ## Agregar pend e hidro ----
  mod_add_attr_server("add_attr", PAS = input$PAS)

  ## Crear uso actual ----
  mod_uso_actual_server("uso_actual_1", crs = reactive(rv$crs), dec_sup = input$n_dec)

  # CARTO y APENDICES ----
  ## Cartografia digital ----
  ### Areas de corta  ----
  areas_def <- mod_read_sf_server(id = "areas_prelim", crs = crs(), fx = function(x){
    x %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_Uso") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("desceros|cat_erosio|clase_eros", case_insensitive = T), ~ "Clase_Eros") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("n_area", case_insensitive = T), ~ "N_Area") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("nom_predio", case_insensitive = T), ~ "Nom_Predio") %>%
      dplyr::mutate(Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(input$n_dec))
  })
  areas_def_ok <- eventReactive(areas_def(), {
    ok <- check_input(
      x = areas_def(),
      names_req = if(input$PAS == 148) c('Nom_Predio', 'N_Area', 'Clase_Uso') else c('Nom_Predio', 'N_Area', 'Clase_Eros'),
      huso = input$huso,
      id = "areas_prelim-sf_file"
    )
    if (ok) {
      check_n_area(areas_def())
      check_sup_ha(areas_def(), "N_Area")
    }
    return(ok)
  })

  ### Rodales ----
  rodales_def <- mod_read_sf_server(id = "rodales_prelim", crs = crs())
  rodales_def_ok <- eventReactive(rodales_def(), {
    ok <- check_input(
      x = rodales_def(),
      names_req = c('N_Predio', 'PID', 'N_Rodal', 'Tipo_For'),
      huso = input$huso,
      id = "rodales_prelim-sf_file"
    )
    if (ok) {
      check_n_rodal(rodales_def())
      check_sup_ha(rodales_def(), "N_Rodal")
    }
    return(ok)
  })

  observeEvent(c(areas_def_ok(), rodales_def_ok()), {
    req(isTRUE(c(areas_def_ok(), rodales_def_ok())))
    req(c(areas_def(), rodales_def()))
    if (
      any(
        rodales_def() %>%
        dplyr::group_by(N_Rodal) %>%
        dplyr::summarise_at("Sup_ha", sum) %>%
        .$Sup_ha < 0.5
      ) &
      input$PAS == 148
    ) {
      shinybusy::report_warning(
        title = "OJO!. Rodales de bosque menores a 0,5 ha",
        text = paste0(
          "Los siguientes rodales de BN presentan una superficie inferior a 0,5 ha:\n",
          rodales_def() %>%
            dplyr::group_by(N_Rodal) %>%
            dplyr::summarise_at("Sup_ha", sum) %>%
            dplyr::filter(Sup_ha < 0.5) %>%
            .$N_Rodal %>%
            shQuote() %>%
            paste0(collapse = ", ")
        ),
        shinybusy::config_report(
          titleFontSize = "24px",
          messageFontSize = "18px"
        )
      )
    }
    if (
      any(
        rodales_def() %>%
        dplyr::group_by(N_Rodal) %>%
        dplyr::summarise_at("Sup_ha", sum) %>%
        .$N_Rodal < 1) &
      input$PAS == 151
    ) {
      shinybusy::report_warning(
        title = "OJO!. Rodales de FX menores a 1 ha",
        text = paste0(
          "Los siguientes rodales presentan una superficie inferior a 1 ha:\n",
          rodales_def() %>%
            dplyr::group_by(N_Rodal) %>%
            dplyr::summarise_at("Sup_ha", sum) %>%
            dplyr::filter(Sup_ha < 1) %>%
            .$N_Rodal %>%
            shQuote() %>%
            paste0(collapse = ", ")
        ),
        shinybusy::config_report(
          titleFontSize = "24px",
          messageFontSize = "18px"
        )
      )
    }
    if (nrow(rodales_def() %>% dplyr::count(N_Rodal)) >
        nrow(rodales_def()[areas_def(), ] %>% dplyr::count(N_Rodal))) {
      shinybusy::report_warning(
        title = "Rodales sin áreas",
        text = paste0(
          "Los siguientes rodales sobran:\n",
          setdiff(
            rodales_def() %>% dplyr::count(N_Rodal) %>% .$N_Rodal,
            rodales_def()[areas_def(),] %>% dplyr::count(N_Rodal) %>% .$N_Rodal
          ) %>%
            shQuote() %>%
            paste(collapse = ", ")
        ),
        shinybusy::config_report(
          titleFontSize = "24px",
          messageFontSize = "18px"
        )
      )
    }
  })

  ### Predios ----
  predios_def <- mod_read_sf_server(id = "predios_prelim", crs = crs())
  observeEvent(predios_def(),{
    ok <- check_input(
      x = predios_def(),
      names_req = c('N_Predio', 'Nom_Predio', 'Rol', 'Propietari'),
      huso = input$huso,
      id = "predios_prelim-sf_file"
    )
    if (ok) {
      check_n_predio(predios_def())
      check_sup_ha(rodales_def(), "N_Predio")
    }
    return(ok)
  })

  ### Caminos ----
  observeEvent(input$add_cam,{
    output$add_cam_ui <- renderUI({
      if(input$add_cam){
        tags$div(
          tags$div(
            id = "flex",
            tags$div(
              id = "inline",
              shinyWidgets::pickerInput(
                inputId = "cut_cam",
                label = "Corte",
                choices = c("clip", "buffer", "crop", "crop_by_row"),
                selected = "clip"
              )
            ),
            tags$div(style = "margin-left: 25px"),
            tags$div(
              id = "inline",
              numericInput(
                inputId = "buffer_cam",
                label = "Buffer",
                value = 0,
                step = 10,
                width = "80px"
              )
            ),
            tags$div(tags$b("m"), style = "margin-top: 10px; margin-left: 15px"),
            style = "margin-top: -5px; margin-bottom: 5px"
          ),
          tags$p(
            "Caminos serán creados a partir de la red vial del MOP actualizado al 07-02-2024 (descargar ",
            tags$a("aqui", .noWS = "outside", href = "https://mapas.mop.gov.cl/red-vial/Red_Vial_Chile.zip"),
            ") ¿Desea crear otra capa de caminos a partir de información de Google?"
          ),
          shinyWidgets::switchInput(
            inputId = "add_cam_osm",
            size = "mini",
            onLabel = "Si",
            offLabel = "No",
            onStatus = "success"
          )
        )
      }
    })
  })

  ### Hidrografia ----
  observeEvent(input$add_hidro,{
    output$add_hidro_ui <- renderUI({
      if(input$add_hidro){
        tags$div(
          tags$div(
            id = "flex",
            tags$div(
              id = "inline",
              shinyWidgets::pickerInput(
                inputId = "fuente_hidro",
                label = "Fuente",
                choices = c("MOP", "BCN"),
                selected = "MOP"
              )
            ),
            tags$div(style = "margin-left: 25px"),
            tags$div(
              id = "inline",
              shinyWidgets::pickerInput(
                inputId = "cut_hidro",
                label = "Corte",
                choices = c("clip", "buffer", "crop", "crop_by_row"),
                selected = "clip"
              )
            ),
            tags$div(style = "margin-left: 25px"),
            tags$div(
              id = "inline",
              numericInput(
                inputId = "buffer_hidro",
                label = "Buffer",
                value = 0,
                step = 10,
                width = "80px"
              )
            ),
            tags$div(tags$b("m"), style = "margin-top: 10px; margin-left: 15px"),
            style = "margin-top: -5px; margin-bottom: 5px"
          ),
          tags$p(
            "Hidrografía será creada a partir de la hidrografía subida a Geoportal actualizada al 31-12-2022 (link ",
            tags$a("aquí", .noWS = "outside", href = "https://www.geoportal.cl/geoportal/catalog/36436/Hidrograf%C3%ADa%20de%20la%20regi%C3%B3n%20de%20Arica%20a%20la%20regi%C3%B3n%20de%20Los%20Lagos"),
            ") ¿Desea crear capa hidrografíca a partir de información de Google?"
          ),
          shinyWidgets::switchInput(
            inputId = "add_hidro_osm",
            size = "mini",
            onLabel = "Si",
            offLabel = "No",
            onStatus = "success"
          )
        )
      }
    })
  })

  ### Curvas de nivel ----
  observeEvent(input$add_CN,{
    output$add_CN_ui <- renderUI({
      if (input$add_CN) {
        tags$div(
          id = "flex",
          tags$div(
            id = "inline",
            shinyWidgets::pickerInput(
              inputId = "cut_cn",
              label = "Corte",
              choices = c("clip", "buffer", "crop", "crop_by_row"),
              selected = "clip"
            )
          ),
          tags$div(style = "margin-left: 25px"),
          tags$div(
            id = "inline",
            numericInput(
              inputId = "buffer_cn",
              label = "Buffer",
              value = 0,
              step = 10,
              width = "100px"
            )
          ),
          tags$div(tags$b("m"), style = "margin-top: 10px; margin-left: 15px"),
          tags$div(style = "margin-left: 25px"),
          tags$div(
            id = "inline",
            numericInput(
              inputId = "step",
              label = "Intérvalo: ",
              value = 10,
              min = 10,
              max = 500,
              step = 10
            )
          ),
          tags$div(tags$b("m"), style = "margin-top: 10px; margin-left: 15px")
        )
      }
    })
  })

  ### BD Flora ----
  bd_flora <- reactive({
    if (isTruthy(input$bd_flora$datapath)) {
      req(c(rodales_def()))
      bd <- tryCatch({
        prepare_bd_flora(
          BD = input$bd_flora$datapath,
          rodales = rodales_def(),
          PAS = input$PAS,
          cut_by_rod = input$cut_bd_by_rodal,
          include_fp = input$include_fp
        )
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error con la BD de flora!",
          text = e$message,
          html = F,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })
      return(bd)
    } else {
      return(NULL)
    }
  })

  shinyjs::disable("add_parcelas")
  shinyjs::disable("check_bd_flora")
  observe({
    req(bd_flora())
    shinyjs::enable("add_parcelas")
    shinyjs::enable("check_bd_flora")
  })

  # observeEvent(bd_flora(), {
    mod_downfiles_server(
      id = "down_bd_flora",
      x = reactive(bd_flora()),
      name_save = c("BD_Flora")
    )
  # })

  observeEvent(input$check_bd_flora,{
    req(bd_flora())
    tryCatch({
      check_bd_flora(x = bd_flora())
    }, error = function(e) {
      shinyalert::shinyalert(
        title = "Problemas al realizar el chequeo!",
        text = e$message,
        html = F,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
    })


    df_ecc <- reactive({
      bd_flora() %>%
        {if (input$PAS == 148) {
          .[] %>% dplyr::filter(
            Habito %>% stringi::stri_trans_general("Latin-ASCII") %>% stringi::stri_detect_regex("arbol", case_insensitive = T),
            RCE %>% stringi::stri_trans_toupper() %not_in% c(NA_character_, "---")
          )
        } else {
          .[] %>% dplyr::filter(
            DS_68 %>% stringi::stri_cmp_equiv("originaria", strength = 1),
            RCE %>% stringi::stri_trans_toupper() %in% c("VU", "EN", "CR")
          )
        }}
    })

    if (df_ecc() %>% nrow() >= 1) {
      shinyalert::shinyalert(
        title = "ECC en parcelas",
        text = tags$p(
          "Ojo con las siguientes especies y parcelas:",
          tags$br(),
          df_ecc() %>%
            dplyr::group_by(Especie, RCE) %>%
            split(.$Especie) %>%
            purrr::map(function(x){
              x %>%
                {if(nrow(.) > 10) {
                  dplyr::sample_n(., 10) %>%
                    dplyr::summarise(Parcelas = paste0(Parcela, collapse = ", ")) %>%
                    dplyr::mutate(Parcelas = purrr::map_chr(Parcelas, ~paste0(.x, ", etc...)")))
                } else {
                  dplyr::summarise(., Parcelas = paste0(Parcela, collapse = ", "))
                }}
            }) %>%
            dplyr::bind_rows() %>%
            kableExtra::kbl() %>%
            kableExtra::kable_styling() %>%
            shiny::HTML()
        ),
        html = TRUE,
        type = ifelse(input$PAS == 148, "error", "warning"),
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
    }
  })

  ### Uso actual ----
  shinyjs::disable("add_uso_actual")
  observe({
    req(predios_def())
    shinyjs::enable("add_uso_actual")
  })

  observeEvent(input$add_uso_actual,{
    output$add_uso_actual_ui <- renderUI({
      if(input$add_uso_actual){
        tags$div(
          mod_read_sf_ui("catastro", "Ingrese capa del catastro de CONAF") %>%
            add_help_text(title = "Campos minimos requeridos:\n'USO', 'SUBUSO', 'ESTRUCTURA'"),
          tags$div(style = "margin-top: -10px"),
          mod_read_sf_ui("suelos_uso_act", "Ingrese capa de suelos del CIREN") %>%
            add_help_text(title = "Campos minimos requeridos:\n'TEXTCAUS'")
        )
      }
    })
  })

  catastro <- mod_read_sf_server(
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
    wkt_filter = if (isTruthy(predios_def())) {
      sf::st_as_text(sf::st_geometry(sf::st_union(predios_def())))
    } else {
      character(0)
    }
  )

  observeEvent(catastro(),{
    req(isTruthy(catastro()))
    check_input(
      x = catastro(),
      names_req = c('USO', 'SUBUSO', 'ESTRUCTURA'),
      id = "catastro-sf_file"
    )
  })

  suelos_uso_act <- mod_read_sf_server(
    id = "suelos_uso_act",
    crs = crs(),
    fx = function(x){
      x %>%
        dplyr::rename_if(
          names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T),
          ~ "Clase_Uso"
        )
    },
    wkt_filter = if (isTruthy(predios_def())) {
      sf::st_as_text(sf::st_geometry(sf::st_union(predios_def())))
    } else {
      character(0)
    }
  )

  observeEvent(suelos_uso_act(),{
    req(isTruthy(suelos_uso_act()))
    check_input(
      x = suelos_uso_act(),
      names_req = c('Clase_Uso'),
      id = "suelos_uso_act-sf_file"
    )
  })

  output$catas <- renderPrint({catastro()})
  output$CUS <- renderPrint({suelos_uso_act()})

  ### Generar y descargar carto ----
  shinyjs::disable("get_carto_btn")
  observe({
    req(c(areas_def(), rodales_def(), predios_def()))
    req(!is.null(input$dem))
    shinyjs::enable("get_carto_btn")
  })

  observeEvent(input$get_carto_btn,{
    shinybusy::show_modal_spinner(
      spin = "flower",
      color = "#6FB58F",
      text = tags$div(
        tags$br(),
        tags$p(
          "Generando cartografía digital.",
          tags$br(),
          " Puede ir a prepararse un café, esto tardarará unos minutos"
        )
      )
    )
    on.exit({
      shinybusy::remove_modal_spinner()
    }, add = TRUE)

    rv$carto_digital <- tryCatch({
      get_carto_digital(
        PAS = input$PAS,
        areas = areas_def(),
        rodales = rodales_def(),
        predios = predios_def(),
        cut_by_prov = if (input$cut_pred_by_prov) input$provincia else NULL,
        dem = input$dem$datapath,
        add_parcelas = input$add_parcelas,
        bd_flora = bd_flora(),
        cut_by_rod = input$cut_bd_by_rodal,
        include_fp = input$include_fp,
        from_RCA = F,
        RCA = NULL,
        add_uso_actual = input$add_uso_actual,
        catastro = if(input$add_uso_actual) catastro() else NULL,
        suelos = if(input$add_uso_actual) suelos_uso_act() else NULL,
        add_caminos = input$add_cam,
        add_caminos_osm = if(input$add_cam == F) F else input$add_cam_osm,
        caminos_arg = if(input$add_cam == F) list(cut = "clip", buffer = 0) else list(cut = input$cut_cam, buffer = input$buffer_cam),
        add_hidro = input$add_hidro,
        fuente_hidro = input$fuente_hidro,
        add_hidro_osm = if(input$add_hidro == F) F else input$add_hidro_osm,
        hidro_arg = if(input$add_hidro == F) list(cut = "clip", buffer = 0) else list(cut = input$cut_hidro, buffer = input$buffer_hidro),
        add_curv_niv = input$add_CN,
        curv_niv_arg = if(input$add_CN == F) list(cut = "clip", buffer = 0) else list(cut = input$cut_cn, buffer = input$buffer_cn),
        step = if(input$add_CN == F) 10 else input$step,
        dec_sup = input$n_dec
      )
    }, error = function(e) {
      shinyalert::shinyalert(
        title = "Error al generar la cartografía digitial!",
        text = as.character(e$message),
        html = TRUE,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
      return(NULL)
    })

    req(rv$carto_digital)
    var_suelo <- if (input$PAS == 148) dplyr::sym("Clase_Uso") else dplyr::sym("Clase_Eros")
    nom_suelo <- if (input$PAS == 148) "Clase Uso Suelo" else "Grado de Erosión"

    ft_planos_areas <- rv$carto_digital$tabla_areas %>%
      dplyr::select(N_Predio, N_Area, Ran_Pend, !!var_suelo, Sup_ha) %>%
      `names<-`(c("N° Predio", "Área N°", "Rango Pendiente (%)", nom_suelo, "Superficie área de corta (ha)")) %>%
      flextable::flextable() %>%
      flextable::merge_v(j = 1) %>%
      flextable::autofit() %>%
      flextable::theme_box() %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center")

    ft_planos_predios <- rv$carto_digital$tabla_predios %>%
      mutate(Pto_ref = NA_character_, Este = as.integer(NA), Norte = as.integer(NA)) %>%
      dplyr::mutate(Pto_ref = NA_character_, Este = as.integer(NA), Norte = as.integer(NA)) %>%
      dplyr::select(N_Predio, Nom_Predio, Propietari, Rol, Comuna, Sup_ha, Pto_ref, Este, Norte) %>%
      dplyr::mutate_at(vars(Comuna), ~purrr::map_vec(., stringi::stri_split_regex, " - ")) %>%
      tidyr::unnest(Comuna) %>%
      dplyr::select(-c(dplyr::matches("provincia"), dplyr::matches("region"))) %>%
      dplyr::left_join(
        sf::read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>%
          sf::st_drop_geometry() %>%
          dplyr::select(COMUNA, PROVINCIA, REGION) %>%
          dplyr::rename_all(stringi::stri_trans_totitle)
      ) %>%
      dplyr::relocate(c(Provincia, Region), .after = Comuna) %>%
      `names<-`(
        c("N° Predio", "Nombre Predio", "Nombre del propietario/a", "ROL", "Comuna", "Provincia",
          "Región", "Superficie predial (ha)", "Punto de referencia", "Este", "Norte")
      ) %>%
      flextable::flextable() %>%
      flextable::merge_v(j = 1, target = c(1:4, 8)) %>%
      flextable::merge_v(j = c(6, 7)) %>%
      flextable::autofit() %>%
      flextable::theme_box() %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center")

    wb <- openxlsx2::wb_workbook() %>%
      openxlsx2::wb_add_worksheet("Areas") %>%
      flexlsx::wb_add_flextable(sheet = "Areas", ft = ft_planos_areas, start_col = 1, start_row = 1) %>%
      openxlsx2::wb_add_worksheet("Predios") %>%
      flexlsx::wb_add_flextable(sheet = "Predios", ft = ft_planos_predios, start_col = 1, start_row = 1)

    rv$wb_planos <- wb

    if (!is.null(rv$carto_digital)) {
      shinybusy::report_success(
        title = "Listo!",
        text = "Se ha generado la cartografía digital y las tablas para los planos",
        button = "Ok"
      )
    }
  })

  mod_downfiles_server(
    id = "down_carto",
    x = reactive(rv$carto_digital),
    name_save = list(Cartografia_digital = c(
      "Area",
      "Rodales",
      "Limite_Predial",
      "Suelos",
      "Rangos_pend",
      "Tabla_predios",
      "Tabla_areas",
      "Parcela",
      "Uso_actual",
      "Caminos",
      "Caminos_osm",
      "Hidrografia",
      "Hidrografia_osm",
      "Curvas_niv"
    ) %>%
      paste(.,input$NOMPREDIO, sep = "_") %>%
      subset(
        c(rep(T, 7),
          input$add_parcelas,
          input$add_uso_actual,
          input$add_cam,
          if(input$add_cam == F) F else input$add_cam_osm,
          input$add_hidro,
          if(input$add_hidro == F) F else input$add_hidro_osm,
          input$add_CN
        )
      ))
  )

  mod_downfiles_server(
    id = "down_tbl_planos",
    x = reactive(rv$wb_planos),
    name_save = c("Tablas_planos")
  )

  # Apendices ----
  observeEvent(input$portada, {
    output$portada_opts_ui <- renderUI({
      if (input$portada == "otra") {
        tags$div(
          id = "flex",
          tags$div(
            shinyWidgets::pickerInput(
              inputId = "tipo_proj",
              label = "Tipo de proyecto",
              choices = c("EIA", "DIA"),
              selected = "EIA",
              width = "120px"
            )
          ),
          tags$div(style = "margin-left: 20px"),
          tags$div(
            textInput(
              inputId = "nom_proj",
              label = "Nombre de proyecto",
              width = "400px",
              placeholder = "Ingrese nombre del proyecto"
            )
          ),
          tags$div(style = "margin-left: 20px"),
          tags$div(
            fileInput(
              inputId = "logo",
              label = "Logo del cliente",
              multiple = FALSE,
              width = "420px",
              accept = "image/*"
            )
          ),
          style = "margin-top: 15px;margin-bottom: -20"
        )
      }
    })
  })

  ## Apendice 2 Y 3 ----
  observeEvent(input$add_bd_pcob,{
    output$add_bd_pcob_ui <- renderUI({
      if(input$add_bd_pcob){
        tags$div(
          fileInput(
            inputId = "bd_pcob",
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
          tags$div(style = "margin-top: -10px")
        )
      }
    })
  })

  bd_pcob <- reactive({
    if (input$add_bd_pcob) {
      req(input$bd_pcob$datapath)
      bd <- tryCatch({
        prepare_bd_pcob(BD = input$bd_pcob$datapath)
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error con la BD de parcelas de cobertura!",
          text = e$message,
          html = F,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })
      return(bd)
    } else {
      return(NULL)
    }
  })

  observeEvent(bd_pcob(), {
    req(rodales_def())
    tryCatch({
      check_bd_pcob(x = bd_pcob(), mask = rodales_def())
    }, error = function(e) {
      shinyalert::shinyalert(
        title = "Chequeo Parcelas de cobertura!",
        text = e$message,
        html = F,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
    })
  })

  observeEvent(input$PAS,{
    output$add_bd_trans_ui_lgl <- renderUI({
      if(input$PAS == 151){
        tags$div(
          shinyWidgets::materialSwitch(
            inputId = "add_bd_trans",
            label = "¿Desea incluir transectos?",
            status = "success"
          ),
        uiOutput("add_bd_trans_ui")
        )
      }
    })
  })

  observeEvent(input$add_bd_trans,{
    output$add_bd_trans_ui <- renderUI({
      if(input$add_bd_trans){
        tags$div(
          fileInput(
            inputId = "bd_trans",
            label = "Ingresar BD de transectos",
            multiple = F,
            accept = c(".xlsx"),
            buttonLabel = "Seleccionar",
            placeholder = "Archivo no seleccionado"
          ) %>%
            add_help_text(
              title = "Campos minimos requeridos:\n
              'Campaña', 'Parcela', 'Habito', 'Especie', 'Cuenta', 'Denominador', 'Cobertura'"
            ),
          tags$div(style = "margin-top: -10px")
        )
      }
    })
  })

  bd_trans <- reactive({
    if (isTruthy(input$bd_trans$datapath)) {
      req(input$bd_trans$datapath)
      bd <- tryCatch({
        prepare_bd_trans(BD = input$bd_trans$datapath)
      }, error = function(e) {
        shinyalert::shinyalert(
          title = "Error con la BD de transectos!",
          text = e,
          html = F,
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          confirmButtonCol = "#6FB58F",
          animation = T
        )
        return(NULL)
      })
      return(bd)
    } else {
      NULL
    }
  })

  observeEvent(bd_trans(),{
    req(rodales_def())
    tryCatch({
      check_bd_trans(x = bd_trans(), mask = rodales_def())
    }, error = function(e) {
      shinyalert::shinyalert(
        title = "Chequeo Parcelas de cobertura!",
        text = e,
        html = F,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
    })
  })

  shinyjs::disable("get_apendices_2y3_btn")
  observe({
    req(c(rodales_def(), predios_def()))
    req(bd_flora())
    shinyjs::enable("get_apendices_2y3_btn")
  })

  observeEvent(c(input$add_bd_pcob, input$add_bd_trans),{
    output$join_by_ui <- renderUI({
      if(any(isTruthy(c(input$add_bd_pcob, input$add_bd_trans)))){
        tags$div(
          id = "inline",
          shinyWidgets::pickerInput(
            inputId = "join_by",
            label = "Unir datos por:",
            choices = c("Parcelas" = "parcela", "Coordenadas" = "coordenadas"),
            selected = "Coordenadas"
          ),
          style = "margin-bottom: 15px"
        )
      }
    })
  })

  observeEvent(input$get_apendices_2y3_btn,{
    req(bd_flora(), rodales_def(), predios_def())
    shinybusy::show_modal_spinner(
      spin = "flower",
      color = "#6FB58F",
      text = tags$div(
        tags$br(),
        tags$p(
          "Generando apéndices 2 y 3.",
          tags$br(),
          "Por favor espere, esto puede tardar unos minutos"
        )
      )
    )
    on.exit({
      shinybusy::remove_modal_spinner()
    }, add = TRUE)

    rv$apendices_2y3 <- tryCatch({
      apendice_2_3(
        PAS = input$PAS,
        bd_flora = bd_flora(),
        bd_pcob = bd_pcob(),
        bd_trans = bd_trans(),
        join_by = input$join_by,
        predios = predios_def(),
        cov_as_range = if(input$PAS == 148) F else input$cov_as_range,
        cov_fp = input$cov_fp,
        provincia = input$provincia,
        portada = input$portada,
        portada_opts = portada_opts(
          tipo_proj = input$tipo_proj,
          nom_proj = input$nom_proj,
          logo = input$logo$datapath
        )
      )
    }, error = function(e) {
      print(e)
      shinyalert::shinyalert(
        title = "Error al generar los apéndices 2 y 3!",
        text = as.character(e),
        html = TRUE,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
      return(NULL)
    })

    if (!is.null(rv$apendices_2y3)) {
      shinybusy::notify_success(
        text = "¡Listo! Apéndices 2 y 3 generados.",
        timeout = 3000, position = "right-bottom"
      )
    }
  })
  mod_downfiles_server(
    id = "down_apendices_2",
    x = reactive(rv$apendices_2y3[[1]]),
    name_save = c("Apéndice 2. Densidad de especies")
  )
  mod_downfiles_server(
    id = "down_apendices_3",
    x = reactive(rv$apendices_2y3[[2]]),
    name_save = c("Apéndice 3. Coordenadas ubicación de parcelas")
  )

  ## Atributos de rodal ----
  shinyjs::disable("get_tabla_attr_rod")
  observe({
    req(predios_def())
    req(bd_flora())
    shinyjs::enable("get_tabla_attr_rod")
  })
  observeEvent(input$get_tabla_attr_rod, {
    shinybusy::show_modal_spinner(
      spin = "flower",
      color = "#6FB58F",
      text = tags$div(
        tags$br(),
        tags$p(
          "Generando tabla de atributación de rodales.",
          tags$br(),
          "Espere unos segundos..."
        )
      )
    )
    on.exit({
      shinybusy::remove_modal_spinner()
    }, add = TRUE)

    rv$tabla_attr_rodal_0 <- tryCatch({
      get_tabla_attr_rodal(
        PAS = input$PAS,
        bd_flora = bd_flora(),
        rodales = rodales_def(),
        umbral_sp_est = input$umbral_sp_est
      )
    }, error = function(e) {
      shinyalert::shinyalert(
        title = "Error al generar la atributación de rodales!",
        text = as.character(e$message),
        html = TRUE,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
      return(NULL)
    })

    if (!is.null(rv$tabla_attr_rodal_0)) {
      shinybusy::notify_success(
        text = "¡Listo! Cartografía digital generada.",
        timeout = 3000, position = "right-bottom"
      )
    }
  })

  mod_downfiles_server(
    id = "tabla_attr_rodal_0",
    x = reactive(rv$tabla_attr_rodal_0),
    name_save = c("Tabla atributacion de rodales")
  )

  observeEvent(input$tabla_attr_rodal, {
    req(input$tabla_attr_rodal)
    rv$tabla_attr_rodal <- openxlsx2::read_xlsx(input$tabla_attr_rodal$datapath)
  })
   observeEvent(rv$tabla_attr_rodal, {
     if(input$PAS == 148) {
       check_input(
         x = rv$tabla_attr_rodal,
         names_req = c("N_Rodal", "Tipo_fores", "Subtipo_fo", "Tipo_veg", "Tipo_attr", "Nom_attr"),
         id = "tabla_attr_rodal"
       )
     } else {
       check_input(
         x = rv$tabla_attr_rodal,
         names_req = c("N_Rodal", "Tipo_veg", "Tipo_attr", "Nom_attr"),
         id = "tabla_attr_rodal"
       )
     }
   })

  observeEvent(rv$tabla_attr_rodal, {
    if(
      rv$tabla_attr_rodal$Tipo_attr %>% 
      unique() %>% 
      stringi::stri_detect_regex("linea.*base", case_insensitive = T) %>% 
      any()
    ) {
      shinyalert::shinyalert(
        title = "Ojo",
        text = "Se requiere ingresar parcelas de línea de base",
        html = F,
        type = "warning",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
    }
  })

  ## Apendice 5 ----
  shinyjs::disable("get_apendice_5_btn")
  observe({
    req(c(rodales_def(), bd_flora(), rv$tabla_attr_rodal))
    req(rv$carto_digital)
    req(rv$carto_digital$tabla_predios, rv$carto_digital$tabla_areas)
    shinyjs::enable("get_apendice_5_btn")
  })

  observeEvent(input$PAS, {
    output$form_nuevo_ui <- renderUI({
      if (input$PAS == 151) {
        shinyWidgets::prettyToggle(
          inputId = "form_nuevo",
          label_on = "Formulario nuevo",
          label_off = "Formulario antiguo",
          status_on = "success",
          status_off = NULL,
          value = TRUE,
          animation = "jelly"
        )
      }
    })
  })

  ### BD flora 2
  observeEvent(input$add_bd_flora_2, {
    output$add_bd_flora_2_ui <- renderUI({
      if (input$add_bd_flora_2) {
        tags$div(
          tags$div(style = "margin-top: -10px"),
          fileInput(
            inputId = "bd_flora_2",
            label = "Base de datos de flora para atribuar con parcelas de LB.",
            multiple = F,
            accept = c(".xlsx"),
            buttonLabel = "Seleccionar",
            placeholder = "Archivo no seleccionado"
          ) %>%
            add_help_text(
              title = "Datos para atributar areas de corta sin parcelas directa o cercana.\n
            Campos minimos requeridos:\n'Parcela', 'Especie', 'Cob_BB', 'Nha', 'Habito'"
            ),
          tags$div(style = "margin-bottom: -10px")
        )
      }
    })
  })

  observeEvent(input$bd_flora_2, {
    req(input$bd_flora_2)
    rv$bd_flora_2 <- openxlsx2::read_xlsx(input$bd_flora_2$datapath)
  })
  observeEvent(rv$bd_flora_2,{
    check_input(
      x = rv$bd_flora_2,
      names_req = c('Parcela', 'Especie', 'Cob_BB', 'Nha', 'Habito'),
      id = "bd_flora_2"
    )
  })

  ### add obras
  obras_ap5 <- mod_read_sf_server(id = "obras_ap5", crs = crs(), fx = function(x){
  x %>%
    dplyr::rename_all(~ ifelse(
      . == "geometry", .,
      stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        type = "sentence"
      )
    ))
  })
  observeEvent(obras_ap5(),{
    req(obras_ap5())
    rv$obras_ap5 <- obras_ap5()
    if (isTruthy(input$form_nuevo)) {
      check_input(
        x = rv$obras_ap5,
        names_req = c('Tipo', 'Tipo_obra', 'Nom_obra', 'Fase'),
        huso = input$huso,
        id_reset = "obras_ap5-sf_file"
      )
    } else {
      check_input(
        x = rv$obras_ap5,
        names_req = c('Tipo', 'Obra'),
        huso = input$huso,
        id_reset = "obras_ap5-sf_file"
      )
    }
  })

  observeEvent(input$get_apendice_5_btn, {
    req(c(rodales_def(), bd_flora(), rv$tabla_attr_rodal))
    req(rv$carto_digital)
    req(rv$carto_digital$tabla_predios, rv$carto_digital$tabla_areas)
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
    on.exit({
      shinybusy::remove_modal_spinner()
    }, add = TRUE)

    rv$apendice_5 <- tryCatch({
      if (input$PAS == 148) {
        apendice_5_PAS148(
          bd_flora = bd_flora(),
          rodales = rodales_def(),
          tabla_predios = rv$carto_digital$tabla_predios,
          tabla_areas = rv$carto_digital$tabla_areas,
          tabla_attr_rodal = rv$tabla_attr_rodal,
          umbral_sp_est = input$umbral_sp_est,
          bd_flora_2 = rv$bd_flora_2,
          portada = input$portada,
          provincia = input$provincia,
          portada_opts = portada_opts(
            tipo_proj = input$tipo_proj,
            nom_proj = input$nom_proj,
            logo = input$logo$datapath
          ),
          carto_uso_actual = if(isTruthy(rv$carto_digital$Uso_actual)) rv$carto_digital$Uso_actual else NULL,
          areas = if(isTruthy(areas_def())) areas_def() else NULL,
          obras = rv$obras_ap5
        )
      } else {
        do.call(
          if (isTruthy(input$form_nuevo)) apendice_5_PAS151_nuevo else apendice_5_PAS151,
          list(
            bd_flora = bd_flora(),
            rodales = rodales_def(),
            tabla_predios = rv$carto_digital$tabla_predios,
            tabla_areas = rv$carto_digital$tabla_areas,
            tabla_attr_rodal = rv$tabla_attr_rodal,
            cov_as_range = input$cov_as_range,
            cov_fp = input$cov_fp,
            umbral_sp_est = input$umbral_sp_est,
            bd_flora_2 = rv$bd_flora_2,
            portada = input$portada,
            portada_opts = portada_opts(
              tipo_proj = input$tipo_proj,
              nom_proj = input$nom_proj,
              logo = input$logo$datapath
            ),
            provincia = input$provincia,
            areas = if(isTruthy(areas_def())) areas_def() else NULL,
          obras = rv$obras_ap5
          )
        )
      }
    }, error = function(e) {
      print(e)
      shinyalert::shinyalert(
        title = "Error al generar el apéndice 5!",
        text = as.character(e),
        html = TRUE,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
      return(NULL)
    })

    if (!is.null(rv$apendice_5)) {
      shinybusy::notify_success(
        text = "¡Listo! Apéndice 5 generado.",
        timeout = 3000, position = "right-bottom"
      )
    }

  })
  mod_downfiles_server(
    id = "down_apendice_5",
    x = reactive(rv$apendice_5),
    name_save = c("Apéndice 5. Tablas formulario CONAF")
  )


  # Autor ----
  output$user <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = "David Martínez",
      image = "https://avatars.githubusercontent.com/u/74486319?s=400&u=c277213b232af5e7710bebdc7a50bb9426ab9a62&v=4",
      title = "Dashboard PAS 148/151",
      subtitle = "Autor",
      footer = fluidRow(
        tags$p(
          shinydashboardPlus::socialButton(href = "https://github.com/DavidJMartinezS", icon = icon("github")) %>% bsplus::bs_embed_tooltip("Mi github"),
          shinydashboardPlus::socialButton(href = "https://geobiota.com/", icon = icon("globe")) %>% bsplus::bs_embed_tooltip("Geobiota"),
          class = "text-center"
        ),
      ),
      "Especialista en plantas de Geobiota"
    )
  })
}
