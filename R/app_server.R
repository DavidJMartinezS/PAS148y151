#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactive reactiveValues renderTable fluidRow tags
#' @importFrom shinydashboardPlus renderUser dashboardUser socialButton
#' @importFrom bsplus bs_embed_tooltip
#'
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 4000 * 1024 ^ 2, timeout = 600)

  crs <- reactive({ifelse(input$huso == "18S", 32718, 32719)})

  # AYUDAS ----
  ## Pred Rod Area ----
  LB <- mod_leer_sf_server(id = "linea_base", crs = crs(), fx = function(x){
    x %>%
      dplyr::rename_all(~ ifelse(
        . == "geometry",
        .,
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

  obras <- mod_leer_sf_server(id = "obras", crs = crs())
  observeEvent(obras(),{
    check_input(
      x = obras(),
      names_req = NULL,
      huso = input$huso,
      id = "obras-sf_file"
    )
  })

  predios <- mod_leer_sf_server(id = "predios", crs = crs(), fx = function(x){
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

  suelos <- mod_leer_sf_server(id = "suelos", crs = crs(), fx = function(x){
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

  areas_prop <- eventReactive(input$get_area,{
    req(LB(), obras(), predios(), suelos())
    get_pred_rod_area(
      PAS = input$PAS,
      LB = LB(),
      obras = obras(),
      predios = predios(),
      suelos = suelos(),
      group_by_LB = input$group_by_LB,
      sep_by_soil = input$sep_by_soil,
      group_by_dist = input$group_by_dist,
      distance_max = input$distance,
      cut_by_prov = input$cut_by_prov,
      provincia = input$provincia,
      n_rodal_ord = input$ord_rodales,
      orden_rodal = input$orden_rodales,
      dec_sup = input$n_dec
    )
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
    req(areas_prop())
    gc(reset = T)
    shinybusy::remove_modal_spinner()
    shinybusy::notify_success(text = "Listo!", timeout = 3000, position = "right-bottom")
  })

  observeEvent(areas_prop(),{
    req(areas_prop())

    if (any(areas_prop()$Rodales %>% dplyr::group_by(N_Rodal) %>% dplyr::summarise_at("Sup_ha", sum) %>% .$Sup_ha < 0.5) & input$PAS == 148) {
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
    if (any(areas_prop()$Rodales %>% dplyr::group_by(N_Rodal) %>% dplyr::summarise_at("Sup_ha", sum) %>% .$N_Rodal < 1) & input$PAS == 151) {
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
    mod_downfiles_server(id = "down_areas", x = areas_prop(), name_save = c("Rodales_propuestos", "Areas_propuestas", "Predios_propuestos"))
  })


  ## Ordenar shp ----
  mod_st_order_server("st_order")

  ## Chequear carto ----
  mod_check_carto_server("check_carto")

  ## Agregar pend e hidro ----
  mod_add_attr_server("add_attr", PAS = input$PAS)

  # CARTO y APENDICES ----
  ## Cartografia digital ----
  ### Areas de corta  ----
  areas_def <- mod_leer_sf_server(id = "cart_area", crs = crs(), fx = function(x){
    x %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_Uso") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("desceros|cat_erosio|clase_eros", case_insensitive = T), ~ "Clase_Eros") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("n_area", case_insensitive = T), ~ "N_Area") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("nom_predio", case_insensitive = T), ~ "Nom_Predio") %>%
      dplyr::mutate(Tipo_Bos = "BN")
  })
  observeEvent(areas_def(),{
    check_input(
      x = areas_def(),
      names_req = if(input$PAS == 148) c('Nom_Predio', 'N_Area', 'Clase_Uso') else c('Nom_Predio', 'N_Area', 'Clase_Eros'),
      huso = input$huso,
      id = "cart_area-sf_file"
    )
  })

  ### Rodales ----
  rodales_def <- mod_leer_sf_server(id = "cart_rodales", crs = crs())
  observeEvent(rodales_def(),{
    check_input(
      x = rodales_def(),
      names_req = c('N_Rodal', 'Tipo_For'),
      huso = input$huso,
      id = "cart_rodales-sf_file"
    )
  })

  observeEvent(c(areas_def(),rodales_def()),{
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
        )
      )
    }
    if (nrow(rodales_def() %>% dplyr::count(N_Rodal)) >
        nrow(rodales_def() %>% dplyr::count(N_Rodal) %>% .[areas_def(), ])) {
      shinybusy::report_warning(
        title = "Rodales sin áreas",
        text = paste0(
          "Los siguientes rodales sobran:\n",
          setdiff(
            rodales_def() %>% dplyr::count(N_Rodal) %>% .$N_Rodal,
            rodales_def() %>% dplyr::count(N_Rodal) %>% .[areas_def(), ] %>% .$N_Rodal
          ) %>%
            shQuote() %>%
            paste(collapse = ", ")
        )
      )
    }
  })

  ### Predios ----
  predios_def <- mod_leer_sf_server(id = "cart_predios", crs = crs())
  observeEvent(predios_def(),{
    check_input(
      x = predios_def(),
      names_req = c('N_Predio', 'Nom_Predio', 'Rol', 'Propietari'),
      huso = input$huso,
      id = "cart_predios-sf_file"
    )
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
            tags$div(
              id = "inline",
              numericInput(
                inputId = "buffer_cam",
                label = "Buffer",
                value = 0,
                step = 10,
                width = "100px"
              ),
              style = "margin-left: 25px;"
            ),
            tags$div(tags$b("m"), style = "margin-top: 10px;"),
            style = "margin-top: -10px; margin-bottom: 5px"
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
            tags$div(
              id = "inline",
              shinyWidgets::pickerInput(
                inputId = "cut_hidro",
                label = "Corte",
                choices = c("clip", "buffer", "crop", "crop_by_row"),
                selected = "clip"
              ),
              style = "margin-left: 25px;"
            ),
            tags$div(
              id = "inline",
              numericInput(
                inputId = "buffer_hidro",
                label = "Buffer",
                value = 0,
                step = 10,
                width = "100px"
              ),
              style = "margin-left: 25px;"
            ),
            tags$div(tags$b("m"), style = "margin-top: 10px;"),
            style = "margin-top: -10px; margin-bottom: 5px"
          ),
          tags$p(
            "Hidrografía será creada a partir de la hidrografía subida a Geoportal actualizada al 31-12-2022 (link ",
            tags$a("aquí", .noWS = "outside", href = "https://www.geoportal.cl/geoportal/catalog/36436/Hidrograf%C3%ADa%20de%20la%20regi%C3%B3n%20de%20Arica%20a%20la%20regi%C3%B3n%20de%20Los%20Lagos"),
            ") ¿Desea crear capa hidrografíca a partir de información de Google? (De lo contrario )"
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
            ),
            style = "margin-left: 25px;"
          ),
          tags$div(
            id = "inline",
            numericInput(
              inputId = "buffer_cn",
              label = "Buffer",
              value = 0,
              step = 10,
              width = "100px"
            ),
            style = "margin-left: 25px;"
          ),
          tags$div(
            id = "inline",
            shinyWidgets::numericInputIcon(
              inputId = "step",
              label = "Intérvalo: ",
              value = 10,
              min = 10,
              max = 500,
              step = 10,
              icon = icon("ruler-horizontal")
            )
          )
        )
      }
    })
  })

  ### BD Flora ----
  # bd_flora <- mod_bd_flora_server("bd_flora", rodales_def = rodales_def())
  bd_flora <- eventReactive(input$bd_flora, {
    req(c(input$bd_flora$datapath, rodales_def()))

    read_xlsx(input$bd_flora$datapath) %>%
      dplyr::rename_all(
        ~ stringi::stri_trans_totitle(
          stringi::stri_trans_general(.,"Latin-ASCII"),
          opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")
        )) %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("cob_bb", strength = 1), ~ "Cob_BB") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("p500", case_insensitive = T), ~ "N_ind") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ds.*68", case_insensitive = T), ~ "DS_68") %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::matches("^rce"), dplyr::contains("UTM"), dplyr::matches("ds_68")),
        stringi::stri_trans_toupper
      ) %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains("Cob_BB")),
        ~ stringi::stri_trim(stringi::stri_trans_tolower(.))
      ) %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::matches("Especie")),
        ~ stringi::stri_trim(stringi::stri_trans_totitle(., opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")))
      ) %>%
      dplyr::mutate_at("N_ind", as.integer) %>%
      {if (input$PAS == 148) {
        .[] %>%
          dplyr::filter(
            Habito %>%
              stringi::stri_trans_general("Latin-ASCII") %>%
              stringi::stri_detect_regex("arbol", case_insensitive = T),
            !Cob_BB %>%
              stringi::stri_trans_tolower() %in% c(NA_character_, "fp", "---"),
            !N_ind %in% c(NA, 0)
          )
      } else {
        .[] %>%
          dplyr::filter(
            DS_68 %>% stringi::stri_cmp_equiv("originaria", strength = 1),
            !Cob_BB %>% stringi::stri_trans_tolower() %in% c(NA_character_, "fp", "---"),
            !N_ind %in% c(NA, 0)
          )
      }} %>%
      dplyr::select(-dplyr::matches("Nom_Predio|N_Rodal|Tipo_veg|Tipo_fores|Tipo_For|Subtipo_fo")) %>%
      sf::st_as_sf(coords = c("UTM_E","UTM_N"), crs = sf::st_crs(rodales_def()), remove = F) %>%
      sf::st_join(rodales_def() %>% dplyr::select(Nom_Predio, N_Rodal, Tipo_fores, Tipo_For, Subtipo_fo, Tipo_veg)) %>%
      dplyr::mutate_at("N_Rodal", as.integer) %>%
      sf::st_drop_geometry() %>%
      dplyr::mutate_at("N_ind", as.integer) %>%
      dplyr::mutate(Nha = N_ind * 20) %>%
      dplyr::arrange(N_Rodal) %>%
      dplyr::group_by(Parcela, UTM_E, UTM_N) %>%
      dplyr::arrange(N_Rodal) %>%
      dplyr::mutate(N = dplyr::cur_group_id()) %>%
      dplyr::group_by(N_Rodal, N) %>%
      dplyr::mutate(N_Parc = dplyr::cur_group_id()) %>%
      dplyr::ungroup()
  })

  observeEvent(bd_flora(),{
    check_bd_flora(x = bd_flora(), shiny = T)

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
        title = "ECC arbórea en las parcelas",
        text = tags$p(paste0(
          "Ojo con las siguientes especies y parcelas:\n",
          df_ecc() %>%
            dplyr::group_by(Especie, RCE) %>%
            dplyr::mutate_at("RCE", ~ paste0("(RCE: ", .x, ")")) %>%
            split(.$Especie) %>%
            purrr::map(function(x){
              x %>%
                {if(nrow(.) > 10) {
                  dplyr::sample_n(., 10) %>%
                    dplyr::summarise(Parcelas = paste0(Parcela, collapse = ", ")) %>%
                    dplyr::mutate(Parcelas = purrr::map_chr(Parcelas, ~paste0("(", .x, ", etc...)")))
                } else {
                  dplyr::summarise(., Parcelas = paste0(Parcela, collapse = ", ")) %>%
                    dplyr::mutate(Parcelas = purrr::map_chr(Parcelas, ~paste0("(", .x, ")")))
                }} %>%
                tidyr::unite("SP", 1:3, remove = T, sep = " ")
            }) %>%
            dplyr::bind_rows() %>%
            dplyr::pull(SP) %>%
            paste0(collapse = "\n")
        )),
        html = TRUE,
        type = "warning",
        closeOnEsc = T,
        showConfirmButton = T,
        animation = T
      )
    }
  })

  ### Uso actual ----
  observeEvent(input$add_uso_actual,{
    output$add_uso_actual_ui <- renderUI({
      if(input$add_uso_actual){
        tags$div(
          mod_leer_sf_ui("catastro", "Ingrese capa del catastro de CONAF") %>%
            add_help_text(title = "Campos minimos requeridos:\n'USO', 'SUBUSO', 'ESTRUCTURA'"),
          tags$div(style = "margin-top: -10px"),
          mod_leer_sf_ui("suelos_uso_act", "Ingrese capa de suelos del CIREN") %>%
            add_help_text(title = "Campos minimos requeridos:\n'TEXTCAUS'")
        )
      }
    })
  })

  catastro <- reactive({
    req(predios_def())
    if (input$add_uso_actual) {
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
        wkt_filter = sf::st_as_text(sf::st_geometry(predios_def() %>% sf::st_union()))
      )
    } else {
      NULL
    }
  })
  observeEvent(catastro(),{
    check_input(
      x = catastro(),
      names_req = c('USO', 'SUBUSO', 'ESTRUCTURA'),
      huso = input$huso,
      id = "catastro-sf_file"
    )
  })

  suelos_uso_act <- reactive({
    req(predios_def())
    if (input$add_uso_actual) {
      mod_leer_sf_server(
        id = "suelos_uso_act",
        crs = crs(),
        fx = function(x){
          x %>%
            dplyr::rename_if(
              names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T),
              ~ "Clase_Uso"
            )
        },
        wkt_filter = sf::st_as_text(sf::st_geometry(predios_def() %>% sf::st_union()))
      )
    } else {
      NULL
    }
  })
  observeEvent(suelos_uso_act(),{
    check_input(
      x = suelos_uso_act(),
      names_req = c('Clase_Uso'),
      huso = input$huso,
      id = "suelos_uso_act-sf_file"
    )
  })

  ### Generar y descargar carto ----
  shinyjs::disable("get_carto_btn")
  observe({
    req(c(areas_def(), rodales_def(), predios_def(), bd_flora()))
    shinyjs::enable("get_carto_btn")
  })
  carto_digital <- eventReactive(input$get_carto_btn,{
    req(c(areas_def(), rodales_def(), predios_def(), bd_flora()))
    get_carto_digital(
      PAS = input$PAS,
      areas = areas_def(),
      rodales = rodales_def(),
      predios = predios_def(),
      TipoFor_num = input$tipo_for,
      dem = input$dem$datapath,
      add_parcelas = input$add_parcelas,
      bd_flora = bd_flora(),
      from_RCA = F,
      RCA = NULL,
      add_uso_actual = input$add_uso_actual,
      catastro = catastro(),
      suelos = suelos_uso_act(),
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
  })

  observeEvent(input$get_carto_btn,{
    req(c(areas_def(), rodales_def(), predios_def(), bd_flora()))
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
    req(carto_digital())
    gc()
    shinybusy::remove_modal_spinner()
    shinybusy::notify_success(text = "Listo!", timeout = 3000, position = "right-bottom")

    mod_downfiles_server(
      id = "down_carto",
      x = carto_digital(),
      name_save = c(
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
        paste0(.,input$NOMPREDIO, sep = "_") %>%
        subset(
          c(rep(T, 7),
            input$add_parcelas,
            input$add_uso_actual,
            input$add_cam,
            if(input$add_cam == F) F else input$add_cam_osm,
            input$add_hidro,
            if(input$add_hidro == F) F else input$add_hidro_osm,
            input$add_curv_niv
          )
        )
    )
  })

  flextable::set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )

  tbl_planos_areas <- eventReactive(carto_digital(),{
    req(carto_digital())
    var_suelo <- if (input$PAS == 148) dplyr::sym("Clase_Uso") else dplyr::sym("Clase_Eros")
    nom_suelo <- if (input$PAS == 148) "Clase Uso Suelo" else "Grado de Erosión"
    carto_digital()$tabla_areas %>%
      dplyr::select(N_Predio, N_Area, Ran_Pend, !!var_suelo, Sup_ha) %>%
      `names<-`(c("N° Predio", "Área N°", "Rango Pendiente (%)", nom_suelo, "Superficie área de corta (ha)")) %>%
      flextable::flextable() %>%
      flextable::merge_v(j = c(1)) %>%
      flextable::autofit() %>%
      flextable::theme_box() %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center")
  })

  tbl_planos_predios <- eventReactive(carto_digital(),{
    req(carto_digital())
    carto_digital()$tabla_predios %>%
      mutate(Pto_ref = NA_character_, Este = as.integer(NA), Norte = as.integer(NA)) %>%
      dplyr::mutate(Pto_ref = NA_character_, Este = as.integer(NA), Norte = as.integer(NA)) %>%
      dplyr::select(N_Predio, Nom_Predio, Propietari, Rol, Comuna, Sup_ha, Pto_ref, Este, Norte) %>%
      dplyr::mutate_at(vars(Comuna), ~purrr::map_vec(., stringi::stri_split_regex, " - ")) %>%
      tidyr::unnest(Comuna) %>%
      dplyr::select(-c(dplyr::matches("provincia"), dplyr::matches("region"))) %>%
      dplyr::left_join(comunas_df %>% dplyr::select(COMUNA, PROVINCIA, REGION) %>% dplyr::rename_all(stringi::stri_trans_totitle)) %>%
      dplyr::relocate(c(Provincia, Region), .after = Comuna) %>%
      `names<-`(
        c("N° Predio", "Nombre Predio", "Nombre del propietario/a", "ROL", "Comuna", "Provincia",
          "Región", "Superficie predial (ha)", "Punto de referencia", "Este", "Norte")
      ) %>%
      flextable::flextable() %>%
      flextable::merge_v(j = c(1:4,6:8)) %>%
      flextable::autofit() %>%
      flextable::theme_box() %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center")
  })

  mod_downfiles_server(
    id = "down_tbl_areas",
    x = tbl_planos_areas(),
    name_save = "Tabla_planos_areas"
  )
  mod_downfiles_server(
    id = "down_tbl_predios",
    x = tbl_planos_predios(),
    name_save = "Tabla_planos_predios"
  )

  # Apendices ----
  ## Apendice 2 Y 3 ----
  observeEvent(input$add_bd_pcob,{
    output$add_bd_pcob_ui <- renderUI({
      if(input$add_bd_pcob){
        div(
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
    req(c(rodales_def(), predios_def(), bd_flora()))
    shinyjs::enable("get_apendices_2y3_btn")
  })

  apendices_2y3 <- eventReactive(input$get_apendices_2y3_btn,{
    req(c(bd_flora(), rodales_def(), predios_def()))
    apendice_2_3(
      PAS = input$PAS,
      bd_flora = bd_flora(),
      bd_pcob = bd_pcob(),
      rodales = rodales_def(),
      predios = predios_def(),
      portada = input$portada,
      provincia = input$provincia,
      huso = input$huso
    )
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
    req(apendices_2y3())
    gc()
    shinybusy::remove_modal_spinner()
    shinybusy::notify_success(text = "Listo!", timeout = 3000, position = "right-bottom")
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
  })

  ## Atributos de rodal ----
  tabla_attr_rodal_0 <- reactive({
    req(c(bd_flora(), rodales_def()))
    get_tabla_attr_rodal(
      PAS = input$PAS,
      parcelas_rodales = bd_flora(),
      rodales_def = rodales_def()
    )
  })
  tabla_attr_rodal <- reactive({
    req(input$tabla_attr_rodal)
    openxlsx2::read_xlsx(input$tabla_attr_rodal$datapath)
  })

  observeEvent(tabla_attr_rodal_0,{
    mod_downfiles_server(
      id = "tabla_attr_rodal_0",
      x = tabla_attr_rodal_0(),
      name_save = c("Tabla atributacion de rodales")
    )
  })

  ## Apendice 5 ----
  shinyjs::disable("get_apendice_5_btn")
  observe({
    req(c(
      rodales_def(),
      carto_digital()$tabla_predios,
      carto_digital()$tabla_areas,
      bd_flora(),
      tabla_attr_rodal()
    ))
    shinyjs::enable("get_apendice_5_btn")
  })

  obras_ap5 <- mod_leer_sf_server(id = "obras_ap5", crs = crs(), fx = function(x){
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
    check_input(
      x = obras_ap5(),
      names_req = c('Tipo', 'Obra'),
      huso = input$huso,
      id_reset = "obras_ap5-sf_file"
    )
  })

  observeEvent(input$PAS,{
    output$bd_fauna_ui <- renderUI({
      if (input$PAS == 148) {
        fileInput(
          inputId = "bd_fauna",
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
      bd_flora(),
      rodales_def(),
      carto_digital()$tabla_predios,
      carto_digital()$tabla_areas,
      tabla_attr_rodal()
    ))
    if (input$PAS == 148) {
      apendice_5_PAS148(
        bd_flora = input$bd_flora,
        rodales = input$rodales_def,
        tabla_predios = carto$tabla_predios,
        tabla_areas = carto$tabla_areas,
        tabla_attr_rodal = tabla_attr_rodal(),
        portada = input$portada,
        provincia = input$provincia,
        carto_uso_actual = carto$uso_actual,
        obras = obras_ap5(),
        bd_fauna = bd_fauna()
      )
    } else {
      apendice_5_PAS148(
        bd_flora = input$bd_flora,
        rodales = input$rodales_def,
        tabla_predios = carto$tabla_predios,
        tabla_areas = carto$tabla_areas,
        tabla_attr_rodal = tabla_attr_rodal(),
        portada = input$portada,
        provincia = input$provincia,
        obras = obras_ap5()
      )
    }
  })

  observeEvent(input$get_apendice_5_btn,{
    req(c(
      bd_flora(),
      rodales_def(),
      carto_digital()$tabla_predios,
      carto_digital()$tabla_areas,
      tabla_attr_rodal()
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
    shinybusy::notify_success(text = "Listo!", timeout = 3000, position = "right-bottom")
    mod_downfiles_server(
      id = "down_apendice_5",
      x = apendice_5(),
      name_save = c("APÉNDICE 5. Tablas formulario CONAF")
    )
  })

  # Autor ----
  output$user <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = "David Martínez",
      image = "https://avatars.githubusercontent.com/u/74486319?s=400&u=c277213b232af5e7710bebdc7a50bb9426ab9a62&v=4",
      title = "Autor Dashboard PAS 148/151",
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
