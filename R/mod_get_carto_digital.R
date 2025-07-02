#' get_carto_digital UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @importFrom shiny fileInput NS tagList textInput uiOutput
mod_get_carto_digital_ui <- function(id) {
  ns <- NS(id)
  tagList(
    mod_leer_sf_ui(ns("cart_area"), "Ingrese shapefile de áreas de corta") %>%
      add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio', 'N_Area', 'Clase_Uso'"),
    tags$div(style = "margin-top: -10px"),
    mod_leer_sf_ui(ns("cart_rodales"), "Ingrese shapefile de rodales") %>%
      add_help_text(title = "Campos minimos requeridos:\n'N_Rodal', 'Tipo_For'"),
    tags$div(style = "margin-top: -10px"),
    tags$div(style = "margin-top: -10px"),
    mod_leer_sf_ui(ns("cart_predios"), "Ingrese shapefile de limites prediales") %>%
      add_help_text(title = "Campos minimos requeridos:\n'N_Predio', 'Nom_Predio', 'Rol', 'Propietari"),
    tags$div(style = "margin-top: -10px"),
    fileInput(
      inputId = ns("dem"),
      label = "Ingresar DEM de Alos Palsar (12,5 x 12,5m)",
      multiple = F,
      accept = c(".tif",".jp2"),
      buttonLabel = "Seleccionar",
      placeholder = "Archivo no seleccionado"
    ) %>%
      add_help_text("Por favor utilizar DEM acotado al área de estudio"),
    tags$div(style = "margin-top: -10px"),
    tags$hr(),
    # Bd flora
    mod_bd_flora_ui(ns("bd_flora")),
    # Uso actual
    shinyWidgets::materialSwitch(
      inputId = ns("add_uso_actual"),
      label = "¿Crear capa de uso actual?",
      status = "success"
    ) %>%
      add_help_text(title = "Puede hacer que tarde mucho en ejecutarse la función"),
    uiOutput(ns("add_uso_actual_ui")),
    tags$div(style = "margin-top: -10px"),
    tags$hr(),
    # BASES CARTOGRAFICAS
    tags$h4("Bases cartográficas", style = "font-weight: bold;"),
    shinyWidgets::materialSwitch(
      inputId = ns("add_cam"),
      label = "¿Crear capa de caminos?",
      status = "success"
    ),
    uiOutput(ns("add_cam_ui")),
    tags$div(style = "margin-top: -10px"),
    shinyWidgets::materialSwitch(
      inputId = ns("add_hidro"),
      label = "¿Crear capa de Hidrografía?",
      status = "success"
    ),
    uiOutput(ns("add_hidro_ui")),
    tags$div(style = "margin-top: -10px"),
    shinyWidgets::materialSwitch(
      inputId = ns("add_CN"),
      label = "¿Crear capa de curvas de nivel?",
      status = "success"
    ) %>%
      add_help_text(title = "Puede hacer que tarde mucho en ejecutarse la función"),
    uiOutput(ns("add_CN_ui")),
    tags$div(style = "margin-top: -10px"),
    tags$hr(),

    # NOMBRE PREDIO
    textInput(ns("NOMPREDIO"), "Ingrese un sufijo para el nombre de los archivos", placeholder = "Ej: CHILICAUQUENALTO, KIMAL, etc"),

    # GET AND DOWNLOAD CARTOGRAFIA
    tags$div(
      id = "flex",
      shinyWidgets::actionBttn(
        inputId = ns("get_carto_btn"),
        label = "Obtener cartografía",
        style = "unite",
        size = "sm",
        color = "success"
      ),
      mod_downfiles_ui(ns("down_carto")),
      mod_downfiles_ui(ns("down_tbl_areas"), label = "Tabla planos áreas", style = "material-flat"),
      mod_downfiles_ui(ns("down_tbl_predios"), label = "Tabla planos predios", style = "material-flat")
    )
  )
}

#' get_carto_digital Server Functions
#'
#' @noRd
#' @importFrom dplyr vars
#' @importFrom shiny eventReactive icon moduleServer numericInput observe observeEvent reactive renderUI req
mod_get_carto_digital_server <- function(id, crs, dec_sup, carto_out, huso, inputs, carto){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # AREAS DE CORTA  ----
    areas_def <- mod_leer_sf_server(id = "cart_area", crs = crs, fx = function(x){
      x %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_Uso") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("n_area", case_insensitive = T), ~ "N_Area") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("nom_predio", case_insensitive = T), ~ "Nom_Predio") %>%
        dplyr::mutate(Tipo_Bos = "BN")
    })
    observeEvent(areas_def(),{
      check_input(
        x = areas_def(),
        names_req = c('Nom_Predio', 'N_Area', 'Clase_Uso'),
        huso = huso,
        id = "cart_area-sf_file"
      )
      inputs$areas_def <- areas_def()
    })

    # RODALES ----
    rodales_def <- mod_leer_sf_server(id = "cart_rodales", crs = crs)
    observeEvent(rodales_def(),{
      check_input(
        x = rodales_def(),
        names_req = c('N_Rodal', 'Tipo_For'),
        huso = huso,
        id = "cart_rodales-sf_file"
      )
      inputs$rodales_def <- rodales_def()
    })

    # PREDIOS
    predios_def <- mod_leer_sf_server(id = "cart_predios", crs = crs)
    observeEvent(predios_def(),{
      check_input(
        x = predios_def(),
        names_req = c('N_Predio', 'Nom_Predio', 'Rol', 'Propietari'),
        huso = huso,
        id = "cart_predios-sf_file"
      )
      inputs$predios_def <- predios_def()
    })

    # CAMINOS
    observeEvent(input$add_cam,{
      output$add_cam_ui <- renderUI({
        if(input$add_cam){
          tags$div(
            tags$div(
              id = "flex",
              tags$div(
                id = "inline",
                shinyWidgets::pickerInput(
                  inputId = ns("cut_cam"),
                  label = "Corte",
                  choices = c("clip", "buffer", "crop", "crop_by_row"),
                  selected = "clip"
                )
              ),
              tags$div(
                id = "inline",
                numericInput(
                  inputId = ns("buffer_cam"),
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
              inputId = ns("add_cam_osm"),
              size = "mini",
              onLabel = "Si",
              offLabel = "No",
              onStatus = "success"
            )
          )
        }
      })
    })

    # HIDROGRAFIA ----
    observeEvent(input$add_hidro,{
      output$add_hidro_ui <- renderUI({
        if(input$add_hidro){
          tags$div(
            tags$div(
              id = "flex",
              tags$div(
                id = "inline",
                shinyWidgets::pickerInput(
                  inputId = ns("fuente_hidro"),
                  label = "Fuente",
                  choices = c("MOP", "BCN"),
                  selected = "MOP"
                )
              ),
              tags$div(
                id = "inline",
                shinyWidgets::pickerInput(
                  inputId = ns("cut_hidro"),
                  label = "Corte",
                  choices = c("clip", "buffer", "crop", "crop_by_row"),
                  selected = "clip"
                ),
                style = "margin-left: 25px;"
              ),
              tags$div(
                id = "inline",
                numericInput(
                  inputId = ns("buffer_hidro"),
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
              inputId = ns("add_hidro_osm"),
              size = "mini",
              onLabel = "Si",
              offLabel = "No",
              onStatus = "success"
            )
          )
        }
      })
    })

    observeEvent(input$add_CN,{
      output$add_CN_ui <- renderUI({
        if (input$add_CN) {
          shinyWidgets::numericInputIcon(
            inputId = ns("step"),
            label = "Intérvalo",
            value = 10,
            min = 10,
            max = 500,
            step = 10,
            icon = icon("ruler-horizontal")
          )
        }
      })
    })

    # BD FLORA ----
    bd_flora <- mod_bd_flora_server("bd_flora", rodales_def = rodales_def())
    observeEvent(bd_flora(),{

      check_bd_flora(x = bd_flora(), shiny = T)

      df <- reactive({
        bd_flora() %>%
          {if (PAS == 148) {
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

      if (df() %>% nrow() >= 1) {
        shinybusy::report_warning(
          title = "ECC arbórea en las parcelas",
          text = paste0(
            "Ojo con las siguientes especies y parcelas:\n",
            df() %>% dplyr::group_by(Especie, RCE) %>%
              dplyr::mutate_at("RCE", ~paste0("(RCE: ", .x, ")")) %>%
              {if(nrow(.) > 10) {
                dplyr::sample_n(., 10) %>%
                  dplyr::summarise(Parcelas = paste0(Parcela, collapse = ", ")) %>%
                  dplyr::mutate(Parcelas = purrr::map_chr(Parcelas, ~paste0("(", .x, ", etc...)")))
              } else {
                dplyr::summarise(., Parcelas = paste0(Parcela, collapse = ", ")) %>%
                  dplyr::mutate(Parcelas = purrr::map_chr(Parcelas, ~paste0("(", .x, ")")))
              }} %>%
              tidyr::unite("SP", 1:3, remove = T, sep = " ") %>%
              dplyr::pull(SP) %>% paste0(collapse = "\n")
          )
        )
      }

      inputs$bd_flora <- bd_flora()
    })

    # USO ACTUAL ----
    observeEvent(input$add_uso_actual,{
      output$add_uso_actual_ui <- renderUI({
        if(input$add_uso_actual){
          tags$div(
            mod_leer_sf_ui(ns("catastro"), "Ingrese capa del catastro de CONAF") %>%
              add_help_text(title = "Campos minimos requeridos:\n'USO', 'SUBUSO', 'ESTRUCTURA'"),
            tags$div(style = "margin-top: -10px"),
            mod_leer_sf_ui(ns("suelos_uso_act"), "Ingrese capa de suelos del CIREN") %>%
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
        huso = huso,
        id = "catastro-sf_file"
      )
    })

    suelos_uso_act <- reactive({
      req(predios_def())
      if (input$add_uso_actual) {
        mod_leer_sf_server(
          id = "suelos_uso_act",
          crs = crs,
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
        huso = huso,
        id = "suelos_uso_act-sf_file"
      )
    })

    # CARTOGRAFÍA DIGITAL ----
    shinyjs::disable("get_carto_btn")
    observe({
      req(c(areas_def(), rodales_def(), predios_def()))
      shinyjs::enable("get_carto_btn")
    })
    carto_digital <- eventReactive(input$get_carto_btn,{
      req(c(areas_def(), rodales_def(), predios_def()))
      get_carto_digital(
        PAS = PAS,
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
        add_caminos_osm = input$add_cam_osm,
        caminos_arg = if(input$add_cam == F) list(cut = "clip", buffer = 0) else list(cut = input$cut_cam, buffer = input$buffer_cam),
        add_hidro = input$add_hidro,
        fuente_hidro = input$fuente_hidro,
        add_hidro_osm = if(input$add_hidro == F) F else input$add_hidro_osm,
        hidro_arg = if(input$add_hidro == F) list(cut = "clip", buffer = 0) else list(cut = input$cut_hidro, buffer = input$buffer_hidro),
        add_curv_niv = input$add_CN,
        curv_niv_arg = if(input$add_CN == F) list(cut = "clip", buffer = 0) else list(cut = input$cut_curv_niv, buffer = input$buffer_curv_niv),
        step = if(input$add_CN == F) 10 else input$step,
        dec_sup = dec_sup
      )
    })

    observeEvent(input$get_carto_btn,{
      req(c(areas_def(), rodales_def(), predios_def(), DEM()))
      shinybusy::show_modal_spinner(
        spin = "flower",
        color = "#35978F",
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
      carto$tabla_predios <- carto_digital()$tabla_predios
      carto$tabla_areas <- carto_digital()$tabla_areas
      if(input$add_uso_actual) arto$uso_actual <- carto_digital()$Uso_actual
      gc()
      shinybusy::remove_modal_spinner()
      shinybusy::notify_success(text = "Listo!", timeout = 3000, position = "right-bottom")
    })

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
        paste(.,input$NOMPREDIO, sep = "_") %>%
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
  })
}

## To be copied in the UI
# mod_get_carto_digital_ui("get_carto_digital_1")

## To be copied in the server
# mod_get_carto_digital_server("get_carto_digital_1")
