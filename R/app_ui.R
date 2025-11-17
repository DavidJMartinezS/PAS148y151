#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    # dashboardPage ----
    shinydashboardPlus::dashboardPage(
      skin = "green",
      shinydashboardPlus::dashboardHeader(
        leftUi = tagList(
          tags$div(
            id = "head_style",
            shinyWidgets::radioGroupButtons(
              inputId = "PAS",
              choiceNames = c("PAS 148", "PAS 151"),
              choiceValues = c(148, 151),
              selected = 148,
              status = "primary"
            )
          ),
          tags$div(
            id = "head_style",
            shinyWidgets::dropdownButton(
              label = "Ajustes bases",
              icon = icon("gear"),
              status = "primary",
              circle = F,
              tags$span(
                id = "flex",
                shinyWidgets::prettyRadioButtons(
                  inputId = "huso",
                  label = "Huso:  ",
                  choices = c("18S", "19S"),
                  selected = "19S",
                  inline = TRUE,
                  status = "primary",
                  fill = TRUE,
                  animation = "smooth",
                ),
                tags$div(
                  numericInput(
                    inputId = "n_dec",
                    label = "N° decimales superficie:",
                    value = 2,
                    min = 0,
                    max = 4,
                    width = "180px"
                  ),
                  style = "margin-left: 20px;"
                )
              )
            )
          ),
          tags$div(
            id = "inline",
            shinyWidgets::pickerInput(
              inputId = "provincia",
              label = "Provincia:",
              choices = provincias_list,
              selected = NULL,
              options = shinyWidgets::pickerOptions(container = "body", style = "btn-primary")
            ),
            style = "color: white; margin-left: 10px;"
          )
        ),
        title = tagList(
          tags$span(class = "logo-lg", "PAS 148/151"),
          tags$img(
            src = "www/logo_geobiota_mini.png",
            height = "35px",
            style = "margin-left: -7px;"
          )
        ),
        tags$li(
          class = "dropdown",
          id = "logo",
          tags$a(tags$img(height = "35px", src = "www/logo_geobiota.svg"))
        ),
        shinydashboardPlus::userOutput("user")
      ),
      # dashboardSidebar ----
      shinydashboardPlus::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Importante", tabName = "importante", icon = icon("circle-info")),
          shinydashboard::menuItem("Ayuda cartográfica", tabName = "ayuda", icon = icon("circle-check")),
          shinydashboard::menuItem("Cartografía y Apéndices", tabName = "carto", icon = icon("layer-group"))
        ),
        tags$footer(
          tags$img(
            src = "www/favicon.ico",
            width = "80%",
            style = "align: center; display: block; margin-left: auto; margin-right: auto;"
          ),
          style = "position: absolute; bottom: 25px"
        )
      ),
      # dashboardBody ----
      shinydashboard::dashboardBody(
        fresh::use_theme(mytheme),
        shinyjs::useShinyjs(),
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip(),
        shinyEffects::setShadow(class = "dropdown-menu"),
        shinyEffects::setShadow(class = "box"),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "importante",
            fluidRow(
              column(
                width = 12,
                shinydashboardPlus::box(
                  title = "Uso adecuado del Dashboard",
                  solidHeader = T,
                  status = "success",
                  width = 12,
                  info_dashboard()
                )
              )
            )
          ),
          ## TABITEM AYUDAS ----
          shinydashboard::tabItem(
            tabName = "ayuda",
            fluidRow(
              col_4(
                ### Pred Rod Area ----
                shinydashboardPlus::box(
                  width = 12,
                  title = "Generar Predios, rodales y areas de corta",
                  solidHeader = T,
                  status = "success",
                  mod_read_sf_ui("linea_base", "Ingrese cartografía de linea base") %>%
                    add_help_text(title = "Campos minimos requeridos:\n'PID', 'Tipo_fores', 'Subtipo_fo', 'Tipo_veg', 'F_ley20283'"),
                  mod_read_sf_ui("obras", "Ingrese shp de obras"),
                  mod_read_sf_ui("predios", "Ingrese shp de predios") %>%
                    add_help_text(title = "Campos minimos requeridos:\n'N_Predio','Nom_Predio', 'Rol', 'Propietari'"),
                  mod_read_sf_ui("suelos", "Ingrese shp de suelos") %>%
                    add_help_text(title = "Campos minimos requeridos:\n'Clase_Uso/Clase_Eros'"),
                  shinyWidgets::pickerInput(
                    inputId = "group_by_LB",
                    label = "Agrupar rodales por (Opcional):",
                    choices = c(NULL),
                    multiple = T,
                    options = list(title = "Selecciona una o más opciones")
                  ),
                  shinyWidgets::materialSwitch(
                    inputId = "sep_by_soil",
                    label = "¿Separar geometrías por CUS/Erosion?",
                    value = T,
                    status = "success"
                  ),
                  shinyWidgets::materialSwitch(
                    inputId = "group_by_dist",
                    label = "¿Agrupar áreas por distancia?",
                    status = "success"
                  ),
                  uiOutput("distanceUI"),
                  shinyWidgets::materialSwitch(
                    inputId = "cut_by_prov",
                    label = "¿Cortar áreas por una provincia?",
                    status = "success"
                  ),
                  uiOutput("select_prov_UI"),
                  tags$div(
                    shinyWidgets::materialSwitch(
                      inputId = "ord_rodales",
                      label = "¿Ordenar rodales espacialmente?",
                      status = "success"
                    ),
                    uiOutput("ord_rodales_UI")
                  ),
                  tags$div(
                    id = "flex",
                    shinyWidgets::actionBttn(
                      inputId = "get_area",
                      label = "Generar capas",
                      style = "unite",
                      size = "sm",
                      color = "success"
                    ),
                    mod_downfiles_ui("down_areas")
                  )
                )
              ),
              col_8(
                fluidRow(
                  shinydashboardPlus::box(
                    width = 6, solidHeader = T, status = "success",
                    title = "Ordenar shapefile",
                    mod_st_order_ui("st_order")
                  ),
                  shinydashboardPlus::box(
                    width = 6, solidHeader = T, status = "success",
                    title = "Chequear cartografía",
                    mod_check_carto_ui("check_carto")
                  )
                ),
                fluidRow(
                  shinydashboardPlus::box(
                    width = 6, solidHeader = T, status = "success",
                    title = "Agregar atributos",
                    mod_add_attr_ui("add_attr")
                  ),
                  shinydashboardPlus::box(
                    width = 6, solidHeader = T, status = "success",
                    title = "Crear uso actual",
                    mod_uso_actual_ui("uso_actual_1")
                  )
                )
              )
            )
          ),
          ## TABITEM CARTO-APEND ----
          shinydashboard::tabItem(
            tabName = "carto",
            fluidRow(
              col_6(
                ### Carto digital ----
                shinydashboardPlus::box(
                  width = 12,
                  title = "Generar cartograía digital",
                  solidHeader = T,
                  status = "success",

                  #### Objetos espaciales ----
                  mod_read_sf_ui("areas_prelim", "Ingrese shapefile de áreas de corta") %>%
                    add_help_text(title = "Campos minimos requeridos:\n'Nom_Predio', 'N_Area', 'Clase_Uso/Clase_Eros'"),
                  tags$div(style = "margin-top: -10px"),
                  mod_read_sf_ui("rodales_prelim", "Ingrese shapefile de rodales") %>%
                    add_help_text(title = "Campos minimos requeridos:\n'N_Predio', 'PID', 'N_Rodal', 'Tipo_For'"),
                  tags$div(style = "margin-top: -10px"),
                  mod_read_sf_ui("predios_prelim", "Ingrese shapefile de limites prediales") %>%
                    add_help_text(title = "Campos minimos requeridos:\n'N_Predio', 'Nom_Predio', 'Rol', 'Propietari"),
                  tags$div(style = "margin-top: -10px"),
                  shinyWidgets::materialSwitch(
                    inputId = "cut_pred_by_prov",
                    label = "¿Cortar predios por provincia?",
                    status = "success"
                  ),
                  tags$div(style = "margin-top: -10px"),
                  fileInput(
                    inputId = "dem",
                    label = "Ingresar DEM de Alos Palsar (12,5 x 12,5m)",
                    multiple = F,
                    accept = c(".tif", ".tiff"),
                    buttonLabel = "Seleccionar",
                    placeholder = "Archivo no seleccionado"
                  ) %>%
                    add_help_text(title = "Por favor utilizar DEM acotado al área de estudio"),
                  tags$div(style = "margin-top: -5px"),
                  tags$hr(),

                  #### BD Flora ----
                  fileInput(
                    inputId = "bd_flora",
                    label = "Ingresar BD de parcelas (Solo datos de las parcelas que se desean incluir)",
                    multiple = F,
                    accept = c(".xlsx"),
                    buttonLabel = "Seleccionar",
                    placeholder = "Archivo no seleccionado"
                  ) %>% add_help_text(
                    title = "Campos minimos requeridos:\n
                      'Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Cob_BB', 'Habito', 'DS_68', 'RCE'"
                  ),
                  tags$div(style = "margin-top: -5px"),
                  tags$div(
                    id = "flex",
                    shinyWidgets::awesomeCheckbox(
                      inputId = "cut_bd_by_rodal",
                      label = "Seleccionar parcelas en rodales",
                      status = "success"
                    ),
                    tags$div(style = "margin-left: 20px"),
                    shinyWidgets::awesomeCheckbox(
                      inputId = "include_fp",
                      label = "Incluir 'fp', '---' y NA",
                      value = FALSE,
                      status = "success"
                    ),
                    tags$div(
                      shinyWidgets::actionBttn(
                        inputId = "check_bd_flora",
                        label = "Chequear BD",
                        icon = icon("circle-check"),
                        style = "gradient",
                        size = "xs",
                        color = "success",
                      ),
                      style = "margin-left: 20px; margin-top: -3px; vertical-align: middle;"
                    ),
                    tags$div(
                      mod_downfiles_ui(id = "down_bd_flora", style = "material-flat", label = "BD flora", size = "xs") %>%
                        bsplus::bs_embed_tooltip(title = "Descargar BD que se esta considerando"),
                      style = "margin-left: 10px; margin-top: -3px; vertical-align: middle;"
                    )
                  ),
                  tags$div(style = "margin-top: 15px"),
                  shinyWidgets::materialSwitch(
                    inputId = "add_parcelas",
                    label = "¿Crear capa de parcelas?",
                    status = "success"
                  ),
                  tags$div(style = "margin-top: -10px"),
                  tags$hr(),

                  #### Uso actual ----
                  shinyWidgets::materialSwitch(
                    inputId = "add_uso_actual",
                    label = "¿Crear capa de uso actual?",
                    status = "success"
                  ) %>%
                    add_help_text(title = "Puede hacer que tarde mucho en ejecutarse la función"),
                  uiOutput("add_uso_actual_ui"),
                  tags$div(style = "margin-top: -10px"),
                  tags$hr(),

                  #### Bases cartograficas ----
                  modal_info_cut_buffer,
                  tags$div(
                    id = "flex",
                    tags$h4("Bases cartográficas", style = "font-weight: bold;"),
                    tags$div(
                      bsplus::shiny_iconlink() %>%
                        bsplus::bs_attach_modal(id_modal = 'ayuda_info_cut_buffer'),
                      style = "margin-left: 5px"
                    )
                  ),
                  shinyWidgets::materialSwitch(
                    inputId = "add_cam",
                    label = "¿Crear capa de caminos?",
                    status = "success"
                  ),
                  uiOutput("add_cam_ui"),
                  tags$div(style = "margin-top: -10px"),
                  shinyWidgets::materialSwitch(
                    inputId = "add_hidro",
                    label = "¿Crear capa de Hidrografía?",
                    status = "success"
                  ),
                  uiOutput("add_hidro_ui"),
                  tags$div(style = "margin-top: -10px"),
                  shinyWidgets::materialSwitch(
                    inputId = "add_CN",
                    label = "¿Crear capa de curvas de nivel?",
                    status = "success"
                  ) %>%
                    add_help_text(title = "Puede hacer que tarde mucho en ejecutarse la función"),
                  uiOutput("add_CN_ui"),
                  tags$div(style = "margin-top: -10px"),
                  tags$hr(),

                  #### Nombre predio ----
                  textInput(
                    "NOMPREDIO",
                    "Ingrese un sufijo para el nombre de los archivos",
                    placeholder = "Ej: CHILICAUQUENALTO, KIMAL, etc"
                  ),

                  #### Generar y descargar Carto ----
                  tags$div(
                    id = "flex",
                    shinyWidgets::actionBttn(
                      inputId = "get_carto_btn",
                      label = "Obtener cartografía",
                      style = "unite",
                      size = "sm",
                      color = "success"
                    ),
                    mod_downfiles_ui("down_carto"),
                    mod_downfiles_ui("down_tbl_planos", label = "Tablas planos", style = "material-flat")
                  )
                ) # final box carto digital
              ), # final col carto digital
              col_6(
                ## Apendices ----
                shinydashboardPlus::box(
                  width = 12,
                  title = "Generar Apéndices",
                  solidHeader = T,
                  status = "success",
                  tags$div(style = "margin-top: -10px"),
                  tags$h3("Apéndices", style = "font-weight: bold;"),
                  tags$div(
                    id = "inline",
                    shinyWidgets::pickerInput(
                      inputId = "portada",
                      label = "Seleccionar portada: ",
                      choices = c("default", "KIM753", "MLP612", "otra"),
                      selected = "default",
                      width = "100px"
                    )
                  ),
                  uiOutput("portada_opts_ui"),
                  tags$div(style = "margin-top: 10px"),
                  # uiOutput("cov_pas151_ui"),
                  tags$div(
                    tags$div(style = "margin-top: 10px"),
                    conditionalPanel(
                      condition = "input.PAS == 151",
                      shinyWidgets::materialSwitch(
                        inputId = "cov_as_range",
                        label = "Presentar coberturas de las parcelas de flora como rango? (ej: '10-25')",
                        status = "success"
                      )
                    ),
                    tags$div(style = "margin-top: -5px"),
                    modal_cov_fp,
                    shinyWidgets::prettyRadioButtons(
                      inputId = "cov_fp",
                      label = "Cobertura indviduos 'fp', '---' o NA (%): ",
                      choices = c("0" = 0, "0,5" = 0.5, "1" = 1),
                      selected = "1",
                      inline = TRUE,
                      icon = icon("check"),
                      width = "280px",
                      status = "success",
                      animation = "jelly"
                    ) %>%
                      bsplus::shinyInput_label_embed(
                        bsplus::shiny_iconlink() %>% bsplus::bs_attach_modal(id_modal = 'ayuda_cov_fp')
                      )
                  ),
                  tags$div(style = "margin-top: 10px"),

                  ### Apendices 2 y 3 ----
                  tags$br(),
                  tags$h4("Apéndices 2 y 3 (Densidad de especies y ubicación de parcela)",
                          style = "font-weight: bold;"),
                  tags$div(
                    style = "color: #0461b8; font-size: 16px",
                    icon("circle-info"),
                    tags$span(
                      "Para poder generar estos apéndices debe cargar la base de datos con las parcelas
                      de inventario floristico y las capas de rodales y predios.
                      De forma opcional puede cargar la base de datos con las parcelas de cobertura y/o
                      transectos para el caso de los PAS 151."
                    )
                  ),
                  tags$div(style = "margin-top: 15px"),
                  shinyWidgets::materialSwitch(
                    inputId = "add_bd_pcob",
                    label = "¿Desea incluir parcelas de cobertura?",
                    status = "success"
                  ),
                  uiOutput("add_bd_pcob_ui"),
                  uiOutput("add_bd_trans_ui_lgl"),
                  uiOutput("join_by_ui"),
                  tags$div(
                    id = "flex",
                    shinyWidgets::actionBttn(
                      inputId = "get_apendices_2y3_btn",
                      label = "Obtener Apéndices 2 y 3",
                      style = "unite",
                      size = "sm",
                      color = "success"
                    ),
                    mod_downfiles_ui("down_apendices_2", label = "Apéndice 2", style = "material-flat"),
                    mod_downfiles_ui("down_apendices_3", label = "Apéndice 3", style = "material-flat")
                  ),
                  # tags$br(),
                  tags$div(style = "margin-top: 5px"),
                  tags$hr(style = "height:2px;border-width:0;color:gray;background-color:gray"),
                  tags$br(),

                  ### Apendice 5 ----
                  tags$h4("Apéndice 5 - Tablas formulario CONAF", style = "font-weight: bold;"),
                  tags$div(
                    style = "color: #0461b8; font-size: 16px",
                    icon("circle-info"),
                    tags$span(
                      "Para poder generar este apéndice debe de haber generado previamente la
                      cartografía digital cargando la base de datos con las parcelas de
                      inventario floristico. De forma opcional puede cargar la capa de obras."
                    )
                  ),
                  tags$div(style = "margin-top: 15px"),
                  uiOutput("form_nuevo_ui"),
                  modal_umbral_sp_est,
                  tags$div(
                    id = "flex",
                    tags$div(
                      bsplus::shiny_iconlink() %>%
                        bsplus::bs_attach_modal(id_modal = 'ayuda_umbral_sp_est'),
                      style = "margin-right: 5px"
                    ),
                    tags$div(
                      id = "inline",
                      shinyWidgets::formatNumericInput(
                        inputId = "umbral_sp_est",
                        label = "Porcentaje umbral",
                        value = 0.05,
                        width = "50px",
                        format = "percentageEU2dec"
                      )
                    ),
                    tags$div(style = "margin-left: 20px"),
                    shinyWidgets::actionBttn(
                      inputId = "get_tabla_attr_rod",
                      label = "Generar atributación de rodales",
                      style = "unite",
                      size = "sm",
                      color = "success"
                    ) %>%
                      bsplus::bs_embed_tooltip(title = "Obtener tabla de atributación de rodales"),
                    mod_downfiles_ui(
                      "tabla_attr_rodal_0",
                      label = "Descargar",
                      style = "material-flat",
                      icon = "file-excel"
                    ),
                    style = "align-items: center;"
                  ),
                  tags$div(style = "margin-top: 15px"),
                  fileInput(
                    inputId = "tabla_attr_rodal",
                    label = "Ingresar tabla con los atributos de rodal definitiva y revisada",
                    multiple = F,
                    accept = c(".xlsx"),
                    buttonLabel = "Seleccionar",
                    placeholder = "Archivo no seleccionado"
                  ) %>%
                    add_help_text(title = "Cargar con los mismos atributos de como se descargó"),
                  tags$div(style = "margin-top: -10px"),
                  shinyWidgets::materialSwitch(
                    inputId = "add_bd_flora_2",
                    label = "¿Atributar con parcelas de LB?",
                    status = "success"
                  ),
                  uiOutput("add_bd_flora_2_ui"),
                  mod_read_sf_ui("obras_ap5", "Ingresar obras (opcional)")  %>%
                    add_help_text(title = "Debe haber cargado la capa de áreas de corta"),
                  tags$div(style = "margin-top: -10px"),
                  tags$div(
                    id = "flex",
                    shinyWidgets::actionBttn(
                      inputId = "get_apendice_5_btn",
                      label = "Obtener tablas formulario",
                      style = "unite",
                      size = "sm",
                      color = "success"
                    ),
                    mod_downfiles_ui("down_apendice_5", label = "Apéndice 5", style = "material-flat")
                  )
                ) # final box apendices
              ) # final col apendices
            ) # final fluidRow carto-apéndices
          ) # final tabItem carto-apéndices
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path bundle_resources favicon
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "PAS148y151"
    ),
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css"),
    # Add here other external resources
    shinyjs::useShinyjs()
    # for example, you can add shinyalert::useShinyalert()
  )
}
