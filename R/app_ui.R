#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @importFrom shiny tagList tags numericInput HTML fluidRow column
#' @importFrom shinydashboard sidebarMenu menuItem tabItems tabItem
#' @importFrom shinydashboardPlus dashboardPage dashboardHeader userOutput dashboardSidebar box
#' @importFrom shinyEffects setShadow
#' @importFrom shinyWidgets radioGroupButtons dropdownButton prettyRadioButtons pickerInput pickerOptions
#' @importFrom shinyjs useShinyjs
#' @importFrom bsplus use_bs_popover use_bs_tooltip
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboardPlus::dashboardPage(
      skin = "green",
      shinydashboardPlus::dashboardHeader(
        leftUi = tagList(
          tags$div(
            shinyWidgets::radioGroupButtons(
              inputId = "PAS",
              choiceNames = c("PAS 148", "PAS 151"),
              choiceValues = c(148, 151),
              selected = 148,
              status = "warning"
            ),
            style = "margin-left: 10px;"
          ),
          shinyWidgets::dropdownButton(
            label = "Ajustes bases",
            icon = icon("gear"),
            status = "warning",
            circle = F,
            tags$span(
              id = "flex",
              shinyWidgets::prettyRadioButtons(
                inputId = "huso",
                label = "Huso:  ",
                choices = c("18S", "19S"),
                selected = "19S",
                inline = TRUE,
                status = "success",
                fill = TRUE,
                animation = "smooth"
              ),
              tags$div(
                numericInput(
                  inputId = "n_dec",
                  label = "N° decimales superficie:",
                  value = 2,
                  min = 0,
                  max = 4
                ),
                style = "margin-left: 20px;"
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
              options = shinyWidgets::pickerOptions(container = "body", style = "btn-warning"),
            ),
            style = "color: white; margin-left: 10px;"
          )
        ),
        title = tagList(
          tags$span(class = "logo-lg", "PAS 148"),
          tags$img(src = "logo_geobiota.png")
        ),
        tags$li(
          class = "dropdown",
          id = "logo",
          tags$a(tags$img(height = "40px", src = "logo-header.svg")),
          tags$style(
            HTML(
              "#logo {
              position: absolute;
              left: 50%;
              top: 50%;
              transform: translate(-50%, -50%);
            }
           #logo > a:hover {
              background-color: transparent !important;
              color: transparent !important;
            }"
            )
          )
        ),
        shinydashboardPlus::userOutput("user")
      ),
      shinydashboardPlus::dashboardSidebar(
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Importante", tabName = "importante", icon = icon("circle-info")),
          shinydashboard::menuItem("Ayuda cartográfica", tabName = "ayuda", icon = icon("circle-check")),
          shinydashboard::menuItem("Cartografáa y Apéndices", tabName = "carto", icon = icon("layer-group"))
        )
      ),
      shinydashboard::dashboardBody(
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
                width = 7,
                shinydashboardPlus::box(
                  title = "Uso adecuado del Dashboard",
                  width = 12,
                  info_dashboard()
                )
              ),
              column(
                width = 5,
                shinydashboardPlus::box(
                  width = 12,
                  title = "Tipos de corte para caminos, hidrografáa y curvas de nivel",
                  solidHeader = T,
                  status = "success",
                  height = "400px",
                  info_cut_buffer()
                )
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "ayuda",
            fluidRow(
              col_6(
                shinydashboardPlus::box(
                  width = 12,
                  title = "Generar Predios, rodales y areas de corta",
                  solidHeader = T,
                  status = "success",
                  mod_PredRodArea_ui("PredRodArea")
                )
              ),
              col_6(
                shinydashboardPlus::box(
                  width = 12,
                  solidHeader = T,
                  status = "success",
                  title = "Ayudas cartográficas",
                  mod_st_order_ui("st_order"),
                  mod_check_carto_ui("check_carto"),
                  mod_add_attr_ui("add_attr")
                )
              )
            )
          ),
          shinydashboard::tabItem(
            tabName = "carto",
            fluidRow(
              col_6(
                shinydashboardPlus::box(
                  width = 12,
                  title = "Generar cartograía digital",
                  solidHeader = T,
                  status = "success",
                  mod_get_carto_digital_ui("carto_digital"),
                  tableOutput("tbl_flora")
                )
              ),
              col_6(
                shinydashboardPlus::box(
                  width = 12,
                  title = "Generar Apéndices",
                  solidHeader = T,
                  status = "success",
                  mod_apendices_ui("apendices")
                )
              )
            )
          )
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
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
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
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
