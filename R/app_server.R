#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @importFrom shiny reactive reactiveValues renderTable fluidRow tags
#' @importFrom shinydashboardPlus renderUser dashboardUser socialButton
#' @importFrom bsplus bs_embed_tooltip
#' @noRd
app_server <- function(input, output, session) {
  options(shiny.maxRequestSize = 4000 * 1024 ^ 2, timeout = 600)

  crs <- reactive({
    ifelse(input$huso == "18S", 32718, 32719)
  })

  inputs <- reactiveValues(bd_flora = NULL, areas_def = NULL, rodales_def = NULL)
  carto <- reactiveValues(tabla_predio = NULL, tabla_areas = NULL, uso_actual = NULL)

  mod_PredRodArea_server(
    "PredRodArea",
    crs = crs(),
    dec_sup = input$n_dec,
    provincia = input$provincia,
    PAS = input$PAS
  )
  mod_st_order_server("st_order")
  mod_check_carto_server("check_carto")
  mod_add_attr_server("add_attr")
  mod_get_carto_digital_server(
    "carto_digital",
    crs = crs(),
    dec_sup = input$n_dec,
    huso = input$huso,
    inputs = inputs,
    carto = carto
  )
  mod_apendices_server(
    "apendices",
    PAS = input$PAS,
    provincia = input$provincia,
    huso = input$huso,
    crs = crs(),
    inputs = inputs,
    carto = carto
  )

  output$tbl_flora <- renderTable({carto$bd_flora %>% head()})

  output$user <- shinydashboardPlus::renderUser({
    shinydashboardPlus::dashboardUser(
      name = "David Martínez",
      image = "https://avatars.githubusercontent.com/u/74486319?s=400&u=c277213b232af5e7710bebdc7a50bb9426ab9a62&v=4",
      title = "Dashboard PAS 148-151",
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
