#' leer_sf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom sf read_sf st_zm st_make_valid
#'
mod_leer_sf_ui <- function(id, ...) {
  ns <- NS(id)
  shiny::fileInput(
    ns("sf_file"),
    ...,
    accept = c('.shp','.dbf','.shx',".prj"),
    multiple = TRUE,
    buttonLabel = "Seleccionar",
    placeholder = "Archivo no seleccionado"
  )
}

#' leer_sf Server Functions
#'
#' @noRd
mod_leer_sf_server <- function(id, crs = NULL, fx = NULL, path = F, ...){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    reactive({
      req(input$sf_file)
      if(!(input$sf_file$datapath %>% tools::file_ext() %in% c("dbf", "prj", "shp", "shx") %>% all() &
           input$sf_file$name %>% tools::file_path_sans_ext() %>% unique() %>% length() == 1 &
           length(input$sf_file$datapath) >= 4)) {
        unlink(input$sf_file$datapath)
        reset(id = "sf_file")
        return(NULL)
      } else {
        if (path) {
          return(input$sf_file$name[1] %>% tools::file_path_sans_ext())
        } else {
          shpdf <- input$sf_file
          tempdirname <- dirname(shpdf$datapath[1])
          for (i in 1:nrow(shpdf)) {
            file.rename(
              shpdf$datapath[i],
              paste0(tempdirname, "/", shpdf$name[i])
            )
          }
          shp <- sf::read_sf(paste(tempdirname, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"), ...) %>%
            sf::st_zm() %>%
            sf::st_make_valid() %>%
            {if (!is.null(fx)) .[] %>% fx() else .} %>%
            {if (!is.null(crs)) .[] %>% st_transform(crs) else .}
          return(shp)
        }
      }
    })
  })
}

## To be copied in the UI
# mod_leer_sf_ui("leer_sf_1")

## To be copied in the server
# mod_leer_sf_server("leer_sf_1")
