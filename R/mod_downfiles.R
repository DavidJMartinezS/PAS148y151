#' downfiles UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList div downloadHandler reactive
#' @importFrom shinyWidgets downloadBttn
#' @importFrom shinyjs enable disable
#' @importFrom sf write_sf
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom purrr map pwalk
#' @importFrom zip zip
#'
mod_downfiles_ui <- function(id, style = "material-circle", icon = "download", ...) {
  ns <- NS(id)
  tagList(
    div(
      shinyWidgets::downloadBttn(
        outputId = ns("downfile"),
        style = style,
        size = "sm",
        color = "success",
        icon = icon(icon),
        ...
      ),
      style = "margin-left: 15px;"
    )
  )
}

#' downfiles Server Functions
#'
#' @noRd
mod_downfiles_server <- function(id, x, name_save){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    shinyjs::disable("downfile_bttn")
    observeEvent(x,{shinyjs::enable("downfile_bttn")})

    filetype <- reactive({
      x %>%
        {if(any(class(.) == "list")) . else list(.)} %>%
        purrr::map( ~ ifelse(
          inherits(., "wbWorkbook"),
          "wb",
          ifelse(
            inherits(., "sf"),
            "sf",
            ifelse(inherits(., "data.frame") & !inherits(., "sf"), "xlsx", "")
          )
        )) %>%
        {if(length(.) == 1) unlist(.) else .}
    })

    output$downfile <- downloadHandler(
      filename = function() {
        if (length(filetype()) > 1) {
          paste0("Archivos_comprimidos",".zip")
        } else {
          paste0(as.character(name_save), ifelse(filetype() == "sf", ".zip", ".xlsx"))
        }
      },
      content = function(file) {
        temp_dir <- tempdir()
        setwd(temp_dir)
        file.remove(list.files(temp_dir))
        purrr::pwalk(
          if(length(filetype()) == 1) list(list(x), list(filetype()), list(name_save)) else list(x, filetype(), name_save),
          .f = function(x, y, z) {
            switch(
              y,
              sf = sf::write_sf(x, paste0(tools::file_path_sans_ext(z), ".shp")),
              wb = openxlsx2::wb_save(x, paste0(tools::file_path_sans_ext(z), ".xlsx"), overwrite = T),
              xlsx = openxlsx2::write_xlsx(x, paste0(tools::file_path_sans_ext(z), ".xlsx"))
            )
          }
        )
        list_files <- unlist(map(name_save, function(x){list.files(temp_dir, pattern = x)}))
        if(tools::file_ext(file) == "zip") {
          zip::zip(zipfile = file, files = list_files)
        } else {
          file.copy(from = list_files, to = file, overwrite = T)
        }
      }
    )
  })
}

## To be copied in the UI
# mod_downfiles_ui("downfiles_1")

## To be copied in the server
# mod_downfiles_server("downfiles_1")
