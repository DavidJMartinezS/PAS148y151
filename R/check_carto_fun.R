#' Check cartography
#'
#' @description
#' Funcion para chequear la presencia y orden de los campos de la cartografia digital en base a los requerimientos tecnicos de CONAF.
#'
#' @param x Objeto sf.
#' @param id Nombre de la capa.
#' @param shiny Logico; si es \code{TRUE}, devuelve una alerta en un modulo de shiny.
#'
#' @return Devuelve alguna alerta en la consola o en un modulo de shiny
#' @export
#'
check_carto <- function(x, id, shiny = F){
  list_check <- list(
    "Áreas" = c("Nom_Predio", "N_Area", "Tipo_Bos", "Sup_ha", "Fuente"),
    "Caminos" = c("Nom_Predio", "Tipo_Cam", "Fuente"),
    "Curvas de nivel" = c("Nom_Predio","Cot_Curva", "Fuente"),
    "Hidrografía" = c("Nom_Predio", "Tip_Dren", "Tipo_Perma", "Fuente"),
    "Límite predial" = c("Nom_Predio", "Rol", "Comuna", "Sup_ha", "Fuente"),
    "Parcelas" = c("Nom_Predio", "N_Rodal", "N_Parc", "Coord_X", "Coord_Y", "Fuente"),
    "Puntos de referencia" = c("Nom_Predio", "Nom_Pto", "Coord_X", "Coord_Y", "Fuente"),
    "Rangos de pendiente" = c("Nom_Predio", "Ran_Pend", "Sup_ha", "Fuente"),
    "Rodales" = c("Nom_Predio", "N_Rodal", "Tipo_Bos", "Tipo_For", "Sup_ha", "Fuente"),
    "Señaletica de incendios" = c("Nom_Predio", "Nom_Pto", "Coord_X", "Coord_Y", "Fuente"),
    "Suelos" = c("Nom_Predio", "Clase_Uso", "Sup_ha", "Fuente"),
    "Uso actual" = c("Nom_Predio", "Uso_Actual", "Sup_ha", "Fuente")
  )
  stopifnot(id %in% names(list_check))
  stopifnot(inherits(x, "sf"))

  names_req <- list_check[[id]]
  names_act <- x %>% sf::st_drop_geometry() %>% names()
  if((names_req %in% names_act) %>% all()){
    if((names_req == names_act) %>% all()){
      if (shiny) {
        shinybusy::report_success(
          title = "Perfecto!",
          text = "Campos en concordancia con los requerimientos de CONAF"
        )
      } else {
        cat("\033[32mPerfecto!","\U0001F601", "Campos en concordancia con los requerimientos de CONAF", "\n")
      }
    } else if(length(names_act) > length(names_req)){
      if (shiny) {
        shinybusy::report_warning(
          title = "Problemas!",
          text = paste0(
            "Shapefile con otros campos adicionales a los requeridos ", tags$br(), tags$br(),
            tags$b("Requeridos: "), paste0(names_req %>% shQuote(), collapse = ", "), tags$br(), tags$br(),
            tags$b("Sobran: "), paste0(setdiff(names_req, names_act) %>% shQuote(), collapse = ", ")
          )
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0", "Shapefile con otros campos adicionales a los requeridos", "\n",
          "\033[34mRequeridos: ", paste0(names_req %>% shQuote(), collapse = ", "), "\n",
          "Faltan: ", paste0(setdiff(names_req, names_act) %>% shQuote(), collapse = ", "), "\n"
        )
      }
    } else {
      if (shiny) {
        shinybusy::report_warning(
          title = "Problemas!",
          text = paste0(
            "Shapefile con los campos requeridos pero desordenados", tags$br(), tags$br(),
            tags$b("\nActual: "), paste0(names_act %>% shQuote(), collapse = ", "), tags$br(), tags$br(),
            tags$b("\nCorrecto: "), paste0(names_req %>% shQuote(), collapse = ", ")
          )
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0", "Shapefile con los campos requeridos pero desordenados", "\n",
          "\033[34mActual: ", paste0(names_act %>% shQuote(), collapse = ", "), "\n",
          "Correcto: ", paste0(names_req %>% shQuote(), collapse = ", "), "\n"
        )
      }
    }
  } else {
    if (shiny) {
      shinybusy::report_failure(
        title = "Error!",
        text = paste0(
          "Shapefile sin los campos requeridos", tags$br(), tags$br(),
          tags$b("Requeridos: "), paste0(names_req %>% shQuote(), collapse = ", "), tags$br(), tags$br(),
          tags$b("Faltan: "), paste0(setdiff(names_req, names_act) %>% shQuote(), collapse = ", ")
        )
      )
    } else {
      cat(
        "\033[31mError!","\U0001FAE0","Shapefile sin los campos requeridos","\n",
        "\033[34mRequeridos: ", paste0(names_act %>% shQuote(), collapse = ", "), "\n",
        "faltan: ", paste0(setdiff(names_act, names_req) %>% shQuote(), collapse = ", "), "\n"
      )
    }
  }
}
