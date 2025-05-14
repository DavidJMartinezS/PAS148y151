#' Chequeo Bases de Datos
#'
#' @param x Base de datos de flora
#' @param y Capa
#' @param shinyalert Logico; usar \code{TRUE} en una shiny app. Default \code{FALSE}
#'
#' @return Devuelve alertas impresas en la consola o bien en modulos de shiny.
#' @rdname check_bd
#' @export
#'
#' @importFrom shiny tags
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy notify_success
#' @importFrom sf st_is st_as_sf st_crs st_intersection st_union st_collection_extract st_drop_geometry
#' @importFrom dplyr count filter pull
check_bd_flora <- function(x, y = NULL, shinyalert = F){
  if(!is.null(y) & all(c('UTM_E', 'UTM_N') %in% names(x))){
    if (any(sf::st_is(y, "POLYGON") | sf::st_is(y, "MULTIPOLYGON"))) {
      x <- x %>%
        sf::st_as_sf(coords = c("UTM_E", "UTM_N"), crs = sf::st_crs(y), remove = F) %>%
        sf::st_intersection(sf::st_union(sf::st_collection_extract(y, "POLYGON"))) %>%
        sf::st_drop_geometry()
    }
  } %>% suppressWarnings()
  # Verificar que estén los campos mínimos ----
  if (!all(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg', 'DS_68', 'RCE') %in% names(x))) {
    if (shinyalert) {
      shinyalerta(
        names_act = names(x),
        names_req = c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg', 'DS_68', 'RCE')
      )
    } else {
      cat(
        "\033[31mProblemas!","\U0001FAE0","Shapefile sin los campos requeridos","\n",
        "\033[34mRequeridos: ", paste0(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg', 'DS_68', 'RCE') %>% shQuote(), collapse = ", "), "\n",
        "Faltan: ", paste0(setdiff(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg', 'DS_68', 'RCE'), names(x)) %>% shQuote(), collapse = ", "),
        "\n"
      )
    }
  } else {
    # Verificar coberturas ----
    if (!x$Cob_BB %in% c("fp", "r", "+", "1", "2", "3", "4", "5", "---") %>% all()) {
      if (shinyalert) {
        shinyalert::shinyalert(
          title = "Coberturas de Braun-Blanquet que no corresponden",
          html = TRUE,
          text = tags$p(
            "Las coberturas de Braun-Blanquet deben limitarse a las siguientes nomenclaturas:",
            rep_br(2),
            paste0(
              c("fp", "r", "+", "1", "2", "3", "4", "5", "---") %>% shQuote(),
              collapse = ", "
            )
          ),
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0","Coberturas de Braun-Blanquet que no corresponden","\n",
          "\033[34mLas coberturas de Braun-Blanquet deben limitarse a las siguientes nomenclaturas:","\n",
          paste0(
            c("fp", "r", "+", "1", "2", "3", "4", "5", "---") %>% shQuote(),
            collapse = ", "
          ),"\n"
        )
      }
    }
    # Verificar coordenadas
    if (
      !x$UTM_E %>%
      stringi::stri_count_regex("\\w") %>%
      all(. == 6) ||
      !x$UTM_N %>%
      stringi::stri_count_regex("\\w") %>%
      all(. == 7)
    ) {
      if (shinyalert) {
        shinyalert::shinyalert(
          title = "Error en coordenadas",
          html = TRUE,
          text = tags$p(
            "Revisar las coordenadas de las siguientes parcelas:", rep_br(2),
            paste0(
              x$Parcela[which(!x$UTM_E %>% stringi::stri_count_regex("\\w") %>% .[] == 6)] %>%
                c(x$Parcela[which(!x$UTM_N %>% stringi::stri_count_regex("\\w") %>% .[] == 7)]) %>%
                unique() %>%
                shQuote(),
              collapse = ", "
            )
          ),
          type = "error",
          closeOnEsc = T,
          showConfirmButton = T,
          animation = T
        )
      } else {
        cat(
          "\033[31mProblemas!","\U0001FAE0", "Error en coordenadas", "\n",
          "\033[34mRevisar las coordenadas de las siguientes parcelas:","\n",
          paste0(
            x$Parcela[which(!x$UTM_E %>% stringi::stri_count_regex("\\w") %>% .[] == 6)] %>%
              c(x$Parcela[which(!x$UTM_N %>% stringi::stri_count_regex("\\w") %>% .[] == 7)]) %>%
              unique() %>%
              shQuote(),
            collapse = ", "
          ),"\n"
        )
      }
    }
    if (c("Campana", "Cuadrilla") %in% names(x) %>% all()) {
      # Verificar coordenadas repetidas
      if (
        x %>%
        dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
        dplyr::count(Campana, Cuadrilla, Parcela, sort = T) %>%
        dplyr::filter(n > 1) %>%
        nrow() %>%
        .[] >= 1
      ) {
        if (shinyalert) {
          shinyalert::shinyalert(
            title = "Mismas parcela, diferentes coordenadas!",
            html = TRUE,
            text = tags$p(
              "Las siguientes parcelas presentan mas de una coordenada teniendo la misma campaña y cuadrilla:",
              rep_br(2),
              str_c(
                x %>%
                  dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
                  dplyr::count(Campana, Cuadrilla, Parcela, sort = T) %>%
                  dplyr::filter(n > 1) %>%
                  dplyr::pull(Parcela) %>%
                  shQuote(),
                collapse = ", "
              )
            ),
            type = "error",
            closeOnEsc = T,
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes coordenadas!", "\n",
            "\033[34mLas siguientes parcelas presentan mas de una coordenada teniendo la misma campaña y cuadrilla:","\n",
            paste0(
              x %>%
                dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
                dplyr::count(Campana, Cuadrilla, Parcela, sort = T) %>%
                dplyr::filter(n > 1) %>%
                dplyr::pull(Parcela) %>%
                shQuote(),
              collapse = ", "
            ),"\n"
          )
        }
      }
      # Verificar cuadrillas repetidas
      if (
        x %>%
        dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
        dplyr::count(Campana, Parcela, UTM_E, UTM_N, sort = T) %>%
        dplyr::filter(n > 1) %>%
        nrow() %>%
        .[] >= 1
      ) {
        if (shinyalert) {
          shinyalert::shinyalert(
            title = "Mismas parcela, diferentes cuadrillas!",
            html = TRUE,
            text = tags$p(
              "Las siguientes parcelas presentan mas de una cuadrilla teniendo la misma campaña y coordenada:",
              tags$br(),  tags$br(),
              str_c(
                x %>%
                  dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
                  dplyr::count(Campana, Parcela, UTM_E, UTM_N, sort = T) %>%
                  dplyr::filter(n > 1) %>%
                  dplyr::pull(Parcela) %>%
                  shQuote(),
                collapse = ", "
              )
            ),
            type = "error",
            closeOnEsc = T,
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes cuadrillas!", "\n",
            "\033[34mLas siguientes parcelas presentan mas de una cuadrilla teniendo la misma campaña y coordenada:", "\n",
            str_c(
              x %>%
                dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
                dplyr::count(Campana, Parcela, UTM_E, UTM_N, sort = T) %>%
                dplyr::filter(n > 1) %>%
                dplyr::pull(Parcela) %>%
                shQuote(),
              collapse = ", "
            ),"\n"
          )
        }
      }
      # Verificar campañas repetidas
      if (
        x %>%
        dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
        dplyr::count(Cuadrilla, Parcela, UTM_E, UTM_N, sort = T) %>%
        dplyr::filter(n > 1) %>%
        nrow() %>%
        .[] >= 1
      ) {
        if (shinyalert) {
          shinyalert::shinyalert(
            title = "Mismas parcela, diferentes campañas!",
            html = TRUE,
            text = tags$p(
              "Las siguientes parcelas se repiten en más de una campaña:",
              rep_br(2),
              str_c(
                x %>%
                  dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
                  dplyr::count(Cuadrilla, Parcela, UTM_E, UTM_N, sort = T) %>%
                  dplyr::filter(n > 1) %>%
                  dplyr::pull(Parcela) %>%
                  shQuote(),
                collapse = ", "
              )
            ),
            type = "error",
            closeOnEsc = T,
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes campañas!", "\n",
            "\033[34mLas siguientes parcelas se repiten en más de una campaña:", "\n",
            paste0(
              x %>%
                dplyr::count(Campana, Cuadrilla, Parcela, UTM_E, UTM_N) %>%
                dplyr::count(Cuadrilla, Parcela, UTM_E, UTM_N, sort = T) %>%
                dplyr::filter(n > 1) %>%
                dplyr::pull(Parcela) %>%
                shQuote(),
              collapse = ", "
            ),"\n"
          )
        }
      } else {
        if (shinyalert) {
          shinybusy::notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
        } else {
          cat("\033[32mPerfecto!","\U0001F601", "No se han encontrado observaciones", "\n")
        }
      }
    } else {
      # Verificar coordenadas repetidas
      if (
        !x %>%
        dplyr::count(Parcela, UTM_E, UTM_N) %>%
        dplyr::count(Parcela, sort = T) %>%
        dplyr::pull(n) %>%
        all(. == 1)
      ) {
        if (shinyalert) {
          shinyalert::shinyalert(
            title = "Mismas parcela, diferentes coordenadas!",
            html = TRUE,
            text = tags$p(
              "Las siguientes parcelas presentan mas de una coordenada:",
              rep_br(2),
              paste0(
                x %>%
                  dplyr::count(Parcela, UTM_E, UTM_N) %>%
                  dplyr::count(Parcela) %>%
                  dplyr::filter(Parcela > 1) %>%
                  dplyr::pull(Parcela) %>%
                  shQuote(),
                collapse = ", "
              )
            ),
            type = "error",
            closeOnEsc = T,
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Mismas parcela, diferentes coordenadas!", "\n",
            "\033[34mLas siguientes parcelas presentan mas de una coordenada:", "\n",
            paste0(
              x %>%
                dplyr::count(Parcela, UTM_E, UTM_N) %>%
                dplyr::count(Parcela) %>%
                dplyr::filter(Parcela > 1) %>%
                dplyr::pull(Parcela) %>%
                shQuote(),
              collapse = ", "
            ),"\n"
          )
        }
      } else {
        if (shinyalert) {
          shinybusy::notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
        } else {
          cat("\033[32mPerfecto!","\U0001F601", "No se han encontrado observaciones", "\n")
        }
      }
    }
  } %>% suppressWarnings()
}
