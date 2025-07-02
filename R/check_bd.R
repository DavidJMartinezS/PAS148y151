#' Chequeo Bases de Datos
#'
#' @param x Base de datos de flora
#' @param y Capa mascara para filtrar las parcelas
#' @param shiny Logico; si es \code{TRUE}, devuelve una alerta en un modulo de shiny.
#'
#' @return Devuelve alertas impresas en la consola o bien en modulos de shiny.
#' @rdname check_bd
#' @export
#'
check_bd_flora <- function(x, y = NULL){
  shiny <- shiny::isRunning()
  ok <- T

  if(!is.null(y) & all(c('UTM_E', 'UTM_N') %in% names(x))){
    if (x %>% dplyr::select(dplyr::starts_with("UTM")) %>% apply(2, function(x){!any(is.na(x)) & is.numeric(x)})) {
      x <- x %>%
        sf::st_as_sf(coords = c("UTM_E", "UTM_N"), crs = sf::st_crs(y), remove = F) %>%
        sf::st_intersection(sf::st_union(sf::st_collection_extract(y, "POLYGON"))) %>%
        sf::st_drop_geometry()
    } else {
      x <- x %>%
        dplyr::mutate_at(c("UTM_E", "UTM_N"), as.numeric) %>%
        tidyr::drop_na(UTM_E, UTM_N) %>%
        sf::st_as_sf(coords = c("UTM_E", "UTM_N"), crs = sf::st_crs(y), remove = F) %>%
        sf::st_intersection(sf::st_union(sf::st_collection_extract(y, "POLYGON"))) %>%
        sf::st_drop_geometry()
      if (shiny) {
        shinybusy::notify_warning(text = "Se quitaron los registros sin coordenadas", timeout = "5000", position = "right-bottom")
      } else {
        cat("\033[33mSe quitaron los registros sin coordenadas", "\n")
      }
    }
    if (any(sf::st_is(y, "POLYGON") | sf::st_is(y, "MULTIPOLYGON"))) {

    }
  } %>% suppressWarnings() %>% suppressMessages()
  # Verificar que estén los campos mínimos ----
  if (!all(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg', 'DS_68', 'RCE') %in% names(x))) {
    if (shiny) {
      check_input(
        x = x,
        names_req = c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg', 'DS_68', 'RCE')
      )
    } else {
      cat(
        "\033[31mProblemas!","\U0001FAE0","Shapefile sin los campos requeridos", "\n",
        "\033[34mRequeridos: ", paste(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg', 'DS_68', 'RCE') %>% shQuote(), collapse = ", "), "\n",
        "Faltan: ", paste(setdiff(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB', 'Tipo_veg', 'DS_68', 'RCE'), names(x)) %>% shQuote(), collapse = ", "), "\n"
      )
    }
    ok <- F
  }
  # Verificar coberturas Braun Blanquet ----
  if ("Cob_BB" %in% names(x)) {
    if (!x$Cob_BB %in% c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---") %>% all()) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Coberturas de Braun-Blanquet que no corresponden",
          text = tags$p(
            "Las coberturas de Braun-Blanquet deben limitarse a las siguientes nomenclaturas:",
            rep_br(1),
            paste0(
              c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---") %>% shQuote(),
              collapse = ", "
            ),
            rep_br(2),
            "Las siguientes no se reconocen:",
            paste0(
              setdiff(unique(x$Cob_BB), c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---")) %>% shQuote(),
              collapse = ", "
            )
          ),
          html = TRUE,
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
            c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---") %>% shQuote(),
            collapse = ", "
          ),"\n\n",
          "Las siguientes no se reconocen:",
          paste0(
            setdiff(unique(x$Cob_BB), c("fp", "r", "+", "1", "2", "3", "4", "5", "6", "---")) %>% shQuote(),
            collapse = ", "
          ), "\n"
        )
      }
    }
  }
  # Verificar coordenadas ----
  if (all(c("Parcela", "UTM_E", "UTM_N") %in% names(x))) {
    ## UTM_E y UTM_N ----
    if (
      !x$UTM_E %>%
      stringi::stri_count_regex("\\w") %>%
      all(. == 6) ||
      !x$UTM_N %>%
      stringi::stri_count_regex("\\w") %>%
      all(. == 7)
    ) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Error en coordenadas",
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
          html = TRUE,
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
      ok <- F
    }
    ## Verificar coordenadas repetidas ----
    if (
      !x %>%
      dplyr::count(Parcela, UTM_E, UTM_N) %>%
      dplyr::count(Parcela, sort = T) %>%
      dplyr::pull(n) %>%
      all(. == 1)
    ) {
      if (shiny) {
        shinyalert::shinyalert(
          title = "Mismas parcela, diferentes coordenadas!",
          text = tags$p(
            "Las siguientes parcelas presentan mas de una coordenada:",
            rep_br(2),
            paste0(
              x %>%
                dplyr::count(Parcela, UTM_E, UTM_N) %>%
                dplyr::count(Parcela) %>%
                dplyr::filter(n > 1) %>%
                dplyr::pull(Parcela) %>%
                shQuote(),
              collapse = ", "
            )
          ),
          html = TRUE,
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
              dplyr::filter(n > 1) %>%
              dplyr::pull(Parcela) %>%
              shQuote(),
            collapse = ", "
          ),"\n"
        )
      }
      ok <- F
    }
    ## Especies repetidas por parcela ----
    if ("Especie" %in% names(x)) {
      if (
        !x %>%
        dplyr::count(Parcela, UTM_E, UTM_N, Especie) %>%
        dplyr::pull(n) %>%
        all(. == 1)
      ) {
        if (shiny) {
          shinyalert::shinyalert(
            title = "Registro duplicado de especies en una parcela!",
            text = tags$p(
              "Las siguientes parcelas presentan el registro de más de una especie en una parcela:",
              rep_br(2),
              paste0(
                x %>%
                  dplyr::count(Parcela, UTM_E, UTM_N, Especie) %>%
                  dplyr::filter(n > 1) %>%
                  dplyr::pull(Parcela) %>%
                  unique() %>%
                  shQuote(),
                collapse = ", "
              )
            ),
            html = TRUE,
            type = "error",
            closeOnEsc = T,
            showConfirmButton = T,
            animation = T
          )
        } else {
          cat(
            "\033[31mProblemas!","\U0001FAE0", "Registro duplicado de especies en una parcela!", "\n",
            "\033[34mLas siguientes parcelas presentan el registro de más de una especie en una parcela:", "\n",
            paste0(
              x %>%
                dplyr::count(Parcela, UTM_E, UTM_N, Especie) %>%
                dplyr::filter(n > 1) %>%
                dplyr::pull(Parcela) %>%
                unique() %>%
                shQuote(),
              collapse = ", "
            ),"\n"
          )
        }
        ok <- F
      }
    }
  } %>% suppressWarnings() %>% suppressMessages()

  # OK ----
  if (ok) {
    if (shiny) {
      shinybusy::notify_success("Perfecto!", timeout = 3000, position = "right-bottom")
    } else {
      cat("\033[32mPerfecto!","\U0001F601", "No se han encontrado observaciones", "\n")
    }
  }
}
