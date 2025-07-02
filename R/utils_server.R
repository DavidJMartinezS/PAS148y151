#' server
#'
#' @description A utils data
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import dataPAS
#' @noRd
comunas_df <- sf::read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>%
  sf::st_drop_geometry()

#' @noRd
provincias_list <- comunas_df %>%
  dplyr::group_by(CUT_REG, REGION, PROVINCIA) %>%
  dplyr::tally() %>% dplyr::ungroup() %>%
  dplyr::mutate_at(
    "CUT_REG",
    ~factor(
      .,
      levels = c("15", "01", "02", "03", "04", "05", "13", "06", "07", "16", "08", "09", "14", "10", "11", "12")
    )
  ) %>%
  dplyr::group_by(CUT_REG, REGION) %>%
  dplyr::summarise(PROVINCIA = list(PROVINCIA)) %>%
  dplyr::mutate(PROVINCIA = setNames(PROVINCIA, REGION)) %>%
  dplyr::arrange(CUT_REG) %>%
  dplyr::pull(PROVINCIA)


#' @noRd
check_input <- function(x, names_req, huso = NULL, id_reset = NULL){
  ok <- T
  if(!all(names_req %in% names(sf::st_drop_geometry(x)))){
    shinybusy::report_failure(
      title = "Ups!",
      text = tags$p(
        "Shapefile sin los campos requeridos", rep_br(2),
        tags$b("Requeridos: "), paste(names_req %>% shQuote(), collapse = ", "), rep_br(2),
        tags$b("Faltan: "), paste(setdiff(names_req, names(sf::st_drop_geometry(x))) %>% shQuote(), collapse = ", ")
      )
    )
    if (!is.null(id_reset)) {
      shinyjs::reset(id = id_reset)
    }
    ok <- F
  }
  if (!is.null(huso)) {
    lon <- x %>% sf::st_union() %>% sf::st_centroid() %>% sf::st_transform(4326) %>% sf::st_coordinates() %>% .[,1]
    if ((lon >= -72 & huso == "18S") | (lon < -72 & huso == "19S")) {
      shinybusy::report_failure(
        title = "Ups!",
        text = "Coordenadas del shp no coinciden con la seleccionada"
      )
      ok <- F
    }
  }
  if (ok) {
    shinybusy::notify_success("Perfecto! Todos los campos necesarios :)", timeout = 3000, position = "right-bottom")
  }
}
