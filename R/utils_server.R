#' server
#'
#' @description A utils data
#'
#' @return The return value, if any, from executing the utility.
#'
#' @importFrom sf read_sf st_drop_geometry
#' @importFrom dplyr group_by tally ungroup mutate_at summarise arrange pull
#' @import dataPAS
#' @noRd
provincias_list <- sf::read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>%
  sf::st_drop_geometry() %>%
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
