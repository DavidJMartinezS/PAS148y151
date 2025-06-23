#' Prepare inputs
#'
#' @param BD data.frame o ruta con la base de datos
#' @param rodales objeto sf con los rodales
#' @param cut_by_rod logical. \code{TRUE} para seleccionar parcelas dentro de rodales
#'
#' @return Inputs formateados
#' @export
#'
prepare_bd_flora <- function(BD, rodales, PAS, cut_by_rod) {
  stopifnot(c("Nom_Predio", "N_Rodal", "Tipo_fores", "Tipo_For", "Subtipo_fo", "Tipo_veg") %in% names(rodales) %>% all() & inherits(rodales, "sf"))
  stopifnot("Parameter must be logical" = is.logical(cut_by_rod))
  stopifnot(
    "BD debe ser un data.frame o bien la ruta del archivo" =
      (class(BD) %in% c("character", "data.frame")) %>% any() & !inherits(BD, "sf")
  )
  if (inherits(BD, "character")) {
    stopifnot("ruta del archivo no encontrada" = file.exists(BD) & tools::file_ext(BD) == "xlsx")
  }
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))

  if (inherits(BD, "data.frame")) BD else openxlsx2::read_xlsx(BD) %>%
    dplyr::rename_all(
      ~ stringi::stri_trans_totitle(
        stringi::stri_trans_general(.,"Latin-ASCII"),
        opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")
      )) %>%
    dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("cob_bb", strength = 1), ~ "Cob_BB") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("punto", strength = 1), ~ "Parcela") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("p500", case_insensitive = T), ~ "N_ind") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ds.*68", case_insensitive = T), ~ "DS_68") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("forma.*vida|habito", case_insensitive = T), ~ "Habito") %>%
    dplyr::rename_at(
      dplyr::vars(dplyr::matches("^rce"), dplyr::contains("UTM"), dplyr::matches("ds_68")),
      stringi::stri_trans_toupper
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::contains("Cob_BB")),
      ~ stringi::stri_trim(stringi::stri_trans_tolower(.))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("Especie")),
      ~ stringi::stri_trim(stringi::stri_trans_totitle(., opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")))
    ) %>%
    dplyr::mutate_at("N_ind", as.integer) %>%
    dplyr::filter(!Especie %>% stringi::stri_detect_regex("identificac|indeter", case_insensitive = T)) %>%
    {if (PAS %in% c(148, 149)) {
      .[] %>%
        dplyr::filter(
          Habito %>%
            stringi::stri_trans_general("Latin-ASCII") %>%
            stringi::stri_detect_regex("arbol", case_insensitive = T),
          !Cob_BB %>%
            stringi::stri_trans_tolower() %in% c(NA_character_, "fp", "---"),
          !N_ind %in% c(NA, 0)
        )
    } else {
      .[] %>%
        dplyr::filter(
          DS_68 %>% stringi::stri_cmp_equiv("originaria", strength = 1),
          !Cob_BB %>% stringi::stri_trans_tolower() %in% c(NA_character_, "fp", "---"),
          !N_ind %in% c(NA, 0)
        )
    }} %>%
    dplyr::select(-dplyr::matches("Nom_Predio|N_Rodal|Tipo_veg|Tipo_fores|Tipo_For|Subtipo_fo")) %>%
    sf::st_as_sf(coords = c("UTM_E","UTM_N"), crs = sf::st_crs(rodales), remove = F) %>%
    {if (cut_by_rod) {
      .[] %>% sf::st_intersection(sf::st_union(rodales))
    } else .} %>%
    sf::st_join(rodales %>% dplyr::select(Nom_Predio, N_Rodal, Tipo_fores, Tipo_For, Subtipo_fo, Tipo_veg), join = st_nearest_feature) %>%
    dplyr::mutate_at("N_Rodal", as.integer) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate_at("N_ind", as.integer) %>%
    dplyr::mutate(Nha = N_ind * 20) %>%
    dplyr::arrange(N_Rodal) %>%
    dplyr::group_by(Parcela, UTM_E, UTM_N) %>%
    dplyr::arrange(N_Rodal) %>%
    dplyr::mutate(N = dplyr::cur_group_id()) %>%
    dplyr::group_by(N_Rodal, N) %>%
    dplyr::mutate(N_Parc = dplyr::cur_group_id()) %>%
    dplyr::ungroup()
}

#' @export
prepare_bd_pcob <- function(BD) {
  stopifnot(
    "BD debe ser un data.frame o bien la ruta del archivo" =
      (class(BD) %in% c("character", "data.frame")) %>% any() & !inherits(BD, "sf")
  )
  if (inherits(BD, "character")) {
    stopifnot("ruta del archivo no encontrada" = file.exists(BD) & tools::file_ext(BD) == "xlsx")
  }

  if (inherits(BD, "data.frame")) BD else openxlsx2::read_xlsx(BD) %>%
    janitor::clean_names() %>%
    dplyr::rename_all( ~ ifelse(
      . == "geometry",.,
      stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")
      )
    )) %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^copa.*ns|^diametro.*1", case_insensitive = T), ~ "Copa_NS") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^copa.*eo|^diametro.*2", case_insensitive = T), ~ "Copa_EO") %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("UTM")), stringi::stri_trans_toupper) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Copa")), stringi::stri_replace_all_regex, "\\,", "\\.") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Copa")), as.numeric)
}

#' @export
prepare_bd_trans <- function(BD) {
  stopifnot(
    "BD debe ser un data.frame o bien la ruta del archivo" =
      (class(BD) %in% c("character", "data.frame")) %>% any() & !inherits(BD, "sf")
  )
  if (inherits(BD, "character")) {
    stopifnot("ruta del archivo no encontrada" = file.exists(BD) & tools::file_ext(BD) == "xlsx")
  }

  if (inherits(BD, "data.frame")) BD else openxlsx2::read_xlsx(BD) %>%
    janitor::clean_names() %>%
    dplyr::rename_all( ~ if_else(
      . == "geometry",.,
      stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")
      )
    )) %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("habito|forma.*vida", case_insensitive = T), ~ "Habito") %>%
    dplyr::mutate_at(dplyr::vars(Cuenta, Denominador, Cobertura), as.numeric)
}
