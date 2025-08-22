#' Prepare BD inputs
#'
#' @description
#' Funciones para preparar formato de los datos ingresados.
#'
#' @param BD data.frame o ruta con la base de datos
#' @param rodales objeto sf con los rodales
#' @param PAS PAS correspondiente. Ingresar \code{148} o \code{151}.
#' @param cut_by_rod logical. \code{TRUE} para seleccionar parcelas dentro de rodales.
#' @param include_fp Incluir especies fuera de parcela. Incluye ademas especies cuya `Cob_BB` sea \code{NA} o \code{---}.
#'
#' @return Inputs formateados
#' @export
#' @name prepare_bd
#'
prepare_bd_flora <- function(BD, rodales, PAS, cut_by_rod, include_fp = FALSE) {
  valid_input(rodales, inherit = "sf", names = c("Nom_Predio", "N_Rodal", "Tipo_fores", "Tipo_For", "Subtipo_fo", "Tipo_veg"))
  valid_input(cut_by_rod, include_fp, inherit = "logical")
  valid_df(BD)
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))

  bd <- tryCatch({
    (if (inherits(BD, "data.frame")) BD else openxlsx2::read_xlsx(BD))
  }, error = function(e) stop("Error en prepare_bd_flora: No se pudo leer el archivo xlsx", call. = F))

  bd_rename <- tryCatch({
    bd %>%
      dplyr::rename_all(~ stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        type = "sentence")
      ) %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("cob_bb", strength = 1), ~ "Cob_BB") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("punto", strength = 1), ~ "Parcela") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("p500", case_insensitive = T), ~ "N_ind") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ds.*68", case_insensitive = T), ~ "DS_68") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("n_parc", case_insensitive = T), ~ "N_Parc") %>%
      dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("forma.*vida|habito", case_insensitive = T), ~ "Habito") %>%
      dplyr::rename_at(
        dplyr::vars(dplyr::matches("^rce"), dplyr::contains("UTM"), dplyr::matches("ds_68")),
        stringi::stri_trans_toupper
      )
  }, error = function(e) {
    stop(
      as.character(e) %>%
        stringi::stri_replace_all_fixed(
          pattern = c(
            "in `rename()`",
            "Names must be unique",
            "These names are duplicated",
            "at locations",
            "and"
          ),
          replacement = c(
            "al renombrar los campos",
            "Los campos deben ser unicos",
            "Los siguientes campos estan duplicados",
            "en las columnas",
            "y"
          ),
          vectorize_all = F
        ) %>%
        gsub("\033\\[[0-9;]*m", "", .) %>%
        gsub("([!✖*])\\s+", "", .)
    )
  })

  valid_input(bd_rename, names = c("Parcela", "UTM_E", "UTM_N", "Especie", "Cob_BB", "N_ind", "Habito", "DS_68", "RCE", "Decreto"))

  bd %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::contains("Cob_BB")),
      ~ stringi::stri_trim(stringi::stri_trans_tolower(.))
    ) %>%
    dplyr::mutate_at(
      dplyr::vars(dplyr::matches("Especie")),
      ~ stringi::stri_trim(stringi::stri_trans_totitle(., type = "sentence"))
    ) %>%
    dplyr::mutate_at("N_ind", as.integer) %>%
    dplyr::mutate_if(is.character, stringi::stri_replace_all_regex, "\\s+", " ") %>%
    dplyr::filter(!Especie %>% stringi::stri_detect_regex("identificac|indeter", case_insensitive = T)) %>%
    {if (PAS %in% c(148, 149)) {
      .[] %>%
        dplyr::filter(
          Habito %>%
            stringi::stri_trans_general("Latin-ASCII") %>%
            stringi::stri_detect_regex("arbo", case_insensitive = T)
        )
    } else {
      .[] %>%
        dplyr::filter(
          DS_68 %>% stringi::stri_cmp_equiv("originaria", strength = 1)
        )
    }} %>%
    {if (!include_fp) {
      .[] %>% dplyr::filter(!Cob_BB %>% stringi::stri_trans_tolower() %not_in% c("r", "+", "1", "2", "3", "4", "5", "6") & !N_ind %in% c(NA, 0))
    } else {
      .[] %>% dplyr::mutate(N_ind = dplyr::case_when(N_ind %in% c(NA, 0) ~ 1, .default = N_ind), Nha = N_ind * 20)
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
    {if (!"N_Parc" %in% names(.)) {
      .[] %>%
        dplyr::group_by(Parcela, UTM_E, UTM_N) %>%
        dplyr::arrange(N_Rodal) %>%
        dplyr::mutate(N = dplyr::cur_group_id()) %>%
        dplyr::group_by(N_Rodal, N) %>%
        dplyr::mutate(N_Parc = dplyr::cur_group_id()) %>%
        dplyr::ungroup()
    } else . } %>%
    dplyr::mutate_at("N_Parc", as.integer)
}

#' @export
#' @rdname prepare_bd
prepare_bd_pcob <- function(BD) {
  valid_df(BD)

  (if (inherits(BD, "data.frame")) BD else openxlsx2::read_xlsx(BD)) %>%
    janitor::clean_names() %>%
    dplyr::rename_all( ~ ifelse(
      . == "geometry",.,
      stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        type = "sentence"
      )
    )) %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^copa.*ns|^diametro.*1", case_insensitive = T), ~ "Copa_NS") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^copa.*eo|^diametro.*2", case_insensitive = T), ~ "Copa_EO") %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("UTM")), stringi::stri_trans_toupper) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Copa")), stringi::stri_replace_all_regex, "\\,", "\\.") %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Copa")), as.numeric)
}

#' @export
#' @rdname prepare_bd
prepare_bd_trans <- function(BD) {
  valid_df(BD)

  (if (inherits(BD, "data.frame")) BD else openxlsx2::read_xlsx(BD)) %>%
    janitor::clean_names() %>%
    dplyr::rename_all( ~ ifelse(
      . == "geometry",.,
      stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        type = "sentence"
      )
    )) %>%
    dplyr::rename_at(dplyr::vars(dplyr::contains("UTM")), stringi::stri_trans_toupper) %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("habito|forma.*vida", case_insensitive = T), ~ "Habito") %>%
    dplyr::mutate_at(dplyr::vars(Cuenta, Denominador, Cobertura), as.numeric)
}
