#' Tabla atributos de rodal
#'
#' @param PAS PAS correspondiente. Ingresar `148` o `151`.
#' @param parcelas_rodales Base de datos de parcelas dentro de los rodales.
#' @param rodales_def Objeto sf con poligono de rodales.
#'
#' @return Tabla con los atributos de rodal
#' @export
#'
#' @importFrom dplyr syms group_by summarise rename arrange select bind_rows filter ungroup count left_join case_when mutate
#' @importFrom sf st_as_sf st_distance st_join st_nearest_feature st_drop_geometry st_as_sf st_collection_extract st_crs
#' @importFrom tidyr complete unnest
#' @importFrom janitor round_half_up
#' @importFrom purrr map
get_tabla_attr_rodal <- function(PAS, parcelas_rodales, rodales_def){
  stopifnot(PAS %in% c(148, 151))
  stopifnot(inherits(rodales_def, "sf"))
  if (PAS == 148) {
    stopifnot(c("Nom_Predio", "N_Rodal", "Parcela", "N_Parc", "UTM_E", "UTM_N", "Tipo_fores", "Subtipo_fo", "Tipo_veg", "Especie", "Nha") %in% names(parcelas_rodales) %>% all())
    stopifnot(c("N_Rodal", "Tipo_fores", "Subtipo_fo", "Tipo_veg") %in% names(rodales_def) %>% all())
  } else {
    stopifnot(c("Nom_Predio", "N_Rodal", "Parcela", "N_Parc", "UTM_E", "UTM_N", "Tipo_veg", "Especie", "Nha") %in% names(parcelas_rodales) %>% all())
    stopifnot(c("N_Rodal", "Tipo_veg") %in% names(rodales_def) %>% all())
  }

  vars_tbl_attr <- if(PAS == 148) {
    dplyr::syms(c("Tipo_fores", "Subtipo_fo", "Tipo_veg"))
  } else {
    dplyr::syms("Tipo_veg")
  }

  nha_parc <- parcelas_rodales %>%
    dplyr::group_by(Nom_Predio, N_Rodal, Parcela, N_Parc, UTM_E, UTM_N, Tipo_veg) %>%
    dplyr::summarise(Nha = sum(Nha,na.rm = T), .groups = "drop") %>%
    dplyr::rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>%
    sf::st_as_sf(coords = c("Coord_X","Coord_Y"), crs = sf::st_crs(rodales_def), remove = F) %>%
    dplyr::arrange(N_Parc)

  if (any(nha_parc %>% sf::st_distance(rodales_def) %>% apply(1, min) %>% .[] > 5)) {
    cat(
      "Los siguientes puntos están a más de 5 metros de los rodales:", "\n",
      nha_parc[nha_parc %>% sf::st_distance(rodales_def) %>% apply(1, min) %>% .[] > 5, ]$Parcela %>%
        {if(length(. > 10)) .[c(1:10)] else .} %>%
        shQuote() %>% paste0(collapse = ", ") %>%
        {if(length(. > 10)) paste0(., ", etc...") else .},
      "Por favor revisar", "\n"
    )
    # shinyalert(
    #   title = "OJO!",
    #   text = tags$p(
    #     "Los siguientes puntos están a más de 5 metros de los rodales:", br(),
        # nha_parc[nha_parc %>% st_distance(rodales_def) %>% apply(1, min) %>% .[] > 5, ]$Parcela %>%
        #   {if(length(. > 10)) .[c(1:10)] else .} %>%
        #   shQuote() %>% paste0(collapse = ", ") %>%
        #   {if(length(. > 10)) paste0(., ", etc...") else .}, br(),
    #     "Por favor revisar"
    #   ),
    #   type = "warning",
    #   html = T,
    #   closeOnEsc = T,
    #   showConfirmButton = T,
    #   animation = TRUE
    # )
  }

  estimaciones_x_tipo <- parcelas_rodales %>%
    dplyr::select(Tipo_veg, N_Parc, Especie, Nha) %>%
    split(.$Tipo_veg) %>%
    purrr::map(~tidyr::complete(.,Tipo_veg, N_Parc, Especie, fill = list(Nha = 0))) %>%
    purrr::map(function(x){
      x %>%
        dplyr::group_by(Tipo_veg, Especie) %>%
        dplyr::summarise(Nha = mean(Nha, na.rm = T) %>% janitor::round_half_up()) %>%
        dplyr::ungroup()
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(Nha != 0) %>%
    dplyr::arrange(Tipo_veg, desc(Nha))

  df <- rodales_def %>%
    dplyr::count(N_Rodal, !!!vars_tbl_attr) %>% dplyr::select(-n) %>%
    sf::st_join(nha_parc %>% dplyr::select(N_Parc, Nha))

  tabla_attr_rodal <- df %>%
    dplyr::bind_rows(
      nha_parc %>%
        dplyr::filter(!N_Parc %in% df$N_Parc) %>%
        dplyr::select(N_Parc, Nha) %>%
        sf::st_join(
          rodales_def %>% dplyr::select(N_Rodal, !!!vars_tbl_attr),
          join = sf::st_nearest_feature
        ) %>%
        sf::st_drop_geometry() %>%
        dplyr::left_join(
          rodales_def %>%
            dplyr::count(N_Rodal, !!!vars_tbl_attr) %>%
            dplyr::select(-n)
        ) %>%
        sf::st_as_sf(crs = sf::st_crs(rodales_def)) %>%
        sf::st_collection_extract("POLYGON")
    ) %>%
    dplyr::group_by(N_Rodal, !!!vars_tbl_attr, geometry) %>%
    dplyr::summarise(
      Parcelas = paste0(N_Parc, collapse = '-'),
      NHA = mean(Nha, na.rm=T),
      .groups = "drop"
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      Tipo_attr = ifelse((is.na(Parcelas) | Parcelas == "NA"), "Estimación", "Parcela directa"),
      Nom_attr = dplyr::case_when(
        Tipo_attr ==  "Parcela directa" ~ paste0("Parcela ", Parcelas),
        Tipo_attr ==  "Estimación" & !(Tipo_veg %in% unique(estimaciones_x_tipo$Tipo_veg)) ~ "Estimación por Tipo vegetacional similar",
        .default = "Estimación por Tipo vegetacional"
      )
    ) %>%
    dplyr::select(-c(Parcelas, NHA)) %>%
    tidyr::unnest(N_Rodal) %>%
    dplyr::arrange(N_Rodal)

  return(tabla_attr_rodal)
}
