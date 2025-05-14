#' get_pred_rod_area
#'
#' @description
#' Función para obtener de forma preliminar capa de Predios, Rodales y Areas de corta
#'
#' @param PAS PAS correspondiente. Ingresar `148` o `151`.
#' @param LB Objeto sf con la linea base de flora.
#' @param obras Objeto sf con las obras del proyecto.
#' @param predios Objeto sf con los limites ´prediales.
#' @param suelos Objeto sf con la informacion de suelo.
#' @param group_by_LB lista de campos por los que agrupar los rodales de linea base.
#' @param sep_by_soil Logico; si es \code{TRUE}, separa las areas de corta por capa de suelo.
#' @param group_by_dist Logico; si es \code{TRUE}, agrupa las areas de corta que estan a cierta distancia.
#' @param distance_max Distancia maxima para agrupar los poligonos. Utilizar si `group_by_dist` es \code{TRUE}.
#' @param cut_by_prov Logico; si es \code{TRUE}, genera las capas para la provincia indicada
#' @param provincia Nombre de la provincia. Utilizar si `cut_by_prov` es \code{TRUE}.
#' @param n_rodal_ord Logico; si es \code{TRUE}, ordena espacialmente los rodales, de lo contrario sigue la numeración del numero de poligono.
#' @param orden_rodal Orden espacial para los rodales. Utilizar si `n_rodal_ord` es \code{TRUE}.
#' @param dec_sup Cantidad de decimales para la superficie en hectareas. Default `2`.
#'
#' @return Lista con los sf de Predios, Rodales y Areas de corta
#'
#' @name get_pred_rod_area
#'
#' @export
#'
#' @import dataPAS
#' @importFrom dplyr filter select sym syms mutate mutate_at group_by summarise summarise_at ungroup case_when cur_group_id arrange vars pull rename count
#' @importFrom janitor round_half_up
#' @importFrom sf read_sf st_transform st_crs st_intersection st_union st_combine st_collection_extract st_make_valid st_cast st_area st_filter st_join
#' @importFrom stringi stri_cmp_equiv stri_detect_regex stri_replace_all_regex stri_trim stri_length
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr replace_na
#' @importFrom units set_units drop_units

get_pred_rod_area <- function(
    PAS,
    LB,
    obras,
    predios,
    suelos,
    group_by_LB = NULL,
    sep_by_soil = T,
    group_by_dist = F,
    distance_max = if(group_by_distance == F) NULL,
    cut_by_prov = F,
    provincia = NULL,
    n_rodal_ord = F,
    orden_rodal = "NS-OE",
    dec_sup = 2
){
  stopifnot(c(sep_by_soil, group_by_dist, cut_by_prov, n_rodal_ord) %>% is.logical())
  stopifnot(c("Tipo_fores", "Subtipo_fo", "Tipo_veg", "Regulacion") %in% names(LB) %>% all())
  stopifnot(c("N_Predio", "Nom_Predio") %in% names(predios) %>% all())
  stopifnot(PAS %in% c(148, 151))
  if (PAS == 148) {
    stopifnot(c("Nom_Predio", "Clase_Uso") %in% names(suelos) %>% all())
    var_suelo <- dplyr::sym("Clase_Uso")
  }
  if (PAS == 151) {
    stopifnot(c("Clase_Eros") %in% names(suelos) %>% all())
    var_suelo <- dplyr::sym("Clase_Eros")
  }
  if (n_rodal_ord) {
    stopifnot(orden_rodal %in% c("NS-EO","NS-OE","SN-EO","SN-OE","EO-NS","EO-SN","OE-NS","OE-SN"))
  }

  if (cut_by_prov) {
    stopifnot(provincia %in% unlist(provincias_list))
    provincia_sf <- sf::read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>%
      dplyr::filter(PROVINCIA == provincia) %>%
      sf::st_transform(sf::st_crs(predios))
    obras <- obras %>% sf::st_intersection(sf::st_union(sf::st_combine(provincia_sf))) %>% sf::st_collection_extract("POLYGON")
    suelos <- suelos %>% sf::st_intersection(sf::st_union(sf::st_combine(provincia_sf))) %>% sf::st_collection_extract("POLYGON")
    LB <- LB[provincia_sf, ]
    predios <- predios[provincia_sf, ]
  } %>% suppressWarnings()

  group_list <- c("N_Predio", "Nom_Predio", "Tipo_fores") %>%
    {if (!is.null(group_by_LB)) c(., group_by_LB) %>% unique() else .} %>%
    dplyr::syms()

  areas <- LB %>%
    {if (PAS == 148){
      .[] %>%
        dplyr::filter(
          Regulacion %>%
            stringi::stri_replace_all_regex("\\s+", " ") %>%
            stringi::stri_trim() %>%
            stringi::stri_cmp_equiv("bosque nativo", strength = 1)
        )
    } else {
      .[] %>%
        dplyr::filter(
          Regulacion %>%
            stringi::stri_replace_all_regex("\\s+", " ") %>%
            stringi::stri_trim() %>%
            stringi::stri_cmp_equiv("formacion xerofitica", strength = 1)
        )
    }} %>%
    sf::st_intersection(sf::st_union(sf::st_combine(obras))) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    my_union(predios %>% dplyr::select(N_Predio, Nom_Predio)) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_cast("POLYGON") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    dplyr::filter(!sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up() == 0) %>%
    suppressWarnings()

  Rodales <- LB %>%
    {if (PAS == 148){
      .[] %>%
        dplyr::filter(
          Regulacion %>%
            stringi::stri_replace_all_regex("\\s+", " ") %>%
            stringi::stri_trim() %>%
            stringi::stri_cmp_equiv("bosque nativo", strength = 1)
        )
    } else {
      .[] %>%
        dplyr::filter(
          Regulacion %>%
            stringi::stri_replace_all_regex("\\s+", " ") %>%
            stringi::stri_trim() %>%
            stringi::stri_cmp_equiv("formacion xerofitica", strength = 1)
        )
    }} %>%
    sf::st_filter(areas, .predicate = st_relate, pattern = "T********") %>%
    {if (is.null(group_by_LB) & !("PID" %in% names(.))) tibble::rowid_to_column(., "PID") else .} %>%
    {if (n_rodal_ord) dplyr::mutate(., N_Rodal = st_order(geometry, order = orden_rodal)) else .} %>%
    my_union(predios %>% dplyr::select(N_Predio, Nom_Predio)) %>%
    sf::st_collection_extract("POLYGON") %>%
    {if (is.null(group_by_LB)){
      .[] %>%
        dplyr::group_by(., PID, Regulacion, N_Predio, Nom_Predio, Tipo_fores, Subtipo_fo, Tipo_veg) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        dplyr::ungroup() %>%
        sf::st_collection_extract("POLYGON")
    } else {
      .[] %>%
        dplyr::group_by(Regulacion, !!!group_list) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        dplyr::ungroup() %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_cast("POLYGON") %>%
        tibble::rowid_to_column("PID") %>%
        {if (!c("Subtipo_fo", "Tipo_veg") %in% group_list %>% all()){
          .[] %>%
            sf::st_join(LB %>% dplyr::select(c("Subtipo_fo", "Tipo_veg")[!c("Subtipo_fo", "Tipo_veg") %in% group_list]), largest = T)
        } else .}
    }} %>%
    {if ("N_Rodal" %in% names(.)) . else .[] %>% dplyr::group_by(PID) %>% dplyr::mutate(N_Rodal = as.integer(dplyr::cur_group_id())) %>% dplyr::ungroup()} %>%
    dplyr::mutate(
      Tipo_Bos = ifelse(PAS == 148, "BN", "No aplica"),
      Tipo_For = dplyr::case_when(
        Tipo_fores %>% stringi::stri_detect_regex("no.*aplica", case_insensitive = T) ~ "No aplica",
        Tipo_fores %>% stringi::stri_detect_regex("alerce", case_insensitive = T) ~ "1",
        Tipo_fores %>% stringi::stri_detect_regex("araucaria", case_insensitive = T) ~ "2",
        Tipo_fores %>% stringi::stri_detect_regex("cordillera", case_insensitive = T) ~ "3",
        Tipo_fores %>% stringi::stri_detect_regex("guaitecas", case_insensitive = T) ~ "4",
        Tipo_fores %>% stringi::stri_detect_regex("magallanes", case_insensitive = T) ~ "5",
        Tipo_fores %>% stringi::stri_detect_regex("tepa", case_insensitive = T) ~ "6",
        Tipo_fores %>% stringi::stri_detect_regex("lenga", case_insensitive = T) ~ "7",
        Tipo_fores %>% stringi::stri_detect_regex("roble.*raul", case_insensitive = T) ~ "8",
        Tipo_fores %>% stringi::stri_detect_regex("roble.*hualo", case_insensitive = T) ~ "9",
        Tipo_fores %>% stringi::stri_detect_regex("siemprev", case_insensitive = T) ~ "10",
        Tipo_fores %>% stringi::stri_detect_regex("escle", case_insensitive = T) ~ "11",
        Tipo_fores %>% stringi::stri_detect_regex("palma", case_insensitive = T) ~ "12",
        .default = Tipo_fores
      ),
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(dec_sup)
    ) %>%
    dplyr::arrange(N_Rodal) %>%
    dplyr::mutate_at(dplyr::vars(Nom_Predio), tidyr::replace_na, "S/I") %>%
    dplyr::select(N_Predio, Nom_Predio, PID, N_Rodal, Tipo_Bos, Tipo_For, Tipo_fores, Subtipo_fo, Tipo_veg, Regulacion, Sup_ha) %>%
    suppressMessages() %>% suppressWarnings()

  if (any(Rodales %>%  dplyr::group_by(N_Rodal) %>%  dplyr::summarise_at("Sup_ha", sum) %>% .$Sup_ha < 0.5) & PAS == 148) {
    warning(
      paste0(
        "Los siguientes rodales de BN presentan una superficie inferior a 0,5 ha:\n",
        Rodales %>%
          dplyr::group_by(N_Rodal) %>%
          dplyr::summarise_at("Sup_ha", sum) %>%
          dplyr::filter(Sup_ha < 0.5) %>%
          dplyr::pull(N_Rodal) %>%
          shQuote() %>%
          paste0(collapse = ", ")
      )
    )
  }
  if (any(Rodales %>% dplyr::group_by(N_Rodal) %>% dplyr::summarise_at("Sup_ha", sum) %>% .$Sup_ha < 1) & PAS == 151) {
    warning(
      paste0(
        "Los siguientes rodales de FX presentan una superficie inferior a 1 ha:\n",
        Rodales %>%
          dplyr::group_by(N_Rodal) %>%
          dplyr::summarise_at("Sup_ha", sum) %>%
          dplyr::filter(Sup_ha < 1) %>%
          dplyr::pull(N_Rodal) %>%
          shQuote() %>%
          paste(collapse = ", ")
      )
    )
  }

  BN_areas <- areas %>%
    sf::st_join(Rodales %>% dplyr::select(N_Rodal, Tipo_For), largest = T) %>%
    dplyr::mutate(N_Pred_ori = N_Predio) %>%
    dplyr::mutate_at("N_Predio", as.character) %>%
    dplyr::mutate_at(dplyr::vars(N_Predio, Nom_Predio), tidyr::replace_na, "S/I") %>%
    dplyr::group_by(N_Pred_ori) %>%
    dplyr::mutate(N_Predio2 = dplyr::cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at("N_Predio2", as.character) %>%
    dplyr::mutate("N_Predio2" = dplyr::case_when(N_Predio == "S/I" ~ N_Predio, .default = N_Predio2)) %>%
    dplyr::select(-N_Predio) %>%
    dplyr::rename(N_Predio = N_Predio2) %>%
    dplyr::arrange(N_Predio) %>%
    {if (sep_by_soil) {
      .[] %>%
        my_union(suelos %>% dplyr::select(!!var_suelo)) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_cast("POLYGON") %>%
        sf::st_make_valid() %>%
        sf::st_collection_extract("POLYGON")
    } else {
      .[] %>%
        sf::st_join(suelos %>% dplyr::select(!!var_suelo)) %>%
        dplyr::group_by(N_Rodal, !!!group_list, N_Pred_ori, geometry) %>%
        dplyr::summarise(var_suelo = paste(unique(!!var_suelo), collapse = " - ")) %>%
        dplyr::ungroup()
    }} %>%
    dplyr::mutate_at(dplyr::vars(!!var_suelo), tidyr::replace_na, "S/I") %>%
    {if (group_by_dist) {
      .[] %>%
        dplyr::group_by(N_Rodal, N_Predio, !!var_suelo) %>%
        dplyr::mutate(group = group_by_distance(geometry, distance = distance_max)) %>%
        dplyr::group_by(N_Rodal, !!!group_list, Tipo_For, Tipo_veg, N_Pred_ori, !!var_suelo, group) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        sf::st_collection_extract("POLYGON") %>%
        dplyr::ungroup()
    } else .} %>%
    dplyr::mutate(
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha)%>% units::drop_units() %>% janitor::round_half_up(dec_sup),
      Sup_m2 = sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up()
    ) %>%
    dplyr::group_by(N_Predio) %>%
    dplyr::mutate(N_r = st_order(geometry)) %>%
    dplyr::arrange(as.numeric(N_Predio), N_r) %>%
    tibble::rowid_to_column("N_Area") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Tipo_Bos = ifelse(PAS == 148, "BN", "No aplica"),
      N_a = paste(N_Predio, stringi::stri_pad_left(N_r, stringi::stri_length(max(N_r)), pad = "0"), sep = ".")
    ) %>%
    dplyr::select(!!!group_list, Tipo_For, Tipo_veg, Tipo_Bos, N_Rodal, N_a, N_Area, N_Pred_ori, !!var_suelo, Sup_ha, Sup_m2) %>%
    suppressWarnings()

  Predios <- predios %>%
    dplyr::filter(N_Predio %in% unique(BN_areas$N_Pred_ori)) %>%
    dplyr::group_by(N_Predio) %>%
    dplyr::mutate(N_Predio2 = cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-N_Predio) %>%
    dplyr::rename(N_Predio = N_Predio2) %>%
    dplyr::arrange(N_Predio) %>%
    dplyr::mutate_at(dplyr::vars(Nom_Predio, Rol, Propietari), tidyr::replace_na, "S/I") %>%
    dplyr::select(N_Predio, Nom_Predio, Rol, Propietari) %>%
    suppressWarnings()

  if (nrow(Rodales %>% dplyr::count(N_Rodal)) > nrow(Rodales %>% dplyr::count(N_Rodal) %>% .[BN_areas, ])) {
    warning(
      paste0(
        "Los siguientes rodales sobran:\n",
        setdiff(
          Rodales %>% dplyr::count(N_Rodal) %>% .$N_Rodal,
          Rodales %>% dplyr::count(N_Rodal) %>% .[BN_areas, ] %>% .$N_Rodal
        ) %>%
          shQuote() %>% paste(collapse = ", ")
      )
    )
  }

  return(
    list(
      Rodales = Rodales,
      Areas = BN_areas,
      Predios = Predios
    )
  )
}
