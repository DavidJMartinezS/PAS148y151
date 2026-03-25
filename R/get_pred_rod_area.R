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
#' @param orden_rodal Orden espacial para los rodales. Se aplica cuando `n_rodal_ord` es \code{TRUE}. Mas detalle en la documentacion de [st_order()].
#' @param dec_sup Cantidad de decimales para la superficie en hectareas. Default `2`.
#'
#' @return Lista con los sf de Predios, Rodales y Areas de corta
#'
#' @name get_pred_rod_area
#'
#' @export
#' @import dataPAS
#'
get_pred_rod_area <- function(
    PAS,
    LB,
    obras,
    predios,
    suelos,
    group_by_LB = NULL,
    sep_by_soil = TRUE,
    group_by_dist = FALSE,
    distance_max = NULL,
    cut_by_prov = FALSE,
    provincia = NULL,
    n_rodal_ord = FALSE,
    orden_rodal = "NS-OE",
    dec_sup = 2L
){
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))
  valid_input(sep_by_soil, group_by_dist, cut_by_prov, n_rodal_ord, inherit = "logical")
  valid_input(LB, obras, predios, suelos, inherit = "sf")
  valid_input(distance_max, inherit = c("integer", "numeric", "NULL"))
  valid_input(dec_sup, inherit = c("integer", "numeric"))

  if (cut_by_prov) {
    provincia <- match.arg(provincia, choices = unlist(provincias_list))
  }
  if (n_rodal_ord) {
    orden_rodal <- match.arg(
      orden_rodal,
      choices = c("NS-EO","NS-OE","SN-EO","SN-OE","EO-NS","EO-SN","OE-NS","OE-SN")
    )
  }

  if (group_by_dist && is.null(distance_max)) {
    distance_max <- 50
  }

  LB <- LB %>%
    dplyr::rename_all(~ ifelse(
      . == "geometry",
      .,
      stringi::stri_trans_totitle(
        stringi::stri_trans_general(., "Latin-ASCII"),
        type = "sentence"
      )
    )) %>%
    dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("pid", strength = 1), ~ "PID") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^tipo.*for", case_insensitive = T), ~ "Tipo_fores") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^tipo.*veg", case_insensitive = T), ~ "Tipo_veg") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^sub.*tipo.*fo", case_insensitive = T), ~ "Subtipo_fo") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ley.*20283", case_insensitive = T), ~ "F_ley20283")
  valid_input(LB, names = c("Tipo_fores", "Subtipo_fo", "Tipo_veg", "F_ley20283"))

  if (!is.null(group_by_LB)) {
    if (!all(group_by_LB %in% names(LB))) {
      stop(sprintf(
        "Los campos %s en 'group_by_LB' no coinciden con los de 'LB'",
        setdiff(group_by_LB, names(LB))
      ))
    }
  }

  predios <- predios %>%
    dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("n_predio", strength = 1), ~ "N_Predio") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("nom_predio", strength = 1), ~ "Nom_Predio") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("rol", strength = 1), ~ "Rol") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("^prop", case_insensitive = T), ~ "Propietari")
  valid_input(predios, names = c("N_Predio", "Nom_Predio"))

  suelos <- suelos %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("textcaus|clase_uso", case_insensitive = T), ~ "Clase_Uso") %>%
    dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("desceros|cat_erosio|clase_eros", case_insensitive = T), ~ "Clase_Eros")

  if (PAS %in% c(148, 149)) {
    valid_input(suelos, names = c("Clase_Uso"))
    var_suelo <- dplyr::sym("Clase_Uso")
  } else {
    valid_input(suelos, names = c("Clase_Eros"))
    var_suelo <- dplyr::sym("Clase_Eros")
  }

  tipo_bos <- switch(
    as.character(PAS),
    "148" = "BN",
    "149" = "PL",
    "No aplica"
  )

  f_ley <- switch(
    as.character(PAS),
    "148" = "bosque nativo",
    "149" = "plantacion forestal",
    "151" = "formacion xerofitica"
  )

  if (nrow(obras[LB,]) == 0) {
    stop(simpleError("No existe intercepción las obras y la línea de base"))
  }

  if (!any(stringi::stri_cmp_equiv(LB$F_ley20283, f_ley, strength = 1))) {
    stop(simpleError(sprintf("No se encuentra la categoría %s en el campo 'F_ley20283'", sQuote(f_ley))))
  }

  if (cut_by_prov) {
    provincia_sf <- sf::read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>%
      dplyr::filter(PROVINCIA == provincia) %>%
      sf::st_transform(sf::st_crs(LB)) %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON")
    LB <- LB[provincia_sf, ]
    if (nrow(LB) == 0) {
      stop(simpleError(
      "Provincia seleccionada fuera de los límites del área de de proyecto.
      Intente nuevamente con otra provincia", call = F))
    }
    obras <- obras %>%
      sf::st_intersection(sf::st_union(provincia_sf)) %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON")
    suelos <- suelos %>%
      sf::st_intersection(sf::st_union(provincia_sf)) %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON")
    predios <- predios[provincia_sf, ]
  } %>%
    suppressWarnings() %>% suppressMessages()

  LB <- LB %>% {if (!("PID" %in% names(.))) tibble::rowid_to_column(., "PID") else .}

  group_list <- c("N_Predio", "Nom_Predio", "Tipo_fores") %>%
    {if (!is.null(group_by_LB)) c(., group_by_LB) %>% unique() else .} %>%
    dplyr::syms()

  areas <- LB %>%
    dplyr::filter(
      F_ley20283 %>%
        stringi::stri_replace_all_regex("\\s+", " ") %>%
        stringi::stri_trim() %>%
        stringi::stri_cmp_equiv(f_ley, strength = 1)
    ) %>%
    {if (nrow(.) == 0) {
      stop(simpleError(sprintf("No se halló la categoría de %s dentro de la provincia ingresada", sQuote(f_ley))))
    } else .[]} %>%
    sf::st_intersection(sf::st_union(obras)) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    {if (nrow(.) == 0) {
      stop(simpleError(sprintf("No se halló %s a intervenir", sQuote(f_ley))))
    } else .[]} %>%
    my_union(predios %>% dplyr::select(N_Predio, Nom_Predio)) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_cast("POLYGON") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>%
    dplyr::filter(sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(1) != 0) %>%
    suppressWarnings() %>% suppressMessages()

  Rod <- LB %>%
    dplyr::filter(PID %in% unique(areas$PID)) %>%
    {if (n_rodal_ord) dplyr::mutate(., N_Rodal = st_order(geometry, order = orden_rodal)) else .} %>% 
    my_union(predios %>% dplyr::select(N_Predio, Nom_Predio)) %>%
    sf::st_collection_extract("POLYGON") 
  Rodales <- Rod %>%
    {if (is.null(group_by_LB)){
      .[] %>%
        {if (n_rodal_ord) {
          dplyr::group_by(., PID, N_Rodal, F_ley20283, N_Predio, Nom_Predio, Tipo_fores, Subtipo_fo, Tipo_veg)
        } else {
          dplyr::group_by(., PID, F_ley20283, N_Predio, Nom_Predio, Tipo_fores, Subtipo_fo, Tipo_veg)
        }} %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        dplyr::ungroup() %>%
        sf::st_collection_extract("POLYGON")
    } else {
      .[] %>% 
        select(F_ley20283, !!!group_list) %>% 
        dplyr::group_by(F_ley20283, !!!group_list) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        dplyr::ungroup() %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_cast("POLYGON") %>%
        {if (n_rodal_ord){
          .[] %>% 
            sf::st_join(Rod %>% dplyr::select(N_Rodal), largest = T) %>% 
            dplyr::arrange(N_Rodal)
        } else .} %>% 
        tibble::rowid_to_column("PID") %>% 
        {if (!c("Subtipo_fo", "Tipo_veg") %in% group_list %>% all()){
          .[] %>%
            sf::st_join(
              LB %>%
                dplyr::select(c("Subtipo_fo", "Tipo_veg")[!c("Subtipo_fo", "Tipo_veg") %in% group_list]),
              largest = T
            )
        } else .}
    }} %>%
    {if ("N_Rodal" %in% names(.)) {
      .[] %>%
        dplyr::group_by(N_Rodal) %>%
        dplyr::mutate(N_Rodal = as.integer(dplyr::cur_group_id())) %>%
        dplyr::ungroup()
    } else {
      .[] %>%
        dplyr::group_by(PID) %>%
        dplyr::mutate(N_Rodal = as.integer(dplyr::cur_group_id())) %>%
        dplyr::ungroup()
    }} %>% 
    dplyr::mutate(
      Tipo_Bos = tipo_bos,
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
    dplyr::select(N_Predio, Nom_Predio, PID, N_Rodal, Tipo_Bos, Tipo_For, Tipo_fores, Subtipo_fo, Tipo_veg, F_ley20283, Sup_ha) %>%
    suppressWarnings() %>% suppressMessages()

  if (any(Rodales %>%  dplyr::group_by(N_Rodal) %>%  dplyr::summarise_at("Sup_ha", sum) %>% .$Sup_ha < 0.5) & PAS %in% c(148, 149)) {
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
    dplyr::count(N_Rodal, Tipo_For, !!!group_list) %>% select(-n) %>% 
    sf::st_collection_extract("POLYGON") %>% 
    sf::st_cast("POLYGON") %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON") %>% 
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
    # {if(!is.null(group_by_LB)) {
    #   .[] %>% group_by()
    # }} %>% 
    {if (sep_by_soil) {
      .[] %>%
        st_intersection(suelos %>% dplyr::select(!!var_suelo)) %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_cast("POLYGON") %>%
        sf::st_make_valid() %>%
        sf::st_collection_extract("POLYGON") %>%
        dplyr::group_by(dplyr::across(-geometry)) %>%
        dplyr::tally() %>% dplyr::ungroup() %>%
        sf::st_collection_extract("POLYGON") %>%
        sf::st_cast("POLYGON") %>%
        sf::st_make_valid() %>%
        sf::st_collection_extract("POLYGON")
    } else {
      .[] %>%
        sf::st_join(suelos %>% dplyr::select(!!var_suelo)) %>%
        dplyr::group_by(N_Rodal, Tipo_For, !!!group_list, N_Pred_ori, geometry) %>%
        dplyr::summarise(!!var_suelo := paste(unique(!!var_suelo), collapse = " - ")) %>%
        dplyr::ungroup()
    }} %>%
    dplyr::mutate_at(dplyr::vars(!!var_suelo), tidyr::replace_na, "S/I") %>%
    dplyr::filter(sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(1) != 0) %>%
    {if (group_by_dist) {
      .[] %>% 
        dplyr::group_by(N_Rodal, N_Predio, !!var_suelo) %>%
        dplyr::mutate(group = group_by_distance(geometry, distance = distance_max)) %>%
        dplyr::group_by(N_Rodal, !!!group_list[as.character(group_list) != "Tipo_veg"], Tipo_For, Tipo_veg, N_Pred_ori, !!var_suelo, group) %>%
        dplyr::summarise(geometry = sf::st_union(geometry)) %>%
        sf::st_collection_extract("POLYGON") %>%
        dplyr::ungroup()
    } else .[]} %>%
    dplyr::mutate(
      Sup_ha = sf::st_area(geometry) %>% units::set_units(ha)%>% units::drop_units() %>% janitor::round_half_up(dec_sup),
      Sup_m2 = sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up()
    ) %>%
    dplyr::group_by(N_Predio) %>%
    dplyr::mutate(sort_by_pred = st_order(geometry), order = orden_rodal, progress = F) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(N_Rodal) %>%
    dplyr::mutate(sort_by_rod = st_order(geometry), order = orden_rodal, progress = F) %>%
    dplyr::arrange(as.numeric(N_Rodal), sort_by_rod) %>%
    tibble::rowid_to_column("N_Area") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      Tipo_Bos = tipo_bos,
      N_a = paste(N_Predio, stringi::stri_pad_left(sort_by_pred, stringi::stri_length(max(sort_by_pred)), pad = "0"), sep = ".")
    ) %>%
    dplyr::select(!!!group_list, Tipo_For, Tipo_veg, Tipo_Bos, N_a, N_Area, N_Pred_ori, !!var_suelo, Sup_ha, Sup_m2) %>%
    suppressWarnings() %>% suppressMessages()

  comunas_sf <- sf::read_sf(
    system.file("Comunas.gdb", package = "dataPAS"),
    wkt_filter = sf::st_as_text(sf::st_geometry(sf::st_union(
      sf::st_transform(predios %>% sf::st_make_valid(), 5360)
    )))
  ) %>%
    sf::st_set_geometry("geometry") %>%
    sf::st_transform(sf::st_crs(predios)) %>%
    sf::st_make_valid() %>%
    sf::st_collection_extract("POLYGON")

  Predios <- predios %>%
    dplyr::filter(N_Predio %in% unique(BN_areas$N_Pred_ori)) %>%
    dplyr::group_by(N_Predio) %>%
    dplyr::mutate(N_Predio2 = cur_group_id()) %>%
    dplyr::ungroup() %>%
    dplyr::select(-N_Predio) %>%
    dplyr::rename(N_Predio = N_Predio2) %>%
    dplyr::arrange(N_Predio) %>%
    dplyr::mutate_at(dplyr::vars(Nom_Predio, Rol, Propietari), tidyr::replace_na, "S/I") %>%
    sf::st_intersection(comunas_sf[, c("COMUNA", "PROVINCIA")] %>% dplyr::rename_all(stringi::stri_trans_totitle)) %>%
    st_collection_extract("POLYGON") %>%
    mutate(Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2)) %>%
    dplyr::select(N_Predio, Nom_Predio, Rol, Propietari, Comuna, Sup_ha) %>%
    suppressWarnings() %>% suppressMessages()

  if (nrow(Rodales %>% dplyr::count(N_Rodal)) > nrow(Rodales[BN_areas, ] %>% dplyr::count(N_Rodal))) {
    warning(
      paste0(
        "Los siguientes rodales sobran:\n",
        setdiff(
          Rodales %>% dplyr::count(N_Rodal) %>% .$N_Rodal,
          Rodales[BN_areas, ] %>% dplyr::count(N_Rodal) %>% .$N_Rodal
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
