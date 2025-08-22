#' Cartografia digital
#'
#' @description
#' Funciones diversas para elaborar la cartografia digital
#'
#' @param PAS PAS correspondiente. Ingresar \code{148} o \code{151}.
#' @param areas Objeto sf de las areas de corta.
#' @param rodales Objeto sf con los rodales de las areas de corta.
#' @param predios Objeto sf con los predios de las areas de corta.
#' @param cut_by_prov Cortar predios por provincia.
#' @param dem Objeto SpatRaster o ruta del archivo raster.
#' @param add_parcelas Logico; \code{TRUE} para incluir capa de parcelas.
#' @param bd_flora Base de datos de flora. Incluir si `add_parcelas` es \code{TRUE}.
#' @param cut_by_rod Seleccionar parcelas dentro de rodales.
#' @param include_fp Incluir especies fuera de parcela. Incluye ademas especies cuya `Cob_BB` sea \code{NA} o \code{---}.
#' @param from_RCA Logico; \code{TRUE} si la capa probiene de una RCA aprobada. Default \code{FALSE}.
#' @param RCA Numero de la RCA aprobada. Icluir si `from_RCA` es \code{TRUE}. Default \code{NULL}.
#' @param add_uso_actual Logico; \code{TRUE} para incluir capa de uso actual.
#' @param catastro Objeto sf del catastro de CONAF.
#' @param suelos Objeto sf con informacion de suelo.
#' @param add_caminos Logico; \code{TRUE} para incluir capa de caminos.
#' @param add_caminos_osm Logico; \code{TRUE} para incluir capa de caminos desde OpenStreetMap.
#' @param caminos_arg Lista de argumentos para cortar capa de caminos. Default `cut`= 'clip'; `buffer` = 0.
#' @param add_hidro Logico; \code{TRUE} para incluir capa de hidrografia.
#' @param fuente_hidro Fuente de la capa hidrografica. `MOP` o `BCN`.
#' @param add_hidro_osm Logico; \code{TRUE} para incluir capa de hidrografia desde OpenStreetMap.
#' @param hidro_arg Lista de argumentos para cortar capa de hidrografia Default `cut`= 'clip'; `buffer` = 0.
#' @param add_curv_niv Logico; \code{TRUE} para incluir capa de curvas de nivel.
#' @param curv_niv_arg Lista de argumentos para cortar capa de curvas de nivel. Default `cut`= 'clip'; `buffer` = 0.
#' @param step Intervalo para curvas de nivel. Incluir si `add_curv_niv` es \code{TRUE}. Default `10`.
#' @param dec_sup Cantidad de decimales para la superficie en hectareas. Default `2`.
#' @param cut Tipo de corte para aplicar a la capa. Opciones: `clip`, `buffer`, `crop`, `crop_by_row`.
#' @param buffer Distancia (m) de buffer para aplicar el corte de la capa.
#'
#' @return Capas vectoriales de la cartografia digital
#' @name carto_digital
#' @export
#'
#' @import dataPAS
cart_rodales <- function(PAS, rodales, from_RCA = F, RCA = NULL, dec_sup = 2L){
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))
  valid_input(rodales, inherit = "sf", names = c("Nom_Predio", "Tipo_For"))
  valid_input(from_RCA, inherit = "logical")
  valid_input(RCA, inherit = c("character", "NULL"))
  valid_input(dec_sup, inherit = c("integer", "numeric"))

  fuente <- ifelse(
    !from_RCA,
    "Elaboración propia",
    ifelse(
      is.null(RCA),
      "RCA",
      paste0("RCA N°", RCA %>% stringi::stri_extract_all_regex("\\d+"))
    )
  )

  tipo_bos <- switch(
    as.character(PAS),
    "148" = "BN",
    "149" = "PL",
    "No aplica"
  )

  if(rodales$Tipo_For %>%
     as.character() %>%
     stringi::stri_detect_regex("\\d") %>%
     any()
    ) {
    TipoFor_num <- T
  } else {
    TipoFor_num <- F
  }

  rodales_2 <- tryCatch({
    rodales %>%
      {if(!TipoFor_num){
        .[] %>%
          dplyr::mutate_if(
            names(.) == "Tipo_For",
            list(Tipo_For = ~dplyr::case_when(
              .x %>% stringi::stri_detect_regex("no.*aplica", case_insensitive = T) ~ "No aplica",
              .x %>% stringi::stri_detect_regex("alerce", case_insensitive = T) ~ "1",
              .x %>% stringi::stri_detect_regex("araucaria", case_insensitive = T) ~ "2",
              .x %>% stringi::stri_detect_regex("cordillera", case_insensitive = T) ~ "3",
              .x %>% stringi::stri_detect_regex("guaitecas", case_insensitive = T) ~ "4",
              .x %>% stringi::stri_detect_regex("magallanes", case_insensitive = T) ~ "5",
              .x %>% stringi::stri_detect_regex("tepa", case_insensitive = T) ~ "6",
              .x %>% stringi::stri_detect_regex("lenga", case_insensitive = T) ~ "7",
              .x %>% stringi::stri_detect_regex("roble.*raul", case_insensitive = T) ~ "8",
              .x %>% stringi::stri_detect_regex("roble.*hualo", case_insensitive = T) ~ "9",
              .x %>% stringi::stri_detect_regex("siemprev", case_insensitive = T) ~ "10",
              .x %>% stringi::stri_detect_regex("escle", case_insensitive = T) ~ "11",
              .x %>% stringi::stri_detect_regex("palma", case_insensitive = T) ~ "12",
              .default = .x)
            )
          )
      } else . } %>%
      {if(PAS == 148){
        dplyr::mutate_at(., "Tipo_For", as.integer)
      } else . } %>%
      dplyr::mutate_at("N_Rodal", as.integer) %>%
      dplyr::arrange(N_Rodal) %>%
      dplyr::mutate(
        Tipo_Bos = tipo_bos,
        Sup_ha = sf::st_area(geometry) %>%
          units::set_units(ha) %>%
          units::drop_units() %>%
          janitor::round_half_up(dec_sup),
        Fuente = fuente
      ) %>%
      dplyr::select(Nom_Predio, N_Rodal, Tipo_Bos, Tipo_For, Sup_ha, Fuente)
  }, error = function(e) stop(paste("Error en cart_rodales:", e$message), call. = F))

  return(rodales_2)
}

#' @rdname carto_digital
#' @export
cart_area <- function(PAS, areas, dec_sup = 2L, from_RCA = F, RCA = NULL){
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))
  valid_input(areas, inherit = "sf", names = c("Nom_Predio", "N_Area"))
  valid_input(from_RCA, inherit = "logical")
  valid_input(RCA, inherit = c("character", "NULL"))
  valid_input(dec_sup, inherit = c("integer", "numeric"))

  fuente <- ifelse(
    !from_RCA,
    "Elaboración propia",
    ifelse(
      is.null(RCA),
      "RCA",
      paste0("RCA N°", RCA %>% stringi::stri_extract_all_regex("\\d+"))
    )
  )

  tipo_bos <- switch(
    as.character(PAS),
    "148" = "BN",
    "149" = "PL",
    "No aplica"
  )
  areas_2 <- tryCatch({
    areas %>%
      dplyr::mutate(
        Tipo_Bos = tipo_bos,
        Sup_ha = sf::st_area(geometry) %>%
          units::set_units(ha) %>%
          units::drop_units() %>%
          janitor::round_half_up(dec_sup),
        Fuente = fuente
      ) %>%
      dplyr::select(Nom_Predio, N_Area, Tipo_Bos, Sup_ha, Fuente)
  }, error = function(e) stop(paste("Error en cart_area:", e$message), call. = F))

  return(areas_2)
}

#' @rdname carto_digital
#' @export
cart_suelos <- function(PAS, areas, dec_sup = 2, from_RCA = F, RCA = NULL){
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))
  valid_input(from_RCA, inherit = "logical")
  valid_input(RCA, inherit = c("character", "NULL"))
  valid_input(dec_sup, inherit = c("integer", "numeric"))

  if (PAS %in% c(148, 149)) {
    valid_input(areas, inherit = "sf", names = c("Nom_Predio", "Clase_Uso"))
    var_suelo <- dplyr::syms("Clase_Uso")
  }
  if (PAS == 151) {
    valid_input(areas, inherit = "sf", names = c("Nom_Predio", "Clase_Eros"))
    var_suelo <- dplyr::syms(c("Cat_Erosio", "Clase_Eros"))
  }

  fuente <- ifelse(
    !from_RCA,
    "Elaboración propia",
    ifelse(
      is.null(RCA),
      "RCA",
      paste0("RCA N°", RCA %>% stringi::stri_extract_all_regex("\\d+"))
    )
  )

  suelos <- tryCatch({
    areas %>%
      dplyr::mutate(
        Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(dec_sup),
        Fuente = fuente
      ) %>%
      {if (PAS == 151) {
        .[] %>% dplyr::mutate(
          Cat_Erosio = dplyr::case_when(
            Clase_Eros %>% stringi::stri_detect_regex("moderada", case_insensitive = T) ~ "1",
            Clase_Eros %>% stringi::stri_detect_regex("muy severa", case_insensitive = T) ~ "3",
            Clase_Eros %>% stringi::stri_detect_regex("severa", case_insensitive = T) ~ "2",
            .default = "4"
          )
        )
      } else . } %>%
      dplyr::select(Nom_Predio, !!!var_suelo, Sup_ha, Fuente)
  }, error = function(e) stop(paste("Error en cart_suelos:", e$message), call. = F))

  return(suelos)
}

#' @rdname carto_digital
#' @export
cart_rang_pend <- function(PAS, areas, dem, dec_sup = 2){
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))
  valid_input(areas, inherit = "sf", names = c("Nom_Predio"))
  valid_dem(dem)
  valid_input(dec_sup, inherit = c("integer", "numeric"))

  ran_pend <- tryCatch({
    slope_per <- get_slope(dem = dem, x = areas)
    areas %>%
      dplyr::mutate(
        Pend_media = as.numeric(slope_per),
        Sup_ha = sf::st_area(geometry) %>%
          units::set_units(ha) %>%
          units::drop_units() %>%
          janitor::round_half_up(dec_sup),
        Fuente = "Elaboración propia"
      ) %>%
      {if(PAS %in% c(148, 149)){
        .[] %>%
          dplyr::mutate(
            Ran_Pend = dplyr::case_when(
              Pend_media >= 0 & Pend_media < 30 ~ "0% - 30%",
              Pend_media >= 30 & Pend_media < 45 ~ "30% - 45%",
              Pend_media >= 45 & Pend_media < 60 ~ "45% - 60%",
              Pend_media >= 60  ~ "60% y más",
              .default = as.character(Pend_media)
            )
          )
      } else if(PAS == 151){
        .[] %>%
          dplyr::mutate(
            Ran_Pend = dplyr::case_when(
              Pend_media >= 0 & Pend_media < 10 ~ "0% - 10%",
              Pend_media >= 10 & Pend_media < 30 ~ "10% - 30%",
              Pend_media >= 30 & Pend_media < 45 ~ "30% - 45%",
              Pend_media >= 45 & Pend_media < 60 ~ "45% - 60%",
              Pend_media >= 60  ~ "60% y más",
              .default = as.character(Pend_media)
            )
          )
      }} %>%
      dplyr::select(Nom_Predio, Pend_media, Ran_Pend, Sup_ha, Fuente)
  },
  error = function(e) stop(paste("Error en cart_rang_pend:", e$message), call. = F))

  return(ran_pend)
}

#' @rdname carto_digital
#' @export
cart_predios <- function(predios, cut_by_prov = NULL, dec_sup = 2){
  valid_input(predios, inherit = "sf", names = c("Nom_Predio", "Rol"))
  valid_input(cut_by_prov, inherit = c("NULL", "character"))
  valid_input(dec_sup, inherit = c("integer", "numeric"))

  if (!is.null(cut_by_prov)) {
    cut_by_prov <- match.arg(cut_by_prov, choices = unlist(provincias_list))
  }

  if (!"Comuna" %in% names(predios) | (!is.null(cut_by_prov) & !"Provincia" %in% names(predios))) {
    comunas_sf <- sf::read_sf(
      system.file("Comunas.gdb", package = "dataPAS"),
      wkt_filter = sf::st_as_text(sf::st_geometry(
        sf::st_transform(sf::st_union(predios), 5360)
      ))
    ) %>%
      sf::st_set_geometry("geometry") %>%
      sf::st_transform(sf::st_crs(predios))
  }
  predios_2 <- tryCatch({
    predios %>%
      {if(!"Comuna" %in% names(predios)) {
        .[] %>%
          sf::st_intersection(comunas_sf[,c(5:6)] %>% dplyr::rename_all(stringi::stri_trans_totitle)) %>%
          sf::st_collection_extract("POLYGON")
      } else . } %>%
      {if (!is.null(cut_by_prov)) {
        .[] %>%
          {if (!"Provincia" %in% names(predios)){
            .[] %>%
              dplyr::left_join(
                comunas_sf[ ,c(5:6)] %>%
                  dplyr::rename_all(stringi::stri_trans_totitle) %>%
                  sf::st_drop_geometry()
              )
          } else .} %>%
          {if (!any(.[]$Provincia == cut_by_prov)) {
            stop("La provincia no se encuentra dentro de los límites del predio
                 ingresado. Corroborar que se haya ingresado la deseada.", call. = F)
          } else {
            dplyr::filter(., Provincia == cut_by_prov)
          }}
      } else . } %>%
      dplyr::mutate(
        Sup_ha = sf::st_area(geometry) %>%
          units::set_units(ha) %>%
          units::drop_units() %>%
          janitor::round_half_up(dec_sup),
        Fuente = "Elaboración propia"
      ) %>%
      dplyr::select(Nom_Predio, Rol, Comuna, Sup_ha, Fuente) %>%
      suppressWarnings()
  }, error = function(e) stop(paste("Error en cart_predios:", e$message), call. = F))

  if (!"Comuna" %in% names(predios)) {
    warning(
      paste0(
        "Campo \"Comuna\" creado a partir de la DPA 2023 (link descarga: ",
        shQuote("https://www.geoportal.cl/geoportal/catalog/download/912598ad-ac92-35f6-8045-098f214bd9c2"), ")"
      ), call. = F
    )
  }

  return(predios_2)
}

#' @rdname carto_digital
#' @export
cart_parcelas <- function(PAS, bd_flora, rodales, cut_by_rod, include_fp){
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))
  valid_df(bd_flora, names = c("Parcela", "UTM_E", "UTM_E", "N_ind", "Habito", "Cob_BB", "DS_68"))
  valid_input(rodales, inherit = "sf", names = c("Nom_Predio", "N_Rodal"), geometry = "POLYGON")
  valid_input(cut_by_rod, include_fp, inherit = "logical")

  parcelas <- tryCatch({
    prepare_bd_flora(bd_flora, rodales = rodales, PAS = PAS, cut_by_rod = cut_by_rod, include_fp = include_fp) %>%
      dplyr::count(Nom_Predio, N_Rodal, N_Parc, UTM_E, UTM_N) %>%
      dplyr::select(-n) %>%
      dplyr::mutate(Fuente = "Elaboracion propia") %>%
      sf::st_as_sf(coords = c("UTM_E","UTM_N"), crs = sf::st_crs(rodales), remove = F) %>%
      dplyr::rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::starts_with("Coord"), N_Parc),
        ~as.integer(janitor::round_half_up(.))
      ) %>%
      dplyr::arrange(N_Parc) %>%
      dplyr::select(Nom_Predio, N_Rodal, N_Parc, Coord_X, Coord_Y, Fuente)
  }, error = function(e) stop(paste("Error en cart_parcelas:", e$message), call. = F))

  return(parcelas)
}

#' @rdname carto_digital
#' @export
cart_uso_actual <- function(predios, catastro, suelos, dec_sup = 2){
  valid_input(predios, inherit = "sf", names = c("Nom_Predio"))
  valid_input(catastro, inherit = "sf", names = c("USO", "SUBUSO", "ESTRUCTURA"))
  valid_input(suelos, inherit = "sf", names = c("Clase_Uso"))
  valid_input(dec_sup, inherit = c("integer", "numeric"))

  uso_actual <- tryCatch({
    catastro %>%
      sf::st_intersection(predios %>% dplyr::select(Nom_Predio)) %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      my_union(suelos %>% select(Clase_Uso)) %>%
      sf::st_collection_extract("POLYGON") %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      dplyr::select(USO, SUBUSO, ESTRUCTURA, Clase_Uso, Nom_Predio) %>%
      sf::st_make_valid() %>%
      sf::st_collection_extract("POLYGON") %>%
      dplyr::mutate(
        Uso_Actual = dplyr::case_when(
          SUBUSO == "Bosque Nativo" ~ paste(SUBUSO, ESTRUCTURA),
          SUBUSO == "Bosque Mixto" ~ SUBUSO,
          SUBUSO == "Plantación" ~ paste(SUBUSO, "(Otros usos)"),
          USO == "Terrenos Agrícolas" & Clase_Uso %in% c("I", "II", "III", "IV") ~ "Uso agrícola y/o Ganadero (I-IV)",
          USO == "Terrenos Agrícolas" & Clase_Uso %in% c("V", "VI", "VII", "VIII") ~ "Uso agrícola y/o Ganadero (V-VIII)",
          (USO == "Terrenos Agrícolas" & (is.na(Clase_Uso) | Clase_Uso == "N.C.")) ~ "Uso agrícola y/o Ganadero (N.C.)",
          USO == "Áreas Desprovistas de Vegetación" ~ "Áreas sin vegetación",
          .default = paste(USO, "(Otros usos)")
        ),
        Fuente = "Elaboracion propia (A partir de las capas del Catastro de CONAF y suelos de CIREN)"
      ) %>%
      dplyr::group_by(Nom_Predio, Uso_Actual, Fuente) %>%
      dplyr::tally() %>%
      dplyr::ungroup() %>%
      sf::st_collection_extract("POLYGON") %>%
      dplyr::filter(sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up() != 0) %>%
      dplyr::mutate(
        Sup_ha = sf::st_area(geometry) %>%
          units::set_units(ha) %>%
          units::drop_units() %>%
          janitor::round_half_up(dec_sup)
      ) %>%
      dplyr::select(Nom_Predio, Uso_Actual, Sup_ha, Fuente)
  }, error = function(e) stop(paste("Error en cart_uso_actual:", e$message), call. = F))

  return(uso_actual)
}

#' @rdname carto_digital
#' @export
cart_hidro <- function(predios, fuente_hidro = c("MOP", "BCN"), cut = c("clip", "buffer", "crop", "crop_by_row"), buffer = 0){
  valid_input(predios, names = c("Nom_Predio"))
  fuente_hidro <- match.arg(fuente_hidro)
  cut <- match.arg(cut)
  valid_input(buffer, inherit = c("integer", "numeric"))

  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("El buffer no puede ser aplicado cuando el argumento 'cut' es 'clip'. Utilizar otro método", call. = F)
  }

  hidro <- tryCatch({
    dplyr::bind_rows(
      sf::read_sf(
        system.file("Red_hidrografica_XV_XIII.gdb", package = "dataPAS"),
        wkt_filter = sf::st_as_text(
          predios %>%
            sf::st_transform(9155) %>%
            sf::st_buffer(buffer) %>%
            sf::st_bbox() %>%
            sf::st_as_sfc() %>%
            sf::st_geometry()
        )
      ),
      sf::read_sf(
        system.file("Red_hidrografica_VI_X.gdb", package = "dataPAS"),
        wkt_filter = sf::st_as_text(
          predios %>%
            sf::st_transform(9155) %>%
            sf::st_buffer(buffer) %>%
            sf::st_bbox() %>%
            sf::st_as_sfc() %>%
            sf::st_geometry()
        )
      )
    ) %>%
      sf::st_set_geometry("geometry") %>%
      sf::st_transform(sf::st_crs(predios)) %>%
      {if(cut %in% c("clip", "buffer")){
        .[] %>% sf::st_intersection(predios %>% sf::st_buffer(buffer) %>% sf::st_union())
      } else if(cut == "crop"){
        .[] %>% sf::st_crop(predios %>% sf::st_buffer(buffer))
      } else if(cut == "crop_by_row"){
        .[] %>%
          sf::st_intersection(
            1:nrow(predios) %>%
              purrr::map_dfr(
                ~predios[.x, ] %>%
                  sf::st_buffer(buffer) %>%
                  sf::st_bbox() %>%
                  sf::st_as_sfc() %>%
                  sf::st_as_sf()
              ) %>%
              sf::st_union()
          )
      }} %>%
      sf::st_collection_extract("LINESTRING") %>%
      {if(nrow(.) == 0){
        .[]
      } else {
        .[] %>%
          dplyr::select(strahler_n, dplyr::contains(fuente_hidro)) %>%
          dplyr::rename_at(dplyr::vars(dplyr::contains(fuente_hidro)), ~stringi::stri_extract(., regex = ".*(?=_)", mode = "first")) %>%
          dplyr::rename(Etiqueta = nombre) %>%
          dplyr::mutate_at("tipo", stringi::stri_trans_general, "Latin-ASCII") %>%
          dplyr::mutate(
            Tip_Dren = dplyr::case_when(
              stringi::stri_detect_regex(tipo, "rio", case_insensitive = T) ~ 1,
              stringi::stri_detect_regex(tipo, "estero", case_insensitive = T) ~ 2,
              stringi::stri_detect_regex(tipo, "arroyo", case_insensitive = T) ~ 3,
              stringi::stri_detect_regex(tipo, "quebrada", case_insensitive = T) ~ 4,
              .default = 5
            ) %>% as.integer(),
            Tipo_Perma = dplyr::case_when(
              stringi::stri_detect_regex(tipo, "rio", case_insensitive = T) ~ 1,
              strahler_n > 3 ~ 1,
              .default = 2
            ) %>% as.integer(),
            Fuente = paste0("Geoportal (", fuente_hidro, ")")
          ) %>%
          my_union(predios %>% dplyr::select(Nom_Predio)) %>%
          sf::st_collection_extract("LINESTRING") %>%
          dplyr::mutate_at("Nom_Predio", tidyr::replace_na, "S/I") %>%
          dplyr::select(Nom_Predio, Tip_Dren, Tipo_Perma, Fuente, Etiqueta)
      }}
  }, error = function(e) stop(paste("Error en cart_hidro:", e$message), call. = F))

  return(hidro)
}

#' @rdname carto_digital
#' @export
cart_hidro_osm <- function(predios, cut = c("clip", "buffer", "crop", "crop_by_row"), buffer = 0){
  valid_input(predios, inherit = "sf", names = c("Nom_Predio"))
  cut <- match.arg(cut)
  valid_input(buffer, inherit = c("integer", "numeric"))

  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("El buffer no puede ser aplicado cuando el argumento 'cut' es 'clip'. Utilizar otro método", call. = F)
  }
  hidro <- tryCatch({
    bbox <- as.matrix(
      data.frame(
        min = predios %>% sf::st_buffer(buffer) %>% sf::st_transform(4326) %>% sf::st_bbox() %>% .[c(1,2)],
        max = predios %>% sf::st_buffer(buffer) %>% sf::st_transform(4326) %>% sf::st_bbox() %>% .[c(3,4)]
      )
    ) %>% `rownames<-`(c("x","y"))

    hidro_osm <- bbox %>%
      osmdata::opq() %>%
      osmdata::add_osm_feature(key = "waterway") %>%
      osmdata::osmdata_sf()

    hidro_osm$osm_lines %>%
      st_transform(sf::st_crs(predios)) %>%
      {if(cut %in% c("clip", "buffer")){
        .[] %>% sf::st_intersection(predios %>% sf::st_buffer(buffer) %>% sf::st_union())
      } else if(cut == "crop"){
        .[] %>% sf::st_crop(predios %>% sf::st_buffer(buffer))
      } else if(cut == "crop_by_row"){
        .[] %>%
          sf::st_intersection(
            1:nrow(predios) %>%
              purrr::map_dfr(
                ~predios[.x, ] %>%
                  sf::st_buffer(buffer) %>%
                  sf::st_bbox() %>%
                  sf::st_as_sfc() %>%
                  sf::st_as_sf()
              ) %>%
              sf::st_union()
          )
      }} %>%
      sf::st_collection_extract("LINESTRING") %>%
      {if(nrow(.) == 0){
        .[]
      } else {
        .[] %>%
          dplyr::mutate(
            Tip_Dren = dplyr::case_when(
              name %>% stringi::stri_detect_regex("rio", case_insensitive = T) ~ 1,
              name %>% stringi::stri_detect_regex("estero", case_insensitive = T) ~ 2,
              name %>% stringi::stri_detect_regex("quebrada", case_insensitive = T) ~ 4,
              name %>% stringi::stri_detect_regex("canal", case_insensitive = T) ~ 5,
              waterway == "river" ~ 1,
              waterway == "stream" ~ 3,
              waterway == "river" ~ 1,
              .default = 5
            ) %>% as.integer(),
            Tipo_Perma = dplyr::case_when(
              Tip_Dren == 1 ~ 1,
              intermittent == "no" ~ 1,
              .default = 2
            ) %>% as.integer(),
            Fuente = "Elaboración propia (OpenStreetMap)"
          ) %>%
          my_union(predios %>% dplyr::select(Nom_Predio)) %>%
          sf::st_collection_extract("LINESTRING") %>%
          dplyr::mutate_at("Nom_Predio", tidyr::replace_na, "S/I") %>%
          dplyr::rename(Etiqueta = name) %>%
          dplyr::select(Nom_Predio, Tip_Dren, Tipo_Perma, Fuente, Etiqueta, waterway)
      }} %>%
      {if(!is.null(hidro_osm$osm_multilines)){
        .[] %>%
          dplyr::bind_rows(
            hidro_osm$osm_multilines %>%
              sf::st_transform(sf::st_crs(predios)) %>%
              {if(cut %in% c("clip", "buffer")){
                .[] %>% sf::st_intersection(predios %>% sf::st_buffer(buffer) %>% sf::st_union())
              } else if(cut == "crop"){
                .[] %>% sf::st_crop(predios %>% sf::st_buffer(buffer))
              } else if(cut == "crop_by_row"){
                .[] %>%
                  sf::st_intersection(
                    1:nrow(predios) %>%
                      purrr::map_dfr(
                        ~predios[.x, ] %>%
                          sf::st_buffer(buffer) %>%
                          sf::st_bbox() %>%
                          sf::st_as_sfc() %>%
                          sf::st_as_sf()
                      ) %>%
                      sf::st_union()
                  )
              }} %>%
              sf::st_collection_extract("LINESTRING") %>%
              dplyr::mutate(
                Tip_Dren = dplyr::case_when(
                  name %>% stringi::stri_detect_regex("rio", case_insensitive = T) ~ 1,
                  name %>% stringi::stri_detect_regex("estero", case_insensitive = T) ~ 2,
                  name %>% stringi::stri_detect_regex("quebrada", case_insensitive = T) ~ 4,
                  name %>% stringi::stri_detect_regex("canal", case_insensitive = T) ~ 5,
                  waterway == "river" ~ 1,
                  waterway == "stream" ~ 3,
                  waterway == "river" ~ 1,
                  .default = 5
                ) %>% as.integer(),
                Tipo_Perma = dplyr::case_when(
                  Tip_Dren == 1 ~ 1,
                  Tip_Dren == 2 ~ 1,
                  .default = 2
                ) %>% as.integer(),
                Fuente = "Elaboración propia (OpenStreetMap)"
              ) %>%
              my_union(predios %>% dplyr::select(Nom_Predio)) %>%
              sf::st_collection_extract("LINESTRING") %>%
              dplyr::mutate_at("Nom_Predio", tidyr::replace_na, "S/I") %>%
              dplyr::rename(Etiqueta = name) %>%
              dplyr::select(Nom_Predio, Tip_Dren, Tipo_Perma, Fuente, Etiqueta, waterway)
          )
      } else .}
  }, error = function(e) stop(paste("Error en cart_hidro_osm:", e$message), call. = F))

  return(hidro)
}

#' @rdname carto_digital
#' @export
#' @importFrom sf st_as_text st_crs
cart_caminos <- function(predios, cut = c("clip", "buffer", "crop", "crop_by_row"), buffer = 0){
  valid_input(predios, inherit = "sf", names = c("Nom_Predio"))
  cut <- match.arg(cut)
  valid_input(buffer, inherit = c("integer", "numeric"))

  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("El buffer no puede ser aplicado cuando el argumento 'cut' es 'clip'. Utilizar otro método", call. = F)
  }

  caminos <- tryCatch({
    sf::read_sf(
      system.file("Red_vial.gdb", package = "dataPAS"),
      wkt_filter = st_as_text(
        predios %>%
          sf::st_transform(5360) %>%
          sf::st_buffer(buffer) %>%
          sf::st_bbox() %>%
          sf::st_as_sfc() %>%
          sf::st_geometry()
      )
    ) %>%
      sf::st_zm() %>%
      sf::st_transform(st_crs(predios)) %>%
      {if(cut %in% c("clip", "buffer")){
        .[] %>% sf::st_intersection(predios %>% sf::st_buffer(buffer) %>% sf::st_union())
      } else if(cut == "crop"){
        .[] %>% sf::st_crop(predios %>% sf::st_buffer(buffer))
      } else if(cut == "crop_by_row"){
        .[] %>%
          sf::st_intersection(
            1:nrow(predios) %>%
              purrr::map_dfr(
                ~predios[.x, ] %>%
                  sf::st_buffer(buffer) %>%
                  sf::st_bbox() %>%
                  sf::st_as_sfc() %>%
                  sf::st_as_sf()
              ) %>%
              sf::st_union()
          )
      }} %>%
      sf::st_collection_extract("LINESTRING") %>%
      {if(nrow(.) == 0){
        .[]
      } else {
        .[] %>%
          dplyr::mutate(
            Tipo_Cam = ifelse(
              stringi::stri_detect_regex(CLASIFICACION, 'Internacional|Nacional|Regional Principal'),
              1,
              ifelse(
                stringi::stri_detect_regex(CLASIFICACION, 'Regional Provincial|Regional Comunal'),
                2,
                ifelse(stringi::stri_detect_regex(CLASIFICACION, 'Acceso'), 3, 4)
              )
            ) %>% as.integer(),
            Fuente = "Dirección de Vialidad, Ministerio de Obras Públicas"
          ) %>%
          my_union(predios %>% dplyr::select(Nom_Predio)) %>%
          sf::st_collection_extract("LINESTRING") %>%
          dplyr::mutate_at("Nom_Predio", tidyr::replace_na, "S/I") %>%
          dplyr::select(Nom_Predio, Tipo_Cam, Fuente)
      }}
  }, error = function(e) stop(paste("Error en cart_caminos:", e$message), call. = F))

  return(caminos)
}

#' @rdname carto_digital
#' @export
#' @importFrom osmdata add_osm_feature opq osmdata_sf
cart_caminos_osm <- function(predios, cut = c("clip", "buffer", "crop", "crop_by_row"), buffer = 0){
  valid_input(predios, inherit = "sf", names = c("Nom_Predio"))
  cut <- match.arg(cut)
  valid_input(buffer, inherit = c("integer", "numeric"))

  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("El buffer no puede ser aplicado cuando el argumento 'cut' es 'clip'. Utilizar otro método", call. = F)
  }

  caminos <- tryCatch({
    bbox <- as.matrix(
      data.frame(
        min = predios %>% sf::st_buffer(buffer) %>% sf::st_transform(4326) %>% sf::st_bbox() %>% .[c(1,2)],
        max = predios %>% sf::st_buffer(buffer) %>% sf::st_transform(4326) %>% sf::st_bbox() %>% .[c(3,4)]
      )
    ) %>% `rownames<-`(c("x","y"))

    caminos_osm <- bbox %>%
      osmdata::opq() %>%
      osmdata::add_osm_feature(
        key = "highway",
        value = c("motorway", "primary","secondary", "tertiary","residential", "living_street", "unclassified","service", "footway")
      ) %>%
      osmdata::osmdata_sf()

    caminos_osm$osm_lines %>%
      sf::st_transform(sf::st_crs(predios)) %>%
      {if(cut %in% c("clip", "buffer")){
        .[] %>% sf::st_intersection(predios %>% sf::st_buffer(buffer) %>% sf::st_union())
      } else if(cut == "crop"){
        .[] %>% sf::st_crop(predios %>% sf::st_buffer(buffer))
      } else if(cut == "crop_by_row"){
        .[] %>%
          sf::st_intersection(
            1:nrow(predios) %>%
              purrr::map_dfr(
                ~predios[.x, ] %>%
                  sf::st_buffer(buffer) %>%
                  sf::st_bbox() %>%
                  sf::st_as_sfc() %>%
                  sf::st_as_sf()
              ) %>%
              sf::st_union()
          )
      }} %>%
      sf::st_collection_extract("LINESTRING") %>%
      {if(nrow(.) == 0){
        .[]
      } else {
        .[] %>%
          dplyr::mutate(
            Tipo_Cam = dplyr::case_when(
              highway %in% c("primary", "motorway") ~ 1,
              highway %in% c("secondary", "tertiary", "residential") ~ 2,
              highway %in% c("unclassified", "service") ~ 3,
              .default = 4
            ),
            Fuente = "Elaboración propia (OpenStreetMap)"
          ) %>%
          my_union(predios %>% dplyr::select(Nom_Predio)) %>%
          sf::st_collection_extract("LINESTRING") %>%
          dplyr::mutate_at("Nom_Predio", tidyr::replace_na, "S/I") %>%
          dplyr::rename(Etiqueta = name) %>%
          dplyr::select(Nom_Predio, Tipo_Cam, Fuente, Etiqueta, highway)
      }}
  }, error = function(e) stop(paste("Error en cart_caminos_osm:", e$message), call. = F))

  return(caminos)
}

#' @rdname carto_digital
#' @export
cart_curv_niv <- function(predios, dem, cut = c("clip", "buffer", "crop", "crop_by_row"), buffer = 0, step = 10){
  valid_input(predios, inherit = "sf", names = c("Nom_Predio"))
  valid_dem(dem)
  cut <- match.arg(cut)
  valid_input(buffer, step, inherit = c("integer", "numeric"))

  if(cut == "clip" & (buffer > 0)){
    buffer <- 0
    warning("El buffer no puede ser aplicado cuando el argumento 'cut' es 'clip'. Utilizar otro método", call. = F)
  }

  cn <- tryCatch({
    dem_predio <- if (inherits(dem, "SpatRaster")) dem else terra::rast(dem) %>%
      terra::`crs<-`(terra::crs(predios)) %>%
      terra::crop(sf::st_buffer(predios, buffer))

    breaks <- seq(
      plyr::round_any(terra::minmax(dem_predio)[1,1], step, ceiling),
      plyr::round_any(terra::minmax(dem_predio)[2,1], step, floor),
      step
    )

    curv <- dem_predio %>%
      terra::as.contour(levels = breaks) %>%
      sf::st_as_sf() %>%
      sf::st_collection_extract("LINESTRING") %>%
      {if(cut %in% c("clip", "buffer")){
        .[] %>% sf::st_intersection(predios %>% sf::st_buffer(buffer) %>% sf::st_union())
      } else if(cut == "crop"){
        .[] %>% sf::st_crop(predios %>% sf::st_buffer(buffer))
      } else if(cut == "crop_by_row"){
        .[] %>%
          sf::st_intersection(
            1:nrow(predios) %>%
              purrr::map_dfr(
                ~predios[.x, ] %>%
                  sf::st_buffer(buffer) %>%
                  sf::st_bbox() %>%
                  sf::st_as_sfc() %>%
                  sf::st_as_sf()
              ) %>%
              sf::st_union()
          )
      }} %>%
      dplyr::mutate(Fuente = "Elaboracion propia (DEM Alos Palsar 12,5 x 12,5m)") %>%
      sf::st_collection_extract("LINESTRING") %>%
      my_union(predios %>% dplyr::select(Nom_Predio)) %>%
      sf::st_collection_extract("LINESTRING") %>%
      dplyr::mutate_at("Nom_Predio", tidyr::replace_na, "S/I") %>%
      dplyr::rename(Cot_Curva = level) %>%
      dplyr::select(Nom_Predio, Cot_Curva, Fuente)
  }, error = function(e) stop(paste("Error en cart_curv_niv:", e$message), call. = F))

  return(cn)
}

#' @rdname carto_digital
#' @export
#' @importFrom dplyr rename_all
get_carto_digital <- function(
    PAS,
    areas,
    rodales,
    predios,
    dem,
    cut_by_prov = NULL,
    add_parcelas = FALSE,
    bd_flora = NULL,
    cut_by_rod = F,
    include_fp = FALSE,
    from_RCA = FALSE,
    RCA = NULL,
    add_uso_actual = FALSE,
    catastro = NULL,
    suelos = NULL,
    add_caminos = FALSE,
    add_caminos_osm = FALSE,
    caminos_arg = list(cut = "clip", buffer = 0),
    add_hidro = FALSE,
    fuente_hidro = c("MOP", "BCN"),
    add_hidro_osm = FALSE,
    hidro_arg = list(cut = "clip", buffer = 0),
    add_curv_niv = FALSE,
    curv_niv_arg = list(cut = "clip", buffer = 0),
    step = 10,
    dec_sup = 2
){
  PAS <- match.arg(as.character(PAS), choices = c(148, 149, 151))
  valid_input(areas, inherit = "sf", names = c("Nom_Predio", "N_Area"))
  valid_input(rodales, inherit = "sf", names = c("Nom_Predio", "Tipo_fores", "Tipo_For", "Tipo_veg"))
  valid_input(predios, inherit = "sf", names = c("N_Predio", "Nom_Predio", "Rol", "Propietari"))
  valid_input(cut_by_prov, inherit = c("NULL", "character"))
  valid_dem(dem)
  valid_input(
    add_parcelas, cut_by_rod, include_fp, from_RCA, add_uso_actual,
    add_caminos, add_caminos_osm, add_hidro, add_hidro_osm, add_curv_niv, inherit = "logical"
  )
  fuente_hidro <- match.arg(fuente_hidro)
  valid_input(caminos_arg, hidro_arg, curv_niv_arg, inherit = "list")
  valid_input(
    bd_flora,
    inherit = c("data.frame", "NULL"),
    names = c("Parcela", "UTM_E", "UTM_E", "N_ind", "Habito", "Cob_BB", "DS_68")
  )
  valid_input(RCA, inherit = c("character", "NULL"))
  valid_input(catastro, inherit = c("sf", "NULL"), names = c("USO", "SUBUSO", "ESTRUCTURA"))
  valid_input(suelos, inherit = c("sf", "NULL"), names = c("Clase_Uso"))
  valid_input(step, dec_sup, inherit = c("integer", "numeric"))

  if (!is.null(cut_by_prov)) {
    cut_by_prov <- match.arg(cut_by_prov, choices = unlist(provincias_list))
  }

  if (PAS %in% c(148, 149)) {
    var_suelo <- dplyr::syms("Clase_Uso")
  } else {
    var_suelo <- dplyr::syms(c("Clase_Eros", "Cat_Erosio"))
  }
  if (nrow(rodales %>% dplyr::count(N_Rodal)) > nrow(rodales[areas, ] %>% dplyr::count(N_Rodal))) {
    warning("Sobran rodales")
  }
  if (nrow(predios %>% dplyr::count(N_Predio)) > nrow(predios[areas, ])) {
    warning("Sobran predios")
  }

  comunas <- sf::read_sf(
    system.file("Comunas.gdb", package = "dataPAS"),
    wkt_filter = sf::st_as_text(sf::st_geometry(
      sf::st_transform(sf::st_union(predios), 5360)
    ))
  ) %>%
    sf::st_transform(sf::st_crs(predios))

  carto_rodales <- cart_rodales(PAS = PAS, rodales = rodales, dec_sup = dec_sup)

  carto_area <- cart_area(PAS = PAS, areas = areas, dec_sup = dec_sup, from_RCA = from_RCA, RCA = RCA)

  carto_suelos <- cart_suelos(PAS = PAS, areas = areas, dec_sup = dec_sup, from_RCA = from_RCA, RCA = RCA)

  carto_ran_pend <- cart_rang_pend(PAS = PAS, areas = areas, dem = dem, dec_sup = dec_sup)

  carto_predios <- cart_predios(predios = predios, cut_by_prov = cut_by_prov, dec_sup = dec_sup)

  if (add_parcelas) {
    stopifnot(!is.null(bd_flora))
    carto_parcelas <- cart_parcelas(
      PAS = PAS,
      bd_flora = bd_flora,
      rodales = rodales,
      cut_by_rod = cut_by_rod,
      include_fp = include_fp
    )
  }
  if (add_uso_actual) {
    stopifnot(!is.null(catastro) & !is.null(suelos))
    carto_uso_actual <- cart_uso_actual(
      predios = carto_predios,
      catastro = catastro,
      suelos = suelos,
      dec_sup = dec_sup
    )
  }
  if (add_caminos) {
    stopifnot(!is.null(caminos_arg) & is.list(caminos_arg))
    stopifnot(c("cut", "buffer") %in% names(caminos_arg) %>% all())
    carto_caminos <- cart_caminos(
      predios = predios,
      cut = caminos_arg$cut,
      buffer = caminos_arg$buffer
    )
  }
  if (add_caminos_osm) {
    stopifnot(!is.null(caminos_arg) & is.list(caminos_arg))
    stopifnot(c("cut", "buffer") %in% names(caminos_arg) %>% all())
    carto_caminos_osm <- cart_caminos_osm(
      predios = predios,
      cut = caminos_arg$cut,
      buffer = caminos_arg$buffer
    )
  }
  if (add_hidro) {
    stopifnot(!is.null(hidro_arg) & is.list(hidro_arg))
    stopifnot(c("cut", "buffer") %in% names(hidro_arg) %>% all())
    carto_hidro <- cart_hidro(
      predios = predios,
      fuente_hidro = fuente_hidro,
      cut = hidro_arg$cut,
      buffer = hidro_arg$buffer
    ) %>%
      tibble::rowid_to_column("ID")
  }
  if (add_hidro_osm) {
    stopifnot(!is.null(hidro_arg) & is.list(hidro_arg))
    stopifnot(c("cut", "buffer") %in% names(hidro_arg) %>% all())
    carto_hidro_osm <- cart_hidro_osm(
      predios = predios,
      cut = hidro_arg$cut,
      buffer = hidro_arg$buffer
    )
  }
  if (add_curv_niv) {
    stopifnot(!is.null(curv_niv_arg) & is.list(curv_niv_arg))
    stopifnot(c("cut", "buffer") %in% names(curv_niv_arg) %>% all())
    carto_curv_niv <- cart_curv_niv(
      predios = predios,
      dem = dem,
      cut = curv_niv_arg$cut,
      buffer = curv_niv_arg$buffer,
      step = step
    )
  }

  tabla_predios <- carto_predios %>%
    sf::st_join(predios %>% select(N_Predio, Propietari), largest = T) %>%
    dplyr::left_join(
      comunas[ ,4:6] %>%
        st_drop_geometry() %>%
        rename_all(stringi::stri_trans_totitle)
      ) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate_at(dplyr::vars(Nom_Predio, Propietari), tidyr::replace_na, "S/I") %>%
    dplyr::mutate_at(dplyr::vars(Rol), tidyr::replace_na, "S/R") %>%
    dplyr::arrange(N_Predio)

  tabla_areas <- carto_area %>%
    dplyr::select(N_Area, Nom_Predio, Sup_ha) %>%
    sf::st_join(comunas[,4:6], largest = T) %>%
    dplyr::rename(Region = REGION, Provincia = PROVINCIA, Comuna = COMUNA) %>%
    sf::st_join(predios %>% dplyr::select(N_Predio), largest = T) %>%
    sf::st_join(carto_rodales %>% dplyr::select(N_Rodal), largest = T) %>%
    sf::st_join(carto_ran_pend %>% dplyr::select(Pend_media, Ran_Pend), join = sf::st_equals) %>%
    sf::st_join(carto_suelos %>% dplyr::select(!!!var_suelo), join = sf::st_equals) %>%
    {if(exists("carto_hidro")){
      .[] %>% dplyr::mutate(
        Distancia    = c(1:nrow(carto_area)) %>%
          purrr::map_dbl(function(x) {
            sf::st_distance(carto_area[x, ], carto_hidro[sf::st_nearest_feature(carto_area[x, ], sf::st_geometry(carto_hidro)), ]) %>%
              units::drop_units() %>%
              janitor::round_half_up()
          })
      ) %>%
        dplyr::bind_cols(
          carto_hidro[sf::st_nearest_feature(carto_area, sf::st_geometry(carto_hidro)),] %>%
            dplyr::select(ID, Tip_Dren, Tipo_Perma, Etiqueta) %>%
            `names<-`(c("ID_Hidro", "Tipo_Dren", "Tipo_Perma", "Nombre_curso")) %>%
            sf::st_drop_geometry()
        ) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          Nombre_curso = dplyr::case_when(Tipo_Dren == 4 & is.na(Nombre_curso) ~ "Quebrada sin nombre", .default = Nombre_curso),
          Tipo_Perma   = dplyr::case_when(Tipo_Perma == 2 ~ "Temporal", T ~ "Permanente")
        )
    } else .} %>%
    sf::st_drop_geometry() %>%
    dplyr::arrange(N_Predio, N_Area)

  return(
    list(
      Areas = carto_area,
      Rodales = carto_rodales,
      Predios = carto_predios,
      Suelos = carto_suelos %>% {if(PAS == 151) dplyr::select(.,-Clase_Eros) else .},
      Ran_pend = carto_ran_pend %>% dplyr::select(-Pend_media),
      tabla_predios = tabla_predios,
      tabla_areas = tabla_areas,
      Parcelas = if(add_parcelas) carto_parcelas,
      Uso_actual = if(add_uso_actual) carto_uso_actual,
      Caminos = if(add_caminos) carto_caminos,
      Caminos_osm = if(add_caminos_osm) carto_caminos_osm,
      Hidrografia = if(add_hidro) carto_hidro %>% dplyr::select(-ID),
      Hidrografia_osm = if(add_hidro_osm) carto_hidro_osm,
      Curvas_nivel = if(add_curv_niv) carto_curv_niv
    ) %>%
      subset(
        c(rep(T, 7),
          add_parcelas,
          add_uso_actual,
          add_caminos,
          if(add_caminos == F) F else add_caminos_osm,
          add_hidro,
          if(add_hidro == F) F else add_hidro_osm,
          add_curv_niv
        )
      )
  )
}
