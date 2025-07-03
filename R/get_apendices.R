#' Apendices
#'
#' @description
#' Funciones para obtener apendices de los PAS
#'
#' @param PAS PAS correspondiente. Ingresar `148` o `151`.
#' @param bd_flora data.frame con base de datos de parcelas flristicas. Incluir si `add_parcelas` es \code{TRUE.}.
#' @param bd_pcob data.frame con base de datos de parcelas de cobertura.
#' @param rodales Objeto sf con capa de rodales.
#' @param predios Objeto sf con capa de predios.
#' @param portada Portada para los apendices. Disponible: `default` o `KIMAL`.
#' @param provincia Provincia. Character.
#' @param huso Huso. `18S` o `19S`.
#' @param tabla_predios data.frame con informacion de predios obtenida de la cartografia digital.
#' @param tabla_areas data.frame con informacion de areas de corta obtenida de la cartografia digital.
#' @param tabla_attr_rodal data.frame con tabla de atributos de rodal.
#' @param carto_uso_actual Objeto sf con capa de uso actual obtenida de la cartografia digital.
#' @param obras Opcional. Objeto sf con capa de rodales.
#' @param bd_fauna Opcional. data.frame con base de datos de fauna.
#'
#' @return Workbook Excel con apendices
#' @rdname get_apendices
#' @export
#'
apendice_2_3 <- function(
    PAS,
    bd_flora,
    bd_pcob = NULL,
    bd_trans = NULL,
    rodales,
    predios,
    portada = "default",
    provincia,
    huso = NULL
){
  stopifnot(all(c('Parcela', 'UTM_E', 'UTM_N', 'Especie', 'N_ind', 'Habito', 'Cob_BB') %in% names(bd_flora)))
  stopifnot(all(c('Nom_Predio', 'N_Rodal') %in% names(rodales)))
  stopifnot(all(c('N_Predio') %in% names(predios)))
  stopifnot(PAS %in% c(148, 151))
  if (is.null(huso)) {
    huso <- ifelse(
      sf::st_crs(rodales)[[2]] %>% stringi::stri_split_regex("\n") %>% .[[1]] %>% .[length(.)] %>% stringi::stri_extract_all_regex("\\d+") == 32719,
      "19S",
      "18S"
    )
  }
  if(!is.null(bd_pcob)){
    stopifnot(all(c('Parcela', 'Especie', 'Copa_NS', 'Copa_EO') %in% names(bd_pcob)))
  }

  parcelas <- bd_flora %>%
    dplyr::count(Nom_Predio, N_Rodal, Tipo_veg, N_Parc, UTM_E, UTM_N) %>% dplyr::select(-n) %>%
    dplyr::mutate(Fuente = "Elaboracion propia") %>%
    sf::st_as_sf(coords = c("UTM_E","UTM_N"), crs = sf::st_crs(rodales), remove = F) %>%
    dplyr::rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>%
    dplyr::mutate_at(dplyr::vars(dplyr::starts_with("Coord")), janitor::round_half_up) %>%
    dplyr::arrange(N_Parc) %>%
    dplyr::select(Nom_Predio, N_Rodal, Tipo_veg, N_Parc, Coord_X, Coord_Y, Fuente)

  ## Preparacion de datos  ----
  df_flora <- bd_flora %>%
    {if (dplyr::count(.[], N_Parc, Especie, sort = T) %>% dplyr::filter(n > 1) %>% nrow() >= 1){
      .[] %>% dplyr::group_by(N_Parc, UTM_E, UTM_N, Especie) %>% dplyr::slice_sample() %>% dplyr::ungroup()
    } else .[]} %>%
    {if (PAS == 148) {
      .[] %>% dplyr::mutate(
        Cob_arb = dplyr::case_when(
          Cob_BB == "r" ~ 1,
          Cob_BB == "+" ~ 3,
          Cob_BB == "1" ~ 5,
          Cob_BB == "2" ~ 7.5,
          Cob_BB == "3" ~ 17.5,
          Cob_BB == "4" ~ 37.5,
          Cob_BB == "5" ~ 62.5,
          Cob_BB == "6" ~ 87.5,
          .default = 0
        ),
        Fuente = "Flora"
      )
    } else {
      .[] %>% dplyr::mutate(
        Cob_arb = dplyr::case_when(
          Cob_BB == "r" ~ "1",
          Cob_BB == "+" ~ "3",
          Cob_BB == "1" ~ "<5",
          Cob_BB == "2" ~ "5-10",
          Cob_BB == "3" ~ "10-25",
          Cob_BB == "4" ~ "25-50",
          Cob_BB == "5" ~ "50-75",
          Cob_BB == "6" ~ "75-100",
          .default = ""
        ),
        Fuente = "Flora"
      )
    }} %>%
    {if(PAS == 148) {
      .[] %>% dplyr::group_by(N_Parc) %>%
        dplyr::mutate(Cob_parc = sum(Cob_arb, na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::select(N_Parc, UTM_E, UTM_N, Especie, Nha, Cob_arb, Cob_parc, Fuente)
    } else {
      .[] %>%
        dplyr::select(N_Parc, UTM_E, UTM_N, Especie, Habito, Nha, Cob_arb)
    }}


  if (!is.null(bd_pcob)) {
    df_pcob <- bd_pcob %>%
      dplyr::select(-dplyr::starts_with("UTM")) %>%
      dplyr::inner_join(
        bd_flora %>% dplyr::count(Parcela, N_Parc, UTM_E, UTM_N) %>% dplyr::select(-n)
      ) %>%
      dplyr::mutate(
        Cob_arb = purrr::map2_dbl(Copa_NS, Copa_EO, cup_coverage, method = "ellipse")
      ) %>%
      dplyr::group_by(N_Parc, UTM_E, UTM_N, Especie) %>%
      dplyr::summarise(
        Cob_arb = sum(Cob_arb, na.rm = T),
        Nha = dplyr::n() * 20
      ) %>%
      dplyr::group_by(N_Parc) %>%
      dplyr::mutate(
        Cob_parc_p = plot_coverage(Cob_arb, percent = T, digits = 1, decimal.mark = ","),
        Cob_parc = purrr::map_dbl(
          Cob_parc_p,
          ~as.numeric(stringi::stri_extract_all_regex(stringi::stri_replace_all_regex(., ",", "\\."), "\\d+\\.\\d"))
        ),
        Fuente = "PCob"
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at("Cob_arb", ~janitor::round_half_up(. / 5, 2)) %>%
      dplyr::select(N_Parc, UTM_E, UTM_N, Especie, Nha, Cob_arb, Cob_parc, Fuente) %>%
      {if(nrow(.[]) == 0) NULL else .}
  } else {
    df_pcob <- NULL
  }

  if(PAS == 151 & !is.null(bd_trans)) {
    df_trans <- bd_trans %>%
      dplyr::filter(
        Parcela %in% unique(bd_flora$Parcela) &
        !(Especie %>% stringi::stri_detect_regex("Suelo.*desnudo|identificac|indeter", case_insensitive = T) | is.na(Especie)) &
        (Habito %>% stringi::stri_detect_regex("arbust", case_insensitive = T) |
         Habito %>% stringi::stri_detect_regex("sucul", case_insensitive = T))) %>%
      {if (dplyr::count(.[], Parcela, Especie, sort = T) %>% dplyr::filter(n > 1) %>% nrow() >= 1){
        .[] %>% dplyr::group_by(parcela, Especie) %>% dplyr::slice_sample() %>% dplyr::ungroup()
      } else .[]} %>%
      {if (dplyr::anti_join(.[], bd_flora %>% dplyr::count(Parcela, N_Parc, UTM_E, UTM_N)) %>% nrow() > 0) {
        warning(
          "Inconsistencias en las parcelas: ",
          dplyr::anti_join(.[], bd_flora %>% dplyr::count(Parcela, N_Parc, UTM_E, UTM_N)) %>%
            dplyr::pull(Parcela) %>% unique() %>% shQuote() %>% paste(collapse = " - "),
          "Se utilizó inner_join en vez de left_join"
        )
        .[] %>% dplyr::inner_join(bd_flora %>% dplyr::count(Parcela, N_Parc, UTM_E, UTM_N) %>% select(-n))
      } else {
        .[] %>% dplyr::left_join(bd_flora %>% dplyr::count(Parcela, N_Parc, UTM_E, UTM_N) %>% select(-n))
      }} %>%
      dplyr::rename(Trans_cob = Cobertura) %>%
      dplyr::mutate_at("Trans_cob", ~janitor::round_half_up(. * 100, 2)) %>%
      dplyr::select(N_Parc, UTM_E, UTM_N, Especie, Trans_cob) %>%
      {if(nrow(.[]) == 0) NULL else .}
  } else {
    df_trans <- NULL
  }

  flextable::set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )

  ## Apendice 2 ----
  if (PAS == 148) {
    ft_2 <- dplyr::bind_rows(df_flora, df_pcob) %>%
      tidyr::pivot_wider(
        names_from = Fuente,
        values_from = c(Nha, Cob_arb, Cob_parc)
      ) %>%
      dplyr::arrange(N_Parc, Especie) %>%
      dplyr::select(N_Parc, UTM_E, UTM_N, Especie, dplyr::ends_with("PCob"), dplyr::ends_with("Flora")) %>%
      `names<-`(
        c(
          "Parcela",
          paste0("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Este"),
          paste0("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Norte"),
          "Especie",
          {if(!is.null(df_pcob)){
            c("P.COB_NHA\n(árb/ha)",
              "P.COB_Cobertura por especie (%)",
              "P.COB_Cobertura por parcela (%)",
              "FLORA_NHA\n(árb/ha)",
              "FLORA_Cobertura por especie (%)",
              "FLORA_Cobertura por parcela (%)")
          }},
          "NHA\n(árb/ha)",
          "Cobertura por especie (%)",
          "Cobertura por parcela (%)"
        )
      ) %>%
      flextable::flextable() %>%
      flextable::separate_header(split = "_") %>%
      flextable::merge_v(j = c(1:3, 7, 10)) %>%
      flextable::italic(j = 4) %>%
      flextable::autofit() %>%
      flextable::theme_box() %>%
      {if(!is.null(df_pcob)) {
        flextable::bg(.,i = ~ `P.COB_Cobertura por parcela (%)` < 10, j = 7, bg = "yellow") %>%
        flextable::bg(i = ~ `FLORA_Cobertura por parcela (%)` < 10, j = 10, bg = "yellow")
      } else {
        flextable::bg(.,i = ~ `Cobertura por parcela (%)` < 10, j = 7, bg = "yellow")
      }} %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center") %>%
      flextable::bg(bg = "#bcc5d4", part = "header")
  } else {
    ft_2 <- df_flora %>%
      {if(!is.null(df_pcob)){
        .[] %>% dplyr::left_join(
          df_pcob %>%
            dplyr::select(N_Parc, Especie, Cob_arb) %>% dplyr::rename(Pcob_cob = Cob_arb)
        )
      } else .[]} %>%
      {if(!is.null(df_trans)){
        .[] %>% dplyr::left_join(
          df_trans %>% dplyr::select(N_Parc, Especie, Trans_cob)
        )
      } else .[]} %>%
      dplyr::arrange(N_Parc, Especie) %>%
      `names<-`(
        c(
          "Parcela",
          paste0("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Este"),
          paste0("Coordenadas UTM Datum WGS84 Huso ", huso, "_Coordenada Norte"),
          "Especie",
          "Hábito",
          "NHA\n(árb/ha)",
          "Cobertura (%)",
          {if(!is.null(df_pcob)){
            c("Cobertura P.Cob (%)")
          }},
          {if(!is.null(df_trans)){
            c("Cobertura Trans (%)")
          }}
        )
      ) %>%
      flextable::flextable() %>%
      flextable::separate_header(split = "_") %>%
      flextable::merge_v(j = c(1:3)) %>%
      flextable::italic(j = 4) %>%
      flextable::autofit() %>%
      flextable::bg(
        i = ~ `Hábito` %>%
          stringi::stri_trans_general("Latin-ASCII") %>%
          stringi::stri_detect_regex("arbol", case_insensitive = T) &
          `Cobertura (%)` %in% c("10-25", "25-50", "50-75", "75-100"),
        j = 7, bg = "yellow"
      ) %>%
      flextable::bg(
        i = ~ `Hábito` %>%
          stringi::stri_trans_general("Latin-ASCII") %>%
          stringi::stri_detect_regex("arbol", case_insensitive = T) &
          `Cobertura P.Cob (%)` >= 10,
        j = 7, bg = "yellow"
      ) %>%
      flextable::theme_box() %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center") %>%
      flextable::bg(bg = "#bcc5d4", part = "header")
  }

  wb_ap2 <- openxlsx2::wb_workbook(theme = "Integral") %>%
    {if(portada == "KIMAL"){
      wb_portada_kimal(., PAS = PAS, apendice = 2, provincia = provincia)
    } else {
      wb_portada_default(., PAS = PAS, apendice = 2, provincia = provincia)
    }} %>%
    openxlsx2::wb_add_worksheet("SP_Nha_y_Cobertura_Parcelas", grid_lines = F) %>%
    flexlsx::wb_add_flextable(sheet = "SP_Nha_y_Cobertura_Parcelas", ft = ft_2, start_col = 1, start_row = 1)

  ## Apendice 3 ----
  ft_3 <- parcelas %>%
    sf::st_join(predios %>% dplyr::select(N_Predio)) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate_at("Tipo_veg", italic_sp) %>%
    dplyr::select(N_Predio, N_Rodal, N_Parc, Tipo_veg, Coord_X, Coord_Y) %>%
    `names<-`(
      c(
        'N° Predio',
        'N° Rodal',
        'N° Parcela',
        'Tipo vegetacional',
        paste0("Coordenadas UTM Datum WGS84 Huso ", huso,"_Coordenada Este"),
        paste0("Coordenadas UTM Datum WGS84 Huso ", huso,"_Coordenada Norte")
      )
    ) %>%
    flextable::flextable() %>%
    flextable::merge_v(j = c(1:2)) %>%
    ftExtra::colformat_md() %>%
    flextable::separate_header(split = "_") %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  wb_ap3 <- openxlsx2::wb_workbook(theme = "Integral") %>%
    {if(portada == "KIMAL"){
      wb_portada_kimal(., PAS = PAS, apendice = 3, provincia = provincia)
    } else {
      wb_portada_default(., PAS = PAS, apendice = 3, provincia = provincia)
    }} %>%
    openxlsx2::wb_add_worksheet("Ubicación_Parcelas", grid_lines = F) %>%
    flexlsx::wb_add_flextable(sheet = "Ubicación_Parcelas", ft = ft_3, start_col = 1, start_row = 1)

  return(
    list(
      Apendice_2 = wb_ap2,
      Apendice_3 = wb_ap3
    )
  )
}

#' @rdname get_apendices
#' @export
apendice_5_PAS148 <- function(
    bd_flora,
    rodales,
    tabla_predios,
    tabla_areas,
    tabla_attr_rodal,
    portada = "default",
    provincia,
    carto_uso_actual = NULL,
    obras = NULL,
    bd_fauna = NULL
){
  # Configuracion flextable ----
  flextable::set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )

  wb_ap5 <- openxlsx2::wb_workbook(theme = "Integral") %>%
    {if(portada == "KIMAL"){
      wb_portada_kimal(., PAS = 148, apendice = 5, provincia = provincia)
    } else {
      wb_portada_default(., PAS = 148, apendice = 5, provincia = provincia)
    }}

  new_border <- openxlsx2::create_border(
    bottom = "thin", bottom_color = openxlsx2::wb_color("black"),
    top = "thin", top_color = openxlsx2::wb_color("black"),
    left = "thin", left_color = openxlsx2::wb_color("black"),
    right = "thin", right_color = openxlsx2::wb_color("black")
  )
  wb_ap5$styles_mgr$add(new_border, "new_border")
  new_fill <- openxlsx2::create_fill(patternType = "solid", fgColor = openxlsx2::wb_color(hex = "#bcc5d4"))
  wb_ap5$styles_mgr$add(new_fill, "new_fill")
  new_font <- openxlsx2::create_font(b = TRUE, color = openxlsx2::wb_color("black"))
  wb_ap5$styles_mgr$add(new_font, "new_font")
  header_cellxfs <- openxlsx2::create_cell_style(
    num_fmt_id = 0,
    horizontal = "center",
    text_rotation = 0,
    fill_id = wb_ap5$styles_mgr$get_fill_id("new_fill"),
    font_id = wb_ap5$styles_mgr$get_font_id("new_font"),
    border_id = wb_ap5$styles_mgr$get_border_id("new_border")
  )
  wb_ap5$styles_mgr$add(header_cellxfs, "header_cellxfs")

  # Tabla predios ----
  tbl_predios <- tabla_predios %>%
    dplyr::select(N_Predio, Nom_Predio, Rol, Propietari, Comuna, Provincia, Region) %>%
    `names<-`(c("N° Predio", "Nombre Predio", "Rol", "Propietario", "Comuna", "Provincia", "Region"))

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Info.Predios") %>%
    openxlsx2::wb_add_data(sheet = "Info.Predios", x = tbl_predios, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = openxlsx2::wb_dims(x = tbl_predios, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tbl_predios, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tbl_predios)), width = "auto")

  # Tabla superficie predios ----
  tbl_predios_sup <- tabla_predios %>%
    dplyr::mutate(Titulo_dominio = "-", SII = "-") %>%
    dplyr::select(N_Predio, Titulo_dominio, SII, Sup_ha) %>%
    `names<-`(c("N° Predio", "Título de dominio", "servicio de Impuestos Internos", "Estudio Técnico"))

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Sup.Predios") %>%
    openxlsx2::wb_add_data(sheet = "Sup.Predios", x = tbl_predios_sup, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = openxlsx2::wb_dims(x = tbl_predios_sup, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tbl_predios_sup, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_add_numfmt(dims = openxlsx2::wb_dims(x = tbl_predios_sup, cols = 4, select = "data"), numfmt = "#,##0.00") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tbl_predios_sup)), width = "auto")

  # Tabla uso actual ----
  if (!is.null(carto_uso_actual)) {
    tbl_uso_actual <- carto_uso_actual %>%
      dplyr::left_join(tabla_predios %>% select(N_Predio, Nom_Predio)) %>%
      sf::st_drop_geometry() %>%
      dplyr::select(N_Predio, Uso_Actual, Sup_ha) %>%
      dplyr::mutate(
        Uso_Actual = dplyr::case_when(
          Uso_Actual %>% stringi::stri_detect_regex("Bosque") ~ paste0("Bosques_", Uso_Actual),
          Uso_Actual %>% stringi::stri_detect_regex("Uso agrícola") ~ Uso_Actual %>% stringi::stri_replace_all_regex(" \\(", "_") %>% stringi::stri_replace_all_regex("\\)", ""),
          Uso_Actual %>% stringi::stri_detect_regex("Otros usos") ~ "Otros usos",
          .default = Uso_Actual
        )
      ) %>%
      dplyr::group_by(N_Predio, Uso_Actual) %>%
      dplyr::summarise(Sup_ha = sum(Sup_ha), .groups = "drop") %>%
      tidyr::pivot_wider(
        names_from = Uso_Actual, values_from = Sup_ha
      ) %>%
      dplyr::mutate(Total = rowSums(select(., -c(1)), na.rm = TRUE)) %>%
      dplyr::select(
        N_Predio,
        dplyr::matches("Bosques.*Adulto"),
        dplyr::matches("Bosques.*Renoval"),
        dplyr::matches("Bosques.*Mixto"),
        dplyr::matches("Uso.*I-IV"),
        dplyr::matches("Uso.*V-VIII"),
        dplyr::matches("Uso.*N.C."),
        dplyr::starts_with("Áreas"),
        dplyr::starts_with("Otros"),
        Total
      ) %>%
      dplyr::rename("N° Predio" = N_Predio) %>%
      flextable::flextable() %>%
      flextable::separate_header(split = "_") %>%
      flextable::autofit() %>%
      flextable::theme_box() %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center") %>%
      flextable::bg(bg = "#bcc5d4", part = "header")
  }
  if (exists("tbl_uso_actual")) {
    wb_ap5 <- wb_ap5 %>%
      openxlsx2::wb_add_worksheet("Uso_Actual", grid_lines = F) %>%
      flexlsx::wb_add_flextable(sheet = "Uso_Actual", ft = tbl_uso_actual, start_col = 1, start_row = 1)
  }

  # Tabla suelos ----
  tbl_suelo <- tabla_areas %>%
    dplyr::select(N_Predio, N_Area, Clase_Uso, Pend_media, Sup_ha) %>%
    `names<-`(c("Predio N°", "Área N°", "CUS", "Pendiente media (%)", "Superficie (ha)")) %>%
    flextable::flextable() %>%
    flextable::merge_v(j = c(1)) %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Suelos", grid_lines = F) %>%
    flexlsx::wb_add_flextable(sheet = "Suelos", ft = tbl_suelo, start_col = 1, start_row = 1)

  # Tabla hidrografia ----
  if (any(c("Nombre_curso", "Tipo_Perma", "Distancia", "Ancho") %in% names(tabla_areas))) {
    tbl_hidro <- tabla_areas %>%
      dplyr::mutate(Ancho = "") %>%
      dplyr::select(N_Predio, N_Area, Nombre_curso, Tipo_Perma, Distancia, Ancho) %>%
      `names<-`(c("Predio N°", "Área N°", "Cursos de agua", "Temporalidad", "Distancia al área a intervenir (m)", "Ancho del cauce (m)")) %>%
      flextable::flextable() %>%
      flextable::merge_v(j = c(1)) %>%
      flextable::autofit() %>%
      flextable::theme_box() %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center") %>%
      flextable::bg(bg = "#bcc5d4", part = "header")

    wb_ap5 <- wb_ap5 %>%
      openxlsx2::wb_add_worksheet("Recursos_hídricos", grid_lines = F) %>%
      flexlsx::wb_add_flextable(sheet = "Recursos_hídricos", ft = tbl_hidro, start_col = 1, start_row = 1)
  }

  # Tabla vegetacion ----
  parc_x_tipo <- bd_flora %>%
    dplyr::group_by(N_Rodal, Tipo_veg, N_Parc) %>%
    dplyr::summarise_at("Nha", sum) %>%
    dplyr::ungroup() %>%
    split(.$Tipo_veg) %>%
    purrr::map(function(x) {
      out <- grDevices::boxplot.stats(x$Nha)$out
      x %>% dplyr::filter(!Nha %in% c(out))
    }) %>%
    dplyr::bind_rows()

  nha_est_x_tipo <- bd_flora %>%
    dplyr::filter(N_Parc %in% parc_x_tipo$N_Parc) %>%
    dplyr::select(Tipo_veg, N_Parc, Especie, Nha) %>%
    split(.$Tipo_veg) %>%
    purrr::map(~complete(.,Tipo_veg, N_Parc, Especie, fill = list(Nha = 0))) %>%
    purrr::map(function(x) {
      x %>%
        dplyr::group_by(Tipo_veg, Especie) %>%
        dplyr::summarise(Nha = mean(Nha, na.rm = T) %>% janitor::round_half_up()) %>%
        dplyr::ungroup()
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(Tipo_veg) %>%
    dplyr::mutate(Percentage = Nha/sum(Nha)) %>%
    dplyr::filter(Percentage > 0.05) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Tipo_veg, desc(Nha)) %>%
    suppressMessages() %>% suppressWarnings()

  nha_ptos <- tabla_areas %>%
    dplyr::left_join(
      tabla_attr_rodal %>%
        mutate(Parcelas = purrr::map(Nom_attr, function(x) {
          x %>%
            stringi::stri_replace_all_regex("Parcela ", "") %>%
            stringi::stri_split_regex("-") %>%
            unlist() %>%
            as.integer()
        }))
    ) %>%
    dplyr::filter(Nom_attr %>% stringi::stri_detect_regex("Parcela")) %>%
    dplyr::mutate(
      nha_x_sp = purrr::map(Parcelas, nha_x_sp_fun, bd = bd_flora)
    ) %>%
    dplyr::select(N_Predio, N_Rodal, N_Area, nha_x_sp) %>%
    tidyr::unnest_legacy() %>%
    dplyr::arrange(N_Predio, N_Area) %>%
    suppressMessages() %>% suppressWarnings()

  nha_est <- tabla_areas %>%
    dplyr::left_join(
      tabla_attr_rodal %>%
        dplyr::select(N_Rodal, Tipo_veg, Tipo_attr)
    ) %>%
    dplyr::filter(Tipo_attr == "Estimación") %>%
    dplyr::left_join(nha_est_x_tipo) %>%
    tidyr::drop_na(Nha) %>%
    dplyr::select(N_Predio, N_Rodal, N_Area, Especie, Nha) %>%
    suppressMessages() %>% suppressWarnings()

  nha_otros <- tabla_areas %>%
    dplyr::left_join(tabla_attr_rodal %>% dplyr::select(N_Rodal, Subtipo_fo, Tipo_veg, Tipo_attr)) %>%
    dplyr::filter(Tipo_veg %in% c(setdiff(unique(rodales$Tipo_veg), unique(bd_flora$Tipo_veg)))) %>% # filtrar areas con tipos vegetacionales sin parcelas
    dplyr::mutate(sp = purrr::map(Tipo_veg, function(x){
      x %>%
        stringi::stri_sub(from = x %>% stringi::stri_locate_all_regex(" de ") %>% unlist() %>% max() %>% .[]+1) %>%
        stringi::stri_split_regex("-") %>%
        unlist() %>%
        stringi::stri_trim()
    })) %>%
    dplyr::mutate(
      bd1 = purrr::map2(sp, Subtipo_fo, function(x, y) {
        bd_flora %>%
          dplyr::filter(
            N_Parc %in% c(
              bd_flora %>%
                dplyr::filter(Especie %in% x, Subtipo_fo == y) %>%
                dplyr::group_by(N_Parc, Especie) %>%
                dplyr::tally() %>%
                dplyr::group_by(N_Parc) %>%
                dplyr::filter(dplyr::n() == length(x)) %>%
                .$N_Parc %>% unique()
            )
          ) %>%
          dplyr::select(N_Parc, Especie, Nha) %>%
          dplyr::arrange(N_Parc, dplyr::desc(Nha)) %>%
          tidyr::complete(N_Parc, Especie)
      }),
      nha_x_sp = purrr::map(bd1, function(x) {
        x %>%
          dplyr::filter(!Especie %in% c(x %>% dplyr::filter(is.na(Nha)) %>% .$Especie %>% unique())) %>%
          dplyr::group_by(Especie) %>%
          dplyr::summarise(Nha = mean(Nha) %>% round())
      }),
      Parcelas = purrr::map(bd1, function(x){
        unique(x$N_Parc)
      })
    ) %>%
    dplyr::mutate_at("Parcelas", ~purrr::map(., paste, collapse = ", ")) %>%
    dplyr::select(N_Predio, N_Rodal, N_Area, nha_x_sp, Parcelas) %>%
    tidyr::unnest(nha_x_sp) %>%
    tidyr::unnest(Parcelas) %>%
    suppressMessages() %>% suppressWarnings()

  obs_fun <- function(x){
    x <- if (is.na(x)) NA else x %>% stringi::stri_split_regex(pattern = "[:punct:]") %>% .[[1]] %>% stringi::stri_trim()
    if (is.na(x) %>% all()) {
      NA_character_
    } else if (length(x) == 1) {
      paste("Se utilizó la parcela ", x)
    } else {
      paste("Se utilizaron las parcelas: ", paste(x %>% shQuote(), collapse = ", "))
    }
  }

  tabla_attr_rodal_final <- tabla_attr_rodal %>%
    dplyr::mutate(
      Nom_attr = dplyr::case_when(
        !N_Rodal %in% unique(dplyr::bind_rows(nha_ptos, nha_est, nha_otros) %>% .$N_Rodal) ~ "Encontrar otro método de estimación",
        .default = Nom_attr
      )
    ) %>%
    dplyr::left_join(
      nha_otros %>%
        dplyr::count(N_Rodal, Parcelas) %>%
        dplyr::select(-n) %>%
        dplyr::bind_rows(
          parc_x_tipo %>%
            dplyr::group_by(Tipo_veg) %>%
            dplyr::summarise(Parcelas = paste(N_Parc, collapse = ", "), .groups = "drop") %>%
            dplyr::inner_join(tabla_attr_rodal %>% dplyr::filter(Nom_attr == "Estimación por Tipo vegetacional") %>% dplyr::select(Tipo_veg, N_Rodal)) %>%
            dplyr::select(-Tipo_veg)
        )
    ) %>%
    dplyr::mutate(Parcelas = purrr::map_chr(Parcelas, obs_fun))

  df_veg <- dplyr::bind_rows(nha_ptos, nha_est, nha_otros) %>%
    dplyr::arrange(N_Predio, N_Area, Nha) %>%
    dplyr::mutate_at("Nha", as.integer) %>%
    dplyr::left_join(
      tabla_areas %>% dplyr::select(N_Rodal, N_Area, Sup_ha)
    ) %>%
    dplyr::left_join(
      tabla_attr_rodal %>%
        dplyr::select(N_Rodal, Tipo_fores, Subtipo_fo)
    ) %>%
    dplyr::mutate_at("Tipo_fores", stringi::stri_replace_all_regex, "Tipo Forestal ", "") %>%
    dplyr::mutate(
      Estructura = "",
      Estado_desarrollo = "",
      Estado_fitosanitario = ""
    ) %>%
    dplyr::arrange(N_Predio, N_Area, dplyr::desc(Nha)) %>%
    dplyr::select(N_Predio, N_Area, Tipo_fores, Subtipo_fo, Sup_ha, Especie, Nha, Estructura, Estado_desarrollo, Estado_fitosanitario)

  stf <- df_veg %>%
    tibble::rowid_to_column("ID") %>%
    dplyr::group_by(Subtipo_fo) %>%
    dplyr::slice_min(ID) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ID) %>%
    dplyr::select(-ID)

  ft_veg <- df_veg %>%
    dplyr::select(-Subtipo_fo) %>%
    `names<-`(c("Predio N°", "Área N°", "Tipo forestal", "Superficie (ha)", "Especies dominantes", "Densidad (ind/ha)", "Estructura actual", "Estado de desarrollo", "Estado sanitario")) %>%
    flextable::flextable() %>%
    flextable::merge_v(j = c(1:2, 4)) %>%
    flextable::italic(j = 5) %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  for (i in seq_len(nrow(stf))) {
    ft_veg <- flextable::footnote(
      ft_veg,
      part = 'body', i = c(which(df_veg$Subtipo_fo == stf$Subtipo_fo[i])), j = 3,
      value = flextable::as_paragraph(stf$Subtipo_fo[i]),
      ref_symbols = paste(rep('*', i), collapse = "")
    )
  }

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Vegetación", grid_lines = F) %>%
    flexlsx::wb_add_flextable(sheet = "Vegetación", ft = ft_veg, start_col = 1, start_row = 1) %>%
    openxlsx2::wb_add_worksheet(sheet = "Attr_Rodal") %>%
    openxlsx2::wb_add_data(sheet = "Attr_Rodal", x = tabla_attr_rodal_final, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = openxlsx2::wb_dims(x = tabla_attr_rodal_final, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tabla_attr_rodal_final, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tabla_attr_rodal_final)), width = "auto")

  # Tabla de la corta ----
  df_corta <- tabla_areas %>%
    dplyr::left_join(
      rodales %>%
        dplyr::count(N_Rodal, Tipo_fores, Subtipo_fo) %>%
        sf::st_drop_geometry()
    ) %>%
    dplyr::mutate(Ano = "-") %>%
    dplyr::select(N_Predio, N_Area, Sup_ha, Ano, Clase_Uso, Tipo_fores, Subtipo_fo) %>%
    dplyr::arrange(N_Predio, N_Area) %>%
    dplyr::mutate_at("N_Area", as.character) %>%
    janitor::adorn_totals()

  ft_corta <- df_corta %>%
    dplyr::select(-Subtipo_fo) %>%
    `names<-`(c("Predio N°", "Área a intervenir_N°", "Área a reforestar_Superficie (ha)", "Año", "Clase Capac. Uso", "Tipo forestal y/o especies a eliminar")) %>%
    flextable::flextable() %>%
    flextable::separate_header(split = "_") %>%
    flextable::merge_v(j = c(1)) %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::merge_h_range(
      i = ~`Predio N°` == "Total",
      j1 = "Predio N°",
      j2 = "Área a intervenir_N°",
      part = "body"
    ) %>%
    flextable::align(i = ~`Predio N°` == "Total", j = c(1:2), align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  for (i in seq_len(nrow(stf))) {
    ft_corta <- flextable::footnote(
      ft_corta,
      part = 'body', i = c(which(df_corta$Subtipo_fo == stf$Subtipo_fo[i])), j = 6,
      value = flextable::as_paragraph(stf$Subtipo_fo[i]),
      ref_symbols = paste(rep('*', i), collapse = "")
    )
  }
  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Corta", grid_lines = F) %>%
    flexlsx::wb_add_flextable(sheet = "Corta", ft = ft_corta, start_col = 1, start_row = 1)

  # Tabla resumen ----
  tbl_resumen <- tabla_areas %>%
    dplyr::group_by(Comuna, Provincia, Region, N_Predio) %>%
    dplyr::summarise(Sup_ha = sum(Sup_ha)) %>%
    dplyr::group_by(Comuna, Provincia, Region) %>%
    dplyr::summarise(N_Predio = dplyr::n(), Sup_ha = sum(Sup_ha)) %>%
    dplyr::mutate(N_Predio_ref = as.integer(NA), Sup_ha_ref = as.double(NA)) %>%
    janitor::adorn_totals() %>%
    `names<-`(c("Comuna", "Provincia", "Region", "Corta_N° predios", "Corta_Superficie (ha)", "Reforestación_N° predios", "Reforestación_Superficie (ha)")) %>%
    flextable::flextable() %>%
    flextable::separate_header(split = "_") %>%
    flextable::autofit() %>%
    flextable::merge_h_range(i = ~ Comuna == "Total", j1 = "Comuna", j2 = "Region", part = "body") %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::align(i = ~ Comuna == "Total", align = "center", j = c(1:3), part = "body") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Resumen", grid_lines = F) %>%
    flexlsx::wb_add_flextable(sheet = "Resumen", ft = tbl_resumen, start_col = 1, start_row = 1)

  # Tabla obras ----
  # if (!is.null(obras)) {
  #   tbl_obras <- areas %>%
  #     sf::st_intersection(obras %>% dplyr::select(Tipo, Obra)) %>%
  #     dplyr::count(Tipo, Obra) %>% dplyr::select(-n) %>%
  #     dplyr::mutate(
  #       Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2),
  #       Sup_m2 = sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(),
  #     ) %>%
  #     sf::st_drop_geometry() %>%
  #     janitor::adorn_totals() %>%
  #     `names<-`(c("Temporalidad de la obra", "Obra", "Superficie (ha)", "Superficie (m2)")) %>%
  #     flextable::flextable() %>%
  #     flextable::separate_header(split = "_") %>%
  #     flextable::autofit() %>%
  #     flextable::merge_v(j = c(1)) %>%
  #     flextable::merge_h_range(i = ~ `Temporalidad de la obra` == "Total", j1 = "Temporalidad de la obra", j2 = "Obra", part = "body") %>%
  #     flextable::theme_box() %>%
  #     ftExtra::colformat_md() %>%
  #     flextable::valign(part = "header", valign = "center") %>%
  #     flextable::align(part = "header", align = "center") %>%
  #     flextable::align(i = ~ `Temporalidad de la obra` == "Total", align = "center", j = c(1:2), part = "body") %>%
  #     flextable::bg(bg = "#bcc5d4", part = "header")
  #
  #   wb_ap5 <- wb_ap5 %>%
  #     wb_add_worksheet("Obras", grid_lines = F) %>%
  #     wb_add_flextable(sheet = "Obras", ft = tbl_obras, start_col = 1, start_row = 1)
  # }

  # Tabla estadisticos ----
  tbl_est <- bd_flora %>%
    dplyr::group_by(Nom_Predio, N_Rodal, Parcela, N_Parc, UTM_E, UTM_N, Tipo_veg) %>%
    dplyr::summarise(Nha = sum(Nha, na.rm = T), .groups = "drop") %>%
    dplyr::summarise(
      Promedio = mean(Nha, na.rm = T) %>% janitor::round_half_up(),
      n = n(),
      Rango = paste0(min(Nha)," - ", max(Nha)),
      cuasivarianza = ((1-(n*(500/10000)/(rodales$Sup_ha %>% sum())))*(stats::sd(Nha)^2/n))%>% janitor::round_half_up(2),
      CV = ((sqrt(cuasivarianza)/Promedio)*100) %>% janitor::round_half_up(1),
      T_est = stats::qt(0.975,n-1) %>% janitor::round_half_up(3),
      E_abs = (T_est * sqrt(cuasivarianza)) %>% janitor::round_half_up(),
      E_rel = ((E_abs/Promedio)*100) %>% janitor::round_half_up(1),
      Int_conf = paste0(janitor::round_half_up(Promedio - E_abs), " - ", janitor::round_half_up(Promedio + E_abs))
    ) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Parámetro", values_to = "Nha Total") %>%
    dplyr::mutate_at(2, stringi::stri_replace_all_regex, "\\.", "\\,")

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Estadisticos") %>%
    openxlsx2::wb_add_data(sheet = "Estadisticos", x = tbl_est, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = wb_dims(x = tbl_est, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tbl_est, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_add_numfmt(dims = openxlsx2::wb_dims(x = tbl_est, cols = 2, rows = 4, select = "data"), numfmt = "#,##0.00") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tbl_est)), width = "auto")

  # Tabla fauna ----
  # if (!is.null(bd_fauna)) {
  #   tbl_fauna <- bd_fauna %>%
  #     dplyr::filter(Nombre_cientifico != "Sin registro", Categoria != "-") %>%
  #     dplyr::select(Nombre_cientifico, dplyr::matches("utm_"), Categoria, Decreto) %>%
  #     dplyr::mutate_at(dplyr::vars(dplyr::starts_with("UTM")), as.numeric) %>%
  #     tidyr::drop_na(UTM_E, UTM_N) %>%
  #     sf::st_as_sf(coords = c("UTM_E","UTM_N"), crs = sf::st_crs(rodales), remove = F) %>%
  #     sf::st_intersection(predios %>% dplyr::select(N_Predio)) %>%
  #     sf::st_drop_geometry() %>%
  #     dplyr::count(N_Predio, Nombre_cientifico, Categoria, Decreto)
  #
  #   wb_ap5 <- wb_ap5 %>%
  #     openxlsx2::wb_add_worksheet("BD_Fauna") %>%
  #     openxlsx2::wb_add_data(sheet = "BD_Fauna", x = tbl_fauna, start_col = 1, start_row = 1) %>%
  #     openxlsx2::wb_set_cell_style(dims = openxlsx2::wb_dims(x = tbl_fauna, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
  #     openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tbl_fauna, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
  #     openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tbl_fauna)), width = "auto")
  # }
  return(wb_ap5)
}

#' @rdname get_apendices
#' @export
apendice_5_PAS151 <- function(
    bd_flora,
    rodales,
    tabla_predios,
    tabla_areas,
    tabla_attr_rodal,
    portada = "default",
    provincia,
    obras = NULL
) {
  # Configuracion flextable ----
  flextable::set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )
  wb_ap5 <- openxlsx2::wb_workbook(theme = "Integral") %>%
    {if(portada == "KIMAL"){
      wb_portada_kimal(., PAS = 151, apendice = 5, provincia = provincia)
    } else {
      wb_portada_default(., PAS = 151, apendice = 5, provincia = provincia)
    }}

  new_border <- openxlsx2::create_border(
    bottom = "thin", bottom_color = openxlsx2::wb_color("black"),
    top = "thin", top_color = openxlsx2::wb_color("black"),
    left = "thin", left_color = openxlsx2::wb_color("black"),
    right = "thin", right_color = openxlsx2::wb_color("black")
  )
  wb_ap5$styles_mgr$add(new_border, "new_border")
  new_fill <- openxlsx2::create_fill(patternType = "solid", fgColor = openxlsx2::wb_color(hex = "#bcc5d4"))
  wb_ap5$styles_mgr$add(new_fill, "new_fill")
  new_font <- openxlsx2::create_font(b = TRUE, color = openxlsx2::wb_color("black"))
  wb_ap5$styles_mgr$add(new_font, "new_font")
  header_cellxfs <- openxlsx2::create_cell_style(
    num_fmt_id = 0,
    horizontal = "center",
    text_rotation = 0,
    fill_id = wb_ap5$styles_mgr$get_fill_id("new_fill"),
    font_id = wb_ap5$styles_mgr$get_font_id("new_font"),
    border_id = wb_ap5$styles_mgr$get_border_id("new_border")
  )
  wb_ap5$styles_mgr$add(header_cellxfs, "header_cellxfs")

  # Tabla predios ----
  tbl_predios <- tabla_predios %>%
    dplyr::select(N_Predio, Nom_Predio, Rol, Propietari, Comuna, Provincia, Region) %>%
    `names<-`(c("N° Predio", "Nombre Predio", "Rol", "Propietario", "Comuna", "Provincia", "Region"))

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Info.Predios") %>%
    openxlsx2::wb_add_data(sheet = "Info.Predios", x = tbl_predios, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = openxlsx2::wb_dims(x = tbl_predios, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tbl_predios, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tbl_predios)), width = "auto")

  # Tabla superficie predios ----
  tbl_sup_predios <- tabla_predios %>%
    dplyr::mutate(Titulo_dominio = "-", SII = "-") %>%
    dplyr::select(N_Predio, Titulo_dominio, SII, Sup_ha) %>%
    `names<-`(c("N° Predio", "Título de dominio", "servicio de Impuestos Internos", "Estudio Técnico"))

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Sup.Predios") %>%
    openxlsx2::wb_add_data(sheet = "Sup.Predios", x = tbl_sup_predios, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = openxlsx2::wb_dims(x = tbl_sup_predios, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tbl_sup_predios, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_add_numfmt(dims = openxlsx2::wb_dims(x = tbl_sup_predios, cols = 4, select = "data"), numfmt = "#,##0.00") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tbl_sup_predios)), width = "auto")

  # Tabla info areas de corta ----
  tbl_areas <- tabla_areas %>%
    dplyr::mutate(SG_SI = "", SG_NO = "", Dist_Area_Prot = "") %>%
    {if(!"Distancia" %in% names(.)) {
      dplyr::mutate(., Distancia = "")
    } else .} %>%
    dplyr::select(N_Predio, N_Area, Sup_ha, Ran_Pend, SG_SI, SG_NO, Clase_Eros, Distancia, Dist_Area_Prot) %>%
    `names<-`(
      c(
        "Predio N°",
        "Área N°",
        "Superficie (ha)",
        "Rango pendiente (%)",
        "Suelo Granítico_Si",
        "Suelo Granítico_No",
        "Grado de Erosión",
        "Distancia a cursos, cuerpos de agua o humedales (m)",
        "Distancia a áreas bajo protección oficial (m)"
      )
    ) %>%
    flextable::flextable() %>%
    flextable::separate_header(split = "_") %>%
    flextable::merge_v(j = c(1)) %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Áreas") %>%
    flexlsx::wb_add_flextable(sheet = "Áreas", ft = tbl_areas, start_col = 1, start_row = 1)

  # Tabla densidad y coberturas ----
  parc_x_tipo <- bd_flora %>%
    dplyr::group_by(N_Rodal, Tipo_veg, N_Parc) %>%
    dplyr::summarise_at("Nha", sum) %>%
    dplyr::ungroup() %>%
    split(.$Tipo_veg) %>%
    purrr::map(function(x) {
      out <- grDevices::boxplot.stats(x$Nha)$out
      x %>% dplyr::filter(!Nha %in% c(out))
    }) %>%
    dplyr::bind_rows()

  nha_est_x_tipo <- bd_flora %>%
    dplyr::filter(N_Parc %in% parc_x_tipo$N_Parc) %>%
    dplyr::select(Tipo_veg, N_Parc, Especie, Nha) %>%
    split(.$Tipo_veg) %>%
    purrr::map(~complete(.,Tipo_veg, N_Parc, Especie, fill = list(Nha = 0))) %>%
    purrr::map(function(x) {
      x %>%
        dplyr::group_by(Tipo_veg, Especie) %>%
        dplyr::summarise(Nha = mean(Nha, na.rm = T) %>% janitor::round_half_up(), .groups = "drop")
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_at("Nha", ~as.integer(janitor::round_half_up(.))) %>%
    dplyr::filter(Nha != 0) %>%
    dplyr::arrange(Tipo_veg, dplyr::desc(Nha)) %>%
    dplyr::left_join(bd_flora %>% dplyr::count(Especie, Habito) %>% dplyr::select(-n)) %>%
    suppressMessages() %>% suppressWarnings()

  cob_est_x_tipo <- bd_flora %>%
    dplyr::select(Tipo_veg, N_Parc, Especie, Habito, Cob_BB) %>%
    split(.$Tipo_veg) %>%
    purrr::map(function(x){
      x %>%
        dplyr::mutate(
          Cob_ind = dplyr::case_match(
            Cob_BB,
            "r" ~ "1",
            "+" ~ "3",
            "1" ~ "<5",
            "2" ~ "7.5",
            "3" ~ "10-25",
            "4" ~ "25-50",
            "5" ~ "50-75",
            "6" ~ "75-100",
            .default = ""
          )
        ) %>%
        dplyr::group_by(Tipo_veg, Especie, Habito) %>%
        dplyr::summarise(Cob_ind = paste0(unique(Cob_ind), collapse = "; "), .groups = "drop")
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(Tipo_veg) %>%
    suppressMessages() %>% suppressWarnings()

  nha_ptos <- tabla_areas %>%
    dplyr::left_join(
      tabla_attr_rodal %>%
        dplyr::mutate(Parcelas = purrr::map(Nom_attr, function(x) {
          x %>%
            stringi::stri_replace_all_regex("Parcela ", "") %>%
            stringi::stri_split_regex("-") %>%
            unlist() %>%
            as.integer()
        }))
    ) %>%
    dplyr::filter(Nom_attr %>% stringi::stri_detect_regex("Parcela")) %>%
    dplyr::mutate(
      nha_x_sp = purrr::map(Parcelas, nha_x_sp_fun, bd = bd_flora, add_var = "Habito"),
      cob_x_sp = purrr::map(Parcelas, cob_x_sp_fun, bd = bd_flora)
    ) %>%
    dplyr::mutate_at("cob_x_sp", ~purrr::map(., dplyr::select, -c(Especie, Habito))) %>%
    dplyr::select(N_Predio, N_Rodal, N_Area, nha_x_sp, cob_x_sp) %>%
    tidyr::unnest_legacy() %>%
    dplyr::arrange(N_Predio, N_Area) %>%
    suppressMessages() %>% suppressWarnings()

  nha_est <- tabla_areas %>%
    dplyr::left_join(tabla_attr_rodal) %>%
    dplyr::filter(Tipo_attr == "Estimación", !Nom_attr %>% stringi::stri_detect_regex("similar", case_insensitive = T)) %>%
    dplyr::left_join(nha_est_x_tipo) %>%
    dplyr::left_join(cob_est_x_tipo) %>%
    tidyr::drop_na(Nha) %>%
    dplyr::select(N_Predio, N_Rodal, N_Area, Especie, Habito, Nha, Cob_ind) %>%
    suppressMessages() %>% suppressWarnings()

  nha_otros <- tabla_areas %>%
    dplyr::left_join(tabla_attr_rodal) %>%
    dplyr::filter(Nom_attr %>% stri_detect_regex("similar", case_insensitive = T)) %>%
    dplyr::mutate(sp = purrr::map(Tipo_veg, function(x){
      x %>%
        stringi::stri_sub(from = x %>% stringi::stri_locate_all_regex(" de ") %>% unlist() %>% max() %>% .[]+1) %>%
        stringi::stri_split_regex("-") %>%
        unlist() %>%
        stringi::stri_trim()
    })) %>%
    dplyr::mutate(
      bd1 = purrr::map(sp, function(x){
        bd_flora %>%
          dplyr::filter(
            N_Parc %in% c(
              bd_flora %>%
                dplyr::filter(Especie %in% x) %>%
                dplyr::group_by(N_Parc, Especie) %>%
                dplyr::tally() %>%
                dplyr::group_by(N_Parc) %>%
                dplyr::filter(dplyr::n() == length(x)) %>%
                .$N_Parc %>%
                unique()
            )
          ) %>%
          dplyr::select(N_Parc, Especie, Nha) %>%
          dplyr::arrange(N_Parc, dplyr::desc(Nha)) %>%
          tidyr::complete(N_Parc, Especie)
      }),
      nha_x_sp = purrr::map(bd1, function(x){
        x %>%
          dplyr::filter(!Especie %in% c(x %>% dplyr::filter(is.na(Nha)) %>% .$Especie %>% unique())) %>%
          dplyr::group_by(Especie) %>%
          dplyr::summarise(Nha = mean(Nha) %>% janitor::round_half_up() %>% as.integer())
      }),
      Parcelas = purrr::map(bd1, function(x){
        unique(x$N_Parc)
      }),
      cob_x_sp = purrr::map2(Parcelas, nha_x_sp, function(x, y){
        cob_x_sp_fun(parcelas = x, bd_flora = bd_flora) %>%
          dplyr::filter(Especie %in% unique(y$Especie))
      })
    ) %>%
    dplyr::mutate_at("cob_x_sp", ~purrr::map(., dplyr::select, -c(Especie))) %>%
    dplyr::mutate_at("Parcelas", ~purrr::map(., paste, collapse = ", ")) %>%
    dplyr::select(N_Predio, N_Rodal, N_Area, nha_x_sp, cob_x_sp, Tipo_veg, Parcelas) %>%
    tidyr::unnest(nha_x_sp) %>%
    tidyr::unnest(cob_x_sp) %>%
    tidyr::unnest(Parcelas) %>%
    suppressMessages() %>% suppressWarnings()

  obs_fun <- function(x){
    x <- if (is.na(x)) NA else x %>% stringi::stri_split_regex(pattern = "[:punct:]") %>% .[[1]] %>% stringi::stri_trim()
    if (is.na(x) %>% all()) {
      NA_character_
    } else if (length(x) == 1) {
      paste("Se utilizó la parcela ", x)
    } else {
      paste("Se utilizaron las parcelas: ", paste(x %>% shQuote(), collapse = ", "))
    }
  }

  tabla_attr_rodal_final <- tabla_attr_rodal %>%
    dplyr::mutate(
      Nom_attr = dplyr::case_when(
        !N_Rodal %in% unique(dplyr::bind_rows(nha_ptos, nha_est, nha_otros) %>% .$N_Rodal) ~ "Encontrar otro método de estimación",
        .default = Nom_attr
      )
    ) %>%
    dplyr::left_join(nha_otros %>% dplyr::count(N_Rodal, Parcelas) %>% dplyr::select(-n)) %>%
    dplyr::mutate(Parcelas = purrr::map_chr(Parcelas, obs_fun))

  tbl_nha_cob <- dplyr::bind_rows(nha_ptos, nha_est, nha_otros %>% dplyr::select(-c(Tipo_veg, Parcelas))) %>%
    dplyr::select(-N_Rodal) %>%
    dplyr::arrange(N_Predio, N_Area, Nha, Cob_ind) %>%
    dplyr::rename(Cobertura = Cob_ind)

  habitos <- tbl_nha_cob$Habito %>% unique()

  for (i in seq_along(habitos)) {
    tbl_nha <- tbl_nha_cob %>%
      dplyr::filter(Habito == habitos[i]) %>%
      tidyr::pivot_wider(names_from = "Habito", values_from = c("Nha", "Cobertura"), names_glue = "{Habito}_{.value}") %>%
      dplyr::arrange(N_Predio, N_Area, Especie) %>%
      dplyr::rename("Predio N°" = 1, "N° Sector" = 2) %>%
      dplyr::rename_at("Especie", ~paste0(habitos[i], "_", .)) %>%
      dplyr::rename_at(dplyr::vars(dplyr::contains("Nha")), stringi::stri_replace_all_regex, "Nha", "Densidad (Ind/ha)") %>%
      dplyr::rename_at(dplyr::vars(dplyr::contains("Cobertura")), stringi::stri_replace_all_regex, "Cobertura", "Cobertura (%)") %>%
      flextable::flextable() %>%
      flextable::separate_header(split = "_") %>%
      flextable::merge_v(j = c(1:2)) %>%
      flextable::italic(j = 3) %>%
      flextable::autofit() %>%
      flextable::theme_box() %>%
      flextable::valign(part = "header", valign = "center") %>%
      flextable::align(part = "header", align = "center") %>%
      flextable::bg(bg = "#bcc5d4", part = "header")
    wb_ap5 <- wb_ap5 %>%
      openxlsx2::wb_add_worksheet(sheet = habitos[i]) %>%
      flexlsx::wb_add_flextable(sheet = habitos[i], ft = tbl_nha, start_col = 1, start_row = 1)
  }

  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet(sheet = "Attr_Rodal") %>%
    openxlsx2::wb_add_data(sheet = "Attr_Rodal", x = tabla_attr_rodal_final, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = openxlsx2::wb_dims(x = tabla_attr_rodal_final, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tabla_attr_rodal_final, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tabla_attr_rodal_final)), width = "auto")

  # Tabla corta ----
  tbl_corta <- tabla_areas %>%
    dplyr::mutate(Ano = "-", Tipo = "Corta y descepado", Descripcion = "Extracción de ejemplares para instalación de obras", Cob_final = 0) %>%
    dplyr::select(N_Predio, N_Area, Ano, Sup_ha, Tipo, Descripcion, Cob_final) %>%
    `names<-`(c("Predio N°", "N° Sector", "Año de intervención (ha)", "Superficie a intervenir (ha)", "Tipo de intervención", "Descripción de la intervención", "Cobertura final (%)")) %>%
    flextable::flextable() %>%
    flextable::separate_header(split = "_") %>%
    flextable::merge_v(j = c(1)) %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")
  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet(sheet = "Programa_de_actividades") %>%
    flexlsx::wb_add_flextable(sheet = "Programa_de_actividades", ft = tbl_corta, start_col = 1, start_row = 1)

  # Tabla obras ----
  # if (!is.null(obras)) {
  #   tbl_obras <- areas %>%
  #     sf::st_intersection(obras %>% dplyr::select(Tipo, Obra)) %>%
  #     dplyr::count(Tipo, Obra) %>% dplyr::select(-n) %>%
  #     dplyr::mutate(
  #       Sup_ha = sf::st_area(geometry) %>% units::set_units(ha) %>% units::drop_units() %>% janitor::round_half_up(2),
  #       Sup_m2 = sf::st_area(geometry) %>% units::drop_units() %>% janitor::round_half_up(),
  #     ) %>%
  #     sf::st_drop_geometry() %>%
  #     janitor::adorn_totals() %>%
  #     `names<-`(c("Temporalidad de la obra", "Obra", "Superficie (ha)", "Superficie (m2)")) %>%
  #     flextable::flextable() %>%
  #     flextable::separate_header(split = "_") %>%
  #     flextable::autofit() %>%
  #     flextable::merge_v(j = c(1)) %>%
  #     flextable::merge_h_range(i = ~ `Temporalidad de la obra` == "Total", j1 = "Temporalidad de la obra", j2 = "Obra", part = "body") %>%
  #     flextable::theme_box() %>%
  #     ftExtra::colformat_md() %>%
  #     flextable::valign(part = "header", valign = "center") %>%
  #     flextable::align(part = "header", align = "center") %>%
  #     flextable::align(i = ~ `Temporalidad de la obra` == "Total", align = "center", j = c(1:2), part = "body") %>%
  #     flextable::bg(bg = "#bcc5d4", part = "header") %>%
  #     flextable::bold(i = ~`Temporalidad de la obra` == "Total", j = c(1:4), bold = TRUE)
  #   wb_ap5 <- wb_ap5 %>%
  #     openxlsx2::wb_add_worksheet(sheet = "Obras") %>%
  #     flexlsx::wb_add_flextable(sheet = "Obras", ft = tbl_obras, start_col = 1, start_row = 1)
  # }

  # Tabla estadisticos ----
  tbl_est <- bd_flora %>%
    dplyr::group_by(Nom_Predio, N_Rodal, Parcela, N_Parc, UTM_E, UTM_N, Tipo_veg) %>%
    dplyr::summarise(Nha = sum(Nha,na.rm = T), .groups = "drop") %>%
    dplyr::summarise(
      Promedio = mean(Nha, na.rm = T) %>% janitor::round_half_up(),
      n = n(),
      Rango = paste0(min(Nha)," - ", max(Nha)),
      cuasivarianza = ((1-(n*(500/10000)/(rodales$Sup_ha %>% sum())))*(stats::sd(Nha)^2/n))%>% janitor::round_half_up(2),
      CV = ((sqrt(cuasivarianza)/Promedio)*100) %>% janitor::round_half_up(1),
      T_est = stats::qt(0.975,n-1) %>% round_half_up(3),
      E_abs = (T_est * sqrt(cuasivarianza)) %>% janitor::round_half_up(),
      E_rel = ((E_abs/Promedio)*100) %>% janitor::round_half_up(1),
      Int_conf = paste0(janitor::round_half_up(Promedio - E_abs), " - ", janitor::round_half_up(Promedio + E_abs))
    ) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Parámetro", values_to = "Nha Total") %>%
    dplyr::mutate_at(2, stringi::stri_replace_all_regex, "\\.", "\\,")
  wb_ap5 <- wb_ap5 %>%
    openxlsx2::wb_add_worksheet("Estadisticos") %>%
    openxlsx2::wb_add_data(sheet = "Estadisticos", x = tbl_est, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = wb_dims(x = tbl_est, select = "col_names"), style = wb_ap5$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tbl_est, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_add_numfmt(dims = openxlsx2::wb_dims(x = tbl_est, cols = 2, rows = 4, select = "data"), numfmt = "#,##0.00") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tbl_est)), width = "auto")

  return(wb_ap5)
}
