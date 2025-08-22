#' Tabla atributos de rodal
#'
#' @param PAS PAS correspondiente. Ingresar `148` o `151`.
#' @param bd_flora Base de datos de parcelas dentro de los rodales.
#' @param rodales Objeto sf con poligono de rodales.
#' @param umbral_sp_est Porcentaje de representividad minimo de una especie cuya densidad se va a estimar. default \code{0.05}.
#'
#' @seealso [apendice_5_PAS148()], [apendice_5_PAS151()], [apendice_5_PAS151_nuevo()]
#'
#' @return Tabla con los atributos de rodal
#' @export
get_tabla_attr_rodal <- function(PAS, bd_flora, rodales, umbral_sp_est = 0.05){
  PAS <- match.arg(as.character(PAS), choices = c(148, 151))
  stopifnot("'umbral_sp_est' debe ser un valor numérico entre 0 y 1" = (umbral_sp_est >= 0 & umbral_sp_est <= 1))
  if (PAS == 148) {
    valid_df(bd_flora, names = c("Nom_Predio", "N_Rodal", "Parcela", "N_Parc", "UTM_E", "UTM_N", "Tipo_fores", "Subtipo_fo", "Tipo_veg", "Especie", "Nha"))
    valid_input(rodales, inherit = "sf", names = c("N_Rodal", "Tipo_fores", "Subtipo_fo", "Tipo_veg"), geometry = "POLYGON")
  } else {
    valid_df(bd_flora, names = c("Nom_Predio", "N_Rodal", "Parcela", "N_Parc", "UTM_E", "UTM_N", "Tipo_veg", "Especie", "Nha"))
    valid_input(rodales, inherit = "sf", names = c("N_Rodal", "Tipo_veg"), geometry = "POLYGON")
  }

  vars_tbl_attr <- if(PAS == 148) {
    dplyr::syms(c("Tipo_fores", "Subtipo_fo", "Tipo_veg"))
  } else {
    dplyr::syms("Tipo_veg")
  }

  nha_parc <- bd_flora %>%
    dplyr::group_by(Nom_Predio, N_Rodal, Parcela, N_Parc, UTM_E, UTM_N, Tipo_veg) %>%
    dplyr::summarise(Nha = sum(Nha,na.rm = T), .groups = "drop") %>%
    dplyr::rename(Coord_X = UTM_E, Coord_Y = UTM_N) %>%
    sf::st_as_sf(coords = c("Coord_X","Coord_Y"), crs = sf::st_crs(rodales), remove = F) %>%
    dplyr::arrange(N_Parc)

  if (any(nha_parc %>% sf::st_distance(rodales) %>% apply(1, min) %>% .[] > 5)) {
    if (shiny::isRunning()) {
      shinyalert::shinyalert(
        title = "OJO!",
        text = tags$p(
          "Los siguientes puntos están a más de 5 metros de los rodales:", tags$br(),
          nha_parc[nha_parc %>% sf::st_distance(rodales) %>% apply(1, min) %>% .[] > 5, ]$Parcela %>%
        {if(length(. > 10)) .[c(1:10)] else .} %>%
        shQuote() %>% paste0(collapse = ", ") %>%
        {if(length(. > 10)) paste0(., ", etc...") else .}, br(),
          "Por favor revisar"
        ),
        type = "warning",
        html = T,
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = TRUE
      )
    } else {
      cat(
        "Los siguientes puntos están a más de 5 metros de los rodales:",
        nha_parc[nha_parc %>% sf::st_distance(rodales) %>% apply(1, min) %>% .[] > 5, ]$Parcela %>%
          {if(length(. > 10)) .[c(1:10)] else .} %>%
          shQuote() %>% paste0(collapse = ", ") %>%
          {if(length(. > 10)) paste0(., ", etc...") else .},
        "Por favor revisar", sep = "\n"
      )
    }
  }

  flextable::set_flextable_defaults(
    decimal.mark = ",",
    big.mark = "."
  )

  parc_x_tipo <- bd_flora %>%
    dplyr::group_by(N_Rodal, Tipo_veg, N_Parc) %>%
    dplyr::summarise_at("Nha", sum) %>%
    dplyr::ungroup() %>%
    split(.$Tipo_veg) %>%
    purrr::map(function(x) {
      out <- grDevices::boxplot.stats(x$Nha)$out
      x %>% dplyr::filter(!Nha %in% c(out))
    }) %>%
    dplyr::bind_rows() %>%
    suppressMessages() %>% suppressWarnings()

  ft_parc_x_tipo <- parc_x_tipo %>%
    dplyr::group_by(Tipo_veg) %>%
    dplyr::summarise(Parcelas = paste(sort(unique(N_Parc)), collapse = ", "), .groups = "drop") %>%
    `names<-`(c("Tipo vegetacional", "Parcela/s")) %>%
    flextable::flextable() %>%
    flextable::merge_v(j = 1) %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  nha_est_x_tipo <- bd_flora %>%
    dplyr::filter(N_Parc %in% parc_x_tipo$N_Parc) %>%
    dplyr::select(Tipo_veg, N_Parc, Especie, Nha) %>%
    split(.$Tipo_veg) %>%
    purrr::map(~tidyr::complete(.,Tipo_veg, N_Parc, Especie, fill = list(Nha = 0))) %>%
    purrr::map(function(x) {
      x %>%
        dplyr::group_by(Tipo_veg, Especie) %>%
        dplyr::summarise(Nha = mean(Nha, na.rm = T) %>% janitor::round_half_up()) %>%
        dplyr::ungroup()
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::group_by(Tipo_veg) %>%
    dplyr::mutate(Percentage = Nha/sum(Nha)) %>%
    dplyr::filter(Percentage >= umbral_sp_est) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(Tipo_veg, desc(Nha)) %>%
    suppressMessages() %>% suppressWarnings()

  ft_nha_est_x_tipo <- nha_est_x_tipo %>%
    dplyr::select(-Percentage) %>%
    `names<-`(c("Tipo vegetacional", "Especie", "Nha")) %>%
    flextable::flextable() %>%
    flextable::merge_v(j = 1) %>%
    flextable::italic(j = 2) %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  ft_estadisticos <- bd_flora %>%
    dplyr::group_by(Nom_Predio, N_Rodal, Parcela, N_Parc, UTM_E, UTM_N, Tipo_veg) %>%
    dplyr::summarise(Nha = sum(Nha,na.rm = T), .groups = "drop") %>%
    dplyr::summarise(
      Promedio = mean(Nha, na.rm = T) %>% janitor::round_half_up(),
      n = dplyr::n(),
      Rango = paste0(mark(min(Nha))," - ", mark(max(Nha))),
      cuasivarianza = ((1 - (n * (500 / 10000) / (rodales$Sup_ha %>% sum()))) * (stats::sd(Nha) ^ 2 / n)) %>% janitor::round_half_up(2),
      CV = ((sqrt(cuasivarianza) / Promedio) * 100) %>% janitor::round_half_up(1),
      T_est = stats::qt(0.975, n - 1) %>% janitor::round_half_up(3),
      E_abs = (T_est * sqrt(cuasivarianza)) %>% janitor::round_half_up(),
      E_rel = ((E_abs / Promedio) * 100) %>% janitor::round_half_up(1),
      Int_conf = paste0(janitor::round_half_up(Promedio - E_abs) %>% mark(), " - ", janitor::round_half_up(Promedio + E_abs) %>% mark())
    ) %>%
    dplyr::mutate_if(is.numeric, mark) %>%
    `names<-`(
      c(
        "Nha (ind/ha)",
        "Tamaño de la muestra (n)",
        "Rango (ind/ha)",
        "Varianza",
        "Coeficiente de variación (%)",
        "T de Student",
        "Error absoluto (ind/ha)",
        "Error relativo (%)",
        "Intervalo de confianza (95%) (ind/ha)"
      )
    ) %>%
    tidyr::pivot_longer(cols = dplyr::everything(), names_to = "Parámetros", values_to = "Nha total") %>%
    flextable::flextable() %>%
    flextable::autofit() %>%
    flextable::theme_box() %>%
    flextable::valign(part = "header", valign = "center") %>%
    flextable::align(part = "header", align = "center") %>%
    flextable::align(j = 2, part = "body", align = "center") %>%
    flextable::bg(bg = "#bcc5d4", part = "header")

  df <- rodales %>%
    dplyr::count(N_Rodal, !!!vars_tbl_attr) %>% dplyr::select(-n) %>%
    sf::st_collection_extract("POLYGON") %>%
    sf::st_join(nha_parc %>% dplyr::select(N_Parc, Nha))

  tabla_attr_rodal <- df %>%
    dplyr::bind_rows(
      nha_parc %>%
        dplyr::filter(!N_Parc %in% df$N_Parc) %>%
        dplyr::select(N_Parc, Nha) %>%
        sf::st_join(
          rodales %>% dplyr::select(N_Rodal, !!!vars_tbl_attr),
          join = sf::st_nearest_feature
        ) %>%
        sf::st_drop_geometry() %>%
        dplyr::left_join(
          rodales %>%
            dplyr::count(N_Rodal, !!!vars_tbl_attr) %>%
            dplyr::select(-n)
        ) %>%
        sf::st_as_sf(crs = sf::st_crs(rodales)) %>%
        sf::st_collection_extract("POLYGON")
    ) %>%
    dplyr::group_by(N_Rodal, !!!vars_tbl_attr, geometry) %>%
    dplyr::summarise(
      Parcelas = paste(N_Parc, collapse = '-'),
      NHA = mean(Nha, na.rm=T),
      .groups = "drop"
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      Tipo_attr = ifelse((is.na(Parcelas) | Parcelas == "NA"), "Estimación", "Parcela directa"),
      Nom_attr = dplyr::case_when(
        Tipo_attr ==  "Parcela directa" ~ paste0("Parcela ", Parcelas),
        Tipo_attr ==  "Estimación" & !(Tipo_veg %in% unique(nha_est_x_tipo$Tipo_veg)) ~ "Estimación por Tipo vegetacional similar",
        .default = "Estimación por Tipo vegetacional"
      )
    ) %>%
    dplyr::select(-c(Parcelas, NHA)) %>%
    tidyr::unnest(N_Rodal) %>%
    dplyr::arrange(N_Rodal) %>%
    suppressMessages() %>% suppressWarnings()

  wb <- openxlsx2::wb_workbook(theme = "Integral") %>%
    apendices_style() %>%
    # Tabla Attr Rodal
    openxlsx2::wb_add_worksheet("tabla_attr_rodal") %>%
    openxlsx2::wb_add_data(sheet = "tabla_attr_rodal", x = tabla_attr_rodal, start_col = 1, start_row = 1, na.strings = "") %>%
    openxlsx2::wb_set_cell_style(dims = openxlsx2::wb_dims(x = tabla_attr_rodal, select = "col_names"), style = .$styles_mgr$get_xf_id("header_cellxfs")) %>%
    openxlsx2::wb_add_border(dims = openxlsx2::wb_dims(x = tabla_attr_rodal, select = "data"), inner_hgrid = "thin", inner_vgrid = "thin") %>%
    openxlsx2::wb_set_col_widths(cols = seq_len(ncol(tabla_attr_rodal)), width = "auto") %>%
    # Parcelas x Tipo
    openxlsx2::wb_add_worksheet("Parcelas_x_Tipo", grid_lines = F) %>%
    openxlsx2::wb_add_data(x = "Puede que no se incluyan todas las parcelas de un tipo vegetacional, puesto que se excluyeron parcelas con Nha outliers para la estimación") %>%
    openxlsx2::wb_add_fill(dims = "A1", color = openxlsx2::wb_color("#C0DAD9")) %>%
    openxlsx2::wb_add_font(dims = "A1", bold = T) %>%
    openxlsx2::wb_merge_cells(dims = "A1:B1", solve = T) %>%
    openxlsx2::wb_add_border(dims = "A1:B1", inner_hgrid = "thin", inner_vgrid = "thin") %>%
    flexlsx::wb_add_flextable(sheet = "Parcelas_x_Tipo", ft = ft_parc_x_tipo, start_col = 1, start_row = 3) %>%
    openxlsx2::wb_set_col_widths(cols = 2, widths = 70) %>%
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 1:500, cols = 1:2), wrap_text = "1") %>%
    # Nha x Tipo
    openxlsx2::wb_add_worksheet("Nha_x_Tipo", grid_lines = F) %>%
    openxlsx2::wb_add_data(x = sprintf("Obtenido con las parcelas indicadas en la pagina anterior. Se incluyen especies sobre un %1.0f%% de representatividad de la densidad total", umbral_sp_est * 100)) %>%
    openxlsx2::wb_add_fill(dims = "A1", color = openxlsx2::wb_color("#C0DAD9")) %>%
    openxlsx2::wb_add_font(dims = "A1", bold = T) %>%
    openxlsx2::wb_merge_cells(dims = "A1:C1", solve = T) %>%
    openxlsx2::wb_add_border(dims = "A1:C1", inner_hgrid = "thin", inner_vgrid = "thin") %>%
    flexlsx::wb_add_flextable(sheet = "Nha_x_Tipo", ft = ft_nha_est_x_tipo, start_col = 1, start_row = 3) %>%
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 1:500, cols = 1:3), wrap_text = "1") %>%
    # Estadísticos
    openxlsx2::wb_add_worksheet("Estadisticos", grid_lines = F) %>%
    openxlsx2::wb_add_data(x = "Estadígrafos del muestreo total de los rodales") %>%
    openxlsx2::wb_add_fill(dims = "A1", color = openxlsx2::wb_color("#C0DAD9")) %>%
    openxlsx2::wb_add_font(dims = "A1", bold = T) %>%
    openxlsx2::wb_merge_cells(dims = "A1:B1", solve = T) %>%
    openxlsx2::wb_add_border(dims = "A1:B1", inner_hgrid = "thin", inner_vgrid = "thin") %>%
    flexlsx::wb_add_flextable(sheet = "Estadisticos", ft = ft_estadisticos, start_col = 1, start_row = 3) %>%
    openxlsx2::wb_add_cell_style(dims = openxlsx2::wb_dims(rows = 1:500, cols = 1:2), wrap_text = "1")

  return(wb)
}
