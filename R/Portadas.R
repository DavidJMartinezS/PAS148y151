#' Portadas de apendices
#'
#' @param wb Workbook.
#' @param PAS PAS PAS correspondiente. Ingresar `148` o `151`.
#' @param apendice Numero de apendice. `2`, Densidad de parcelas; `3`, Ubicacion de parcelas; `5`, Tablas formulario.
#' @param nom_proj Nombre del proyecto.
#' @param provincia Nombre de la provincia.
#'
#' @return Workbook con portada.
#' @noRd
#'
wb_portada_kimal_PAS148y151 <- function(wb, PAS, apendice, provincia) {
  stopifnot(inherits(wb, "wbWorkbook"))
  stopifnot(PAS %in% c(148, 151))
  stopifnot(apendice %in% c(2, 3, 5))
  stopifnot(provincia %in% unlist(provincias_list))

  mes <- Sys.Date() %>% format('%B') %>% stringi::stri_trans_totitle(opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))
  yr <- Sys.Date() %>% format('%Y') %>% stringi::stri_trans_totitle(opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))

  nom_apendice <- switch(
    as.character(apendice),
    "2" = "APÉNDICE 2. Densiadad de especies",
    "3" = "APÉNDICE 3. Coordenadas ubicación de parcelas",
    "5" = "APÉNDICE 5. Tablas formulario CONAF"
  )

  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Portada", grid_lines = F) %>%
    openxlsx2::wb_page_setup(paper_size = 1) %>%
    # img
    openxlsx2::wb_add_image(dims = "C7", file = app_sys("app/www/logo-header.svg"), width = 7, height = 1.75, units = 'cm') %>%
    # EIA
    openxlsx2:: wb_add_data(x = "ESTUDIO DE IMPACTO AMBIENTAL", start_col = 2, start_row = 16) %>%
    openxlsx2::wb_add_font(dims = "B16", bold = T, size = 14) %>%
    openxlsx2::wb_add_cell_style(dims = "B16", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 16, cols = 2:5), solve = T) %>%
    # proyecto
    openxlsx2::wb_add_data(x = stringi::stri_trans_toupper("LÍNEA DE TRANSMISIÓN ELÉCTRICA HVDC KIMAL - LO AGUIRRE"), dims = "A18") %>%
    openxlsx2::wb_add_font(dims = "A18",bold = T, size = 16) %>%
    openxlsx2::wb_add_cell_style(dims = "A18", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 18:19, cols = 1:6), solve = T) %>%
    # apéndice
    openxlsx2::wb_add_data(x = nom_apendice, dims = "B22") %>%
    openxlsx2::wb_add_cell_style(dims = "B22", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 22, cols = 2:5), solve = T) %>%
    openxlsx2::wb_add_data(x = sprintf("PAS %s - Provincia de %s, %s", PAS, provincia, provincias_list[grep(provincia, provincias_list)] %>% names()), dims = "A24") %>%
    openxlsx2::wb_add_cell_style(dims = "A24", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 24, cols = 1:6), solve = T) %>%
    # Elaborado por
    openxlsx2::wb_add_data(x = "Elaborado por Geobiota para:", start_col = 2, start_row = 33) %>%
    openxlsx2::wb_add_font(dims = "B33", bold = T, size = 14) %>%
    openxlsx2::wb_add_cell_style(dims = "B33", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 33, cols = 2:5), solve = T) %>%
    openxlsx2::wb_add_image(dims = "C35", file = app_sys("app/www/CONEXION.png"), width = 5.8, height = 1.79, units = 'cm') %>%
    # Fecha
    openxlsx2::wb_add_data(x = paste(mes,yr, sep = ", "),dims = "C43") %>%
    openxlsx2::wb_add_cell_style(dims = "C43", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 43, cols = 3:4), solve = T) %>%
    openxlsx2::wb_set_col_widths(cols = 1:6, widths = 13)
  return(wb)
}

#' @noRd
wb_portada_default_PAS148y151 <- function(wb, PAS, apendice, nom_proj = NULL, provincia){
  stopifnot(inherits(wb, "wbWorkbook"))
  stopifnot(PAS %in% c(148, 151))
  stopifnot(apendice %in% c(2, 3, 5))
  stopifnot(provincia %in% unlist(provincias_list))

  mes <- Sys.Date() %>% format('%B') %>% stringi::stri_trans_totitle(opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))
  yr <- Sys.Date() %>% format('%Y') %>% stringi::stri_trans_totitle(opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))

  if (is.null(nom_proj)) {
    nom_proj <- "INGRESE NOMBRE DEL PROYECTO"
  }

  nom_apendice <- switch(
    as.character(apendice),
    "2" = "APÉNDICE 2. Densiadad de especies",
    "3" = "APÉNDICE 3. Coordenadas ubicación de parcelas",
    "5" = "APÉNDICE 5. Tablas formulario CONAF"
  )

  wb <- wb %>%
    openxlsx2::wb_add_worksheet("Portada", grid_lines = F) %>%
    openxlsx2::wb_page_setup(paper_size = 1) %>%
    # img
    openxlsx2::wb_add_image(dims = "C7", file = app_sys("app/www/logo-header.svg"), width = 7, height = 1.75, units = 'cm') %>%
    # EIA
    openxlsx2::wb_add_data(x = "ESTUDIO DE IMPACTO AMBIENTAL", start_col = 2, start_row = 16) %>%
    openxlsx2::wb_add_font(dims = "B16", bold = T, size = 14) %>%
    openxlsx2::wb_add_cell_style(dims = "B16", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 16, cols = 2:5), solve = T) %>%
    # proyecto
    openxlsx2::wb_add_data(x = stringi::stri_trans_toupper(nom_proj), dims = "A18") %>%
    openxlsx2::wb_add_font(dims = "A18",bold = T, size = 16) %>%
    openxlsx2::wb_add_cell_style(dims = "A18", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 18:19, cols = 1:6), solve = T) %>%
    # apéndice
    openxlsx2::wb_add_data(x = nom_apendice, dims = "B22") %>%
    openxlsx2::wb_add_cell_style(dims = "B22", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 22, cols = 2:5), solve = T) %>%
    openxlsx2::wb_add_data(x = sprintf("PAS %s - Provincia de %s, %s", PAS, provincia, provincias_list[grep(provincia, provincias_list)] %>% names()), dims = "A24") %>%
    openxlsx2::wb_add_cell_style(dims = "A24", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 24, cols = 1:6), solve = T) %>%
    # Elaborado por
    openxlsx2::wb_add_data(x = "Elaborado por Geobiota para:", start_col = 2, start_row = 31) %>%
    openxlsx2::wb_add_font(dims = "B31", bold = T, size = 14) %>%
    openxlsx2::wb_add_cell_style(dims = "B31", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = wb_dims(rows = 31, cols = 2:5), solve = T) %>%
    openxlsx2::wb_add_image(dims = "C33", file = app_sys("app/www/logo_default.png"), width = 135.2899, height = 141.7323, units = 'px', dpi = 72) %>%
    # Fecha
    openxlsx2::wb_add_data(x = paste(mes,yr, sep = ", "),dims = "C43") %>%
    openxlsx2::wb_add_cell_style(dims = "C43", horizontal = "center", vertical = "center") %>%
    openxlsx2::wb_merge_cells(dims = openxlsx2::wb_dims(rows = 43, cols = 3:4), solve = T) %>%
    openxlsx2::wb_set_col_widths(cols = 1:6, widths = 13)
  return(wb)
}

