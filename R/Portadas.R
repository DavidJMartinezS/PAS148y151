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
#' @importFrom openxlsx2 wb_add_worksheet wb_page_setup wb_add_data wb_add_font wb_add_cell_style wb_merge_cells wb_dims wb_add_image wb_set_col_widths
#' @importFrom stringi stri_trans_totitle stri_opts_brkiter stri_trans_toupper
wb_portada_kimal <- function(wb, PAS, apendice, provincia) {
  stopifnot(inherits(wb, "wbWorkbook"))
  stopifnot(PAS %in% c(148, 151))
  stopifnot(apendice %in% c(1, 2, 5))
  stopifnot(provincia %>% unlist(provincias_list))

  mes <- Sys.Date() %>% format('%B') %>% stringi::stri_trans_totitle(opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))
  yr <- Sys.Date() %>% format('%Y') %>% stringi::stri_trans_totitle(opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))

  nom_apendice <- switch(
    apendice,
    "2" = "APÉNDICE 2. Densiadad de especies",
    "3" = "APÉNDICE 3. Coordenadas ubicación de parcelas",
    "5" = "APENDICE 5. Tablas formulario CONAF"
  )

  wb <- wb %>%
    wb_add_worksheet("Portada", grid_lines = F) %>%
    wb_page_setup(paper_size = 1) %>%
    # EIA
    wb_add_data(x = "ESTUDIO DE IMPACTO AMBIENTAL", start_col = 2, start_row = 16) %>%
    wb_add_font(dims = "B16", bold = T, size = 14) %>%
    wb_add_cell_style(dims = "B16", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 16, cols = 2:5), solve = T) %>%
    # proyecto
    wb_add_data(x = stringi::stri_trans_toupper("LÍNEA DE TRANSMISIÓN ELÉCTRICA HVDC KIMAL - LO AGUIRRE"), dims = "A18") %>%
    wb_add_font(dims = "A18",bold = T, size = 16) %>%
    wb_add_cell_style(dims = "A18", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 18:19, cols = 1:6), solve = T) %>%
    # apéndice
    wb_add_data(x = nom_apendice, dims = "B22") %>%
    wb_add_cell_style(dims = "B22", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 22, cols = 2:5), solve = T) %>%
    wb_add_data(x = sprintf("PAS %s - Provincia de %s, %s", PAS, provincia, provincias_list[grep(provincia, provincias_list)] %>% names()), dims = "A24") %>%
    wb_add_cell_style(dims = "A24", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 24, cols = 1:6), solve = T) %>%
    # Elaborado por
    wb_add_data(x = "Elaborado por Geobiota para:", start_col = 2, start_row = 33) %>%
    wb_add_font(dims = "B33", bold = T, size = 14) %>%
    wb_add_cell_style(dims = "B33", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 33, cols = 2:5), solve = T) %>%
    wb_add_image(dims = "C35", file = "CONEXION.png", width = 6.5, height = 2, units = 'cm') %>%
    # Fecha
    wb_add_data(x = paste0(mes,yr, sep = ", "),dims = "C43") %>%
    wb_add_cell_style(dims = "C43", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 43, cols = 3:4), solve = T) %>%
    wb_set_col_widths(cols = 1:6, widths = 13)
  return(wb)
}

#' @noRd
wb_portada_default <- function(wb, PAS, apendice, nom_proj = NULL, provincia){
  stopifnot(inherits(wb, "wbWorkbook"))
  stopifnot(PAS %in% c(148, 151))
  stopifnot(apendice %in% c(1, 2, 5))
  stopifnot(provincia %>% unlist(provincias_list))

  mes <- Sys.Date() %>% format('%B') %>% stringi::stri_trans_totitle(opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))
  yr <- Sys.Date() %>% format('%Y') %>% stringi::stri_trans_totitle(opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))

  if (is.null(nom_proj)) {
    nom_proj <- "INGRESE NOMBRE DEL PROYECTO"
  }

  om_apendice <- switch(
    apendice,
    "2" = "APÉNDICE 2. Densiadad de especies",
    "3" = "APÉNDICE 3. Coordenadas ubicación de parcelas",
    "5" = "APENDICE 5. Tablas formulario CONAF"
  )

  wb <- wb %>%
    wb_add_worksheet("Portada", grid_lines = F) %>%
    wb_page_setup(paper_size = 1) %>%
    # EIA
    wb_add_data(x = "ESTUDIO DE IMPACTO AMBIENTAL", start_col = 2, start_row = 16) %>%
    wb_add_font(dims = "B16", bold = T, size = 14) %>%
    wb_add_cell_style(dims = "B16", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 16, cols = 2:5), solve = T) %>%
    # proyecto
    wb_add_data(x = stringi::stri_trans_toupper(nom_proj), dims = "A18") %>%
    wb_add_font(dims = "A18",bold = T, size = 16) %>%
    wb_add_cell_style(dims = "A18", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 18:19, cols = 1:6), solve = T) %>%
    # apéndice
    wb_add_data(x = nom_apendice, dims = "B22") %>%
    wb_add_cell_style(dims = "B22", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 22, cols = 2:5), solve = T) %>%
    wb_add_data(x = sprintf("PAS %s - Provincia de %s, %s", PAS, provincia, provincias_list[grep(provincia, provincias_list)] %>% names()), dims = "A24") %>%
    wb_add_cell_style(dims = "A24", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 24, cols = 1:6), solve = T) %>%
    # Elaborado por
    wb_add_data(x = "Elaborado por Geobiota para:", start_col = 2, start_row = 31) %>%
    wb_add_font(dims = "B31", bold = T, size = 14) %>%
    wb_add_cell_style(dims = "B31", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 31, cols = 2:5), solve = T) %>%
    wb_add_image(dims = "C33",file = "logo_default.png", width = 135.2899, height = 141.7323, units = 'px', dpi = 72) %>%
    # Fecha
    wb_add_data(x = paste0(mes,yr, sep = ", "),dims = "C43") %>%
    wb_add_cell_style(dims = "C43", horizontal = "center", vertical = "center") %>%
    wb_merge_cells(dims = wb_dims(rows = 43, cols = 3:4), solve = T) %>%
    wb_set_col_widths(cols = 1:6, widths = 13)
  return(wb)
}
