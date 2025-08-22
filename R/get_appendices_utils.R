#' Cobertura de copa
#'
#' @description
#' Calcular cobertura de copas de un individuo.
#'
#'
#' @param x,y Diametros de copa.
#' @param method Metodo; `circle` o `ellipse`. Default `circle`.
#' @param na.value Devuelve este valor si la cobertura es \code{NA}. Default `0`.
#'
#' @return Valor numerico con cobertura de copa.
#' @export
#'
#' @examples
#' cup_coverage(x = 7, y = 3)
#' cup_coverage(x = 7, y = 3, method = "ellipse")
#' cup_coverage(x = NA, y = NA, na.value = 0)
#'
cup_coverage <- function(x, y, method = "ellipse", na.value = 0){
  method <- match.arg(method, choices = c("circle", "ellipse"))
  stopifnot(na.value %in% c(0, NA))

  if (method == "circle") {
    val <- pi * (mean(c(x, y), na.rm = T) / 2) ^ 2
  }
  if (method == "ellipse"){
    val <- pi * x / 2 * y / 2
  }
  if (is.na(val)) {
    return(na.value)
  } else {
    return(val)
  }
}



#' Cobertura de la parcela
#'
#' @description
#' Calcular cobertura de copas de la parcela.
#'
#' @param x Vector numerico de coberturas individuales de copa.
#' @param plot_size Superficie de la parcela en m2. Default \code{500}.
#' @param percent Logico; si es \code{TRUE} agrega el simbolo de porcentaje.
#' @param digits Cantidad de decimales.
#' @param ... Otros argumentos de la funcion base [formatC()].
#'
#' @return Cobertura de parcela formato texto.
#' @export
#'
#' @examples
#' plot_coverage(c(20, 15), percent = FALSE, digits = 2, decimal.mark = ",")
#' plot_coverage(c(NA, NA, 20, 15), percent = TRUE, digits = 1, decimal.mark = ",")
#'
plot_coverage <- function(x, plot_size = 500, percent = FALSE, digits = 1L, ...){
  valid_input(x, plot_size, digits, inherit = c("integer", "numeric"))
  valid_input(percent, inherit = "logical")

  x <- sum(x, na.rm = T) / plot_size
  if (percent) {
    paste0(formatC(x * 100, format = "f", digits = digits, ...), "%")
  } else {
    formatC(x, digits = digits, format = "f", ...)
  }
}



#' Especies en cursiva
#'
#' @param x Tipo vegetacional.
#'
#' @return Mismo texto con `*` para dar formato cursiva.
#' @export
#'
#' @examples
#' italic_sp(x = c(
#' "Bosque de Schinus latifolius-Quillaja saponaria",
#' "Matorral con suculentas de Flourensia thurifera",
#' "Plantación de especies nativas de Acacia caven-Quillaja saponaria"))
#'
italic_sp <- function(x){
  tv <- lapply(x, split_tail, pattern = " de ")
  el1 <- lapply(tv, function(a) {return(a[1])})
  el2 <- lapply(tv, function(a) {
    if (length(a) > 1) {
      return(paste0("*", a[2], "*"))
    } else {
      return(NA)
    }
  })
  italic <- purrr::map2_chr(el1, el2, function(x, y){
    if (is.na(y)) {
      return(x)
    } else {
      return(paste0(x, " de ", y) )
    }
  })
  return(italic)
}



#' Densidad por especie
#'
#' @param parcelas Nombre de parcelas.
#' @param bd Base de datos con las parcelas. Debe contener el Nha por especie y parcela de muestreo.
#' @param field_parc Nombre del campo con ID de parcela.
#' @param add_var Variable/s que desea agregar. default \code{NULL}.
#'
#' @return data.frame con densidad (ind/ha) por especie.
#' @export
#'
nha_x_sp_fun <- function(parcelas, bd, field_parc = "N_Parc", add_var = NULL){
  valid_input(parcelas, inherit = c("integer", "numeric", "character"))
  valid_input(field_parc, inherit = "character")
  valid_input(add_var, inherit = c("character", "NULL"))
  valid_df(bd, names = c(field_parc, add_var, "Especie", "Nha"))

  if (all(parcelas %in% unique(dplyr::pull(bd, !!dplyr::sym(field_parc))))) {
    warning(
      sprintf(
        "Faltan las siguientes parcelas en la bd:\n%s",
        setdiff(parcelas, unique(dplyr::pull(bd, !!dplyr::sym(field_parc))))
      )
    )
  }

  bd %>%
    dplyr::select(!!dplyr::sym(field_parc), Especie, Nha) %>%
    dplyr::filter(!!dplyr::sym(field_parc) %in% parcelas) %>%
    tidyr::complete(!!dplyr::sym(field_parc), Especie, fill = list(Nha = 0)) %>%
    dplyr::left_join(bd %>% dplyr::count(Especie, dplyr::across(add_var)) %>% dplyr::select(-n)) %>%
    dplyr::group_by(Especie, dplyr::across(add_var)) %>%
    dplyr::summarise(Nha = mean(Nha,na.rm = T) %>% janitor::round_half_up(), .groups = "drop") %>%
    dplyr::mutate_at("Nha", as.integer) %>%
    suppressMessages() %>% suppressWarnings()
}



#' Cobertura por especie
#'
#' @param parcelas Nombre de parcelas.
#' @param bd_flora Base de datos con las parcelas.
#' @param field_parc Nombre del campo con ID de parcela.
#' @param cov_as_range logical. \code{TRUE} si desea dejar la cobertura de la flora como un rango. Valido solo para el PAS `151`.
#' @param cov_fp Cobertura para especies con COB_BB `fp` o no especificada. Elegir entre `0`, `0.5` o `1`. Default `1`.
#'
#' @return data.frame con cobertura de copas por especie.
#' @export
#'
cob_x_sp_fun <- function(parcelas, bd_flora, field_parc = "N_Parc", cov_as_range = F, cov_fp = 1){
  valid_input(parcelas, inherit = c("integer", "numeric", "character"))
  valid_input(field_parc, inherit = "character")
  valid_df(bd_flora, names = c(field_parc, "Especie", "Habito", "Cob_BB"))
  valid_input(cov_as_range, inherit = "logical")
  cov_fp <- match.arg(as.character(cov_fp), choices = c(0, 0.5, 1))

  if (all(parcelas %in% unique(dplyr::pull(bd_flora, !!dplyr::sym(field_parc))))) {
    warning(
      sprintf(
        "Faltan las siguientes parcelas en la bd:\n%s",
        setdiff(parcelas, unique(dplyr::pull(bd_flora, !!dplyr::sym(field_parc))))
      )
    )
  }

  bd_flora %>%
    dplyr::select(!!dplyr::sym(field_parc), Especie, Habito, Cob_BB) %>%
    dplyr::filter(!!dplyr::sym(field_parc) %in% parcelas) %>%
    {if (cov_as_range) {
      dplyr::mutate(.,
        Cob_ind = dplyr::case_match(
          Cob_BB,
          "r" ~ "1",
          "+" ~ "3",
          "1" ~ "<5",
          "2" ~ "5-10",
          "3" ~ "10-25",
          "4" ~ "25-50",
          "5" ~ "50-75",
          "6" ~ "75-100",
          .default = as.character(cov_fp)
        )
      ) %>%
        dplyr::group_by(Especie, Habito) %>%
        dplyr::summarise(Cob_ind = paste0(unique(Cob_ind), collapse = "; "), .groups = "drop")
    } else {
      dplyr::mutate(.,
        Cob_ind = dplyr::case_match(
          Cob_BB,
          "r" ~ 1,
          "+" ~ 3,
          "1" ~ 5,
          "2" ~ 7.5,
          "3" ~ 17.5,
          "4" ~ 37.5,
          "5" ~ 62.5,
          "6" ~ 87.5,
          .default = cov_fp
        )
      ) %>%
        dplyr::group_by(Especie, Habito) %>%
        dplyr::summarise(Cob_ind = mean(Cob_ind), .groups = "drop")
    }} %>%
    suppressMessages() %>% suppressWarnings()
}



#' Style WorkBook
#'
#' @param wb wbWorkbook object
#'
#' @noRd
#'
apendices_style <- function(wb) {
  valid_input(wb, inherit = "wbWorkbook")

  new_border <- openxlsx2::create_border(
    bottom = "thin", bottom_color = openxlsx2::wb_color("black"),
    top = "thin", top_color = openxlsx2::wb_color("black"),
    left = "thin", left_color = openxlsx2::wb_color("black"),
    right = "thin", right_color = openxlsx2::wb_color("black")
  )
  wb$styles_mgr$add(new_border, "new_border")
  new_fill <- openxlsx2::create_fill(patternType = "solid", fgColor = openxlsx2::wb_color(hex = "#bcc5d4"))
  wb$styles_mgr$add(new_fill, "new_fill")
  new_font <- openxlsx2::create_font(b = TRUE, color = openxlsx2::wb_color("black"))
  wb$styles_mgr$add(new_font, "new_font")
  header_cellxfs <- openxlsx2::create_cell_style(
    num_fmt_id = 0,
    horizontal = "center",
    text_rotation = 0,
    fill_id = wb$styles_mgr$get_fill_id("new_fill"),
    font_id = wb$styles_mgr$get_font_id("new_font"),
    border_id = wb$styles_mgr$get_border_id("new_border")
  )
  wb$styles_mgr$add(header_cellxfs, "header_cellxfs")
  return(wb)
}



#' Format decimal and bigmark
#'
#' @param x number
#'
#' @noRd
#'
mark <- function(x) format(x, decimal.mark = ",", big.mark = ".")
