#' Cobertura de copa
#'
#' @param x,y Diametros de copa.
#' @param method Metodo; `circle` o `ellipse`. Default `circle`.
#' @param Na.value Devuelve este valor si la cobertura es \code{NA}. Default `0`.
#'
#' @return Valor numerico con cobertura de copa.
#' @noRd
#' @examples
#' cup_coverage(x = 7, y = 3)
#' cup_coverage(x = 7, y = 3, method = "ellipse")
#' cup_coverage(x = NA, y = NA, Na.value = 0)
cup_coverage <- function(x, y, method = "circle", Na.value = 0){
  stopifnot(method %in% c("circle", "ellipse"))
  stopifnot(Na.value %in% c(0, NA))

  if (method == "circle") {
    val <- pi*(mean(c(x,y), na.rm = T)/2)^2
  }
  if (method == "ellipse"){
    val <- pi * x/2 * y/2
  }
  if (is.na(val)) {
    return(Na.value)
  } else {
    return(val)
  }
}

#' Cobertura de copa en la parcela
#'
#' @param x Vector numerico de coberturas individuales de copa.
#' @param plot_size Superficie de la parcela en m2. Default `500`.
#' @param percent Logico; si es \code{TRUE} agrega el simbolo de `%`.
#' @param digits Cantidad de decimales.
#' @param ... Otros argumentos de la funcion `formatC`.
#'
#' @return Cobertura de parcela formato texto.
#' @noRd
#'
#' @examples
#' plot_coverage(c(20, 15), percent = F, digits = 2, decimal.mark = ",")
#' plot_coverage(c(NA, NA, 20, 15), percent = T, digits = 1, decimal.mark = ",")
plot_coverage <- function(x, plot_size = 500, percent = F, digits = 1, ...){
  x <- sum(x, na.rm = T)/plot_size
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
#' @importFrom purrr map2_chr
#' @importFrom stringi stri_split_fixed
#'
#' @examples
#' italic_sp(x = c(
#' "Bosque de Schinus latifolius-Quillaja saponaria",
#' "Matorral con suculentas de Flourensia thurifera",
#' "Plantación de especies nativas de Acacia caven-Quillaja saponaria"))
italic_sp <- function(x){
  split_de <- function(texto, patron){
    if (is.na(texto)) {
      return(texto)
    }
    posiciones <- gregexpr(patron, texto)[[1]]
    if (length(posiciones) > 0 && posiciones[1] != -1) {
      ultima_posicion <- tail(posiciones, n = 1)
      parte1 <- substr(texto, 1, ultima_posicion - 1)
      parte2 <- substr(texto, ultima_posicion + nchar(patron), nchar(texto))
      resultado <- c(trimws(parte1), trimws(parte2))
    } else {
      resultado <- texto
    }
    return(resultado)
  }
  tv <- lapply(x, split_de, patron = " de ")
  el1 <- lapply(tv, function(a) {return(a[1])})
  el2 <- lapply(tv, function(a) {
    if (length(a) > 1) {
      return(paste0("*",a[2],"*"))
    } else {
      return(NA) # O cualquier otro valor que desees para los elementos que no tienen un segundo elemento
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
#' @param bd Base de datos con las parcelas.
#' @param add_var `NULL`; Variable/s que desea agregar.
#'
#' @return data.frame con densidad (ind/ha) por especie.
#' @export
#'
nha_x_sp_fun <- function(parcelas, bd, add_var = NULL){
  bd %>%
    select(N_Parc, Especie, Nha) %>%
    filter(N_Parc %in% parcelas) %>%
    complete(N_Parc, Especie, fill = list(Nha = 0)) %>%
    left_join(bd %>% count(Especie, across(add_var)) %>% select(-n)) %>%
    group_by(Especie, across(add_var)) %>%
    summarise(Nha = mean(Nha,na.rm = T) %>% round_half_up(), .groups = "drop") %>%
    mutate_at("Nha", as.integer) %>%
    suppressMessages() %>% suppressWarnings()
}

#' Cobertura por especie
#'
#' @param parcelas Nombre de parcelas.
#' @param bd_flora Base de datos con las parcelas.
#'
#' @return data.frame con cobertura de copas por especie.
#' @export
#' @importFrom dplyr select filter mutate case_match group_by summarise
cob_x_sp_fun <- function(parcelas, bd_flora){
  bd_flora %>%
    dplyr::select(N_Parc, Especie, Habito, Cob_BB) %>%
    dplyr::filter(N_Parc %in% parcelas) %>%
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
    dplyr::group_by(Especie, Habito) %>%
    dplyr::summarise(Cob_ind = paste0(unique(Cob_ind), collapse = "; "), .groups = "drop") %>%
    suppressMessages() %>% suppressWarnings()
}
