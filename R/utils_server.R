#' server
#'
#' @description A utils data
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import dataPAS
#' @noRd
provincias_list <- sf::read_sf(system.file("Comunas.gdb", package = "dataPAS")) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(CUT_REG, REGION, PROVINCIA) %>%
  dplyr::tally() %>% dplyr::ungroup() %>%
  dplyr::mutate_at(
    "CUT_REG",
    ~factor(
      .,
      levels = c("15", "01", "02", "03", "04", "05", "13", "06", "07", "16", "08", "09", "14", "10", "11", "12")
    )
  ) %>%
  dplyr::group_by(CUT_REG, REGION) %>%
  dplyr::summarise(PROVINCIA = list(PROVINCIA)) %>%
  dplyr::mutate(PROVINCIA = setNames(PROVINCIA, REGION)) %>%
  dplyr::arrange(CUT_REG) %>%
  dplyr::pull(PROVINCIA)


#' @noRd
check_input <- function(x, names_req, huso = NULL, id_reset = NULL){
  ok <- T
  if (inherits(x, "sf")) {
    if (!all(any(grepl("POLYGON", sf::st_geometry_type(x))))) {
      shinybusy::report_failure(
        title = "Ups!",
        text = "Ingresar shapefile de poligonos"
      )
      if (!is.null(id_reset)) {
        shinyjs::reset(id = id_reset)
      }
      ok <- F
    }
  }
  if(!all(names_req %in% names(sf::st_drop_geometry(x)))){
    shinybusy::report_failure(
      title = "Ups!",
      text = tags$p(
        "Shapefile sin los campos requeridos", rep_br(2),
        tags$b("Requeridos: "), paste(names_req %>% shQuote(), collapse = ", "), rep_br(2),
        tags$b("Faltan: "), paste(setdiff(names_req, names(sf::st_drop_geometry(x))) %>% shQuote(), collapse = ", ")
      )
    )
    if (!is.null(id_reset)) {
      shinyjs::reset(id = id_reset)
    }
    ok <- F
  }

  if (!is.null(huso)) {
    lon <- x %>% sf::st_union() %>% sf::st_centroid() %>% sf::st_transform(4326) %>% sf::st_coordinates() %>% .[,1]
    if ((lon >= -72 & huso == "18S") | (lon < -72 & huso == "19S")) {
      shinybusy::report_failure(
        title = "Ups!",
        text = "Coordenadas del shp no coinciden con la seleccionada"
      )
      ok <- F
    }
  }
  if (ok) {
    shinybusy::notify_success("Perfecto! Todos los campos necesarios :)", timeout = 3000, position = "right-bottom")
  }
  return(ok)
}

#' @noRd
split_tail <- function(text, pattern){
  if (is.na(text)) {
    return(text)
  }
  position <- gregexpr(pattern, text)[[1]]
  if (length(position) > 0 && position[1] != -1) {
    last_position <- utils::tail(position, n = 1)
    part_1 <- substr(text, 1, last_position - 1)
    part_2 <- substr(text, last_position + nchar(pattern), nchar(text))
    resultado <- c(trimws(part_1), trimws(part_2))
  } else {
    resultado <- text
  }
  return(resultado)
}

#' Chequear N_Area
#'
#' @description función para chequear numeración de áreas preliminares
#' @param areas Objeto sf de las areas de corta.
#' @return reporte de errores.
#' @rdname check_prelim
#' @export
check_n_area <- function(areas){
  valid_input(
    areas,
    inherit = "data.frame",
    names = c("N_Area")
  )
  shiny <- shiny::isRunning()
  n_area <- areas$N_Area
  if (!all(seq_len(length(n_area)) %in% unique(n_area))) {
    fal <- which(!seq_len(length(n_area)) %in% unique(n_area))
    rep <- names(which(table(n_area) > 1))
    sob <- unique(n_area)[which(!unique(n_area) %in% seq_len(length(n_area)))]
    text <- paste0(
      ngettext(length(fal), "Falta el área: ", "Faltan las áreas: "),
      paste(fal, collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y "),"\n"
    )
    if (any(table(n_area) > 1)) {
      text <- paste0(
        text,
        ngettext(length(rep), "Se repite el área: ", "Se repiten las áreas: "),
        paste(rep, collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y "),"\n"
      )
    }
    if (any(!unique(n_area) %in% seq_len(length(n_area)))) {
      text <- paste0(
        text,
        ngettext(length(sob), "Sobra el área: ", "Sobran las áreas: "),
        paste(sob, collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y "),"\n"
      )
    }
    if (shiny) {
      shinyalert::shinyalert(
        title = "Áreas de corta mal enumeradas!",
        text = text,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
    }
  } else {
    text <- "Áreas de corta bien enumeradas!\n"
  }
  cat(text)
  return(invisible())
}

#' Chequear N_Rodal
#'
#' @description función para chequear numeración de rodales preliminares
#' @param rodales Objeto sf con los rodales de las areas de corta.
#' @return reporte de errores.
#' @rdname check_prelim
#' @export
check_n_rodal <- function(rodales){
  valid_input(
    rodales,
    inherit = "data.frame",
    names = c("N_Predio", "N_Rodal", "PID")
  )
  shiny <- shiny::isRunning()
  res <- list()
  error <- F
  df1 <- rodales %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(N_Predio, N_Rodal) %>%
    dplyr::summarise(
      N_PID_rep = length(unique(PID)),
      PID = paste(unique(PID), collapse = " - "),
      .groups = "drop"
    ) %>%
    dplyr::relocate(N_Rodal) %>%
    dplyr::filter(N_PID_rep > 1) %>%
    suppressMessages() %>% suppressWarnings()
  df2 <- rodales %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(PID) %>%
    dplyr::summarise(
      N_rep = length(unique(N_Rodal)),
      N_Rodal = paste(unique(N_Rodal), collapse = " - "),
      .groups = "drop"
    ) %>%
    dplyr::filter(N_rep > 1) %>%
    suppressMessages() %>% suppressWarnings()

  if (length(unique(rodales$PID)) > length(unique(rodales$N_Rodal))) {
    fal <- which(!seq_len(length(unique(rodales$PID))) %in% unique(rodales$N_Rodal))
    res[[length(res) + 1]] <- paste0(
      ngettext(length(fal), "Falta el N_Rodal: ", "Faltan los N_rodal: "),
      paste(fal, collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y ")
    )
    error <- T
  }
  if (nrow(df1) != 0) {
    if (error) {
      res[[length(res) + 1]] <- if(shiny) tags$br() else "\n"
    }
    res[[length(res) + 1]] <- "'N_Rodal' repetido en rodales que son diferentes:"
    res[[length(res) + 1]] <- if (shiny) {
      kableExtra::kbl(df1) %>%
        kableExtra::kable_styling() %>%
        shiny::HTML()
    } else {
      kableExtra::kbl(df1, format = "simple")
    }
    error <- T
  }
  if (nrow(df2) != 0) {
    if (error) {
      res[[length(res) + 1]] <- if(shiny) tags$br() else "\n"
    }
    res[[length(res) + 1]] <- "Rodales con más de un 'N_Rodal':"
    res[[length(res) + 1]] <- if (shiny) {
      kableExtra::kbl(df2) %>%
        kableExtra::kable_styling() %>%
        shiny::HTML()
    } else {
      kableExtra::kbl(df2, format = "simple")
    }
    error <- T
  }

  if (error) {
    if (shiny) {
      shinyalert::shinyalert(
        title = "Rodales mal enumerados!",
        text = do.call(tags$p, res),
        type = "error",
        html = T,
        closeOnEsc = T,
        confirmButtonCol = "#6FB58F",
        showConfirmButton = T,
        animation = T
      )
    } else {
      purrr::map(res, ~cat(as.character(.x), sep = "\n"))
    }
  } else {
    cat("Rodales bien enumerados!\n")
  }
  return(invisible())
}

#' Chequear N_Predio
#'
#' @description función para chequear numeración de predios preliminares
#' @param predios Objeto sf con los predios de las areas de corta.
#' @return reporte de errores.
#' @rdname check_prelim
#' @export
check_n_predio <- function(predios) {
  valid_input(
    predios,
    inherit = "data.frame",
    names = c("N_Predio", "Nom_Predio", "Rol")
  )
  shiny <- shiny::isRunning()
  n_predio <- predios$N_Predio
  if (!all(seq_len(nrow(
    predios %>% dplyr::count(Nom_Predio, Rol, sort = T))) %in% unique(predios$N_Predio)
  )) {
    fal <- which(!seq_len(length(n_predio)) %in% unique(n_predio))
    rep <- names(which(table(n_predio) > 1))
    sob <- unique(n_predio)[which(!unique(n_predio) %in% seq_len(length(n_predio)))]
    text <- paste0(
      ngettext(length(fal), "Falta el predio: ", "Faltan los predios: "),
      paste(fal, collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y "),"\n"
    )
    if (any(table(n_predio) > 1)) {
      text <- paste0(
        text,
        ngettext(length(rep), "Se repite el predio: ", "Se repiten los predios: "),
        paste(rep, collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y "),"\n"
      )
    }
    if (any(!unique(n_predio) %in% seq_len(length(n_predio)))) {
      text <- paste0(
        text,
        ngettext(length(sob), "Sobra el predio: ", "Sobran los predios: "),
        paste(sob, collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y "),"\n"
      )
    }
    if (shiny) {
      shinyalert::shinyalert(
        title = "Predios mal enumeradas!",
        text = text,
        type = "error",
        closeOnEsc = T,
        showConfirmButton = T,
        confirmButtonCol = "#6FB58F",
        animation = T
      )
    }
  } else {
    text <- "Predios bien enumeradas!\n"
  }
  cat(text)
  return(invisible())
}

#' @noRd
check_sup_ha <- function(x, target) {
  valid_input(x, inherit = "data.frame", names = c("Sup_ha"))
  valid_input(target, inherit = "character")
  stopifnot(length(target) == 1)
  stopifnot("'target' no encontrado en x" = target %in% names(x))

  shiny <- shiny::isRunning()
  eval_sup <- x$Sup_ha == 0
  text <- ""
  if (any(eval_sup)) {
    text <- paste0(
      ngettext(length(sum(eval_sup)), "Poligonos con Sup_ha = 0!", "Poligonos con Sup_ha = 0!"),
      "\n", target,": ",
      paste(sf::st_drop_geometry(x)[which(eval_sup), ] %>% dplyr::pull(target) %>% unique(), collapse = ", ") %>%
        split_tail(",") %>%
        paste(collapse = " y "), "\n"
    )
  }
  cat(text)
  return(invisible())
}

#' @noRd
valid_input <- function(..., inherit = NULL, names = NULL, geometry = NULL) {
  x <- list(...)
  arg_x <- as.character(substitute(list(...)))[-1]
  class_x <- lapply(x, class)
  if (!is.null(inherit)) {
    if (!all(sapply(x, inherits, inherit))) {
      args_e <- arg_x[grep(paste(inherit, collapse = "|"), class_x, invert = T)]
      stop(
        paste(
          ngettext(length(args_e), "El argumento", "Los argumentos"),
          paste(sQuote(args_e), collapse = ", "),
          ngettext(length(inherit), "debe ser de clase", "deben ser de una de las clases"),
          paste(sQuote(inherit), collapse = ", ")
        ),
        call. = F
      )
    }
  }
  if (!is.null(geometry) & all(sapply(x, inherits, "sf"))) {
    geometry <- match.arg(geometry, choices = c("POLYGON", "POINT", "LINESTRING"))
    if (!all(purrr::map_lgl(x, ~ any(grepl(geometry, sf::st_geometry_type(.)))))) {
      args_e <- arg_x[which(!purrr::map_lgl(x, ~ any(grepl(geometry, sf::st_geometry_type(.)))))]
      stop(
        paste(
          ngettext(length(args_e), "El objeto", "Los objetos"),
          paste(sQuote(args_e), collapse = ", "),
          ngettext(length(inherit), "debe", "deben"),
          "ser de tipo polígono"
        ),
        call. = F
      )
    }
  }
  if (!is.null(names) & length(x) == 1 & inherits(x[[1]], "data.frame")) {
    if (!all(names %in% names(x[[1]]))) {
      stop(
        paste(
          "El argumento", sQuote(arg_x), "debe contener al menos",
          ngettext(length(names), "el campo:", "los campos:"),
          paste(sQuote(names), collapse = ", ")
        ),
        call. = F
      )
    }
  }
  invisible()
}

#' @noRd
valid_dem <- function(dem) {
  arg_dem <- substitute(dem)
  if (!inherits(dem, c("character", "SpatRaster"))) {
    stop("DEM debe ser un objeto 'SpatRaster' o bien la ruta del archivo raster", call. = F)
  }
  if (inherits(dem, "character")) {
    if (length(dem) > 1) {
      stop(paste("Argumento", sQuote(arg_dem), "debe ser solo una ruta"), call. = F)
    }
    if (!file.exists(dem)) {
      stop(paste("La ruta del archivon ingresada para el argumento", sQuote(arg_dem), "no existe."), call. = F)
    }
    if(!tools::file_ext(dem) %in% c("tif", "tiff")) {
      stop(sprintf(
        "Extensión no válida para el argumento %s. Se permite: %s",
        shQuote(arg_dem),
        paste(c(".tif", ".tiff"), collapse = ", ")
      ))
    }
  }
  invisible()
}

#' @noRd
valid_df <- function(df, names = NULL) {
  arg_df <- substitute(df)
  if (!inherits(df, c("character", "data.frame")) | inherits(df, "sf")) {
    stop(paste("Argumento", sQuote(arg_df), "debe ser un objeto 'data.frame' o bien la ruta del archivo xlsx"), call. = F)
  }
  if (inherits(df, "data.frame")) {
    if (nrow(df) == 0) {
      stop(paste("Argumento", sQuote(arg_df), "no contiene datos."))
    }
    if (!is.null(names) & !all(names %in% names(df))) {
      diff <- setdiff(names, names(df))
      stop(
        paste(
          "El argumento", sQuote(arg_df), "debe contener al menos",
          ngettext(length(names), "el campo:", "los campos:"),
          paste0(paste(sQuote(names), collapse = ", "),"\n",
          ngettext(length(diff), "Falta:", "Faltan:")),
          paste(sQuote(diff), collapse = ", ") %>% split_tail(",") %>% paste(collapse = " y ")
        ),
        call. = F
      )
    }
  }
  if (inherits(df, "character")) {
    if (length(df) > 1) {
      stop(paste("Argumento", sQuote(arg_df), "debe ser solo una ruta"), call. = F)
    }
    if (!file.exists(df)) {
      stop(paste("La ruta del archivon ingresada para el argumento", sQuote(arg_df), "no existe."), call. = F)
    }
    if(!tools::file_ext(df) %in% c("xlsx")) {
      stop(sprintf(
        "Extensión no válida para el argumento %s. ingresar un archivo '.xlsx'",
        shQuote(arg_df)
      ))
    }
  }
  invisible()
}

#' download_files
#'
#' @param x objecto o lista de objetos. admite objetos 'data.frame', 'sf' y 'wbWorkbook'.
#' @param name_save vector de caracteres con los nombres de los objetos.
#' @param dir_save directorio donde guardar el o los objetos.
#'
#' @returns archivos 'xlsx', 'shp' o 'zip'.
#' @name download_files
#' @export
download_files <- function(x, name_save, dir_save) {
  stopifnot(dir.exists(dir_save))

  filetype <- x %>%
    {if(any(class(.) == "list")) . else list(.)} %>%
    purrr::map( ~ ifelse(
      inherits(., "wbWorkbook"),
      "wb",
      ifelse(
        inherits(., "sf"),
        "sf",
        ifelse(inherits(., "data.frame") & !inherits(., "sf"), "xlsx", "")
      )
    )) %>%
    {if(length(.) == 1) unlist(.) else .}

  stopifnot("Solo se admiten objetos de tipo 'data.frame', 'sf' y 'wbWorkbook'" = all(filetype %in% c("sf", "wb", "xlsx")))

  file <- file.path(dir_save, ifelse(
    length(filetype) > 1,
    ifelse(is.null(names(name_save)), "Archivos_comprimidos.zip", paste0(names(name_save), ".zip")),
    paste0(as.character(name_save), ifelse(filetype == "sf", ".zip", ".xlsx"))
  ))

  wd <- getwd()
  temp_dir <- tempdir()
  setwd(temp_dir)
  file.remove(list.files(pattern = "\\."))
  purrr::pwalk(
    if(length(filetype) == 1) {
      list(list(x), list(filetype), ifelse(inherits(name_save, "list"), name_save, list(name_save)))
    } else {
      list(x, filetype, unlist(name_save))
    },
    .f = function(x, y, z) {
      switch(
        y,
        sf = sf::write_sf(x, paste0(tools::file_path_sans_ext(z), ".shp")),
        wb = openxlsx2::wb_save(x, paste0(tools::file_path_sans_ext(z), ".xlsx"), overwrite = T),
        xlsx = openxlsx2::write_xlsx(x, paste0(tools::file_path_sans_ext(z), ".xlsx"), overwrite = T)
      )
    }
  )
  list_files <- unname(unlist(map(unlist(name_save), function(x) {list.files(pattern = x)})))
  if(tools::file_ext(file) == "zip") {
    zip::zip(zipfile = file, files = list_files)
  } else {
    file.copy(from = list_files, to = file, overwrite = T)
  }
  setwd(wd)
}
