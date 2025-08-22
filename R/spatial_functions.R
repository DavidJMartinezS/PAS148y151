#' Funciones Geoespaciales
#'
#' @description
#' `group_by_distance` crea agrupaciones en base a la distancia ingresada.
#' `my_union` regresa la union de dos capas, se crucen o no las geometrias.
#' `get_slope` regresa la pendiente media.
#' `st_order` crea un orden espacial del objeto sf ingresado.
#'
#' @param x,y Objeto sf.
#' @param distance Distancia maxima para agrupar.
#' @param dem Digital Elevation Model. Puede ser un objeto SpatRaster o bien la ruta de la imagen.
#' @param order Orden espacial. see `details`.
#' @param progress Valor logico. \code{TRUE} para que se mostrar una barra de progreso en la consola.
#'
#' @details
#' El argumento `order` consiste en una combinacion de 4 letras con las iniciales de los 4 puntos cardinales (N, S, E, O) separadas con un guión segun si es latitud o longitud. Por ejemplo, eel argumento por default tiene un orden \code{'NS-OE'}, lo cual indica que que se seguira un orden de Norte a Sur primeramente y de Oste a Este de manera secundaria.
#'
#'
#' @return Objetos sf o numeros
#' @rdname spatial_functions
#' @export
#'
group_by_distance <- function(x, distance){
  valid_input(x, inherit = c("sf", "sfc"))
  valid_input(distance, inherit = "numeric")

  dist_matrix = sf::st_distance(x, by_element = FALSE)
  class(dist_matrix) = NULL
  connected = dist_matrix <= distance
  g = igraph::graph_from_adjacency_matrix(connected)
  return(igraph::components(g)$membership)
}

#' @rdname spatial_functions
#' @export
my_union <- function(x, y) {
  valid_input(x, y, inherit = c("sf", "sfc"))

  sf::st_agr(x) = "constant"
  sf::st_agr(y) = "constant"
  geom_type <- if(any(sf::st_is(x, "POLYGON") | sf::st_is(x, "MULTIPOLYGON"))) {
    "POLYGON"
  } else if(any(sf::st_is(x, "LINESTRING") | sf::st_is(x, "MULTILINESTRING"))) {
    "LINESTRING"
  } else if(any(sf::st_is(x, "POINT") | sf::st_is(x, "MULTILIPOINT"))) {
    "POINT"
  }
  x %>%
    sf::st_difference(sf::st_union(y)) %>%
    sf::st_collection_extract(geom_type) %>%
    dplyr::bind_rows(
      sf::st_intersection(x, y) %>%
        sf::st_collection_extract(geom_type)
    )
}

#' @rdname spatial_functions
#' @export
get_slope <- function (dem, x) {
  valid_dem(dem)
  valid_input(x, inherit = c("sf", "sfc"))

  rast <- tryCatch({
    (if (inherits(dem, "SpatRaster")) dem else terra::rast(dem))
  }, error = function(e) stop("Error en get_slope: No se pudo leer archivo raster", call. = F))

  rast_proj <- tryCatch({
    rast %>%
      {if(terra::crs(rast) %>% stringi::stri_split(fixed = "\n") %>% .[[1]] %>% .[1] ==
          terra::crs(x) %>% stringi::stri_split(fixed = "\n") %>% .[[1]] %>% .[1]) {
        terra::`crs<-`(., terra::crs(x))
      } else {
        terra::project(., x)
      }}
  }, error = function(e) stop("Error en get_slope: No se pudo reprojectar el archivo raster", call. = F))

  rast_crop <- tryCatch({
    rast_proj %>% terra::crop(sf::st_buffer(x, 50))
  }, error = function(e) stop("Error en get_slope: La extensión del DEM no se superpone con la de los datos espaciales de 'x'", call. = F))

  if (all(rast_crop %>% as.vector() %>% unique() == 0)) {
    stop("Error en get_slope: El DEM no se superpone con los límites de los datos espaciales 'x'", call. = F)
  }

  slope <- rast_crop %>%
    terra::terrain(v = "slope", neighbors = 8, unit = "degrees") %>%
    {\(x) tan(x * pi / 180) * 100}() %>%
    terra::extract(y = x, touches = T) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(slope = mean(slope, na.rm = T)) %>%
    dplyr::pull(slope) %>%
    janitor::round_half_up(1)

  if (all(slope %in% c(0, NA))) {
    stop("Error en get_slope: El DEM no se superpone con los límites de los datos espaciales 'x'", call. = F)
  }

  if (any(is.na(slope))) {
    warning(
      paste0(
        "Cuidado en get_slope: Valores 'NA' en ",
        ngettext(length(which(is.na(slope))), "la entidad ", "las entidades "),
        paste(sQuote(which(is.na(slope))), collapse = ", "), "."
      ), call. = F
    )
  }
  if (any(slope == 0)) {
    warning(
      paste0(
        "Cuidado en get_slope: Valores 'NA' en ",
        ngettext(length(which(slope == 0)), "la entidad ", "las entidades "),
        paste(sQuote(which(slope == 0)), collapse = ", "), "."
      ), call. = F
    )
  }

  return(slope)
}

#' @rdname spatial_functions
#' @export
st_order <- function(x, order = "NS-OE", progress = T){
  valid_input(x, inherit = c("sf", "sfc"))
  valid_input(progress, inherit = c("logical"))

  order <- match.arg(
    order,
    choices = c("NS-EO","NS-OE","SN-EO","SN-OE","EO-NS","EO-SN","OE-NS","OE-SN")
  )

  order_selected <- list(
    "NS-EO" = c(dplyr::expr(dplyr::desc(Y)), dplyr::expr(dplyr::desc(X))),
    "NS-OE" = c(dplyr::expr(dplyr::desc(Y)), expr(X)),
    "SN-EO" = c(dplyr::expr(Y), dplyr::expr(dplyr::desc(X))),
    "SN-OE" = c(dplyr::expr(Y), dplyr::expr(X)),
    "EO-NS" = c(dplyr::expr(dplyr::desc(X)),dplyr::expr(dplyr::desc(Y))),
    "EO-SN" = c(dplyr::expr(dplyr::desc(X)),dplyr::expr(Y)),
    "OE-NS" = c(dplyr::expr(X),dplyr::expr(dplyr::desc(Y))),
    "OE-SN" = c(dplyr::expr(X),dplyr::expr(Y))
  )[order] %>% unlist()

  x <- x %>% sf::st_centroid()
  ord <- rep(NA, length(sf::st_geometry(x)))
  listo <- rep(F, length(sf::st_geometry(x)))
  area_last <- sf::st_geometry(x) %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    dplyr::arrange(!!!order_selected) %>%
    dplyr::slice(1) %>%
    unlist() %>%
    sf::st_point() %>%
    sf::st_sfc(crs = sf::st_crs(x))
  i = 1
  ord[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- i
  listo[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- T
  if (progress) {
    len <- length(ord[is.na(ord)])
    progreso <- round((i / (len)) * 50)
    espacios <- 50 - progreso
    lleva <- paste0(rep("=", progreso), collapse = "")
    falta <- paste0(rep(" ", espacios), collapse = "")
    cat(sprintf(
      "\rProgreso: [%s%s] %s de %s (%1.0f%%)", lleva,  falta, i, len, i / len * 100
    ))
  }
  while (any(is.na(ord))) {
    n <- sf::st_nearest_feature(area_last, x[!listo])
    area_last <- x[!listo][n]
    if (progress) {
      progreso <- round(((i + 1) / len) * 50)
      espacios <- 50 - progreso
      lleva <- paste0(rep("=", progreso), collapse = "")
      falta <- paste0(rep(" ", espacios), collapse = "")
      cat(sprintf(
        "\rProgreso: [%s%s] %s de %s (%1.0f%%)", lleva, falta, i+1, len, (i + 1) / len * 100
      ))
    }
    i <- i + 1
    ord[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- i
    listo[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- T
  }
  return(as.integer(ord))
}

