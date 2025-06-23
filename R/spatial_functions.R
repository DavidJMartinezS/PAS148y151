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
#' @param order Orden espacial.
#'
#' @return Objetos sf o numeros
#' @rdname spatial_functions
#' @export
#'
#' @importFrom igraph graph_from_adjacency_matrix components
#' @importFrom sf st_distance st_agr st_difference st_union st_intersection st_buffer st_centroid st_coordinates st_nearest_feature st_geometry st_point st_sfc st_crs
#' @importFrom dplyr bind_rows group_by summarise pull mutate slice arrange expr desc
#' @importFrom terra rast `crs<-` crop terrain extract crs
#' @importFrom janitor round_half_up
#'
group_by_distance <- function(x, distance){
  stopifnot("El objeto ingresado no es un objeto de la clase 'sf'." = inherits(x, "sf") || inherits(x, "sfc"))
  dist_matrix = sf::st_distance(x, by_element = FALSE)
  class(dist_matrix) = NULL
  connected = dist_matrix <= distance
  g = igraph::graph_from_adjacency_matrix(connected)
  return(igraph::components(g)$membership)
}

#' @rdname spatial_functions
#' @export
my_union <- function(x, y) {
  stopifnot("El objeto ingresado no es un objeto de la clase 'sf'." = inherits(x, "sf") || inherits(x, "sfc"))
  stopifnot("El objeto ingresado no es un objeto de la clase 'sf'." = inherits(y, "sf") || inherits(x, "sfc"))
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
    st_collection_extract(geom_type) %>%
    dplyr::bind_rows(
      sf::st_intersection(x, y) %>%
        st_collection_extract(geom_type)
    )
}

#' @rdname spatial_functions
#' @export
get_slope <- function (dem, x) {
  stopifnot("El objeto ingresado no es un objeto de la clase 'sf'." = inherits(x, "sf") || inherits(x, "sfc"))
  stopifnot("DEM debe ser un objeto SpatRaster o bien la ruta del archivo" = (class(dem) %in% c("character", "SpatRaster")) %>% any())
  if (inherits(dem, "character")) {
    stopifnot("ruta del archivo no encontrada" = file.exists(dem))
  }
  if (inherits(dem, "SpatRaster")) dem else terra::rast(dem) %>%
    terra::`crs<-`(terra::crs(x)) %>%
    terra::crop(sf::st_buffer(x, 50)) %>%
    terra::terrain(v = "slope", neighbors = 8, unit = "degrees") %>%
    {\(x) tan(x * pi / 180) * 100}() %>%
    terra::extract(y = x, touches = T) %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(slope = mean(slope)) %>%
    dplyr::pull(slope) %>%
    janitor::round_half_up(1)
}

#' @rdname spatial_functions
#' @export
st_order <- function(x, order = "NS-OE"){
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

  if("sf" %in% class(x)){
    x <- x %>% sf::st_centroid()
    ord <- rep(NA, nrow(x))
    listo <- rep(F, nrow(x))
    area_last <- x %>%
      dplyr::mutate(X = sf::st_coordinates(geometry)[,1], Y = sf::st_coordinates(geometry)[,2]) %>%
      dplyr::arrange(!!!order_selected) %>%
      dplyr::slice(1)
    i = 1
    ord[which(st_geometry(x) == sf::st_geometry(area_last))] <- i
    listo[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- T
    while (any(is.na(ord))) {
      n <- sf::st_nearest_feature(area_last, x[!listo,])
      area_last <- x[!listo,][n,]
      i <- i + 1
      ord[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- i
      listo[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- T
    }
    return(as.integer(ord))
  } else {
    x <- x %>% sf::st_centroid()
    ord <- rep(NA, length(x))
    listo <- rep(F, length(x))
    area_last <- x %>%
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
    while (any(is.na(ord))) {
      n <- sf::st_nearest_feature(area_last, x[!listo])
      area_last <- x[!listo][n]
      i <- i + 1
      ord[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- i
      listo[which(sf::st_geometry(x) == sf::st_geometry(area_last))] <- T
    }
    return(as.integer(ord))
  }
}

