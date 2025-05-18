#' ui
#'
#' @description A utils function for ui
#'
#' @return The return value, if any, from executing the utility.
#'
#' @importFrom bsplus bs_accordion bs_set_opts bs_append bs_carousel bs_set_data shinyInput_label_embed shiny_iconlink bs_embed_tooltip bs_carousel_image
#'
#' @noRd
info_dashboard <- function(){
  bsplus::bs_accordion(id = "accordion") %>%
    bsplus::bs_set_opts(
      use_heading_link = T,
      panel_type = "success"
    ) %>%
    bsplus::bs_append(
      title = "Objetivos e instrucciones del Dashboard",
      content = tags$p(
        "Este Dashboard esta elaborado con el objetivo de apoyar la elaboración del PAS 148 y 151, proporcionando
        resultados descargables de capas y tablas (formatos .shp y .xlsx respectivamente)."
      )
    ) %>%
    bsplus::bs_append(
      title = "Consideraciones generales",
      content = tags$div(
        tags$ul(
          list_to_li(c(
            "En la parte superior izquierda del dashboard se presentan algunos parámetros generales a los que debe prestarle atención.",
              "Para ingresar los shapefiles, debe cargar los 4 archivos necesarios para su lectura, que son: 'dbf', 'prj' 'shp' y 'shx'.
            De otro modo no se reconocerá el shapefile.",
              "Los shapefile ingresados deben contener los campos requeridos que se señalan. Deben contener los mismo caracteres que se
            indican, pero pueden diferir en los tildes y mayúsculas.",
              "Por favor ingresar los shapefiles con los campos que se requieren para un buen funcionamiento del dashboard y evitar
            errores en la obtención de los resultados.",
              "No confiar 100% en los resultados, haga siempre un chequeo de los resultados, tanto shapefiles como tablas."
          ))
        )
      )
    ) %>%
    bsplus::bs_append(
      title = "Consideraciones particulares",
      content = tags$div(
        tags$ul(
          tags$li(
            "Ayudas cartográficas",
            tags$ul(
              tags$li(
                "Ordenar Shapefiles",
                tags$ul(
                  list_to_li(c(
                    "Esta funcion crea una columna llamada 'ID_ord' el cual enumera los poligonos trantando de ordenarlos de
                    norte a sur y de oeste considerando la contiguidad de los poligonos.",
                    "Puede crear una enumeración por grupo. Por ejemplo si ingresa un shapefile de áreas de corta y quiere enumerarlas
                    por predio, entonces seleccione el campo que indentifica cada predio. Puede agrupar por multiples campos.",
                    "Tener en consideración que los resultados pueden variar mucho dependiendo de la disposición espcial y
                    forma de los poligonos. Funciona mejor en poligonos dispuestos de forma más lineal."
                  ))
                )
              ),
              tags$li(
                "Generar rodales y áreas de corta",
                tags$ul(
                  list_to_li(c(
                    "Esta función es para generar los rodales y áreas de corta a partir de los shp cargador anteriormente.
                    La app da diferentes opciones de como generar las áreas de corta.",
                    "Puede agrupar o no los rodales de linea base. Si agrupa por Tipo forestal, entonces dos poligonos contiguos
                    del mismo tipo forestal serán unidas sus geometrías.",
                    "Si elige la opción de separar por clase de uso de suelo (CUS) entonces un área de corta con 2 CUS será
                    dividida por su geometría, de lo contrario no se divide el poligono pero queda el campo con las dos CUS."
                  ))
                )
              ),
              tags$li(
                "Añadir atributos de pendiente e hidrografía",
                tags$ul(
                  list_to_li(c(
                    "Ingrese shp que al que desea agregar los atributos y seleccione los atributos que desea agregar.
                    Ingresar el o los archivos necesarios para agregar los campos.",
                    "Lo campos que se agregan de pendiente son; 'Pend_media' y 'Ran_Pend', y de hidrografía 'Distancia' y los campos que se seleccionen."
                  ))
                )
              )
            )
          ),
          tags$li(
            "Cartografía y Apéndices",
            tags$ul(
              tags$li(
                "Cartografía digital",
                tags$ul(
                  list_to_li(c(
                    "Las capas de áreas, predios y rodales que se ingresan deben haber sido revisadas previamente para poder
                    generar la cartografía digital definitiva.",
                    "Por favor ingresar DEM acotado al área de estudio, de lo contrario la aplicación no lo soportará.",
                    "Tener en consideración que la elaboración de las capas de uso actual, y la de bases cartográficas
                    podrian hacer que el codigo tarde más de lo normal si es que son muy grandes.",
                    "Al costado de estas instrucciones se encuentran unos ejemplos de como utilizar los parámetros para
                    utilizadas para las bases cartográficas.",
                    "Se ha dispuesto la opción de decargar información cartográfica de caminos e hidrografía a partir de
                    información de OpenStreetMaps para proporcionar una información adicional y poder complementar.
                    Tener en cuenta que la capa contiene más campos de los requeridos, para que se pueda verificar la nomenclatura
                    de los tipos de caminos o drenaje, cualquiera sea el caso. Esto dado que la nomenclatura puede diferir de la
                    capa hidrografica"
                  ))
                )
              ),
              tags$li(
                "Apéndices",
                tags$ul(
                  list_to_li(c(
                    paste0(
                      "Corroborar que exista una nomenclatura única en los nombres de las especies y no aparezcan por ejemplo:",
                      tags$em('Acacia caven'), "y", tags$em('Vachellia caven, Lithrea caustica'), "y", tags$em('Lithraea caustica'),", etc."
                    ),
                    "Para generar los apéndices 2 y 3 se requiere cargar la capa de rodales, predios y la BD de flora.
                    Todas estas cargadas previamente para la cartografía digital.
                    De manera opcional se puede incluir la base de datos de parcelas de cobertura.",
                    "Para generar el apéndice 5 con las tablas del formulario se debe ingresar la tabla con los
                    atributos de rodal descargada y revisada previamente, además de haber generado la cartografía digital.
                    De manera opcional se puede ingresar el shp de obras y la BD de fauna con los campos requeridos."
                  ))
                )
              )
            )
          )
        )
      )
    )
}

#' @noRd
info_cut_buffer <- function(){
  bsplus::bs_carousel(id = "hidro_example", use_indicators = T, use_controls = T) %>%
    bsplus::bs_set_data(interval = FALSE) %>%
    bsplus::bs_append(content = bsplus::bs_carousel_image(src = "www/clip.png")) %>%
    bsplus::bs_append(content = bsplus::bs_carousel_image(src = "www/buffer_2000.png")) %>%
    bsplus::bs_append(content = bsplus::bs_carousel_image(src = "www/crop.png")) %>%
    bsplus::bs_append(content = bsplus::bs_carousel_image(src = "www/crop_2000.png")) %>%
    bsplus::bs_append(content = bsplus::bs_carousel_image(src = "www/crop_by_row.png")) %>%
    bsplus::bs_append(content = bsplus::bs_carousel_image(src = "www/crop_by_row_2000.png"))
}

#' @noRd
add_help_text <- function(x, ...){
  bsplus::shinyInput_label_embed(x,
    bsplus::shiny_iconlink() %>%
      bsplus::bs_embed_tooltip(
        ..., placement = "left"
      )
  )
}

#' @noRd
mytheme <- fresh::create_theme(
  fresh::adminlte_color(
    # green = "#69A897"
    # green = "#B3C0A4"
    # green = "#a5c3a7"
    green = "#6FB58F"
  ),
  fresh::adminlte_sidebar(
    # width = "400px",
    dark_bg = "#343A40",
    dark_hover_bg = "#494E53",
    dark_hover_color = "#FFFFFF",
    dark_color = "#C2C7D0"
  ),
  fresh::adminlte_global(
    content_bg = "#ECF0F5",
    box_bg = "#FFFFFF",
    info_box_bg = "#FFFFFF"
  )
)
