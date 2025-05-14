#' bd_flora UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList tags fileInput reactive req observeEvent
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinyalert shinyalert
#' @importFrom dplyr rename_all mutate_at vars matches contains filter select mutate group_by arrange cur_group_id ungroup slice sample_n summarise pull
#' @importFrom stringi stri_trans_totitle stri_trans_general stri_opts_brkiter stri_cmp_equiv stri_detect_regex stri_trans_toupper stri_trans_tolower stri_trim
#' @importFrom sf st_as_sf st_crs st_join st_drop_geometry
#' @importFrom tidyr unite
#' @importFrom purrr map_chr
mod_bd_flora_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Bd flora
    tags$div(
      fileInput(
        inputId = ns("bd_flora"),
        label = "Ingresar BD de parcelas (Solo datos de las parcelas que se desean incluir)",
        multiple = F,
        accept = c(".xlsx"),
        buttonLabel = "Seleccionar",
        placeholder = "Archivo no seleccionado"
      ) %>%
        add_help_text(
          title = "Campos minimos requeridos:\n
                'Parcela', 'UTM_E', 'UTM_N', 'Especie', 'Copa_NS', 'Copa_EO', 'Habito'"
        )
    ),
    tags$div(style = "margin-top: -10px"),
    shinyWidgets::materialSwitch(
      inputId = ns("add_parcelas"),
      label = "¿Crear capa de parcelas?",
      status = "success"
    ),
    tags$div(style = "margin-top: -10px"),
    tags$hr()
  )
}

#' bd_flora Server Functions
#'
#' @noRd
mod_bd_flora_server <- function(id, rodales_def){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    reactive({
      req(c(input$bd_flora$datapath, rodales_def))

      read_xlsx(input$bd_flora$datapath) %>%
        dplyr::rename_all(~stringi::stri_trans_totitle(stringi::stri_trans_general(.,"Latin-ASCII"), opts_brkiter = stringi::stri_opts_brkiter(type = "sentence"))) %>%
        dplyr::rename_if(names(.) %>% stringi::stri_cmp_equiv("cob_bb", strength = 1), ~ "Cob_BB") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("p500", case_insensitive = T), ~ "N_ind") %>%
        dplyr::rename_if(names(.) %>% stringi::stri_detect_regex("ds.*68", case_insensitive = T), ~ "DS_68") %>%
        dplyr::rename_at(dplyr::vars(dplyr::matches("^rce"), dplyr::contains("UTM"), dplyr::matches("ds_68")), stringi::stri_trans_toupper) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::contains("Cob_BB")), ~stringi::stri_trim(stringi::stri_trans_tolower(.))) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::matches("Especie")), ~stringi::stri_trim(stringi::stri_trans_totitle(., opts_brkiter = stringi::stri_opts_brkiter(type = "sentence")))) %>%
        dplyr::mutate_at("N_ind", as.integer) %>%
        {if (PAS == 148) {
          .[] %>%
            dplyr::filter(
              Habito %>% stringi::stri_trans_general("Latin-ASCII") %>% stringi::stri_detect_regex("arbol", case_insensitive = T),
              !Cob_BB %>% stringi::stri_trans_tolower() %in% c(NA_character_, "fp", "---"),
              !N_ind %in% c(NA, 0)
            )
        } else {
          .[] %>% dplyr::filter(
            DS_68 %>% stringi::stri_cmp_equiv("originaria", strength = 1),
            !Cob_BB %>% stringi::stri_trans_tolower() %in% c(NA_character_, "fp", "---"),
            !N_ind %in% c(NA, 0)
          )
        }} %>%
        dplyr::select(-dplyr::matches("Nom_Predio|N_Rodal|Tipo_veg|Tipo_For|Subtipo_fo")) %>%
        sf::st_as_sf(coords = c("UTM_E","UTM_N"), crs = sf::st_crs(rodales_def), remove = F) %>%
        sf::st_join(rodales_def %>% dplyr::select(Nom_Predio, N_Rodal, Tipo_For, Subtipo_fo, Tipo_veg)) %>%
        dplyr::mutate_at("N_Rodal", as.integer) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate_at("N_ind", as.integer) %>%
        dplyr::mutate(Nha = N_ind * 20) %>%
        dplyr::arrange(N_Rodal) %>%
        dplyr::group_by(Parcela, UTM_E, UTM_N) %>%
        dplyr::arrange(N_Rodal) %>%
        dplyr::mutate(N = dplyr::cur_group_id()) %>%
        dplyr::group_by(N_Rodal, N) %>%
        dplyr::mutate(N_Parc = dplyr::cur_group_id()) %>%
        dplyr::ungroup()
    })
  })
}

## To be copied in the UI
# mod_bd_flora_ui("bd_flora_1")

## To be copied in the server
# mod_bd_flora_server("bd_flora_1")
