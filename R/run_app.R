#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom grDevices boxplot.stats
#' @importFrom golem with_golem_options
#' @importFrom shiny shinyApp eventReactive fileInput fluidRow icon isTruthy need numericInput observe observeEvent reactive renderUI req uiOutput validate column tagList textInput NS moduleServer downloadHandler
#' @importFrom bsplus bs_accordion bs_append bs_carousel bs_carousel_image bs_embed_tooltip bs_set_data bs_set_opts shiny_iconlink shinyInput_label_embed use_bs_popover use_bs_tooltip
#' @importFrom config get
#' @importFrom dplyr across anti_join arrange bind_cols bind_rows case_match case_when contains count cur_group_id desc ends_with everything expr filter group_by inner_join last_col left_join matches mutate mutate_all mutate_at mutate_if n pull relocate rename rename_all rename_at rename_if sample_n select slice slice_min slice_sample starts_with summarise summarise_at sym syms tally ungroup vars
#' @importFrom flexlsx wb_add_flextable
#' @importFrom flextable align as_paragraph autofit bg flextable footnote italic merge_h_range merge_v separate_header set_flextable_defaults theme_box valign
#' @importFrom fresh adminlte_color adminlte_global adminlte_sidebar create_theme use_theme
#' @importFrom ftExtra colformat_md
#' @importFrom igraph components graph_from_adjacency_matrix
#' @importFrom janitor adorn_totals clean_names round_half_up
#' @importFrom openxlsx2 create_border create_cell_style create_font read_xlsx wb_add_border wb_add_cell_style wb_add_data wb_add_font wb_add_image wb_add_worksheet wb_color wb_dims wb_merge_cells wb_page_setup wb_save wb_set_cell_style wb_set_col_widths wb_workbook write_xlsx
#' @importFrom osmdata add_osm_feature opq osmdata_sf
#' @importFrom plyr round_any
#' @importFrom purrr map map_chr map_dbl map_dfr map_vec map2 map2_chr map2_dbl pwalk
#' @importFrom sf read_sf st_agr st_area st_as_sf st_as_sfc st_as_text st_bbox st_buffer st_cast st_centroid st_collection_extract st_coordinates st_crop st_crs st_difference st_distance st_drop_geometry st_equals st_geometry st_intersection st_is st_join st_make_valid st_nearest_feature st_point st_set_geometry st_sfc st_transform st_union st_zm write_sf
#' @importFrom shinyEffects setShadow
#' @importFrom shinyWidgets actionBttn awesomeCheckbox downloadBttn dropdownButton materialSwitch numericInputIcon pickerInput pickerOptions prettyRadioButtons radioGroupButtons switchInput updatePickerInput virtualSelectInput
#' @importFrom shinyalert shinyalert
#' @importFrom shinybusy config_report notify_success notify_warning remove_modal_spinner report_failure report_success report_warning show_modal_spinner
#' @importFrom shinydashboard dashboardBody menuItem sidebarMenu tabItem tabItems
#' @importFrom shinydashboardPlus box dashboardHeader dashboardPage dashboardSidebar dashboardUser renderUser socialButton userOutput
#' @importFrom shinyjs disable enable reset useShinyjs
#' @importFrom stats qt sd
#' @importFrom stringi stri_cmp_equiv stri_count_regex stri_detect_regex stri_extract stri_extract_all_regex stri_length stri_locate_all_regex stri_opts_brkiter stri_pad_left stri_replace_all_regex stri_split_regex stri_sub stri_trans_general stri_trans_tolower stri_trans_totitle stri_trans_toupper stri_trim
#' @importFrom terra as.contour crop crs `crs<-` extract minmax rast terrain
#' @importFrom tibble rowid_to_column
#' @importFrom tidyr complete drop_na pivot_longer pivot_wider replace_na unite unnest unnest_legacy
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom units drop_units set_units
#' @importFrom zip zip
#'
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
