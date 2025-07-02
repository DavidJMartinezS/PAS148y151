#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom golem with_golem_options
#' @importFrom shiny shinyApp
#' @importFrom bsplus
#' @importFrom dplyr
#' @importFrom shinyjs
#' @importFrom flexlsx
#' @importFrom flextable
#' @importFrom fresh
#' @importFrom ftExtra
#' @importFrom igraph
#' @importFrom janitor
#' @importFrom openxlsx2
#' @importFrom osmdata
#' @importFrom plyr
#' @importFrom purrr
#' @importFrom sf
#' @importFrom shinyalert
#' @importFrom shinybusy
#' @importFrom shinydashboard
#' @importFrom shinydashboardPlus
#' @importFrom shinyEffects
#' @importFrom shinyjs
#' @importFrom shinyWidgets
#' @importFrom stats
#' @importFrom stringi
#' @importFrom terra
#' @importFrom tibble
#' @importFrom tidyr
#' @importFrom tools
#' @importFrom units
#' @importFrom zip
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
