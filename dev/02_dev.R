# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "leer_sf") # Name of the module
golem::add_module(name = "downfiles") # Name of the module
golem::add_module(name = "header_config")
golem::add_utils("server")
golem::add_fct("functions")
golem::add_utils("ui")
golem::add_utils(name = "get", module = "PredRodArea")
golem::add_module("PredRodArea")
golem::add_module("st_order")
golem::add_module("check_carto")
golem::add_fct("fun", module = "check_carto")
golem::add_module("add_attr")
golem::add_module("get_carto_digital")
golem::add_module("apendices", utils = "")
golem::add_module("bd_flora")
golem::add_module("tabla_attr_rodal")
golem::add_module("check_input")


usethis::use_pipe(export = T)
usethis::use_package('bsplus')
usethis::use_package('dplyr')
usethis::use_package('dataPAS')
usethis::use_package('flexlsx')
usethis::use_package('flextable')
usethis::use_package('ftExtra')
usethis::use_package('fresh')
usethis::use_package('igraph')
usethis::use_package('janitor')
usethis::use_package('openxlsx2')
usethis::use_package('osmdata')
usethis::use_package('purrr')
usethis::use_package('plyr')
usethis::use_package('sf')
usethis::use_package('shinyalert')
usethis::use_package('shinybusy')
usethis::use_package('shinydashboard')
usethis::use_package('shinydashboardPlus')
usethis::use_package('shinyEffects')
usethis::use_package('shinyjs')
usethis::use_package('shinyWidgets')
usethis::use_package('stringr')
usethis::use_package('stringi')
usethis::use_package('terra')
usethis::use_package('tibble')
usethis::use_package('tidyr')
usethis::use_package('tools')
usethis::use_package('units')
usethis::use_package('zip')

## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("helpers", with_test = F)
golem::add_utils("helpers", with_test = F)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("custom")
golem::add_sass_file("custom")
golem::add_any_file("file.json")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("PAS148y151")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
