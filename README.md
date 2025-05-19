
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{PAS148y151}`

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Installation

You can install the development version of `{PAS148y151}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
PAS148y151::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-05-18 13:34:28 -04"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> Registered S3 method overwritten by 'ftExtra':
#>   method                  from     
#>   as_flextable.data.frame flextable
#> ℹ Loading PAS148y151
#> Warning in CPL_read_ogr(dsn, layer, query, as.character(options), quiet, : GDAL
#> Message 1: organizePolygons() received a polygon with more than 100 parts.  The
#> processing may be really slow.  You can skip the processing by setting
#> METHOD=SKIP.
#> ── R CMD check results ────────────────────────────── PAS148y151 0.0.0.9000 ────
#> Duration: 2m 10.1s
#> 
#> ❯ checking R files for non-ASCII characters ... WARNING
#>   Found the following files with non-ASCII characters:
#>     Portadas.R
#>     app_server.R
#>     app_ui.R
#>     carto_digital.R
#>     check_bd.R
#>     check_carto_fun.R
#>     get_apendices.R
#>     get_tabla_attr_rodal.R
#>     mod_PredRodArea.R
#>     mod_add_attr.R
#>     mod_apendices.R
#>     mod_bd_flora.R
#>     mod_check_carto.R
#>     mod_get_carto_digital.R
#>     mod_st_order.R
#>     utils_ui.R
#>   Portable packages must use only ASCII characters in their R code,
#>   except perhaps in comments.
#>   Use \uxxxx escapes for other characters.
#> 
#> ❯ checking package dependencies ... NOTE
#>   Imports includes 32 non-default packages.
#>   Importing from so many packages makes the package vulnerable to any of
#>   them becoming unavailable.  Move as many as possible to Suggests and
#>   use conditionally.
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespace in Imports field not imported from: 'stringr'
#>     All declared Imports should be used.
#> 
#> ❯ checking R code for possible problems ... [23s] NOTE
#>   apendice_2_3: no visible binding for global variable '.'
#>   apendice_2_3: no visible binding for global variable 'Nom_Predio'
#>   apendice_2_3: no visible binding for global variable 'N_Rodal'
#>   apendice_2_3: no visible binding for global variable 'Tipo_veg'
#>   apendice_2_3: no visible binding for global variable 'N_Parc'
#>   apendice_2_3: no visible binding for global variable 'UTM_E'
#>   apendice_2_3: no visible binding for global variable 'UTM_N'
#>   apendice_2_3: no visible binding for global variable 'Coord_X'
#>   apendice_2_3: no visible binding for global variable 'Coord_Y'
#>   apendice_2_3: no visible binding for global variable 'Fuente'
#>   apendice_2_3: no visible binding for global variable 'Campana'
#>   apendice_2_3: no visible binding for global variable 'Parcela'
#>   apendice_2_3: no visible binding for global variable 'Copa_NS'
#>   apendice_2_3: no visible binding for global variable 'Copa_EO'
#>   apendice_2_3: no visible binding for global variable 'Especie'
#>   apendice_2_3: no visible binding for global variable 'Cob_arb'
#>   apendice_2_3: no visible binding for global variable 'Cob_parc_p'
#>   apendice_2_3: no visible binding for global variable 'Nha'
#>   apendice_2_3: no visible binding for global variable 'Cob_parc'
#>   apendice_2_3: no visible binding for global variable 'Habito'
#>   apendice_2_3: no visible binding for global variable 'N_Predio'
#>   apendice_5_PAS148: no visible binding for global variable '.'
#>   apendice_5_PAS148: no visible binding for global variable 'N_Predio'
#>   apendice_5_PAS148: no visible binding for global variable 'Nom_Predio'
#>   apendice_5_PAS148: no visible binding for global variable 'Rol'
#>   apendice_5_PAS148: no visible binding for global variable 'Propietari'
#>   apendice_5_PAS148: no visible binding for global variable 'Comuna'
#>   apendice_5_PAS148: no visible binding for global variable 'Provincia'
#>   apendice_5_PAS148: no visible binding for global variable 'Region'
#>   apendice_5_PAS148: no visible binding for global variable
#>     'Titulo_dominio'
#>   apendice_5_PAS148: no visible binding for global variable 'SII'
#>   apendice_5_PAS148: no visible binding for global variable 'Sup_ha'
#>   apendice_5_PAS148: no visible binding for global variable 'predios'
#>   apendice_5_PAS148: no visible binding for global variable 'Uso_Actual'
#>   apendice_5_PAS148: no visible binding for global variable 'Total'
#>   apendice_5_PAS148: no visible binding for global variable 'N_Area'
#>   apendice_5_PAS148: no visible binding for global variable 'Clase_Uso'
#>   apendice_5_PAS148: no visible binding for global variable 'Pend_media'
#>   apendice_5_PAS148: no visible binding for global variable
#>     'Nombre_curso'
#>   apendice_5_PAS148: no visible binding for global variable 'Tipo_Perma'
#>   apendice_5_PAS148: no visible binding for global variable 'Distancia'
#>   apendice_5_PAS148: no visible binding for global variable 'Ancho'
#>   apendice_5_PAS148: no visible binding for global variable 'Tipo_veg'
#>   apendice_5_PAS148: no visible binding for global variable 'N_Parc'
#>   apendice_5_PAS148: no visible binding for global variable 'Especie'
#>   apendice_5_PAS148: no visible binding for global variable 'Nha'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'Tipo_veg'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'Especie'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'Nha'
#>   apendice_5_PAS148: no visible binding for global variable 'Nom_attr'
#>   apendice_5_PAS148: no visible binding for global variable 'Parcelas'
#>   apendice_5_PAS148: no visible binding for global variable 'nha_x_sp'
#>   apendice_5_PAS148: no visible binding for global variable 'N_Rodal'
#>   apendice_5_PAS148: no visible binding for global variable 'Tipo_attr'
#>   apendice_5_PAS148: no visible binding for global variable 'Subtipo_fo'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'stringi'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     '.'
#>   apendice_5_PAS148: no visible binding for global variable 'sp'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'N_Rodal'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'Subtipo_fo'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'Tipo'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'Tipo_attr'
#>   apendice_5_PAS148 : <anonymous>: no visible binding for global variable
#>     'N_Parc'
#>   apendice_5_PAS148: no visible binding for global variable 'Tipo_fores'
#>   apendice_5_PAS148: no visible binding for global variable 'Estructura'
#>   apendice_5_PAS148: no visible binding for global variable
#>     'Estado_desarrollo'
#>   apendice_5_PAS148: no visible binding for global variable
#>     'Estado_fitosanitario'
#>   apendice_5_PAS148: no visible binding for global variable 'ID'
#>   apendice_5_PAS148: no visible binding for global variable 'Ano'
#>   apendice_5_PAS148: no visible binding for global variable 'areas'
#>   apendice_5_PAS148: no visible binding for global variable 'Tipo'
#>   apendice_5_PAS148: no visible binding for global variable 'Obra'
#>   apendice_5_PAS148: no visible binding for global variable 'geometry'
#>   apendice_5_PAS148: no visible binding for global variable 'ha'
#>   apendice_5_PAS148: no visible binding for global variable 'Parcela'
#>   apendice_5_PAS148: no visible binding for global variable 'UTM_E'
#>   apendice_5_PAS148: no visible binding for global variable 'UTM_N'
#>   apendice_5_PAS148: no visible binding for global variable 'Rodales'
#>   apendice_5_PAS148: no visible binding for global variable
#>     'cuasivarianza'
#>   apendice_5_PAS148: no visible binding for global variable 'Promedio'
#>   apendice_5_PAS148: no visible binding for global variable 'T_est'
#>   apendice_5_PAS148: no visible binding for global variable 'E_abs'
#>   apendice_5_PAS148: no visible binding for global variable
#>     'Nombre_cientifico'
#>   apendice_5_PAS148: no visible binding for global variable 'Categoria'
#>   apendice_5_PAS148: no visible binding for global variable 'Decreto'
#>   apendice_5_PAS151: no visible binding for global variable '.'
#>   apendice_5_PAS151: no visible binding for global variable 'N_Predio'
#>   apendice_5_PAS151: no visible binding for global variable 'Nom_Predio'
#>   apendice_5_PAS151: no visible binding for global variable 'Rol'
#>   apendice_5_PAS151: no visible binding for global variable 'Propietari'
#>   apendice_5_PAS151: no visible binding for global variable 'Comuna'
#>   apendice_5_PAS151: no visible binding for global variable 'Provincia'
#>   apendice_5_PAS151: no visible binding for global variable 'Region'
#>   apendice_5_PAS151: no visible binding for global variable
#>     'Titulo_dominio'
#>   apendice_5_PAS151: no visible binding for global variable 'SII'
#>   apendice_5_PAS151: no visible binding for global variable 'Sup_ha'
#>   apendice_5_PAS151: no visible binding for global variable 'N_Area'
#>   apendice_5_PAS151: no visible binding for global variable 'Ran_Pend'
#>   apendice_5_PAS151: no visible binding for global variable 'SG_SI'
#>   apendice_5_PAS151: no visible binding for global variable 'SG_NO'
#>   apendice_5_PAS151: no visible binding for global variable 'Clase_Eros'
#>   apendice_5_PAS151: no visible binding for global variable 'Distancia'
#>   apendice_5_PAS151: no visible binding for global variable
#>     'Dist_Area_Prot'
#>   apendice_5_PAS151: no visible binding for global variable 'Tipo_veg'
#>   apendice_5_PAS151: no visible binding for global variable 'N_Parc'
#>   apendice_5_PAS151: no visible binding for global variable 'Especie'
#>   apendice_5_PAS151: no visible binding for global variable 'Nha'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     'Tipo_veg'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     'Especie'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     'Nha'
#>   apendice_5_PAS151: no visible binding for global variable 'Habito'
#>   apendice_5_PAS151: no visible binding for global variable 'Cob_BB'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     'Cob_BB'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     'Habito'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     'Cob_ind'
#>   apendice_5_PAS151: no visible binding for global variable 'Nom_attr'
#>   apendice_5_PAS151: no visible binding for global variable 'Parcelas'
#>   apendice_5_PAS151: no visible binding for global variable 'N_Rodal'
#>   apendice_5_PAS151: no visible binding for global variable 'nha_x_sp'
#>   apendice_5_PAS151: no visible binding for global variable 'cob_x_sp'
#>   apendice_5_PAS151: no visible binding for global variable 'Tipo_attr'
#>   apendice_5_PAS151: no visible binding for global variable 'Cob_ind'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     'stringi'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     '.'
#>   apendice_5_PAS151: no visible binding for global variable 'sp'
#>   apendice_5_PAS151 : <anonymous>: no visible binding for global variable
#>     'N_Parc'
#>   apendice_5_PAS151: no visible binding for global variable 'bd1'
#>   apendice_5_PAS151 : obs_fun: no visible binding for global variable '.'
#>   apendice_5_PAS151 : obs_fun: no visible global function definition for
#>     'pasteo'
#>   apendice_5_PAS151: no visible binding for global variable 'Ano'
#>   apendice_5_PAS151: no visible binding for global variable 'Tipo'
#>   apendice_5_PAS151: no visible binding for global variable 'Descripcion'
#>   apendice_5_PAS151: no visible binding for global variable 'Cob_final'
#>   apendice_5_PAS151: no visible binding for global variable 'areas'
#>   apendice_5_PAS151: no visible binding for global variable 'Obra'
#>   apendice_5_PAS151: no visible binding for global variable 'geometry'
#>   apendice_5_PAS151: no visible binding for global variable 'ha'
#>   apendice_5_PAS151: no visible binding for global variable 'Parcela'
#>   apendice_5_PAS151: no visible binding for global variable 'UTM_E'
#>   apendice_5_PAS151: no visible binding for global variable 'UTM_N'
#>   apendice_5_PAS151: no visible binding for global variable 'Rodales'
#>   apendice_5_PAS151: no visible binding for global variable
#>     'cuasivarianza'
#>   apendice_5_PAS151: no visible binding for global variable 'Promedio'
#>   apendice_5_PAS151: no visible binding for global variable 'T_est'
#>   apendice_5_PAS151: no visible binding for global variable 'E_abs'
#>   app_server : <anonymous>: no visible binding for global variable '.'
#>   app_server: warning in check_input(x = LB(), names_req =
#>     c("Tipo_fores", "Subtipo_fo", "Tipo_veg", "Regulacion"), huso =
#>     input$huso, id = "linea_base-sf_file"): argumentos parcialmente
#>     correctos de 'id' a 'id_reset'
#>   app_server: warning in check_input(x = obras(), names_req = NULL, huso
#>     = input$huso, id = "obras-sf_file"): argumentos parcialmente
#>     correctos de 'id' a 'id_reset'
#>   app_server: warning in check_input(x = predios(), names_req =
#>     c("N_Predio", "Nom_Predio", "Rol", "Propietari"), huso = input$huso,
#>     id = "predios-sf_file"): argumentos parcialmente correctos de 'id' a
#>     'id_reset'
#>   app_server: warning in check_input(x = suelos(), names_req =
#>     c("Clase_Uso"), huso = input$huso, id = "suelos-sf_file"): argumentos
#>     parcialmente correctos de 'id' a 'id_reset'
#>   app_server: warning in check_input(x = suelos(), names_req =
#>     ("Clase_Eros"), huso = input$huso, id = "suelos-sf_file"): argumentos
#>     parcialmente correctos de 'id' a 'id_reset'
#>   app_server: no visible binding for global variable 'N_Rodal'
#>   app_server: no visible binding for global variable '.'
#>   app_server: no visible binding for global variable 'Sup_ha'
#>   app_server: warning in check_input(x = areas_def(), names_req =
#>     c("Nom_Predio", "N_Area", "Clase_Uso"), huso = input$huso, id =
#>     "cart_area-sf_file"): argumentos parcialmente correctos de 'id' a
#>     'id_reset'
#>   app_server: warning in check_input(x = rodales_def(), names_req =
#>     c("N_Rodal", "Tipo_For"), huso = input$huso, id =
#>     "cart_rodales-sf_file"): argumentos parcialmente correctos de 'id' a
#>     'id_reset'
#>   app_server: warning in check_input(x = predios_def(), names_req =
#>     c("N_Predio", "Nom_Predio", "Rol", "Propietari"), huso = input$huso,
#>     id = "cart_predios-sf_file"): argumentos parcialmente correctos de
#>     'id' a 'id_reset'
#>   app_server: no visible binding for global variable 'Habito'
#>   app_server: no visible binding for global variable 'Cob_BB'
#>   app_server: no visible binding for global variable 'N_ind'
#>   app_server: no visible binding for global variable 'DS_68'
#>   app_server: no visible binding for global variable 'Nom_Predio'
#>   app_server: no visible binding for global variable 'Tipo_For'
#>   app_server: no visible binding for global variable 'Subtipo_fo'
#>   app_server: no visible binding for global variable 'Tipo_veg'
#>   app_server: no visible binding for global variable 'Parcela'
#>   app_server: no visible binding for global variable 'UTM_E'
#>   app_server: no visible binding for global variable 'UTM_N'
#>   app_server: no visible binding for global variable 'N'
#>   app_server: no visible binding for global variable 'RCE'
#>   app_server: no visible binding for global variable 'Especie'
#>   app_server: no visible binding for global variable 'Parcelas'
#>   app_server: no visible binding for global variable 'SP'
#>   app_server: warning in check_input(x = catastro(), names_req = c("USO",
#>     "SUBUSO", "ESTRUCTURA"), huso = input$huso, id = "catastro-sf_file"):
#>     argumentos parcialmente correctos de 'id' a 'id_reset'
#>   app_server: warning in check_input(x = suelos_uso_act(), names_req =
#>     c("Clase_Uso"), huso = input$huso, id = "suelos_uso_act-sf_file"):
#>     argumentos parcialmente correctos de 'id' a 'id_reset'
#>   app_server: no visible global function definition for 'str_c'
#>   app_server: warning in check_input(x = bd_fauna(), names_req =
#>     c("Nombre_cientifico", "UTM_E", "UTM_N", "Categoria", "Decreto"), id
#>     = "bd_fauna"): argumentos parcialmente correctos de 'id' a 'id_reset'
#>   app_server: no visible binding for global variable 'carto'
#>   cart_area: no visible binding for global variable 'geometry'
#>   cart_area: no visible binding for global variable 'ha'
#>   cart_area: no visible binding for global variable 'Nom_Predio'
#>   cart_area: no visible binding for global variable 'N_Area'
#>   cart_area: no visible binding for global variable 'Tipo_Bos'
#>   cart_area: no visible binding for global variable 'Sup_ha'
#>   cart_area: no visible binding for global variable 'Fuente'
#>   cart_caminos: no visible binding for global variable '.'
#>   cart_caminos: no visible global function definition for 'str_detect'
#>   cart_caminos: no visible binding for global variable 'CLASIFICACION'
#>   cart_caminos: no visible binding for global variable 'Nom_Predio'
#>   cart_caminos: no visible binding for global variable 'Tipo_Cam'
#>   cart_caminos: no visible binding for global variable 'Fuente'
#>   cart_caminos_osm: no visible binding for global variable '.'
#>   cart_caminos_osm: no visible binding for global variable 'osmdata'
#>   cart_caminos_osm: no visible binding for global variable 'Nom_Predio'
#>   cart_caminos_osm: no visible binding for global variable 'name'
#>   cart_caminos_osm: no visible binding for global variable 'Tipo_Cam'
#>   cart_caminos_osm: no visible binding for global variable 'Fuente'
#>   cart_caminos_osm: no visible binding for global variable 'Etiqueta'
#>   cart_caminos_osm: no visible binding for global variable 'highway'
#>   cart_curv_niv: no visible binding for global variable '.'
#>   cart_curv_niv: no visible binding for global variable 'Nom_Predio'
#>   cart_curv_niv: no visible binding for global variable 'Cot_Curva'
#>   cart_curv_niv: no visible binding for global variable 'Fuente'
#>   cart_hidro: no visible binding for global variable '.'
#>   cart_hidro: no visible binding for global variable 'strahler_n'
#>   cart_hidro: no visible binding for global variable 'nombre'
#>   cart_hidro: no visible binding for global variable 'Nom_Predio'
#>   cart_hidro: no visible binding for global variable 'Tip_Dren'
#>   cart_hidro: no visible binding for global variable 'Tipo_Perma'
#>   cart_hidro: no visible binding for global variable 'Fuente'
#>   cart_hidro: no visible binding for global variable 'Etiqueta'
#>   cart_hidro_osm: no visible binding for global variable '.'
#>   cart_hidro_osm: no visible binding for global variable 'Nom_Predio'
#>   cart_hidro_osm: no visible binding for global variable 'name'
#>   cart_hidro_osm: no visible binding for global variable 'Tip_Dren'
#>   cart_hidro_osm: no visible binding for global variable 'Tipo_Perma'
#>   cart_hidro_osm: no visible binding for global variable 'Fuente'
#>   cart_hidro_osm: no visible binding for global variable 'Etiqueta'
#>   cart_hidro_osm: no visible binding for global variable 'waterway'
#>   cart_parcelas: no visible binding for global variable '.'
#>   cart_parcelas: no visible binding for global variable 'Habito'
#>   cart_parcelas: no visible binding for global variable 'Cob_BB'
#>   cart_parcelas: no visible binding for global variable 'N_ind'
#>   cart_parcelas: no visible binding for global variable 'DS_68'
#>   cart_parcelas: no visible binding for global variable 'Nom_Predio'
#>   cart_parcelas: no visible binding for global variable 'N_Rodal'
#>   cart_parcelas: no visible binding for global variable 'Parcela'
#>   cart_parcelas: no visible binding for global variable 'UTM_E'
#>   cart_parcelas: no visible binding for global variable 'UTM_N'
#>   cart_parcelas: no visible binding for global variable 'N'
#>   cart_parcelas: no visible binding for global variable 'N_Parc'
#>   cart_parcelas: no visible binding for global variable 'Coord_X'
#>   cart_parcelas: no visible binding for global variable 'Coord_Y'
#>   cart_parcelas: no visible binding for global variable 'Fuente'
#>   cart_predios: no visible binding for global variable 'geometry'
#>   cart_predios: no visible binding for global variable 'ha'
#>   cart_predios: no visible binding for global variable 'Nom_Predio'
#>   cart_predios: no visible binding for global variable 'Rol'
#>   cart_predios: no visible binding for global variable 'Propietari'
#>   cart_predios: no visible binding for global variable 'Sup_ha'
#>   cart_rang_pend: no visible binding for global variable 'geometry'
#>   cart_rang_pend: no visible binding for global variable 'ha'
#>   cart_rang_pend: no visible binding for global variable '.'
#>   cart_rang_pend: no visible binding for global variable 'Pend_media'
#>   cart_rang_pend: no visible binding for global variable 'Nom_Predio'
#>   cart_rang_pend: no visible binding for global variable 'Ran_Pend'
#>   cart_rang_pend: no visible binding for global variable 'Sup_ha'
#>   cart_rodales: no visible binding for global variable '.'
#>   cart_rodales: no visible binding for global variable 'geometry'
#>   cart_rodales: no visible binding for global variable 'ha'
#>   cart_rodales: no visible binding for global variable 'Nom_Predio'
#>   cart_rodales: no visible binding for global variable 'N_Rodal'
#>   cart_rodales: no visible binding for global variable 'Tipo_Bos'
#>   cart_rodales: no visible binding for global variable 'Tipo_For'
#>   cart_rodales: no visible binding for global variable 'Sup_ha'
#>   cart_rodales: no visible binding for global variable 'Fuente'
#>   cart_suelos: no visible binding for global variable 'geometry'
#>   cart_suelos: no visible binding for global variable 'ha'
#>   cart_suelos: no visible binding for global variable '.'
#>   cart_suelos: no visible binding for global variable 'Nom_Predio'
#>   cart_suelos: no visible binding for global variable 'Sup_ha'
#>   cart_suelos: no visible binding for global variable 'Fuente'
#>   cart_uso_actual: no visible binding for global variable 'Nom_Predio'
#>   cart_uso_actual: no visible binding for global variable 'Clase_Uso'
#>   cart_uso_actual: no visible binding for global variable 'USO'
#>   cart_uso_actual: no visible binding for global variable 'SUBUSO'
#>   cart_uso_actual: no visible binding for global variable 'ESTRUCTURA'
#>   cart_uso_actual: no visible binding for global variable 'Uso_Actual'
#>   cart_uso_actual: no visible binding for global variable 'Fuente'
#>   cart_uso_actual: no visible binding for global variable 'geometry'
#>   cart_uso_actual: no visible binding for global variable 'ha'
#>   cart_uso_actual: no visible binding for global variable 'Sup_ha'
#>   check_bd_flora: no visible binding for global variable '.'
#>   check_bd_flora: no visible binding for global variable 'Campana'
#>   check_bd_flora: no visible binding for global variable 'Cuadrilla'
#>   check_bd_flora: no visible binding for global variable 'Parcela'
#>   check_bd_flora: no visible binding for global variable 'UTM_E'
#>   check_bd_flora: no visible binding for global variable 'UTM_N'
#>   check_bd_flora: no visible global function definition for 'str_c'
#>   check_input: no visible binding for global variable '.'
#>   cob_x_sp_fun: no visible binding for global variable 'N_Parc'
#>   cob_x_sp_fun: no visible binding for global variable 'Especie'
#>   cob_x_sp_fun: no visible binding for global variable 'Habito'
#>   cob_x_sp_fun: no visible binding for global variable 'Cob_BB'
#>   cob_x_sp_fun: no visible binding for global variable 'Cob_ind'
#>   get_carto_digital: no visible binding for global variable 'N_Rodal'
#>   get_carto_digital: no visible binding for global variable '.'
#>   get_carto_digital: no visible binding for global variable 'N_Predio'
#>   get_carto_digital: no visible binding for global variable 'geometry'
#>   get_carto_digital: no visible binding for global variable 'ha'
#>   get_carto_digital: no visible binding for global variable 'Nom_Predio'
#>   get_carto_digital: no visible binding for global variable 'Rol'
#>   get_carto_digital: no visible binding for global variable 'Propietari'
#>   get_carto_digital: no visible binding for global variable 'Sup_ha'
#>   get_carto_digital: no visible binding for global variable 'Sup_ind'
#>   get_carto_digital: no visible binding for global variable 'Sup_prop'
#>   get_carto_digital: no visible binding for global variable 'COMUNA'
#>   get_carto_digital: no visible binding for global variable 'PROVINCIA'
#>   get_carto_digital: no visible binding for global variable 'REGION'
#>   get_carto_digital: no visible binding for global variable 'N_Area'
#>   get_carto_digital: no visible binding for global variable 'Pend_media'
#>   get_carto_digital: no visible binding for global variable 'Ran_Pend'
#>   get_carto_digital: no visible binding for global variable
#>     'Nombre_curso'
#>   get_carto_digital: no visible binding for global variable 'Clase_Eros'
#>   get_pred_rod_area: no visible binding for global variable 'PROVINCIA'
#>   get_pred_rod_area: no visible binding for global variable '.'
#>   get_pred_rod_area: no visible binding for global variable 'Regulacion'
#>   get_pred_rod_area: no visible binding for global variable 'N_Predio'
#>   get_pred_rod_area: no visible binding for global variable 'Nom_Predio'
#>   get_pred_rod_area: no visible binding for global variable 'geometry'
#>   get_pred_rod_area: no visible binding for global variable 'st_relate'
#>   get_pred_rod_area: no visible binding for global variable 'PID'
#>   get_pred_rod_area: no visible binding for global variable 'Tipo_fores'
#>   get_pred_rod_area: no visible binding for global variable 'Subtipo_fo'
#>   get_pred_rod_area: no visible binding for global variable 'Tipo_veg'
#>   get_pred_rod_area: no visible binding for global variable 'ha'
#>   get_pred_rod_area: no visible binding for global variable 'N_Rodal'
#>   get_pred_rod_area: no visible binding for global variable 'Tipo_Bos'
#>   get_pred_rod_area: no visible binding for global variable 'Tipo_For'
#>   get_pred_rod_area: no visible binding for global variable 'Sup_ha'
#>   get_pred_rod_area: no visible binding for global variable 'N_Pred_ori'
#>   get_pred_rod_area: no visible binding for global variable 'N_Predio2'
#>   get_pred_rod_area: no visible binding for global variable 'group'
#>   get_pred_rod_area: no visible binding for global variable 'N_r'
#>   get_pred_rod_area: no visible binding for global variable 'N_a'
#>   get_pred_rod_area: no visible binding for global variable 'N_Area'
#>   get_pred_rod_area: no visible binding for global variable 'Sup_m2'
#>   get_pred_rod_area: no visible binding for global variable 'Rol'
#>   get_pred_rod_area: no visible binding for global variable 'Propietari'
#>   get_slope: no visible binding for global variable 'ID'
#>   get_slope: no visible binding for global variable 'slope'
#>   get_tabla_attr_rodal: no visible binding for global variable
#>     'Nom_Predio'
#>   get_tabla_attr_rodal: no visible binding for global variable 'N_Rodal'
#>   get_tabla_attr_rodal: no visible binding for global variable 'Parcela'
#>   get_tabla_attr_rodal: no visible binding for global variable 'N_Parc'
#>   get_tabla_attr_rodal: no visible binding for global variable 'UTM_E'
#>   get_tabla_attr_rodal: no visible binding for global variable 'UTM_N'
#>   get_tabla_attr_rodal: no visible binding for global variable 'Tipo_veg'
#>   get_tabla_attr_rodal: no visible binding for global variable 'Nha'
#>   get_tabla_attr_rodal: no visible binding for global variable '.'
#>   get_tabla_attr_rodal: no visible binding for global variable 'Especie'
#>   get_tabla_attr_rodal : <anonymous>: no visible binding for global
#>     variable 'Tipo_veg'
#>   get_tabla_attr_rodal : <anonymous>: no visible binding for global
#>     variable 'Especie'
#>   get_tabla_attr_rodal : <anonymous>: no visible binding for global
#>     variable 'Nha'
#>   get_tabla_attr_rodal: no visible binding for global variable 'geometry'
#>   get_tabla_attr_rodal: no visible binding for global variable 'Parcelas'
#>   get_tabla_attr_rodal: no visible binding for global variable 'NHA'
#>   italic_sp : split_de: no visible global function definition for 'tail'
#>   mod_PredRodArea_server : <anonymous> : <anonymous>: no visible binding
#>     for global variable '.'
#>   mod_PredRodArea_server : <anonymous>: warning in check_input(x = LB(),
#>     names_req = c("Tipo_fores", "Subtipo_fo", "Tipo_veg", "Regulacion"),
#>     huso = huso, id = "linea_base-sf_file"): argumentos parcialmente
#>     correctos de 'id' a 'id_reset'
#>   mod_PredRodArea_server : <anonymous>: warning in check_input(x =
#>     obras(), names_req = NULL, huso = huso, id = "obras-sf_file"):
#>     argumentos parcialmente correctos de 'id' a 'id_reset'
#>   mod_PredRodArea_server : <anonymous>: warning in check_input(x =
#>     predios(), names_req = c("N_Predio", "Nom_Predio", "Rol",
#>     "Propietari"), huso = huso, id = "predios-sf_file"): argumentos
#>     parcialmente correctos de 'id' a 'id_reset'
#>   mod_PredRodArea_server : <anonymous>: warning in check_input(x =
#>     suelos(), names_req = c("Clase_Uso"), huso = huso, id =
#>     "suelos-sf_file"): argumentos parcialmente correctos de 'id' a
#>     'id_reset'
#>   mod_PredRodArea_server : <anonymous>: warning in check_input(x =
#>     suelos(), names_req = ("Clase_Eros"), huso = huso, id =
#>     "suelos-sf_file"): argumentos parcialmente correctos de 'id' a
#>     'id_reset'
#>   mod_PredRodArea_server : <anonymous>: no visible binding for global
#>     variable 'N_Rodal'
#>   mod_PredRodArea_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_PredRodArea_server : <anonymous>: no visible binding for global
#>     variable 'Sup_ha'
#>   mod_add_attr_server : <anonymous>: no visible binding for global
#>     variable 'shinyWidgets'
#>   mod_add_attr_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_apendices_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_apendices_server : <anonymous>: no visible global function
#>     definition for 'shinyalerta'
#>   mod_apendices_server : <anonymous>: warning in check_input(x =
#>     bd_fauna(), names_req = c("Nombre_cientifico", "UTM_E", "UTM_N",
#>     "Categoria", "Decreto"), id = "bd_fauna"): argumentos parcialmente
#>     correctos de 'id' a 'id_reset'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'PAS'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'Habito'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'Cob_BB'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'N_ind'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'DS_68'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'Nom_Predio'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'N_Rodal'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'Tipo_For'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'Subtipo_fo'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'Tipo_veg'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'Parcela'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'UTM_E'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'UTM_N'
#>   mod_bd_flora_server : <anonymous>: no visible binding for global
#>     variable 'N'
#>   mod_downfiles_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_get_carto_digital_server : <anonymous> : <anonymous>: no visible
#>     binding for global variable '.'
#>   mod_get_carto_digital_server : <anonymous>: warning in check_input(x =
#>     areas_def(), names_req = c("Nom_Predio", "N_Area", "Clase_Uso"), huso
#>     = huso, id = "cart_area-sf_file"): argumentos parcialmente correctos
#>     de 'id' a 'id_reset'
#>   mod_get_carto_digital_server : <anonymous>: warning in check_input(x =
#>     rodales_def(), names_req = c("N_Rodal", "Tipo_For"), huso = huso, id
#>     = "cart_rodales-sf_file"): argumentos parcialmente correctos de 'id'
#>     a 'id_reset'
#>   mod_get_carto_digital_server : <anonymous>: warning in check_input(x =
#>     predios_def(), names_req = c("N_Predio", "Nom_Predio", "Rol",
#>     "Propietari"), huso = huso, id = "cart_predios-sf_file"): argumentos
#>     parcialmente correctos de 'id' a 'id_reset'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable 'PAS'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable '.'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable 'Habito'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable 'RCE'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable 'DS_68'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable 'Especie'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable 'Parcela'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable 'Parcelas'
#>   mod_get_carto_digital_server : <anonymous>: no visible binding for
#>     global variable 'SP'
#>   mod_get_carto_digital_server : <anonymous>: warning in check_input(x =
#>     catastro(), names_req = c("USO", "SUBUSO", "ESTRUCTURA"), huso =
#>     huso, id = "catastro-sf_file"): argumentos parcialmente correctos de
#>     'id' a 'id_reset'
#>   mod_get_carto_digital_server : <anonymous>: warning in check_input(x =
#>     suelos_uso_act(), names_req = c("Clase_Uso"), huso = huso, id =
#>     "suelos_uso_act-sf_file"): argumentos parcialmente correctos de 'id'
#>     a 'id_reset'
#>   mod_get_carto_digital_server : <anonymous>: no visible global function
#>     definition for 'DEM'
#>   mod_get_carto_digital_server : <anonymous>: no visible global function
#>     definition for 'str_c'
#>   mod_leer_sf_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_st_order_server : <anonymous>: no visible binding for global
#>     variable '.'
#>   mod_st_order_server : <anonymous>: no visible binding for global
#>     variable 'geometry'
#>   nha_x_sp_fun: no visible binding for global variable 'N_Parc'
#>   nha_x_sp_fun: no visible binding for global variable 'Especie'
#>   nha_x_sp_fun: no visible binding for global variable 'Nha'
#>   nha_x_sp_fun: no visible global function definition for 'across'
#>   st_order: no visible binding for global variable 'Y'
#>   st_order: no visible binding for global variable 'X'
#>   st_order: no visible binding for global variable 'geometry'
#>   wb_portada_default: no visible binding for global variable
#>     'nom_apendice'
#>   Undefined global functions or variables:
#>     . Ancho Ano CLASIFICACION COMUNA Campana Categoria Clase_Eros
#>     Clase_Uso Cob_BB Cob_arb Cob_final Cob_ind Cob_parc Cob_parc_p Comuna
#>     Coord_X Coord_Y Copa_EO Copa_NS Cot_Curva Cuadrilla DEM DS_68 Decreto
#>     Descripcion Dist_Area_Prot Distancia ESTRUCTURA E_abs Especie
#>     Estado_desarrollo Estado_fitosanitario Estructura Etiqueta Fuente
#>     Habito ID N NHA N_Area N_Parc N_Pred_ori N_Predio N_Predio2 N_Rodal
#>     N_a N_ind N_r Nha Nom_Predio Nom_attr Nombre_cientifico Nombre_curso
#>     Obra PAS PID PROVINCIA Parcela Parcelas Pend_media Promedio
#>     Propietari Provincia RCE REGION Ran_Pend Region Regulacion Rodales
#>     Rol SG_NO SG_SI SII SP SUBUSO Subtipo_fo Sup_ha Sup_ind Sup_m2
#>     Sup_prop T_est Tip_Dren Tipo Tipo_Bos Tipo_Cam Tipo_For Tipo_Perma
#>     Tipo_attr Tipo_fores Tipo_veg Titulo_dominio Total USO UTM_E UTM_N
#>     Uso_Actual X Y across areas bd1 carto cob_x_sp cuasivarianza geometry
#>     group ha highway name nha_x_sp nom_apendice nombre osmdata pasteo
#>     predios shinyWidgets shinyalerta slope sp st_relate str_c str_detect
#>     strahler_n stringi tail waterway
#>   Consider adding
#>     importFrom("utils", "tail")
#>   to your NAMESPACE file.
#>   
#>   Found if() conditions comparing class() to string:
#>   File 'PAS148y151/R/carto_digital.R': if (class(dem) == "character") ...
#>   Use inherits() (or maybe is()) instead.
#> 
#> 0 errors ✔ | 1 warning ✖ | 3 notes ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> PAS148y151 Coverage: 0.00%
#> R/app_config.R: 0.00%
#> R/app_server.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/carto_digital.R: 0.00%
#> R/check_bd.R: 0.00%
#> R/check_carto_fun.R: 0.00%
#> R/get_apendices.R: 0.00%
#> R/get_pred_rod_area.R: 0.00%
#> R/get_tabla_attr_rodal.R: 0.00%
#> R/golem_utils_server.R: 0.00%
#> R/golem_utils_ui.R: 0.00%
#> R/mod_add_attr.R: 0.00%
#> R/mod_apendices.R: 0.00%
#> R/mod_apendices_utils_fun.R: 0.00%
#> R/mod_bd_flora.R: 0.00%
#> R/mod_check_carto.R: 0.00%
#> R/mod_downfiles.R: 0.00%
#> R/mod_get_carto_digital.R: 0.00%
#> R/mod_leer_sf.R: 0.00%
#> R/mod_PredRodArea.R: 0.00%
#> R/mod_st_order.R: 0.00%
#> R/Portadas.R: 0.00%
#> R/run_app.R: 0.00%
#> R/spatial_functions.R: 0.00%
#> R/utils_server.R: 0.00%
#> R/utils_ui.R: 0.00%
```
