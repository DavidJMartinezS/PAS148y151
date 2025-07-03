
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{PAS148y151}` <a href='https://github.com/DavidJMartinezS/PAS148y151'><img src='inst/app/www/favicon.ico' align="right" height="175" /></a>

<!-- badges: start -->

[![Package-License](http://img.shields.io/badge/license-GPL--3-brightgreen.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Este paquete está elaborado con el propósito de ayudar en la elaboración
de los Permisos Ambientales Sectoriales (PAS) 148 y 151, por medio de
una aplicación que facilita la elaboración de la cartografía digital y
los apéndices. El paquete consta principalmente de la aplicación, además
de algunas funciones que son utilizadas en esta misma que podrian
ejecutarse en un entorno de R. Para un uso correcto de la aplicación,
por favor tener en consideración las instrucciones que están en la
primera página de la aplicación.

## Installation

Para la instalación del paquete `{PAS148y151}` debe de ejecutar:

``` r
options(timeout = 600)
remotes::install_github("DavidJMartinezS/dataPAS", dependencies = T, force = T)
remotes::install_github("DavidJMartinezS/PAS148y151", dependencies = T, force = T)
```

## Run

Puede iniciar la aplicación ejecutando:

``` r
PAS148y151::run_app()
```
