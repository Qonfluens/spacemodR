# Hack to get rid of spurious notes in package check, caused by uses
# of dplyr::{rename, filter}.
if (getRversion() >= "4.0.0")  utils::globalVariables(c(
 "trophic_tbl", "x_from", "y_from", "x_to", "y_to", "y",
  "node", "spcmdl_habitat"))

#' The 'spacemodR' package.
#'
#' @description
#' Workflow for Environmental Risk Assessment: Habitat,
#' Food Web, Dispersal, Exposure and Risk. A set of function for
#'
#' @name spacemodR-package
#' @aliases spacemodR
#' @useDynLib spacemodR, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2. https://mc-stan.org
NULL

