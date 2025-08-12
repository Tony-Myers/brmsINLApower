#' brmsINLApower package imports
#'
#' Centralised package-wide imports for brmsINLApower.
#' This file gathers all external functions from other packages
#' used throughout the package code into a single location.
#' Update this if you add or remove dependencies or functions.
#'
#' @keywords internal
#' @name brmsINLApower-imports
#'
#' @importFrom stats gaussian as.formula terms rnorm qbeta dbeta optimize pnorm dnorm
#' @importFrom stats quantile weighted.mean setNames plogis rbinom rpois rbeta rweibull rexp runif
#' @importFrom utils flush.console packageVersion
#' @importFrom magrittr %>%
#' @importFrom rlang .data `:=`
#' @importFrom dplyr bind_rows filter group_by summarise arrange slice mutate select inner_join
#' @importFrom tibble tibble
#' @importFrom ggplot2 scale_fill_viridis_d scale_fill_stepsn scale_fill_viridis_c scale_fill_gradientn
#' @importFrom ggplot2 geom_contour geom_point geom_line geom_tile geom_ribbon geom_contour_filled
#' @importFrom ggplot2 labs aes ggplot theme_minimal scale_y_continuous scale_colour_manual
#' @importFrom scales percent_format
#' @importFrom MASS rnegbin
NULL
