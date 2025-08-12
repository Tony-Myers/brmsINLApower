#' Map a brms Family to an INLA Family
#'
#' Converts a \pkg{brms} `family` object or family name string into the
#' equivalent INLA family name.
#'
#' @param family A \pkg{brms} `family` object (e.g., `gaussian()`) or a string naming the family.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{inla}{Character string naming the INLA family (for \pkg{INLA}).}
#'   \item{brms}{Lowercase family string (as interpreted from \pkg{brms}).}
#' }
#' @keywords internal
.to_inla_family <- function(family) {
  fam <- if (!is.null(family$family)) family$family else as.character(family)
  fam <- tolower(fam)
  inla_fam <- switch(fam,
                     "gaussian"          = "gaussian",
                     "student"           = "T",
                     "bernoulli"         = "binomial",
                     "binomial"          = "binomial",
                     "poisson"           = "poisson",
                     "negbinomial"       = "nbinomial",
                     "negative binomial" = "nbinomial",
                     "beta"              = "beta",
                     "beta_binomial"     = "betabinomial",
                     "betabinomial"      = "betabinomial",
                     "lognormal"         = "lognormal",
                     "weibull"           = "weibull",
                     "exponential"       = "exponential",
                     "skew_normal"       = "sn",
                     "skew-normal"       = "sn",
                     "von_mises"         = "vm",
                     "von-mises"         = "vm",
                     "gen_extreme_value" = "gev",
                     "gev"               = "gev",
                     fam
  )
  list(inla = inla_fam, brms = fam)
}

#' Parse brms-like Random Effects Terms
#'
#' Extracts random-effects specifications from a \pkg{brms}-style formula string.
#' Supports:
#' \itemize{
#'   \item Fixed effects: intercept and standard numeric predictors
#'   \item Random effects: `(1 | g)` and `(1 + x | g)` — only one slope per term
#'   \item Intercept and slope are modelled as independent IID effects
#' }
#' Correlated intercept–slope structures are out of scope.
#'
#' @param formula A model formula.
#'
#' @return A list of random effect specifications.
#' Each element is a list with:
#' \code{group}, \code{has_intercept}, \code{slope}, \code{id_intercept}, \code{id_slope}.
#' @keywords internal
.parse_re_terms <- function(formula) {
  ftxt <- paste(deparse(formula), collapse = "")
  raw <- gregexpr("\\([^\\|\\)]+\\|[^\\)]+\\)", ftxt, perl = TRUE)
  matches <- regmatches(ftxt, raw)[[1]]
  if (length(matches) == 0L) return(list())

  re_specs <- list(); idx <- 0L
  for (m in matches) {
    inside <- sub("^\\(", "", sub("\\)$", "", m))
    parts  <- strsplit(inside, "\\|", fixed = FALSE)[[1]]
    lhs    <- trimws(parts[1]); grp <- trimws(parts[2])
    comps  <- trimws(strsplit(lhs, "\\+")[[1]])

    has_intercept <- any(comps == "1")
    slope_vars    <- setdiff(comps, "1")
    if (length(slope_vars) > 1L) {
      stop("At most one random slope per RE term is supported. Found: ",
           paste(slope_vars, collapse = ", "))
    }

    idx <- idx + 1L
    re_specs[[idx]] <- list(
      group        = grp,
      has_intercept = has_intercept,
      slope         = if (length(slope_vars) == 1L) slope_vars[[1]] else NULL,
      id_intercept  = paste0(".re_id_", idx, "_int"),
      id_slope      = if (length(slope_vars) == 1L) paste0(".re_id_", idx, "_slope") else NULL
    )
  }
  re_specs
}

#' Convert brms Formula to INLA Formula
#'
#' Translates a \pkg{brms}-style model formula into an \pkg{INLA}-compatible formula,
#' replacing each random-effect term with an appropriate `f()` specification.
#'
#' @inheritParams .parse_re_terms
#' @param drop_fixed Optional character vector of fixed effect term names to drop.
#'
#' @return A list with:
#' \describe{
#'   \item{inla_formula}{Formula with INLA `f()` terms for random effects.}
#'   \item{re_specs}{List of RE specifications from \code{.parse_re_terms()}.}
#' }
#' @keywords internal
.brms_to_inla_formula2 <- function(formula, drop_fixed = NULL) {
  re_specs <- .parse_re_terms(formula)

  # Fixed part of the formula
  ftxt <- paste(deparse(formula), collapse = "")
  ftxt_fixed <- gsub("\\([^\\|\\)]+\\|[^\\)]+\\)", "", ftxt, perl = TRUE)
  ftxt_fixed <- gsub("\\+\\s*\\+", "+", ftxt_fixed)
  ftxt_fixed <- gsub("~\\s*\\+", "~", ftxt_fixed)
  ftxt_fixed <- gsub("\\+\\s*$", "", ftxt_fixed)
  f_fixed <- as.formula(ftxt_fixed)

  resp <- as.character(f_fixed[[2L]])
  tt   <- terms(f_fixed)
  rhs_terms     <- attr(tt, "term.labels")
  has_intercept <- attr(tt, "intercept") == 1
  if (!is.null(drop_fixed)) rhs_terms <- setdiff(rhs_terms, drop_fixed)

  rhs <- character(0)
  rhs <- c(rhs, if (has_intercept) "1" else "-1")
  if (length(rhs_terms) > 0L) rhs <- c(rhs, rhs_terms)

  for (re in re_specs) {
    if (isTRUE(re$has_intercept)) {
      rhs <- c(rhs, sprintf("f(%s, model='iid')", re$id_intercept))
    }
    if (!is.null(re$slope)) {
      rhs <- c(rhs, sprintf("f(%s, %s, model='iid')", re$id_slope, re$slope))
    }
  }

  rhs_str <- if (length(rhs) > 0L) paste(rhs, collapse = " + ") else "1"
  inla_f  <- as.formula(paste(resp, "~", rhs_str))
  list(inla_formula = inla_f, re_specs = re_specs)
}

#' Map brms Priors to INLA Priors
#'
#' Parses a \pkg{brms} prior specification and maps fixed-effect priors to INLA's
#' `control.fixed` list elements.
#'
#' Supports \code{normal()} and \code{student_t()} (optionally variance-matched to Normal if df > 2).
#'
#' @param priors A \pkg{brms} `prior` object or data.frame.
#' @param approx_t_as_normal Logical; if TRUE, Student-t priors with df > 2 are approximated by a Normal.
#'
#' @return A list with:
#' \describe{
#'   \item{control_fixed}{Named list for `control.fixed` in INLA.}
#'   \item{hyper_by_re}{Reserved; list for hyperparameters by RE (empty by default).}
#' }
#' @keywords internal
.map_brms_priors_to_inla <- function(priors, approx_t_as_normal = TRUE) {
  out <- list(control_fixed = list(), hyper_by_re = list())
  if (is.null(priors)) return(out)

  pr_df <- tryCatch(as.data.frame(priors), error = function(e) NULL)
  if (is.null(pr_df) || !"class" %in% names(pr_df) || !"prior" %in% names(pr_df)) return(out)

  parse_normal <- function(s) {
    m <- regmatches(s, regexec("^\\s*normal\\s*\\(([^,]+),\\s*([^\\)]+)\\)\\s*$", s, ignore.case = TRUE))[[1]]
    if (length(m) == 3) list(mu = as.numeric(m[2]), sigma = as.numeric(m[3])) else NULL
  }
  parse_student_t <- function(s) {
    m <- regmatches(s, regexec("^\\s*student_t\\s*\\(([^,]+),\\s*([^,]+),\\s*([^\\)]+)\\)\\s*$", s, ignore.case = TRUE))[[1]]
    if (length(m) == 4) list(df = as.numeric(m[2]), mu = as.numeric(m[3]), sigma = as.numeric(m[4])) else NULL
  }

  mean_named <- list()
  prec_named <- list()
  mean_intercept <- NULL
  prec_intercept <- NULL

  for (i in seq_len(nrow(pr_df))) {
    cls <- pr_df$class[i]
    coef <- pr_df$coef[i]
    pr_s <- pr_df$prior[i]
    if (!is.character(pr_s) || !nzchar(pr_s)) next

    mu <- NA_real_; sigma <- NA_real_
    if (grepl("^\\s*normal\\s*\\(", pr_s, ignore.case = TRUE)) {
      p <- parse_normal(pr_s)
      if (!is.null(p)) { mu <- p$mu; sigma <- p$sigma }
    } else if (grepl("^\\s*student_t\\s*\\(", pr_s, ignore.case = TRUE)) {
      p <- parse_student_t(pr_s)
      if (!is.null(p)) {
        if (approx_t_as_normal && is.finite(p$df) && p$df > 2) {
          var_t <- (p$df / (p$df - 2)) * (p$sigma^2)
          mu <- p$mu; sigma <- sqrt(var_t)
        } else {
          mu <- p$mu; sigma <- p$sigma
        }
      }
    } else {
      next
    }

    if (!is.finite(mu) || !is.finite(sigma) || sigma <= 0) next

    if (identical(cls, "Intercept")) {
      mean_intercept <- mu
      prec_intercept <- 1 / (sigma^2)
    } else if (identical(cls, "b")) {
      if (!is.na(coef) && nzchar(coef)) {
        mean_named[[coef]] <- mu
        prec_named[[coef]] <- 1 / (sigma^2)
      }
    }
  }

  cf <- list()
  if (!is.null(mean_intercept)) cf$mean.intercept <- mean_intercept
  if (!is.null(prec_intercept)) cf$prec.intercept <- prec_intercept
  if (length(mean_named) > 0)  cf$mean <- mean_named
  if (length(prec_named) > 0)  cf$prec <- prec_named

  out$control_fixed <- cf
  out
}
