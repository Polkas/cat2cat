#' Adjusted summary for regressions on replicated datasets
#' @description adjusting lm/glm object results according to original number of
#' degree of freedom.
#' The standard errors, test statistics and p values have to be adjusted because of
#' replicated observations.
#' @param x lm or glm object
#' @param df_old integer number of d.f in original dataset. For bigger datasets
#' `nrow` should be sufficient.
#' @param df_new integer number of d.f in dataset with replicated rows,
#' Default: x$df.residual
#' @return data.frame with additional columns over a regular summary output,
#' like correct and statistics adjusted by it.
#' @importFrom stats pt
#' @details The replication step in \code{cat2cat} inflates the nominal sample
#' size: the model sees \code{n_rep} rows but only \code{n_orig} are independent
#' observations. Naive OLS therefore under-estimates standard errors.
#'
#' The correction factor is \code{sqrt(df_new / df_old)}, where \code{df_new} is
#' the residual d.f. from the replicated model and \code{df_old} the residual
#' d.f. from the original dataset. Standard errors are multiplied by this
#' factor, test statistics divided by it, and p-values recomputed from the
#' appropriate reference distribution: \code{t(df_old)} for t-based summaries
#' and standard normal for z-based summaries.
#'
#' This is a pragmatic d.f. adjustment. It works well when per-subject
#' weights sum to one and no extreme weights dominate.
#'
#' Note: Ordinary R-squared can be interpreted in neutral replication cases
#' where the response and covariates do not vary across replicated copies and
#' per-observation weights sum to the original observation weight. Adjusted
#' R-squared, AIC, and BIC depend on sample-size and degrees-of-freedom
#' conventions, so the values from the replicated model should not be reported
#' without recomputing them on the intended original-observation scale. If the
#' harmonised category itself enters the model, all fit statistics are
#' conditional on the chosen harmonisation weights.
#' @importFrom stats pnorm predict
#' @examples
#' data("occup_small", package = "cat2cat")
#' data("trans", package = "cat2cat")
#'
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' occup_new <- occup_small[occup_small$year == 2010, ]
#'
#' occup_2 <- cat2cat(
#'   data = list(
#'     old = occup_old,
#'     new = occup_new,
#'     cat_var = "code",
#'     time_var = "year"
#'   ),
#'   mappings = list(trans = trans, direction = "backward"),
#'   ml = list(
#'     data = occup_new,
#'     cat_var = "code",
#'     method = "knn",
#'     features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'     args = list(k = 10)
#'   )
#' )
#'
#' # Regression
#' # we have to adjust size of std as we artificialy enlarge degrees of freedom
#' lms <- lm(
#'   formula = I(log(salary)) ~ age + sex + factor(edu) + parttime + exp,
#'   data = occup_2$old,
#'   weights = multiplier * wei_freq_c2c
#' )
#'
#' summary_c2c(lms, df_old = nrow(occup_old))
#' @export
#'
summary_c2c <- function(x, df_old, df_new = x$df.residual) {
  stopifnot("`x` must be an lm or glm object" = inherits(x, c("lm", "glm")))

  if (!is.numeric(df_old) || length(df_old) != 1 || !is.finite(df_old) || df_old <= 0) {
    stop("`df_old` must be a single positive finite numeric value.")
  }
  if (!is.numeric(df_new) || length(df_new) != 1 || !is.finite(df_new) || df_new <= 0) {
    stop("`df_new` must be a single positive finite numeric value.")
  }

  ss <- summary(x)
  cc <- ss$coefficients

  if (is.null(cc) || !(is.matrix(cc) || is.data.frame(cc))) {
    stop("`summary(x)$coefficients` must be a matrix or data.frame.")
  }

  correct <- sqrt(df_new / df_old)
  dd <- as.data.frame(cc)

  if (!"Std. Error" %in% names(dd)) {
    stop("`summary(x)$coefficients` must contain a 'Std. Error' column.")
  }

  dd$correct <- correct
  dd$std.error_c <- dd$`Std. Error` * correct

  if ("t value" %in% names(dd)) {
    dd$statistic_c <- dd$`t value` / correct
    dd$p.value_c <- 2 * pt(abs(dd$statistic_c), df_old, lower.tail = FALSE)
    dd$reference_dist <- "t"
  } else if ("z value" %in% names(dd)) {
    dd$statistic_c <- dd$`z value` / correct
    dd$p.value_c <- 2 * pnorm(abs(dd$statistic_c), lower.tail = FALSE)
    dd$reference_dist <- "normal"
  } else {
    stop("`summary(x)$coefficients` must contain either 't value' or 'z value'.")
  }

  dd
}
