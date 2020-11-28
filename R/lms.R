#' Summary for data.frame with replicated rows
#' @description transforming summary.lm object according to real number of d.f.
#' The standard errors, t statistics and p values have to be adjusted because of replicated rows.
#' @param x lm object
#' @param df_old integer number of d.f in original dataset
#' @param df_new integer number of d.f in dataset with replicated rows
#' @return data.frame with additional columns over a regular summary.lm output like correct and statistics adjusted by it.
#' @details The size of the correction is equal to sqrt(df_new / df_old).
#' Where standard errors are multiplied and t statistics divided by it.
#' @importFrom stats pnorm predict
#' @examples
#' data(occup_small)
#' data(trans)
#'
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' occup_new <- occup_small[occup_small$year == 2010, ]
#'
#' occup_2 <- cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans, direction = "backward"),
#'   ml = list(
#'     method = "knn",
#'     features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'     args = list(k = 10)
#'   )
#' )
#'
#' # Regression
#' # we have to adjust size of std as we artificialy enlarge degrees of freedom
#' lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_2$old,
#'   weights = multiplier * wei_freq_c2c
#' )
#' summary_c2c(lms, df_old = nrow(occup_old), df_new = nrow(occup_2$old))
#' @export
summary_c2c <- function(x, df_old, df_new) {
  ss <- summary(x)$coefficients
  correct <- sqrt(df_new / df_old)
  dd <- as.data.frame(ss)
  dd$correct <- correct
  dd$std.error_c <- dd$`Std. Error` * correct
  dd$statistic_c <- dd$`t value` / correct
  dd$p.value_c <- 2 * (1 - pnorm(abs(dd$statistic_c)))
  dd
}
