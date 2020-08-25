#' Summary for data.frame with replicated rows
#' @description transforming sumary.lm object according to real number of d.f.
#' @param x summary.lm object
#' @param df_old integer number of d.f in orginal dataset
#' @param df_new integer number of d.f in dataset with replicated rows
#' @return data.frame
#' @examples
#' data(occup)
#' data(trans)
#'
#' occup_old = occup[occup$year == 2008,]
#' occup_new = occup[occup$year == 2010,]
#'
#' occup_2 = cat2cat(
#'  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'  mappings = list(trans = trans, direction = "backward"),
#'  ml = list(method = "knn", features = c("age", "sex", "edu", "exp", "parttime", "salary"), args = list(k = 10))
#')
#'
#'# Regression
#'# we have to adjust size of std as we artificialy enlarge degrees of freedom
#'lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_2$old, weights = multipier * wei_c2c)
#'summary_c2c(lms, df_old = nrow(occup_old), df_new = nrow(occup_2$old))
#' @export
summary_c2c <- function(x, df_old, df_new) {
ss = summary(x)$coefficients
correct = sqrt(df_new/df_old)
dd = as.data.frame(ss)
dd$std.error_c = dd$`Std. Error` * correct
dd$statistic_c = dd$`t value` / correct
dd$p.value_c = 2 * (1 - pnorm(abs(dd$statistic_c)))
dd
}
