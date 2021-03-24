#' Summary plots for cat2cat results.
#' @description This function help to understand properties of cat2cat resuls and possibly before further preprocessing.
#' @param data data.frame
#' @param weis character - name of a certain wei_*_c2c column added by cat2cat function. Default wei_freq_c2c
#' @param type character - one of 3 types both, hist, bar
#' @return base plot graphics
#' @importFrom graphics hist barplot par
#' @importFrom utils head
#' @note It will work only for data.frame produced by cat2cat function.
#' @export
#' @examples
#' data(occup_small)
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' occup_new <- occup_small[occup_small$year == 2010, ]
#'
#' occup_2 <- cat2cat(
#' data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#' mappings = list(trans = trans, direction = "backward")
#' )
#'
#' plot_c2c(occup_2$old, type = c("both"))
#' plot_c2c(occup_2$old, type = c("hist"))
#' plot_c2c(occup_2$old, type = c("bar"))
#'

plot_c2c <- function(data, weis = "wei_freq_c2c", type = "both") {
  assert_that(is.data.frame(data))
  assert_that(!is.null(data[[weis]]), msg = paste("There are no cat2cat additional columns/variables like", weis))
  assert_that(nrow(data) != sum(data[[weis]]), msg = "There are no replications. Probably you should switch between old/new period.")
  assert_that(length(type) == 1 && (type %in% c("both", "hist", "bar")))

  if (type == "both") {
    default_mfrow <- par("mfrow")
    par(mfrow = c(1,2))
    on.exit(par(mfrow = default_mfrow))
  }
  if (type %in% c("both", "bar")) {
    data_s = unlist(tapply(data$rep_c2c, data$index_c2c, function(x) head(x, 1)))
    barplot(prop.table(table(data_s)), main = "Number of Replications", ylab = "Fraction", xlab = "rep_c2c")
  }
  if (type %in% c("both", "hist")) {
    hist(data[[weis]], freq = FALSE, main = "Replications Probabilities", xlab = weis)
  }
}

