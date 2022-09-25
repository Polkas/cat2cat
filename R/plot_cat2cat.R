#' Summary plots for cat2cat results
#' @description This function help to understand properties of cat2cat results.
#' It is recommended to run it before further processing, like next iterations.
#' @param data `data.frame` - one of the data.frames returned by
#' the `cat2cat` function.
#' @param weis `character(1)` - name of a certain wei_*_c2c column,
#' added by cat2cat function. Default `wei_freq_c2c`.
#' @param type `character(1)` - one of 3 types `"both"`, `"hist"`, `"bar"`.
#' @return base plot graphics
#' @importFrom graphics hist barplot par
#' @importFrom utils head
#' @note It will work only for data.frame produced by cat2cat function.
#' @export
#' @examples
#' data("occup_small", package = "cat2cat")
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' occup_new <- occup_small[occup_small$year == 2010, ]
#'
#' occup_2 <- cat2cat(
#'   data = list(
#'     old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
#'   ),
#'   mappings = list(trans = trans, direction = "backward")
#' )
#'
#' plot_c2c(occup_2$old, type = c("both"))
#' plot_c2c(occup_2$old, type = c("hist"))
#' plot_c2c(occup_2$old, type = c("bar"))
plot_c2c <- function(
    data,
    weis = "wei_freq_c2c",
    type = c("both", "hist", "bar")
  ) {
  stopifnot(is.data.frame(data))
  stopifnot(is.character(weis) && (length(weis) == 1))
  stopifnot(
    "There are no cat2cat additional wei_* columns/variables" =
      weis %in% colnames(data)
  )
  stopifnot(
    "There are no replications. Probably you should switch between old/new period." =
      nrow(data) != sum(data[[weis]])
  )
  type <- match.arg(type)

  if (type == "both") {
    default_mfrow <- par("mfrow")
    par(mfrow = c(1, 2))
    on.exit(par(mfrow = default_mfrow))
  }
  if (type %in% c("both", "bar")) {
    data_s <- unlist(
      tapply(data$rep_c2c, data$index_c2c, function(x) head(x, 1))
    )
    barplot(
      prop.table(table(data_s)),
      main = "Number of Replications",
      ylab = "Fraction",
      xlab = "rep_c2c"
    )
  }
  if (type %in% c("both", "hist")) {
    hist(
      data[[weis]],
      freq = FALSE,
      main = "Replications Probabilities",
      xlab = weis
    )
  }
}
