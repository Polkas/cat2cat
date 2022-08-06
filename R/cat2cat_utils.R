#' A set of prune methods which will be useful after transition process
#'
#' @description user could specify one from four methods to prune replications
#'
#' @param df data.frame
#' @param index character default wei_freq_c2c
#' @param column character default index_c2c
#' @param method character one of four available methods: default "nonzero", "highest", "highest1", "morethan"
#' @param percent integer from 0 to 99
#' @return data.frame
#' @details
#' method - specify method to reduce number of replications
#' \itemize{
#'  \item{"nonzero"}{ remove nonzero probabilities}
#'  \item{"highest"} { leave only highest probabilities for each subject- accepting ties}
#'  \item{"highest1"} { leave only highest probabilities for each subject- not accepting ties so always one is returned}
#'  \item{"morethan"}{ leave rows where a probability is higher than value specify by percent argument }
#' }
#' @export
#' @examples
#' \dontrun{
#' data(occup_small)
#' data(occup)
#' data(trans)
#'
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' occup_new <- occup_small[occup_small$year == 2010, ]
#'
#' occup_ml <- cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
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
#' prune_c2c(occup_ml$old, method = "nonzero")
#' prune_c2c(occup_ml$old, method = "highest")
#' prune_c2c(occup_ml$old, method = "highest1")
#' prune_c2c(occup_ml$old, method = "morethan", percent = 90)
#'
#' prune_c2c(occup_ml$old, column = "wei_knn_c2c", method = "nonzero")
#' }
#'
prune_c2c <- function(df, index = "index_c2c", column = "wei_freq_c2c", method = "nonzero", percent = 50) {
  stopifnot(is.data.frame(df))
  stopifnot(all(c(index, column) %in% colnames(df)))
  stopifnot(isTRUE(method %in% c("nonzero", "highest", "highest1", "morethan")))
  stopifnot(length(percent) == 1 && (percent >= 0 && percent < 100))

  df <- df[order(df[[index]]), ]

  df <- switch(method,
    nonzero = df[df[[column]] > 0, ],
    highest1 = df[unlist(tapply(df[[column]], df[[index]], function(x) seq_along(x) == which.max(x))), ],
    highest = df[unlist(tapply(df[[column]], df[[index]], function(x) x == max(x))), ],
    morethan = df[df[[column]] > percent / 100, ]
  )
  # reweight to still sum to 1 per subject
  df[[column]] <- unlist(tapply(df[[column]], df[[index]], function(x) x / sum(x)))

  df
}

#' a function to make a combination of weights from different methods by each row
#'
#' @description adding additional column which is a mix of weights columns by each row
#'
#' @param df data.frame
#' @param cols character vector default all columns follow regex like "wei_.*_c2c"
#' @param weis numeric vector  Default vector the same length as cols and with equally spaced values summing to 1.
#' @param na.rm logical if NA should be skipped, default TRUE
#' @return data.frame with an additional column wei_cross_c2c
#' @export
#' @examples
#' \dontrun{
#' data(occup_small)
#' data(occup)
#' data(trans)
#'
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' occup_new <- occup_small[occup_small$year == 2010, ]
#'
#' # mix of methods - forward direction, try out backward too
#' occup_mix <- cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans, direction = "backward"),
#'   ml = list(
#'     data = occup_new,
#'     cat_var = "code",
#'     method = c("knn"),
#'     features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'     args = list(k = 10, ntree = 20)
#'   )
#' )
#' # correlation between ml model
#' occup_mix_old <- occup_mix$old
#' cor(occup_mix_old[occup_mix_old$rep_c2c != 1, c("wei_knn_c2c", "wei_freq_c2c")])
#' # cross all methods and subset one highest probability category for each subject
#' occup_old_highest1_mix <- prune_c2c(cross_c2c(occup_mix$old),
#'   column = "wei_cross_c2c", method = "highest1"
#' )
#' }
#'
cross_c2c <- function(df,
                      cols = colnames(df)[grepl("^wei_.*_c2c$", colnames(df))],
                      weis = rep(1 / length(cols), length(cols)),
                      na.rm = TRUE) {
  stopifnot(is.data.frame(df))
  stopifnot(all(cols %in% colnames(df)))
  stopifnot(length(weis) == length(cols))
  stopifnot(is.logical(na.rm))

  weis <- weis / sum(weis)

  df[["wei_cross_c2c"]] <- as.vector(rowSums(t(t(as.matrix(df[, cols])) * weis), na.rm = na.rm))

  df
}


#' Add default cat2cat columns to a `data.frame`
#' @description a utils function to add default cat2cat columns to a `data.frame`.
#' It will be useful e.g. for a boarder periods which will not have additional `cat2cat` columns.
#' @param df `data.frame`
#' @param cat_var `character` a categorical variable name.
#' @param ml `character` vector of ml models applied, any of `c("knn", "rf", "lda")`.
#' @export
#' @examples
#' \dontrun{
#' dummy_c2c(airquality, "Month")
#'
#' data(occup_small)#'
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' dummy_c2c(occup_old, "code")
#' }
dummy_c2c <- function(df, cat_var, ml = NULL) {
  stopifnot(is.data.frame(df))
  stopifnot(length(cat_var) == 1 && is.character(cat_var))
  stopifnot(isTRUE(cat_var %in% colnames(df)))
  stopifnot(is.null(ml) || (all(ml %in% c("knn", "rf", "lda")) || all(ml %in% paste0("wei_", c("knn", "rf", "lda"), "_c2c"))))

  if (!all(c("index_c2c", "g_new_c2c", "wei_freq_c2c", "rep_c2c", "wei_naive_c2c") %in% colnames(df))) {
    df$index_c2c <- seq_len(nrow(df))
    df$g_new_c2c <- df[[cat_var]]
    df$wei_freq_c2c <- 1
    df$rep_c2c <- 1
    df$wei_naive_c2c <- 1
  }

  if (!is.null(ml)) {
    df[, setdiff(paste0("wei_", ml, "_c2c"), colnames(df))] <- 1
  }

  df
}
