#' Manual transforming of a categorical variable according to a new encoding
#' @description Manual transforming of a categorical variable according to a new encoding where user suppling transsitions by equtions.
#' @param data list with 5 named fileds `old` `new` `cat_var` `time_var` `freq_var`
#' @param ... equations
#' @return list of data.frame objects
#' @details
#' data args
#' \itemize{
#'  \item{"old"}{ data.frame}
#'  \item{"new"}{ data.frame}
#'  \item{"cat_var"}{ name of the caterogical variable}
#'  \item{"time_var"}{ name of time variable}
#'  \item{"freq_var"}{ name of frequency variable}
#' }
#' ... equations where direction is set by `>`,`<`,`%>%`,`%<%`
#'
#' @export
cat2cat_man <- function(data = list(old = NULL, new = NULL, cat_var = NULL, time_var = NULL, freq_var = NULL), ...) {
  # methods most frequent, proportional

  stopifnot(
    is.list(data) &&
      length(data) == 5 &&
      all(vapply(data, Negate(is.null), logical(1))) &&
      inherits(data$old, "data.frame") &&
      inherits(data$new, "data.frame") &&
      all(c("old", "new", "cat_var", "time_var", "freq_var") %in% names(data)) &&
      all(c(data$cat_var, data$freq_var) %in% colnames(data$old)) &&
      all(c(data$cat_var, data$freq_var) %in% colnames(data$new))
  )

  d_old <- length(unique(data$old[[data$time_var]]))
  d_new <- length(unique(data$new[[data$time_var]]))

  stopifnot((d_old == 1) && (d_new == 1))

  t <- enexprs(...)

  trans <- read_eq(!!!t)
  trans_map <- lapply(trans, format_trans)

  df_old <- data$old
  df_old$prop <- 1
  col_df_old <- colnames(df_old)
  df_new <- data$new
  df_new$prop <- 1

  col_df_new <- colnames(df_new)

  stopifnot(identical(col_df_old, col_df_new))

  for (i in trans_map) {
    stopifnot(((length(i[[2]]) == 1) || (length(i[[3]]) == 1)))

    if (i$direction == "forward") {
      base <- df_new[!(df_new[, 1] %in% i[[3]]), ]
      base_rm <- df_new[df_new[, 1] %in% i[[3]], ]

      # dd = df_old[df_old[,1] %in% i[[2]],]

      if (length(i[[2]]) > 1) {
        base_rm <- base_rm[rep(1, length(i[[2]])), ]
        base_rm[, 1] <- i[[2]]
        base_rm$prop <- prop.table(df_old[df_old[, 1] %in% i[[2]], 2])
      } else {
        base_rm[, 1] <- i[[2]]
        base_rm$prop <- 1
      }

      df_new <- rbind(base, base_rm)
    } else if (i$direction == "backword") {
      base <- df_old[!(df_old[, 1] %in% i[[2]]), ]
      base_rm <- df_old[df_old[, 1] %in% i[[2]], ]

      if (length(i[[3]]) > 1) {
        base_rm <- base_rm[rep(1, length(i[[3]])), ]
        base_rm[, 1] <- i[[3]]
        base_rm$prop <- prop.table(df_new[df_new[, 1] %in% i[[3]], 2])
      } else {
        base_rm[, 1] <- i[[3]]
        base_rm$prop <- 1
      }

      df_old <- rbind(base, base_rm)
    } else {
      stop()
    }
  }

  list(old = df_old, new = df_new)
}
