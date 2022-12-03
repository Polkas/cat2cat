#' Manual mapping for an aggregated panel dataset
#' @description Manual mapping of an inconsistently coded categorical variable
#' according to the user provided mappings (equations).
#' @param data list with 5 named fields
#'  `old`, `new`, `cat_var`, `time_var`, `freq_var`.
#' @param ... mapping equations where direction is set with any of,
#'  `>`, `<`, `\%>\%`, `\%<\%`.
#' @return `named list` with 2 fields old and new - 2 data.frames.
#' There will be added additional columns to each.
#' The new columns are added instead of the additional metadata as
#' we are working with new datasets
#' where observations could be replicated.
#' For the transparency the probability and number of replications are part of
#' each observation in the `data.frame`.
#' @details data argument - list with fields
#' \itemize{
#'  \item{"old"}{ data.frame older time point in the panel}
#'  \item{"new"} { data.frame more recent time point in the panel}
#'  \item{"cat_var"}{
#'    character - deprecated - name of the categorical variable
#'  }
#'  \item{"cat_var_old"}{
#'    character name of the categorical variable in the old period
#'  }
#'  \item{"cat_var_new"}{
#'    character name of the categorical variable in the new period
#'  }
#'  \item{"time_var"}{ character name of time variable}
#'  \item{"freq_var"}{ character name of frequency variable}
#' }
#' @note All mapping equations have to be valid ones.
#' @export
#' @examples
#' data("verticals", package = "cat2cat")
#' agg_old <- verticals[verticals$v_date == "2020-04-01", ]
#' agg_new <- verticals[verticals$v_date == "2020-05-01", ]
#'
#' # cat2cat_agg - can map in both directions at once
#' # although usually we want to have the old or the new representation
#'
#' agg <- cat2cat_agg(
#'   data = list(
#'     old = agg_old,
#'     new = agg_new,
#'     cat_var_old = "vertical",
#'     cat_var_new = "vertical",
#'     time_var = "v_date",
#'     freq_var = "counts"
#'   ),
#'   Automotive %<% c(Automotive1, Automotive2),
#'   c(Kids1, Kids2) %>% c(Kids),
#'   Home %>% c(Home, Supermarket)
#' )
#'
#' ## possible processing
#' library("dplyr")
#' agg %>%
#'   bind_rows() %>%
#'   group_by(v_date, vertical) %>%
#'   summarise(
#'     sales = sum(sales * prop_c2c),
#'     counts = sum(counts * prop_c2c),
#'     v_date = first(v_date)
#'   )
cat2cat_agg <- function(data = list(
                          old = NULL,
                          new = NULL,
                          cat_var_old = NULL,
                          cat_var_new = NULL,
                          time_var = NULL,
                          freq_var = NULL
                        ), ...) {
  if (!is.null(data$cat_var)) {
    data$cat_var_old <- data$cat_var
    data$cat_var_new <- data$cat_var
  }
  stopifnot(
    is.list(data) &&
      (length(data) >= 5 || length(data) <= 6) &&
      all(vapply(data, Negate(is.null), logical(1))) &&
      inherits(data$old, "data.frame") &&
      inherits(data$new, "data.frame") &&
      all(
        c("old", "new", "cat_var_old", "cat_var_new",
          "time_var", "freq_var") %in% names(data)
      ) &&
      all(c(data$cat_var, data$freq_var) %in% colnames(data$old)) &&
      all(c(data$cat_var, data$freq_var) %in% colnames(data$new))
  )

  d_old <- length(unique(data$old[[data$time_var]]))
  d_new <- length(unique(data$new[[data$time_var]]))

  stopifnot((d_old == 1) && (d_new == 1))

  stopifnot(all(table(data$old[[data$cat_var_old]]) == 1) &&
    all(table(data$new[[data$cat_var_new]]) == 1))

  t <- as.list(substitute(list(...))[-1])

  trans <- do.call(read_eq, t)
  trans_map <- lapply(trans, format_trans)
  old_cats <- unlist(lapply(trans_map, function(x) x[["old"]]))
  new_cats <- unlist(lapply(trans_map, function(x) x[["new"]]))
  stopifnot(all(old_cats %in% unique(data[["old"]][[data$cat_var_old]])))
  stopifnot(all(new_cats %in% unique(data[["new"]][[data$cat_var_new]])))

  df_old <- data$old
  df_old$prop_c2c <- 1
  col_df_old <- colnames(df_old)
  df_new <- data$new
  df_new$prop_c2c <- 1

  col_df_new <- colnames(df_new)

  stopifnot(
    identical(
      setdiff(col_df_old, data$cat_var_old),
      setdiff(col_df_new, data$cat_var_new)
    )
  )

  for (i in trans_map) {
    stopifnot(((length(i[[2]]) == 1) || (length(i[[3]]) == 1)))

    if (i$direction == "forward") {
      base <- df_new[!(df_new[, data$cat_var_new] %in% i[[3]]), ]
      base_rm <- df_new[df_new[, data$cat_var_new] %in% i[[3]], ]

      if (length(i[[2]]) > 1) {
        base_rm <- base_rm[rep(1, length(i[[2]])), ]
        base_rm[, data$cat_var_new] <- i[[2]]
        base_rm$prop_c2c <- prop.table(
          df_old[df_old[, data$cat_var_old] %in% i[[2]], data$freq_var]
        )
      } else {
        base_rm[, data$cat_var_new] <- i[[2]]
        base_rm$prop_c2c <- 1
      }

      df_new <- rbind(base, base_rm)
    } else if (i$direction == "backward") {
      base <- df_old[!(df_old[, data$cat_var_old] %in% i[[2]]), ]
      base_rm <- df_old[df_old[, data$cat_var_old] %in% i[[2]], ]

      if (length(i[[3]]) > 1) {
        base_rm <- base_rm[rep(1, length(i[[3]])), ]
        base_rm[, data$cat_var_old] <- i[[3]]
        base_rm$prop_c2c <- prop.table(
          df_new[df_new[, data$cat_var_new] %in% i[[3]], data$freq_var]
        )
      } else {
        base_rm[, data$cat_var_old] <- i[[3]]
        base_rm$prop_c2c <- 1
      }

      df_old <- rbind(base, base_rm)
    }
  }

  list(old = df_old, new = df_new)
}
