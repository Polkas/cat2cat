#' Manual mapping of a categorical variable according to a new encoding for aggregated panel dataset
#' @description Aggregate panel dataset - Manual mapping of a categorical variable according to a new encoding where user providing transitions with equations.
#' @param data list with 5 named fields `old`, `new`, `cat_var`, `time_var`, `freq_var`.
#' @param ... equations where direction is set with any of `>`, `<`, `\%>\%`, `\%<\%`.
#' @return list of two data.frame objects.
#' @details data argument - list with fields
#' \itemize{
#'  \item{"old"}{ data.frame older time point in the panel}
#'  \item{"new"} { data.frame more recent time point in the panel}
#'  \item{"cat_var"}{ character name of the categorical variable}
#'  \item{"time_var"}{ character name of time variable}
#'  \item{"freq_var"}{ character name of frequency variable}
#' }
#' @export
#' @examples
#' data(verticals)
#' agg_old <- verticals[verticals$v_date == "2020-04-01", ]
#' agg_new <- verticals[verticals$v_date == "2020-05-01", ]
#'
#' ## cat2cat_agg - could map in both directions at once although
#' ## usually we want to have old or new representation
#'
#' agg <- cat2cat_agg(
#'   data = list(
#'     old = agg_old,
#'     new = agg_new,
#'     cat_var = "vertical",
#'     time_var = "v_date",
#'     freq_var = "counts"
#'   ),
#'   Automotive %<% c(Automotive1, Automotive2),
#'   c(Kids1, Kids2) %>% c(Kids),
#'   Home %>% c(Home, Supermarket)
#' )
cat2cat_agg <- function(data = list(
                          old = NULL,
                          new = NULL,
                          cat_var = NULL,
                          time_var = NULL,
                          freq_var = NULL
                        ), ...) {
  # methods most frequent, proportional

  assert_that(
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

  assert_that((d_old == 1) && (d_new == 1))

  assert_that(all(table(data$old[[data$cat_var]]) == 1) && all(table(data$new[[data$cat_var]]) == 1))

  t <- enexprs(...)

  trans <- read_eq(!!!t)
  trans_map <- lapply(trans, format_trans)

  df_old <- data$old
  df_old$prop_c2c <- 1
  col_df_old <- colnames(df_old)
  df_new <- data$new
  df_new$prop_c2c <- 1

  col_df_new <- colnames(df_new)

  assert_that(identical(col_df_old, col_df_new))

  for (i in trans_map) {
    assert_that(((length(i[[2]]) == 1) || (length(i[[3]]) == 1)))

    if (i$direction == "forward") {
      base <- df_new[!(df_new[, data$cat_var] %in% i[[3]]), ]
      base_rm <- df_new[df_new[, data$cat_var] %in% i[[3]], ]

      if (length(i[[2]]) > 1) {
        base_rm <- base_rm[rep(1, length(i[[2]])), ]
        base_rm[, data$cat_var] <- i[[2]]
        base_rm$prop_c2c <- prop.table(df_old[df_old[, data$cat_var] %in% i[[2]], data$freq_var])
      } else {
        base_rm[, data$cat_var] <- i[[2]]
        base_rm$prop_c2c <- 1
      }

      df_new <- rbind(base, base_rm)
    } else if (i$direction == "backward") {
      base <- df_old[!(df_old[, data$cat_var] %in% i[[2]]), ]
      base_rm <- df_old[df_old[, data$cat_var] %in% i[[2]], ]

      if (length(i[[3]]) > 1) {
        base_rm <- base_rm[rep(1, length(i[[3]])), ]
        base_rm[, data$cat_var] <- i[[3]]
        base_rm$prop_c2c <- prop.table(df_new[df_new[, data$cat_var] %in% i[[3]], data$freq_var])
      } else {
        base_rm[, data$cat_var] <- i[[3]]
        base_rm$prop_c2c <- 1
      }

      df_old <- rbind(base, base_rm)
    } else {
      stop()
    }
  }

  list(old = df_old, new = df_new)
}
