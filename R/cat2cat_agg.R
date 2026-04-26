#' Manual mapping for an aggregated panel dataset
#'
#' @description
#' Applies user-defined mapping equations to redistribute aggregated counts
#' (and other numeric columns) when categorical encodings change between
#' time periods. Unlike \code{\link{cat2cat}}, which works on micro-data,
#' this function operates on pre-aggregated data where each row represents
#' a category with associated counts/totals.
#'
#' @param data Named list with 5-6 fields describing the datasets:
#' \describe{
#'   \item{\code{old}}{\code{data.frame} - older time period (one row per category).}
#'   \item{\code{new}}{\code{data.frame} - newer time period (one row per category).}
#'   \item{\code{cat_var}}{\code{character(1)} - (deprecated) category variable name
#'     if identical in both periods. Use \code{cat_var_old}/\code{cat_var_new} instead.}
#'   \item{\code{cat_var_old}}{\code{character(1)} - category variable name in old period.}
#'   \item{\code{cat_var_new}}{\code{character(1)} - category variable name in new period.}
#'   \item{\code{time_var}}{\code{character(1)} - time variable name.}
#'   \item{\code{freq_var}}{\code{character(1)} - frequency/count variable used to
#'     compute proportions when splitting one category into many.}
#' }
#'
#' @param ... Mapping equations specifying how categories relate across periods.
#'   See **Equation syntax** in Details.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{\code{$old}}{\code{data.frame} - old period with \code{prop_c2c} column added.
#'     Categories may be replicated if split by backward equations.}
#'   \item{\code{$new}}{\code{data.frame} - new period with \code{prop_c2c} column added.
#'     Categories may be replicated if split by forward equations.}
#' }
#' The \code{prop_c2c} column contains the proportion (0-1) to apply when
#' aggregating. For rows not affected by any equation, \code{prop_c2c = 1}.
#' For split categories, proportions sum to 1 within the original category.
#'
#' @details
#'
#' \strong{Equation syntax}
#'
#' Each equation has the form:
#' \preformatted{OLD_SIDE  DIRECTION  NEW_SIDE}
#'
#' where:
#' \itemize{
#'   \item \strong{OLD_SIDE} - one or more old-period category names
#'     (use \code{c(A, B)} for multiple)
#'   \item \strong{DIRECTION} - one of:
#'     \itemize{
#'       \item \code{\%>\%} or \code{>} - \strong{forward}: replicates the NEW period,
#'         renaming/splitting new categories to match old encoding
#'       \item \code{\%<\%} or \code{<} - \strong{backward}: replicates the OLD period,
#'         renaming/splitting old categories to match new encoding
#'     }
#'   \item \strong{NEW_SIDE} - one or more new-period category names
#' }
#'
#' \strong{How proportions are calculated}
#'
#' When one category maps to multiple:
#' \itemize{
#'   \item \strong{Backward} (\code{\%<\%}): proportions come from \code{freq_var}
#'     in the \strong{new} period (the target encoding)
#'   \item \strong{Forward} (\code{\%>\%}): proportions come from \code{freq_var}
#'     in the \strong{old} period (the target encoding)
#' }
#'
#' \strong{Examples of valid equations}
#'
#' \describe{
#'   \item{\code{Automotive \%<\% c(Automotive1, Automotive2)}}{
#'     Backward: the old "Automotive" row is split into two rows
#'     ("Automotive1", "Automotive2") with proportions from new-period counts.}
#'   \item{\code{c(Kids1, Kids2) \%>\% c(Kids)}}{
#'     Forward: the new "Kids" row is split into two rows
#'     ("Kids1", "Kids2") with proportions from old-period counts.}
#'   \item{\code{Home \%>\% c(Home, Supermarket)}}{
#'     Forward: the new "Home" and "Supermarket" rows are each renamed to
#'     "Home" (1-to-many from new perspective; after aggregation they merge).}
#' }
#'
#' \strong{Typical workflow}
#'
#' 1. Call \code{cat2cat_agg()} with all mapping equations.
#' 2. Bind \code{$old} and \code{$new} together.
#' 3. Group by time and the (now unified) category variable.
#' 4. Summarise numeric columns as \code{sum(value * prop_c2c)}.
#'
#' @note
#' \itemize{
#'   \item All equations must be valid - unknown category names cause an error.
#'   \item Each equation must have exactly one category on one side
#'     (the "one" in one-to-many). Many-to-many within a single equation
#'     is not allowed.
#'   \item Each category in \code{old} and \code{new} must appear exactly once
#'     (no duplicates allowed before mapping).
#' }
#' @seealso \code{vignette("cat2cat_advanced")} for a complete workflow example.
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
    "`data` must be a list with old, new, cat_var_old/new, time_var, freq_var; both old and new must be data.frames containing the specified columns" =
      is.list(data) &&
      (length(data) >= 5 || length(data) <= 6) &&
      all(vapply(data, Negate(is.null), logical(1))) &&
      inherits(data$old, "data.frame") &&
      inherits(data$new, "data.frame") &&
      all(
        c(
          "old", "new", "cat_var_old", "cat_var_new",
          "time_var", "freq_var"
        ) %in% names(data)
      ) &&
      all(c(data$cat_var, data$freq_var) %in% colnames(data$old)) &&
      all(c(data$cat_var, data$freq_var) %in% colnames(data$new))
  )

  d_old <- length(unique(data$old[[data$time_var]]))
  d_new <- length(unique(data$new[[data$time_var]]))

  stopifnot(
    "Each period must have exactly one unique time value in `time_var`" =
      (d_old == 1) && (d_new == 1)
  )

  stopifnot(
    "Each category must appear exactly once per period (no duplicates)" =
      all(table(data$old[[data$cat_var_old]]) == 1) &&
      all(table(data$new[[data$cat_var_new]]) == 1)
  )

  t <- as.list(substitute(list(...))[-1])

  trans <- do.call(read_eq, t)
  trans_map <- lapply(trans, format_trans)
  old_cats <- unlist(lapply(trans_map, function(x) x[["old"]]))
  new_cats <- unlist(lapply(trans_map, function(x) x[["new"]]))
  stopifnot(
    "All old categories in equations must exist in `data$old`" =
      all(old_cats %in% unique(data[["old"]][[data$cat_var_old]]))
  )
  stopifnot(
    "All new categories in equations must exist in `data$new`" =
      all(new_cats %in% unique(data[["new"]][[data$cat_var_new]]))
  )

  df_old <- data$old
  df_old$prop_c2c <- 1
  col_df_old <- colnames(df_old)
  df_new <- data$new
  df_new$prop_c2c <- 1

  col_df_new <- colnames(df_new)

  stopifnot(
    "Old and new data.frames must have identical columns (except cat_var)" =
      identical(
        setdiff(col_df_old, data$cat_var_old),
        setdiff(col_df_new, data$cat_var_new)
      )
  )

  for (i in trans_map) {
    stopifnot(
      "Each equation must have exactly one category on one side (one-to-many)" =
        ((length(i[[2]]) == 1) || (length(i[[3]]) == 1))
    )

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
