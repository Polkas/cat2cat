#' Automatic mapping in a panel dataset
#' @description
#' The objective is to unify an inconsistently coded categorical variable
#' in a panel dataset according to a mapping (transition) table.
#' The mapping (transition) table is the core element of the process.
#' The function has a modular design with three arguments `data`, `mappings`, and `ml`. Each
#' of these arguments is of a `list` type, wherein the
#' `ml` argument is optional. Arguments are separated to
#' identify the core elements of the `cat2cat` procedure.
#' Although this function seems
#' complex initially, it is built to offer a wide range of
#' applications for complex tasks. The function contains
#' many validation checks to prevent incorrect usage.
#' The function has to be applied iteratively for each two neighboring periods
#' of a panel dataset.
#' The \code{prune_c2c} function could be needed to limit growing number
#' of replications.
#' @param data `named list` with fields `old`, `new`,
#' `cat_var` (or `cat_var_old` and `cat_var_new`), `time_var` and
#' optional `id_var`,`multiplier_var`.
#' @param mappings `named list` with 3 fields `trans`, `direction` and
#' optional `freqs_df`.
#' @param ml `named list` (optional) with up to 5 fields
#' `data`, `cat_var`, `method`, `features` and optional `args`.
#' @details
#' data args
#' \describe{
#'  \item{"old"}{ data.frame older time point in a panel}
#'  \item{"new"}{ data.frame more recent time point in a panel}
#'  \item{"time_var"}{ character(1) name of the time variable.}
#'  \item{"cat_var"}{ character(1) name of the categorical variable.}
#'  \item{"cat_var_old"}{
#'  Optional character(1) name of the categorical variable
#'  in the older time point. Default `cat_var`.
#'  }
#'  \item{"cat_var_new"}{
#'  Optional character(1) name of the categorical variable
#'  in the newer time point. Default `cat_var`.
#'  }
#'  \item{"id_var"}{Optional character(1) name of the unique identifier variable
#'   - if this is specified then for subjects observed in both periods,
#'  the direct mapping is applied.
#'  }
#'  \item{"multiplier_var"}{
#'  Optional character(1) name of the multiplier variable -
#'  number of replication needed to reproduce the population
#'  }
#'  \item{"freqs_df"}{
#'  Only for the backward compatibility check the definition in the description
#'  of the mappings argument
#'  }
#' }
#' mappings args
#' \describe{
#'  \item{"trans"}{ data.frame with 2 columns - mapping (transition) table -
#'   all categories for cat_var in old and new datasets have to be included.
#'   First column contains an old encoding and second a new one.
#'   The mapping (transition) table should to have a candidate for each category
#'   from the targeted for an update period.
#' }
#'  \item{"direction"}{ character(1) direction - "backward" or "forward"}
#'  \item{"freqs_df"}{
#'  Optional - data.frame with 2 columns where first one
#'  is category name (base period) and second counts.
#'  If It is not provided then is assessed automatically.
#'  Artificial counts for each variable level in the base period.
#'  It is optional nevertheless will be often needed, as gives more control.
#'  It will be used to assess the probabilities.
#'  The multiplier variable is omitted so sb has to apply it in this table.
#'  }
#' }
#' Optional ml args
#' \describe{
#'  \item{"data"}{ data.frame - dataset with features and the `cat_var`.}
#'  \item{"cat_var"}{ character(1) - the dependent variable name.}
#'  \item{"method"}{
#'  character vector - one or a few from
#'  "knn", "rf" and "lda" methods - "knn" k-NearestNeighbors,
#'  "lda" Linear Discrimination Analysis, "rf" Random Forest
#'  }
#'  \item{"features"}{
#'  character vector of features names where all
#'  have to be numeric or logical
#'  }
#'  \item{"args"}{ optional - list parameters: knn: k ; rf: ntree  }
#' }
#' @return `named list` with 2 fields old and new - 2 data.frames.
#' There will be added additional columns like
#' index_c2c, g_new_c2c, wei_freq_c2c, rep_c2c, wei_(ml method name)_c2c.
#' Additional columns will be informative only for a one data.frame
#' as we always make the changes to one direction.
#' The new columns are added instead of the additional metadata as
#' we are working with new datasets
#' where observations could be replicated.
#' For the transparency the probability and number of replications are part
#'  of each observation in the `data.frame`.
#' @importFrom stats predict complete.cases setNames
#' @importFrom MASS lda
#' @details
#' Without ml section only simple frequencies are assessed.
#' When ml model is broken then weights from simple frequencies are taken.
#' `knn` method is recommended for smaller datasets.
#' @note
#' `trans` arg columns and the `cat_var` column have to be of the same type.
#' The mapping (transition) table should to have a candidate for each category
#' from the targeted for an update period.
#' The observation from targeted for an updated period without a matched
#' category from base period is removed.
#' If you want to leave NA values add `c(NA, NA)` row to the `trans` table.
#' Please check the vignette for more information.
#' @export
#' @examples
#' \dontrun{
#' data("occup_small", package = "cat2cat")
#' data("occup", package = "cat2cat")
#' data("trans", package = "cat2cat")
#'
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' occup_new <- occup_small[occup_small$year == 2010, ]
#'
#' # Adding the dummy level to the mapping table for levels without a candidate
#' # The best to fill them manually with proper candidates, if possible
#' # In this case it is only needed for forward mapping, to suppress warnings
#' trans2 <- rbind(
#'   trans,
#'   data.frame(
#'     old = "no_cat",
#'     new = setdiff(c(occup_new$code), trans$new)
#'   )
#' )
#'
#' # default only simple frequencies
#' occup_simple <- cat2cat(
#'   data = list(
#'     old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
#'   ),
#'   mappings = list(trans = trans2, direction = "forward")
#' )
#'
#' mappings <- list(trans = trans, direction = "backward")
#'
#' ml_setup <- list(
#'     data = occup_small[occup_small$year >= 2010, ],
#'     cat_var = "code",
#'     method = "knn",
#'     features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'     args = list(k = 10)
#' )
#'
#' # ml model performance check
#' print(cat2cat_ml_run(mappings, ml_setup))
#'
#' # additional probabilities from knn
#' occup_ml <- cat2cat(
#'   data = list(
#'     old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
#'   ),
#'   mappings = mappings,
#'   ml = ml_setup
#' )
#' }
#'
cat2cat <- function(data = list(
                      old = NULL,
                      new = NULL,
                      time_var = NULL,
                      cat_var = NULL,
                      cat_var_old = NULL,
                      cat_var_new = NULL,
                      id_var = NULL,
                      multiplier_var = NULL
                    ),
                    mappings = list(
                      trans = NULL,
                      direction = NULL,
                      freqs_df = NULL
                    ),
                    ml = list(
                      data = NULL,
                      cat_var = NULL,
                      method = NULL,
                      features = NULL,
                      args = NULL
                    )) {
  stopifnot(is.list(data))
  stopifnot(is.list(mappings))
  stopifnot(is.list(ml))

  # Backward compatibility
  if (is.null(data$freqs_df)) data$freqs_df <- mappings$freqs_df
  if (is.null(data$cat_var_old)) data$cat_var_old <- data$cat_var
  if (is.null(data$cat_var_new)) data$cat_var_new <- data$cat_var

  validate_data(data)
  validate_mappings(mappings)

  mapps <- get_mappings(mappings$trans)

  if (mappings$direction == "forward") {
    base_name <- "old"
    target_name <- "new"
  } else if (mappings$direction == "backward") {
    base_name <- "new"
    target_name <- "old"
  }

  cat_base_year <- data[[base_name]]
  cat_target_year <- data[[target_name]]
  cat_var_base <- data[[paste0("cat_var_", base_name)]]
  cat_var_target <- data[[paste0("cat_var_", target_name)]]
  cat_mid <- NULL # direct match

  is_direct_match <- !is.null(data$id_var)
  if (is_direct_match) {
    id_inner <- intersect(data$old[[data$id_var]], data$new[[data$id_var]])
    id_outer <- setdiff(
      data[[target_name]][[data$id_var]],
      data[[base_name]][[data$id_var]]
    )

    cat_target_year <-
      data[[target_name]][data[[target_name]][[data$id_var]] %in% id_outer, ]
    cat_mid <- dummy_c2c(
      data[[target_name]][data[[target_name]][[data$id_var]] %in% id_inner, ],
      cat_var_base
    )

    tos <- merge(data$old[, c(data$id_var, data$cat_var_old)],
      data$new[, c(data$id_var, data$cat_var_new)],
      by = data$id_var
    )
    colnames(tos) <- c("id", "cat_old", "cat_new")
    cat_mid$g_new_c2c <-
      tos[[paste0("cat_", base_name)]][match(cat_mid[[data$id_var]], tos$id)]
  }

  cats_target <- cat_target_year[[cat_var_target]]

  mapp <- mapps[[paste0("to_", base_name)]]

  validate_cover_cats(unique(cats_target), mapp)

  # frequencies
  fre <- resolve_frequencies(
    cat_base_year = cat_base_year, cat_var_base = cat_var_base,
    freqs_df = data$freqs_df, multiplier_var = data$multiplier_var
  )
  # frequencies per category
  freqs_list <- cat_apply_freq(mapp, fre)

  # Replicate and apply freq probabilities, target period
  g_vec <- mapp[match(cats_target, names(mapp))]
  rep_vec <- unname(lengths(g_vec))
  wei_vec <- freqs_list[match(cats_target, names(freqs_list))]
  wei_vec <- unlist(wei_vec, use.names = FALSE)
  cat_target_year$index_c2c <- seq_len(nrow(cat_target_year))
  cat_target_rep <-
    cat_target_year[rep(seq_len(nrow(cat_target_year)), times = rep_vec), ]

  # Target
  cat_target_rep$g_new_c2c <- unlist(g_vec, use.names = FALSE)
  cat_target_rep$wei_freq_c2c <- wei_vec
  cat_target_rep$rep_c2c <- rep(rep_vec, times = rep_vec)
  cat_target_rep$wei_naive_c2c <- 1 / cat_target_rep$rep_c2c
  # Base
  cat_base_year <- dummy_c2c(cat_base_year, cat_var_base)

  # ML
  if (sum(vapply(ml, Negate(is.null), logical(1))) >= 1) {
    # Backward compatibility
    if (is.null(ml$data)) ml$data <- cat_base_year
    if (is.null(ml$cat_var)) ml$cat_var <- cat_var_base

    ml_names <- paste0("wei_", unique(ml$method), "_c2c")

    cat_base_year[, setdiff(ml_names, colnames(cat_base_year))] <- 1
    if (is_direct_match) cat_mid[, setdiff(ml_names, colnames(cat_mid))] <- 1

    ml_results <- cat2cat_ml(
      ml = ml,
      mapp = mapp,
      target_data = cat_target_rep,
      cat_var_target = cat_var_target
    )

    cat_target_rep <- ml_results$target_data
  }
  # bind direct match if applied
  cat_target_rep_f <- rbind(cat_target_rep, cat_mid)

  res <- list()
  res[[base_name]] <- cat_base_year
  res[[target_name]] <- cat_target_rep_f
  res
}

#' Validate if the trans table contains all proper mappings
#' @param u_cats_target vector of unique target period categories
#' @param mapp transition (mapping) table process with `get_mappings`,
#'  the "to_base" direction is taken.
#' @keywords internal
validate_cover_cats <- function(u_cats_target, mapp) {
  if (
    cats_target_diff_len <-
      length(cats_target_diff <- setdiff(u_cats_target, names(mapp)))
  ) {
    warning(
      paste(
        sprintf(
          paste0(
            "trans table does not cover some levels for a target period, ",
            "so the result will not contain all original observations.",
            " Lacking %s levels: "
          ),
          cats_target_diff_len
        ),
        paste(head(cats_target_diff, 10), collapse = ", "),
        if (cats_target_diff_len >= 10) "..." else NULL
      )
    )
  }
}

#' Resolve the frequencies
#' @param cat_base_year `data.frame` a base period dataset.
#' @param cat_var_base `character(1)` name of the base period
#' categorical variable.
#' @param freqs_df `data.frame` with 2 columns, the first one with category
#' and second with counts.
#' @param multiplier_var `character(1)` name of the multiplier variable.
#' @keywords internal
resolve_frequencies <- function(cat_base_year,
                                cat_var_base,
                                freqs_df,
                                multiplier_var) {
  cats_base <- cat_base_year[[cat_var_base]]
  freqs <- if (is.data.frame(freqs_df)) {
    freqs_df
  } else if ("wei_freq_c2c" %in% colnames(cat_base_year)) {
    stats::aggregate(
      if (!is.null(multiplier_var)) {
        cat_base_year[["wei_freq_c2c"]] * cat_base_year[[multiplier_var]]
      } else {
        cat_base_year[["wei_freq_c2c"]]
      },
      list(g = cats_base),
      function(x) round(sum(x, na.rm = TRUE))
    )
  } else {
    get_freqs(
      cats_base,
      if (!is.null(multiplier_var)) cat_base_year[[multiplier_var]]
    )
  }
  freqs
}

#" Validate cat2cat data
#' @keywords internal
validate_data <- function(data) {
  stopifnot(inherits(data$old, "data.frame"))
  stopifnot(inherits(data$new, "data.frame"))
  stopifnot(!is.null(data$cat_var_old) && !is.null(data$cat_var_new))

  stopifnot(all(c(data$cat_var_old, data$time_var) %in% colnames(data$old)))
  stopifnot(all(c(data$cat_var_new, data$time_var) %in% colnames(data$new)))
  stopifnot(
    is.null(data$multiplier_var) ||
      (data$multiplier_var %in% colnames(data$new) &&
         data$multiplier_var %in% colnames(data$old))
  )


  stopifnot(is.null(data$freqs_df) ||
              (is.data.frame(data$freqs_df) && ncol(data$freqs_df) == 2))
  stopifnot((length(unique(data$old[[data$time_var]])) == 1) &&
              (length(unique(data$new[[data$time_var]])) == 1))
  stopifnot(is.null(data$id_var) || ((data$id_var %in% colnames(data$old)) &&
                                       (data$id_var %in% colnames(data$new)) &&
                                       !anyDuplicated(data$old[[data$id_var]]) &&
                                       !anyDuplicated(data$new[[data$id_var]])))
}

#" Validate cat2cat mappings
#' @keywords internal
validate_mappings <- function(mappings) {
  stopifnot(all(vapply(mappings, Negate(is.null), logical(1))))
  stopifnot(all(c("trans", "direction") %in% names(mappings)))
  stopifnot(isTRUE(mappings$direction %in% c("forward", "backward")))
  stopifnot(is.data.frame(mappings$trans) && ncol(mappings$trans) == 2)
}
