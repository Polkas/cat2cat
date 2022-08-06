#' Automatic mapping of a categorical variable in a panel dataset according to a new encoding
#' @description This function is built to work for two time points at once.
#' Thus for more periods some recursion will be needed.
#' The \code{prune_c2c} might be needed when we have many interactions to limit growing number of replications.
#' This function might seems to be a complex at the first glance though it is built to offer a wide range of applications for complex tasks.
#' @param data `named list` with fields `old`, `new`, `cat_var` (or `cat_var_old` and `cat_var_new`), `time_var` and optional `id_var`,`multiplier_var`,`freq_df`.
#' @param mappings `named list` with 2 fields `trans` and `direction`.
#' @param ml `named list` with up to 5 fields `data`, `cat_var`, `method`, `features` and optional `args`.
#' @details
#' data args
#' \itemize{
#'  \item{"old"}{ data.frame older time point in a panel}
#'  \item{"new"} { data.frame more recent time point in a panel}
#'  \item{"cat_var"}{ character name of the categorical variable.}
#'  \item{"cat_var_old"}{ optional character name of the categorical variable in the older time point. Default `cat_var`.}
#'  \item{"cat_var_new"}{ optional character name of the categorical variable in the newer time point. Default `cat_var`.}
#'  \item{"time_var"}{ character name of the time variable.}
#'  \item{"id_var"}{ optional character name of the unique identifier variable - if this is specified then for subjects observe in both periods the direct mapping is applied.}
#'  \item{"multiplier_var"}{ optional character name of the multiplier variable - number of replication needed to reproduce the population}
#'  \item{"freqs_df"}{ optional - data.frame with 2 columns where first one is category name and second counts.
#'  It is optional nevertheless will be very often needed, as give more control.
#'  It will be used to assess the probabilities. The multiplier variable is omit so sb has to apply it in this table.}
#' }
#' mappings args
#' \itemize{
#'  \item{"trans"}{ data.frame with 2 columns - transition table - all categories for cat_var in old and new datasets have to be included.
#'   First column contains an old encoding and second a new one.
#'   The transition table should to have a candidate for each category from the targeted for an update period.
#' }
#'  \item{"direction"}{ character direction - "backward" or "forward"}
#' }
#' optional ml args
#' \itemize{
#'  \item{"data"}{ data.frame - dataset with features and the `cat_var`.}
#'  \item{"cat_var"}{ character - the dependent variable name.}
#'  \item{"method"}{ character vector - one or a few from "knn", "rf" and "lda" methods - "knn" k-NearestNeighbors, "lda" Linear Discrimination Analysis, "rf" Random Forest }
#'  \item{"features"}{ character vector of features names where all have to be numeric or logical}
#'  \item{"args"}{ optional - list parameters: knn: k ; rf: ntree  }
#' }
#' @return named list with 2 fields old an new - 2 data.frames.
#' There will be added additional columns like index_c2c, g_new_c2c, wei_freq_c2c, rep_c2c, wei_(ml method name)_c2c.
#' Additional columns will be informative only for a one data.frame as we always make a changes to one direction.
#' @importFrom stats predict complete.cases setNames
#' @importFrom MASS lda
#' @details Without ml section only simple frequencies are assessed.
#' When ml model is broken then weights from simple frequencies are taken.
#' `knn` method is recommended for smaller datasets.
#' @note
#' The transition table should to have a candidate for each category from the targeted for an update period.
#' The observation from targeted for an updated period without a matched category from base period is removed.
#' If you want to leave NA values add `c(NA, NA)` row to the `trans` table.
#' Please check the vignette for more information.
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
#' # Adding the dummy level to the mapping table for levels without the candidate
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
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans2, direction = "forward")
#' )
#'
#' # additional probabilities from knn
#' occup_ml <- cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans, direction = "backward"),
#'   ml = list(
#'     data = occup_old,
#'     cat_var = "code",
#'     method = "knn",
#'     features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'     args = list(k = 10)
#'   )
#' )
#' }
#'
cat2cat <- function(data = list(
                      old = NULL,
                      new = NULL,
                      cat_var = NULL,
                      cat_var_old = NULL,
                      cat_var_new = NULL,
                      id_var = NULL,
                      time_var = NULL,
                      multiplier_var = NULL,
                      freqs_df = NULL
                    ),
                    mappings = list(trans = NULL, direction = NULL),
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

  # data validation
  stopifnot(inherits(data$old, "data.frame"))
  stopifnot(inherits(data$new, "data.frame"))
  stopifnot(!is.null(data$cat_var) || (!is.null(data$cat_var_old) && !is.null(data$cat_var_new)))
  if (is.null(data$cat_var_old)) data$cat_var_old <- data$cat_var
  if (is.null(data$cat_var_new)) data$cat_var_new <- data$cat_var
  stopifnot(all(c(data$cat_var_old, data$time_var) %in% colnames(data$old)))
  stopifnot(all(c(data$cat_var_new, data$time_var) %in% colnames(data$new)))
  stopifnot(
    is.null(data$multiplier_var) ||
      (data$multiplier_var %in% colnames(data$new) && data$multiplier_var %in% colnames(data$old))
  )
  stopifnot(is.null(data$freqs_df) || (is.data.frame(data$freqs_df) && ncol(data$freqs_df) == 2))
  stopifnot((length(unique(data$old[[data$time_var]])) == 1) && (length(unique(data$new[[data$time_var]])) == 1))
  stopifnot(is.null(data$id_var) || ((data$id_var %in% colnames(data$old)) &&
    (data$id_var %in% colnames(data$new)) &&
    !anyDuplicated(data$old[[data$id_var]]) &&
    !anyDuplicated(data$new[[data$id_var]])))

  # mappings validation
  stopifnot(all(vapply(mappings, Negate(is.null), logical(1))))
  stopifnot(all(c("trans", "direction") %in% names(mappings)))
  stopifnot(isTRUE(mappings$direction %in% c("forward", "backward")))
  stopifnot(is.data.frame(mappings$trans) && ncol(mappings$trans) == 2)

  is_direct_match <- !is.null(data$id_var)
  mapps <- get_mappings(mappings$trans)

  if (is_direct_match) {
    id_inner <- intersect(data$old[[data$id_var]], data$new[[data$id_var]])
    tos <- merge(data$old[, c(data$id_var, data$cat_var_old)], data$new[, c(data$id_var, data$cat_var_new)], by = data$id_var)
    colnames(tos) <- c(data$id_var, "cat_old", "cat_new")
    if (mappings$direction == "forward") {
      id_outer <- setdiff(data$new[[data$id_var]], data$old[[data$id_var]])
      tos_df <- tos[, c(data$id_var, "cat_old")]
    } else if (mappings$direction == "backward") {
      id_outer <- setdiff(data$old[[data$id_var]], data$new[[data$id_var]])
      tos_df <- tos[, c(data$id_var, "cat_new")]
    }
    colnames(tos_df) <- c("id", "cat")
  }

  if (mappings$direction == "forward") {
    cat_var_base <- data$cat_var_old
    cat_var_target <- data$cat_var_new
    cat_base_year <- data$old
    cat_target_year <- if (is_direct_match) data$new[data$new[[data$id_var]] %in% id_outer, ] else data$new
    cat_mid <- if (is_direct_match) data$new[data$new[[data$id_var]] %in% id_inner, ] else NULL
    mapp <- mapps$to_old
    res_ord <- c(1, 2)
  } else if (mappings$direction == "backward") {
    cat_var_base <- data$cat_var_new
    cat_var_target <- data$cat_var_old
    cat_base_year <- data$new
    cat_target_year <- if (is_direct_match) data$old[data$old[[data$id_var]] %in% id_outer, ] else data$old
    cat_mid <- if (is_direct_match) data$old[data$old[[data$id_var]] %in% id_inner, ] else NULL
    mapp <- mapps$to_new
    res_ord <- c(2, 1)
  }

  cats_base <- cat_base_year[[cat_var_base]]
  cats_target <- cat_target_year[[cat_var_target]]

  if (cats_target_diff_len <- length(cats_target_diff <- setdiff(unique(cats_target), names(mapp)))) {
    warning(
      paste(
        sprintf("trans table does not cover some levels for a target period, so the result will not contain all original observations. Lacking %s levels: ",
                cats_target_diff_len),
        paste(head(cats_target_diff, 10), collapse = ", "),
        if (cats_target_diff_len >= 10) "..." else NULL)
    )
  }

  if (is_direct_match) {
    cat_mid <- dummy_c2c(cat_mid, cat_var_base)
    cat_mid$g_new_c2c <- tos_df$cat[match(cat_mid[[data$id_var]], tos_df$id)]
  }

  fre <- if (is.data.frame(data$freqs_df)) {
    data$freqs_df
  } else if ("wei_freq_c2c" %in% colnames(cat_base_year)) {
    stats::aggregate(
      if (!is.null(data$multiplier_var)) cat_base_year[["wei_freq_c2c"]] * cat_base_year[[data$multiplier_var]] else cat_base_year[["wei_freq_c2c"]],
      list(g = cat_base_year[[cat_var_base]]),
      function(x) round(sum(x, na.rm = TRUE))
    )
  } else {
    if (!is.null(data$multiplier_var)) {
      get_freqs(cats_base, cat_base_year[[data$multiplier_var]])
    } else {
      get_freqs(cats_base)
    }
  }
  freqs_list <- cat_apply_freq(mapp, fre)

  # Replicate and apply freq probabilities
  g_vec <- mapp[match(cats_target, names(mapp))]
  rep_vec <- unname(lengths(g_vec))
  wei_vec <- freqs_list[match(cats_target, names(freqs_list))]
  wei_vec <- unlist(wei_vec, use.names = FALSE)
  cat_target_year$index_c2c <- seq_len(nrow(cat_target_year))
  cat_target_rep <- cat_target_year[rep(seq_len(nrow(cat_target_year)), times = rep_vec), ]

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

  cat_target_rep_f <- rbind(cat_target_rep, cat_mid)

  res <- list(cat_base_year, cat_target_rep_f)[res_ord]
  names(res) <- c("old", "new")
  res
}

#' The internal function used in the cat2cat ones
#' @description apply the ml models to the cat2cat data
#' @param ml `list` the same `ml` argument as provided to `cat2cat` function.
#' @param mapp `list` a mapping table
#' @param target_data `data.frame`
#' @param cat_var_target `character` name of the categorical variable in the target period.
#' @keywords internal
cat2cat_ml <- function(ml, mapp, target_data, cat_var_target) {
  stopifnot(all(c("method", "features") %in% names(ml)))
  stopifnot(all(ml$features %in% colnames(target_data)))
  stopifnot(all(ml$features %in% colnames(ml$data)))
  stopifnot(all(vapply(target_data[, ml$features, drop = FALSE], function(x) is.numeric(x) || is.logical(x), logical(1))))
  stopifnot(all(vapply(ml$data[, ml$features, drop = FALSE], function(x) is.numeric(x) || is.logical(x), logical(1))))
  stopifnot(all(ml$method %in% c("knn", "rf", "lda")))
  stopifnot(ml$cat_var %in% colnames(ml$data))
  stopifnot(cat_var_target %in% colnames(target_data))

  features <- unique(ml$features)
  methods <- unique(ml$method)
  ml_names <- paste0("wei_", methods, "_c2c")

  target_data[, ml_names] <- target_data["wei_freq_c2c"]

  cat_ml_year_g <- split(ml$data[, c(features, ml$cat_var), drop = FALSE], factor(ml$data[[ml$cat_var]], exclude = NULL))
  target_data_cats <- target_data[[cat_var_target]]
  target_data_cat_c2c <- split(target_data, factor(target_data_cats, exclude = NULL))

  for (cat in unique(names(target_data_cat_c2c))) {
    try(
      {
        matched_cat <- match(cat, names(target_data_cat_c2c))
        target_data_cat <- target_data_cat_c2c[[matched_cat]]
        dis <- do.call(rbind, cat_ml_year_g[mapp[[match(cat, names(mapp))]]])
        udc <- unique(dis[[ml$cat_var]])
        if (length(udc) <= 1) {
          target_data_cat_c2c[[matched_cat]][ml_names] <- target_data_cat$wei_freq_c2c
          next
        }
        if (
          length(unique(target_data_cat$g_new_c2c)) > 1 &&
            length(udc) >= 1 &&
            nrow(target_data_cat) > 0 &&
            any(target_data_cat$g_new_c2c %in% names(cat_ml_year_g))
        ) {
          base_ml <- target_data_cat[!duplicated(target_data_cat[["index_c2c"]]), c("index_c2c", features)]
          cc <- complete.cases(base_ml[, features])
          for (m in methods) {
            ml_name <- paste0("wei_", m, "_c2c")
            if (m == "knn") {
              if (suppressPackageStartupMessages(requireNamespace("caret", quietly = TRUE))) {
                kkk <- suppressWarnings(
                  caret::knn3(
                    x = dis[, features, drop = FALSE],
                    y = factor(dis[[ml$cat_var]]),
                    k = min(ml$args$k, ceiling(nrow(dis) / 4))
                  )
                )
                pp <- as.data.frame(stats::predict(kkk, base_ml[cc, features, drop = FALSE], type = "prob"))
              } else {
                stop("Please install caret package to use the knn model in the cat2cat function.")
              }
            } else if (m == "rf") {
              if (suppressPackageStartupMessages(requireNamespace("randomForest", quietly = TRUE))) {
                kkk <- suppressWarnings(
                  randomForest::randomForest(
                    y = factor(dis[[ml$cat_var]]),
                    x = dis[, features, drop = FALSE],
                    ntree = min(ml$args$ntree, 100)
                  )
                )
                pp <- as.data.frame(stats::predict(kkk, base_ml[cc, features, drop = FALSE], type = "prob"))
              } else {
                stop("Please install randomForest package to use the rf model in the cat2cat function.")
              }
            } else if (m == "lda") {
              kkk <- suppressWarnings(
                MASS::lda(
                  grouping = factor(dis[[ml$cat_var]]),
                  x = as.matrix(dis[, features, drop = FALSE])
                )
              )
              pp <- as.data.frame(stats::predict(kkk, as.matrix(base_ml[cc, features, drop = FALSE]))$posterior)
            }
            ll <- setdiff(unique(target_data_cat$g_new_c2c), colnames(pp))
            # imputing rest of the class to zero prob
            if (length(ll)) {
              pp[ll] <- 0
            }
            pp_stack <- utils::stack(pp)
            pp[["index_c2c"]] <- base_ml[["index_c2c"]][cc]
            res <- cbind(pp_stack, index_c2c = rep(pp$index_c2c, ncol(pp) - 1))
            colnames(res) <- c("val", "g_new_c2c", "index_c2c")
            ress <- merge(target_data_cat[, c("index_c2c", "g_new_c2c")], res, by = c("index_c2c", "g_new_c2c"), all.x = TRUE, sort = FALSE)
            resso <- ress[order(ress$index_c2c), ]
            target_data_cat_c2c[[match(cat, names(target_data_cat_c2c))]][[ml_name]] <- resso$val
          }
        }
      },
      silent = TRUE
    )
  }

  target_data <- do.call(rbind, target_data_cat_c2c)
  target_data <- target_data[order(target_data[["index_c2c"]]), ]

  list(target_data = target_data)
}
