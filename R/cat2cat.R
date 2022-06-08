#' Transforming a transition table with mappings to two associative lists
#' @description to rearrange the one classification encoding into another, an associative list that maps keys to values is used.
#' More precisely, an association list is used which is a linked list in which each list element consists of a key and value or values.
#' An association list where unique categories codes are keys and matching categories from next or previous time point are values.
#' A transition table is used to build such associative lists.
#' @param x data.frame or matrix - transition table with 2 columns where first column is assumed to be the older encoding.
#' @details the named list will be a more efficient solution than hash map as we are not expecting more than a few thousand keys.
#' @return a list with 2 named lists `to_old` and `to_new`.
#' @examples
#' data(trans)
#'
#' mappings <- get_mappings(trans)
#' mappings$to_old[1:4]
#' mappings$to_new[1:4]
#' @export
get_mappings <- function(x = data.frame()) {
  stopifnot(ncol(x) == 2)

  x <- as.matrix(x)

  ff <- x[, 1]
  ss <- x[, 2]

  from_old <- unique(ff)
  from_new <- unique(ss)

  to_old <- lapply(from_new, function(i) {
    tryCatch(
      unique(ff[ss %in% i]),
      error = function(e) {
        NA
      }
    )
  })
  names(to_old) <- from_new

  to_new <- lapply(from_old, function(i) {
    tryCatch(
      unique(ss[ff %in% i]),
      error = function(e) {
        NA
      }
    )
  })
  names(to_new) <- from_old

  list(to_old = to_old, to_new = to_new)
}

#' Applying frequencies to the object returned by get_mappings function
#' @description applying frequencies to the object returned by get_mappings.
#' We will get a symmetric object to returned one by get_mappings function, nevertheless categories are replaced with frequencies.
#' Frequencies for each category/key are sum to 1, so could be interpreted as probabilities.
#' @param to_x list object returned by get_mappings.
#' @param freqs vector object returned by get_freqs.
#' @return a list with 2 named lists `to_old` and `to_new`.
#' @examples
#' data(trans)
#' data(occup)
#'
#' mappings <- get_mappings(trans)
#'
#' mappings$to_old[1:4]
#' mappings$to_new[1:4]
#'
#' mapp_p <- cat_apply_freq(
#'   mappings$to_old,
#'   get_freqs(
#'     occup$code[occup$year == "2008"],
#'     occup$multiplier[occup$year == "2008"]
#'   )
#' )
#' head(data.frame(I(mappings$to_old), I(mapp_p)))
#' mapp_p <- cat_apply_freq(
#'   mappings$to_new,
#'   get_freqs(
#'     occup$code[occup$year == "2010"],
#'     occup$multiplier[occup$year == "2010"]
#'   )
#' )
#' head(data.frame(I(mappings$to_new), I(mapp_p)))
#' @export
cat_apply_freq <- function(to_x, freqs) {
  stopifnot(ncol(freqs) == 2)
  res <- lapply(
    to_x,
    function(x) {
      alls <- freqs[, 2, drop = TRUE][match(x, freqs[, 1, drop = TRUE])]
      ff <- alls / sum(alls, na.rm = T)
      # NA to 0
      ifelse(is.na(ff) | is.nan(ff), 0, ff)
    }
  )
  # all equal to zero so proportional probability
  res_out <- lapply(
    res,
    function(x) {
      ifelse(rep(all(x == 0), length(x)), 1 / length(x), x)
    }
  )
  names(res_out) <- names(to_x)
  res_out
}

#' Getting frequencies from a `character` vector with an optional multiplier argument
#' @description getting frequencies for a vector with an optional multiplier argument
#' @param x character vector categorical variable to summarize.
#' @param multiplier numeric vector how many times to repeat certain value, additional weights.
#' @return data.frame with two columns `input` `Freq`
#' @note without multiplier variable it is a basic `table` function wrapped with the `as.data.frame` function.
#' The `table` function is used with the `useNA = "ifany"` argument.
#' @export
#' @examples
#' data(occup)
#'
#' head(get_freqs(occup$code[occup$year == "2008"]))
#' head(get_freqs(occup$code[occup$year == "2010"]))
#'
#' head(get_freqs(occup$code[occup$year == "2008"], occup$multiplier[occup$year == "2008"]))
#' head(get_freqs(occup$code[occup$year == "2010"], occup$multiplier[occup$year == "2010"]))
get_freqs <- function(x, multiplier = NULL) {
  stopifnot(is.null(multiplier) || length(x) == length(multiplier))

  input <- if (!is.null(multiplier)) {
    rep(x, times = as.numeric(multiplier))
  } else {
    x
  }
  res <- as.data.frame(table(input, useNA = "ifany"), stringsAsFactors = F)
  res
}

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
#'  \item{"cat_var"}{ character - the dependent variable name. It has to be compatible with the `cat_var` in the target period.}
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
#' # default only simple frequencies
#'
#' occup_simple <- cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans, direction = "forward")
#' )
#'
#' # additionally add probabilities from knn
#' occup_ml <- cat2cat(
#'   data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
#'   mappings = list(trans = trans, direction = "forward"),
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
  stopifnot(is.null(data$freqs_df) || (ncol(data$freqs_df) == 2))
  d_old <- length(unique(data$old[[data$time_var]]))
  d_new <- length(unique(data$new[[data$time_var]]))
  stopifnot((d_old == 1) && (d_new == 1))
  stopifnot(is.null(data$id_var) || ((data$id_var %in% colnames(data$old)) &&
    (data$id_var %in% colnames(data$new)) &&
    !anyDuplicated(data$old[[data$id_var]]) &&
    !anyDuplicated(data$new[[data$id_var]])))

  # mappings validation
  stopifnot(length(mappings) == 2)
  stopifnot(all(c("trans", "direction") %in% names(mappings)))
  stopifnot(mappings$direction %in% c("forward", "backward"))
  stopifnot(all(vapply(mappings, Negate(is.null), logical(1))))
  stopifnot(is.data.frame(mappings$trans))
  stopifnot(ncol(mappings$trans) == 2)

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
      target_data = cat_target_rep
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
#' @keywords internal
cat2cat_ml <- function(ml,
                       mapp,
                       target_data) {
  stopifnot(all(c("method", "features") %in% names(ml)))
  stopifnot(all(ml$features %in% colnames(target_data)))
  stopifnot(all(ml$features %in% colnames(ml$data)))
  stopifnot(all(vapply(target_data[, ml$features, drop = FALSE], function(x) is.numeric(x) || is.logical(x), logical(1))))
  stopifnot(all(vapply(ml$data[, ml$features, drop = FALSE], function(x) is.numeric(x) || is.logical(x), logical(1))))
  stopifnot(all(ml$method %in% c("knn", "rf", "lda")))
  stopifnot(ml$cat_var %in% colnames(ml$data))

  features <- unique(ml$features)
  methods <- unique(ml$method)
  ml_names <- paste0("wei_", methods, "_c2c")

  target_data[, ml_names] <- target_data["wei_freq_c2c"]

  cat_ml_year_g <- split(ml$data[, c(features, ml$cat_var), drop = FALSE], factor(ml$data[[ml$cat_var]], exclude = NULL))
  target_data_cats <- target_data[[ml$cat_var]]
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
  stopifnot(inherits(df, "data.frame"))
  stopifnot(all(c(index, column) %in% colnames(df)))
  stopifnot(method %in% c("nonzero", "highest", "highest1", "morethan"))
  stopifnot(length(percent) == 1)
  stopifnot(percent >= 0 && percent < 100)

  df <- df[order(df[[index]]), ]

  df <- switch(method,
    nonzero = df[df[[column]] > 0, ],
    highest1 = df[unlist(tapply(df[[column]], df[[index]], function(x) seq_along(x) == which.max(x))), ],
    highest = df[unlist(tapply(df[[column]], df[[index]], function(x) x == max(x))), ],
    morethan = df[df[[column]] > percent / 100, ]
  )

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
  stopifnot(inherits(df, "data.frame"))
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
#' @param ml `character` vector of ml models applied, any of `c("wei_knn_c2c", "wei_rf_c2c", "wei_lda_c2c")`.
#' @export
#' @examples
#' data(occup_small)
#' data(occup)
#' data(trans)
#'
#' occup_old <- occup_small[occup_small$year == 2008, ]
#' dummy_c2c(occup_old, "code")
dummy_c2c <- function(df, cat_var, ml = NULL) {
  stopifnot(is.data.frame(df))
  stopifnot(length(cat_var) == 1 && is.character(cat_var))
  stopifnot(cat_var %in% colnames(df))
  stopifnot(is.null(ml) || all(ml %in% c("wei_knn_c2c", "wei_rf_c2c", "wei_lda_c2c")))

  if (!all(c("index_c2c", "g_new_c2c", "wei_freq_c2c", "rep_c2c", "wei_naive_c2c") %in% colnames(df))) {
    df$index_c2c <- seq_len(nrow(df))
    df$g_new_c2c <- df[[cat_var]]
    df$wei_freq_c2c <- 1
    df$rep_c2c <- 1
    df$wei_naive_c2c <- 1
  }

  if (!is.null(ml)) {
    df[, setdiff(ml, colnames(df))] <- 1
  }

  df
}
