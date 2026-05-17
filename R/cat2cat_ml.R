#' The internal function used in the cat2cat one
#' @description apply the ml models to the cat2cat data
#' @param ml `list` the same `ml` argument as provided to `cat2cat` function.
#' @param mapp `list` a mapping table
#' @param target_data `data.frame`
#' @param cat_var_target `character(1)` name of the categorical variable
#' in the target period.
#' @keywords internal
cat2cat_ml <- function(ml, mapp, target_data, cat_var_target) {
  encoded <- encode_factor_features(ml, target_data)
  ml <- encoded$ml
  target_data <- encoded$target_data

  ml <- validate_ml(ml)

  stopifnot(
    "All `ml$features` must be columns in target_data" =
      all(ml$features %in% colnames(target_data))
  )
  stopifnot(
    "`cat_var_target` must be a column in target_data" =
      cat_var_target %in% colnames(target_data)
  )

  stopifnot(
    "All `ml$features` columns must be numeric or logical" =
      all(vapply(
        target_data[, ml$features, drop = FALSE],
        function(x) is.numeric(x) || is.logical(x), logical(1)
      ))
  )

  features <- unique(ml$features)
  methods <- unique(ml$method)
  ml_names <- paste0("wei_", methods, "_c2c")

  target_data[ml_names] <- rep(list(NA_real_), length(ml_names))

  cat_ml_year_g <- split(
    ml$data[, c(features, ml$cat_var), drop = FALSE],
    factor(ml$data[[ml$cat_var]], exclude = NULL)
  )
  target_data_cats <- target_data[[cat_var_target]]
  target_data_cat_c2c <- split(
    target_data,
    factor(target_data_cats, exclude = NULL)
  )

  for (cat in unique(names(target_data_cat_c2c))) {
    matched_cat <- match(cat, names(target_data_cat_c2c))
    target_data_cat <- target_data_cat_c2c[[matched_cat]]

    dis <- try(do.call(rbind, cat_ml_year_g[mapp[[match(cat, names(mapp))]]]), silent = TRUE)
    if (inherits(dis, "try-error")) next

    udc <- unique(dis[[ml$cat_var]])

    if (length(udc) <= 1) {
      target_data_cat_c2c[[matched_cat]][ml_names] <-
        target_data_cat$wei_freq_c2c
      next
    }

    if (
      !(length(unique(target_data_cat$g_new_c2c)) > 1 &&
          length(udc) >= 1 &&
          nrow(target_data_cat) > 0 &&
          any(unique(target_data_cat$g_new_c2c) %in% names(cat_ml_year_g)))
    ) {
      next
    }

    base_ml <-
      target_data_cat[
        !duplicated(target_data_cat[["index_c2c"]]),
        c("index_c2c", features)
      ]
    cc <- complete.cases(base_ml[, features])

    for (m in methods) {
      ml_name <- paste0("wei_", m, "_c2c")

      try(
        {
          if (m == "knn") {
            group_prediction <- suppressWarnings(
              caret::knn3(
                x = dis[, features, drop = FALSE],
                y = factor(dis[[ml$cat_var]]),
                k = min(ml$args$k, ceiling(nrow(dis) / 4))
              )
            )
            pp <- as.data.frame(
              stats::predict(
                group_prediction,
                base_ml[cc, features, drop = FALSE],
                type = "prob"
              )
            )
          } else if (m == "rf") {
            group_prediction <- suppressWarnings(
              randomForest::randomForest(
                y = factor(dis[[ml$cat_var]]),
                x = dis[, features, drop = FALSE],
                ntree = min(ml$args$ntree, 100)
              )
            )
            pp <- as.data.frame(
              stats::predict(
                group_prediction,
                base_ml[cc, features, drop = FALSE],
                type = "prob"
              )
            )
          } else if (m == "lda") {
            group_prediction <- suppressWarnings(
              MASS::lda(
                grouping = factor(dis[[ml$cat_var]]),
                x = as.matrix(dis[, features, drop = FALSE])
              )
            )
            pp <- as.data.frame(
              stats::predict(
                group_prediction,
                as.matrix(base_ml[cc, features, drop = FALSE])
              )$posterior
            )
          } else if (m == "nb") {
            group_prediction <- suppressWarnings(
              e1071::naiveBayes(
                x = dis[, features, drop = FALSE],
                y = factor(dis[[ml$cat_var]])
              )
            )
            pp <- as.data.frame(
              stats::predict(
                group_prediction,
                base_ml[cc, features, drop = FALSE],
                type = "raw"
              )
            )
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
          ress <- merge(
            target_data_cat[, c("index_c2c", "g_new_c2c")],
            res,
            by = c("index_c2c", "g_new_c2c"),
            all.x = TRUE,
            sort = FALSE
          )
          resso <- ress[order(ress$index_c2c), ]
          target_data_cat_c2c[[matched_cat]][[ml_name]] <- resso$val
        },
        silent = TRUE
      )
    }
  }

  target_data <- do.call(rbind, target_data_cat_c2c)
  target_data <- target_data[order(target_data[["index_c2c"]]), ]
  grp_id <- match(target_data[["index_c2c"]], unique(target_data[["index_c2c"]]))

  if (ml$on_fail == "freq") {
    fallback <- target_data[["wei_freq_c2c"]]
  } else if (ml$on_fail == "naive") {
    fallback <- target_data[["wei_naive_c2c"]]
  } else {
    fallback <- rep(NA_real_, nrow(target_data))
  }

  for (ml_name in ml_names) {
    col <- target_data[[ml_name]]
    failed <- is.na(col) | !is.finite(col)

    if (any(failed)) {
      n_rows <- sum(failed)
      n_obs <- length(unique(target_data[["index_c2c"]][failed]))
      total_rows <- length(col)
      total_obs <- length(unique(target_data[["index_c2c"]]))
      pct_rows <- 100 * n_rows / total_rows
      pct_obs <- 100 * n_obs / total_obs

      if (ml$on_fail == "error") {
        stop(sprintf(
          paste0(
            "ML weights failed for method '%s': ",
            "%.1f%% rows (%d/%d) and %.1f%% observations (%d/%d)."
          ),
          sub("^wei_(.*)_c2c$", "\\1", ml_name),
          pct_rows, n_rows, total_rows,
          pct_obs, n_obs, total_obs
        ))
      }

      if (isTRUE(ml$fail_warn)) {
        warning(sprintf(
          paste0(
            "ML weights failed for method '%s': ",
            "%.1f%% rows (%d/%d) and %.1f%% observations (%d/%d); ",
            "on_fail = '%s' was applied."
          ),
          sub("^wei_(.*)_c2c$", "\\1", ml_name),
          pct_rows, n_rows, total_rows,
          pct_obs, n_obs, total_obs,
          ml$on_fail
        ), call. = FALSE)
      }

      col[failed] <- fallback[failed]
    }

    sum_by_group <- as.numeric(rowsum(col, grp_id, reorder = FALSE)[, 1])
    scale_factor <- sum_by_group[grp_id]
    can_scale <- is.finite(scale_factor) & scale_factor > 0
    col[can_scale] <- col[can_scale] / scale_factor[can_scale]
    target_data[[ml_name]] <- col
  }

  list(target_data = target_data)
}

# " Validate cat2cat ml
#' @keywords internal
validate_ml <- function(ml) {
  if (is.null(ml$on_fail)) ml$on_fail <- "freq"
  ml$on_fail <- tolower(ml$on_fail)
  if (is.null(ml$fail_warn)) ml$fail_warn <- TRUE

  stopifnot(
    "`ml` must contain 'method', 'features', and 'data'" =
      all(c("method", "features", "data") %in% names(ml))
  )
  stopifnot(
    "`ml$method` must be one or more of: 'knn', 'rf', 'lda', 'nb'" =
      all(ml$method %in% c("knn", "rf", "lda", "nb"))
  )

  stopifnot(
    "`ml$on_fail` must be one of: 'freq', 'naive', 'na', 'error'" =
      length(ml$on_fail) == 1 && ml$on_fail %in% c("freq", "naive", "na", "error")
  )
  stopifnot(
    "`ml$fail_warn` must be TRUE or FALSE" =
      is.logical(ml$fail_warn) && length(ml$fail_warn) == 1 && !is.na(ml$fail_warn)
  )

  if ("rf" %in% ml$method) {
    delayed_package_load(
      "randomForest",
      sprintf("Please install %s package to use the %s model in the cat2cat function.", "randomForest", "rf")
    )
  }

  if ("knn" %in% ml$method) {
    delayed_package_load(
      "caret",
      sprintf("Please install %s package to use the %s model in the cat2cat function.", "caret", "knn")
    )
  }

  if ("nb" %in% ml$method) {
    delayed_package_load(
      "e1071",
      sprintf("Please install %s package to use the %s model in the cat2cat function.", "e1071", "nb")
    )
  }

  stopifnot(
    "`ml$cat_var` must be a column in `ml$data`" =
      ml$cat_var %in% colnames(ml$data)
  )
  stopifnot(
    "All `ml$features` must be columns in `ml$data`" =
      all(ml$features %in% colnames(ml$data))
  )
  stopifnot(
    "All `ml$features` columns in `ml$data` must be numeric or logical" =
      all(vapply(
        ml$data[, ml$features, drop = FALSE],
        function(x) is.numeric(x) || is.logical(x), logical(1)
      ))
  )

  ml
}

# " Delayed load of a package
#' @keywords internal
delayed_package_load <- function(package, msg = sprintf("Please install %s package.", package)) {
  if (isFALSE(suppressPackageStartupMessages(requireNamespace(package, quietly = TRUE)))) {
    stop(msg)
  }
}

# " Normalized multiclass Brier score
#' @keywords internal
brier_score <- function(prob_matrix, true_cats, classes = colnames(prob_matrix)) {
  prob_matrix <- as.data.frame(prob_matrix)
  classes <- as.character(classes)
  true_cats <- as.character(true_cats)

  missing_classes <- setdiff(classes, colnames(prob_matrix))
  if (length(missing_classes)) {
    prob_matrix[missing_classes] <- 0
  }
  prob_matrix <- as.matrix(prob_matrix[, classes, drop = FALSE])

  truth <- matrix(0, nrow = length(true_cats), ncol = length(classes))
  colnames(truth) <- classes
  matched <- match(true_cats, classes)
  truth[cbind(seq_along(true_cats), matched)] <- 1

  mean(rowSums((prob_matrix - truth)^2) / 2)
}

# " One-hot encode factor features in `ml$data` and `target_data`
#' @description Replaces any `factor` columns listed in `ml$features` with
#' 0/1 indicator columns built from the union of levels observed in either
#' dataset. Numeric/logical features are left unchanged. Character columns
#' are not auto-encoded; convert them to `factor` explicitly.
#' @keywords internal
#' @noRd
encode_factor_features <- function(ml, target_data) {
  feats <- ml$features
  is_factorish <- function(x) is.factor(x)

  to_encode <- feats[vapply(feats, function(f) {
    (f %in% colnames(ml$data) && is_factorish(ml$data[[f]])) ||
      (f %in% colnames(target_data) && is_factorish(target_data[[f]]))
  }, logical(1))]

  if (length(to_encode) == 0) {
    return(list(ml = ml, target_data = target_data))
  }

  new_features <- setdiff(feats, to_encode)
  for (f in to_encode) {
    train_vals <- if (f %in% colnames(ml$data)) as.character(ml$data[[f]]) else character(0)
    target_vals <- if (f %in% colnames(target_data)) as.character(target_data[[f]]) else character(0)
    lv <- unique(c(train_vals, target_vals))
    lv <- lv[!is.na(lv)]
    for (l in lv) {
      col_name <- paste0(f, "_", l)
      ml$data[[col_name]] <- as.integer(
        !is.na(ml$data[[f]]) & as.character(ml$data[[f]]) == l
      )
      target_data[[col_name]] <- as.integer(
        !is.na(target_data[[f]]) & as.character(target_data[[f]]) == l
      )
      new_features <- c(new_features, col_name)
    }
  }
  ml$features <- new_features
  list(ml = ml, target_data = target_data)
}

#' Cross-validation diagnostics for cat2cat ML models
#'
#' @description
#' Evaluates whether machine-learning models used in the \code{ml} argument of
#' \code{\link{cat2cat}} actually improve category assignment over simpler
#' baselines. The function runs a per-group train/test split across every
#' mapping group defined by the transition table.
#'
#' @details
#' For each mapping group (set of candidate categories linked by the transition
#' table) the function:
#' \enumerate{
#'   \item Collects all observations from \code{ml$data} whose category belongs
#'         to the group.
#'   \item Randomly splits them into training (\code{1 - test_prop}) and test
#'         (\code{test_prop}) sets.
#'   \item Computes two baselines on the test set:
#'         \itemize{
#'           \item \strong{naive}: accuracy of a random guess (\eqn{1 / k}
#'                 where \eqn{k} is the number of candidate categories).
#'           \item \strong{freq}: accuracy of always predicting the most
#'                 frequent category in the training set.
#'         }
#'   \item Trains each ML model specified in \code{ml$method} on the training
#'         set and records its classification accuracy on the test set.
#' }
#'
#' Groups with fewer than 5 observations or only one candidate category are
#' skipped (their accuracy is recorded as \code{NA}).
#'
#' \subsection{Baseline-Only Diagnostics}{
#' To inspect only baseline diagnostics (\code{naive}, \code{freq}, and their
#' Brier/mean-probability variants), pass empty model and feature vectors:
#' \code{ml$method = character(0)} and \code{ml$features = character(0)}.
#' In this mode, no ML models are trained, but baseline diagnostics are still
#' computed for each mapping group.
#' }
#'
#' \subsection{Understanding the Metrics}{
#' Three complementary metrics evaluate model quality:
#'
#' \strong{Accuracy} measures how often the model's top prediction matches
#' the true category. Use this when you only care about the single most likely
#' assignment. Higher is better; theoretical maximum is 1.0.
#'
#' \strong{Mean P(true class)} measures the average probability the model
#' assigns to the correct category. This evaluates the full probability
#' distribution, not just the top prediction. For cat2cat, where weights ARE
#' probabilities distributed across candidates, this metric directly measures
#' weight quality. Higher is better; range is \eqn{[0, 1]}.
#'
#' \strong{Brier score} measures the squared error between predicted
#' probabilities and the one-hot encoded true outcome, normalized to
#' \eqn{[0, 1]}. Unlike log-loss, Brier score is bounded and does not explode
#' when P(true) is near zero. Lower is better; 0 means perfect prediction. For k
#' categories, the naive baseline (uniform 1/k) gives Brier = \eqn{(1 - 1/k) / 2}.
#' }
#'
#' \subsection{Choosing a Method}{
#' \itemize{
#'   \item If accuracy and mean P(true) are similar across methods, prefer
#'         simpler methods (freq, lda) over complex ones (rf, knn).
#'   \item If ML methods rarely beat the frequency baseline, use
#'         \code{wei_freq_c2c} --- ML adds complexity without benefit.
#'   \item If Brier score for ML is similar to or worse than naive, the model
#'         is not well-calibrated. Consider \code{wei_freq_c2c} or a different ML method.
#'   \item Use \code{\link{cross_c2c}} to ensemble multiple methods if no
#'         single method dominates.
#' }
#' }
#'
#' Because the split is random, results will vary between runs. For more
#' stable estimates, call the function several times or use a larger
#' \code{ml$data} set (e.g. pool multiple survey waves).
#'
#' @param ... additional options:
#' \describe{
#'   \item{\code{test_prop}}{\code{numeric(1)} proportion of observations
#'     held out for testing in each mapping group. Must be in \eqn{(0, 1)}.
#'     Default: \code{0.2}.}
#'   \item{\code{min_match}}{\code{numeric(1)} minimum fraction of
#'     \code{ml$data[[ml$cat_var]]} values that must appear in the transition
#'     table. If the match rate falls below this threshold, the function
#'     stops with an error --- usually indicating the wrong \code{direction}.
#'     Must be in \eqn{[0, 1)}. Default: \code{0.5}.}
#' }
#' @inheritParams cat2cat
#' @return An object of class \code{"cat2cat_ml_run"} (a named list). Each
#'   element corresponds to one mapping group and contains:
#'   \describe{
#'     \item{\code{naive}}{\code{numeric(1)} --- random-guess baseline accuracy
#'       (1/k where k is number of categories). Theoretical lower bound.}
#'     \item{\code{freq}}{\code{numeric(1)} --- most-frequent-category baseline
#'       accuracy. A simple but often strong baseline.}
#'     \item{\code{acc}}{Named \code{numeric} vector --- test-set accuracy for
#'       each ML method. Higher is better; compare to \code{freq}.}
#'     \item{\code{brier}}{Named \code{numeric} vector --- Brier score
#'       for each ML method, computed from the full probability vector.
#'       Lower is better; range is [0, 1].}
#'     \item{\code{mean_prob}}{Named \code{numeric} vector --- average probability
#'       assigned to the true class. Higher is better. This directly measures
#'       the quality of probability weights used by cat2cat.}
#'     \item{\code{naive_brier}}{\code{numeric(1)} --- Brier score for uniform
#'       baseline (= (1 - 1/k) / 2). Serves as a calibration reference.}
#'     \item{\code{naive_mean_prob}}{\code{numeric(1)} --- mean P(true) for
#'       uniform baseline (= 1/k). Equals \code{naive} by definition.}
#'     \item{\code{freq_brier}}{\code{numeric(1)} --- Brier score using training
#'       set category frequencies as probabilities.}
#'     \item{\code{freq_mean_prob}}{\code{numeric(1)} --- mean P(true) using
#'       training set category frequencies.}
#'   }
#'   The object also carries an \code{"ml_models"} attribute listing the
#'   methods evaluated. Use \code{print()} for a human-readable summary.
#' @seealso \code{\link{cat2cat}} for the main mapping function,
#'   \code{\link{cross_c2c}} for ensembling weights from multiple methods.
#' @export
#' @rdname cat2cat_ml_run
#' @examples
#' \donttest{
#' library("cat2cat")
#' data("occup", package = "cat2cat")
#' data("trans", package = "cat2cat")
#'
#' occup_2006 <- occup[occup$year == 2006, ]
#' occup_2008 <- occup[occup$year == 2008, ]
#'
#' # Forward direction: old encoding -> new encoding
#' # Use data from OLD encoding periods (2006, 2008)
#' ml_setup <- list(
#'   data = rbind(occup_2006, occup_2008),
#'   cat_var = "code",
#'   method = c("knn", "rf", "lda", "nb"),
#'   features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'   args = list(k = 10, ntree = 50)
#' )
#' mappings <- list(trans = trans, direction = "forward")
#'
#' set.seed(1234)
#' res <- cat2cat_ml_run(mappings, ml_setup, test_prop = 0.2)
#' print(res)
#'
#' # Typical good results show:
#' # - ML accuracy > freq baseline (ML adds value)
#' # - ML Brier < naive (well-calibrated probabilities)
#' # - ML mean P(true) > freq (better probability weights)
#' #
#' # If Brier(ML) >= Brier(naive), the model is poorly calibrated
#' # and wei_freq_c2c may be safer. Use cross_c2c() to ensemble.
#'
#' # High failure rate is normal - most groups have <5 observations
#'
#' # Baseline-only diagnostics (no ML models):
#' ml_baseline <- list(
#'   data = rbind(occup_2006, occup_2008),
#'   cat_var = "code",
#'   method = character(0),
#'   features = character(0)
#' )
#' baseline_cv <- cat2cat_ml_run(mappings, ml_baseline)
#' print(baseline_cv)
#' }
#'
cat2cat_ml_run <- function(mappings, ml, ...) {
  stopifnot("`ml` must be a list" = is.list(ml))
  stopifnot("`mappings` must be a list" = is.list(mappings))

  ml <- encode_factor_features(ml, ml$data)$ml

  elargs <- list(...)
  if (is.null(elargs$test_prop)) elargs$test_prop <- 0.2
  stopifnot(
    "`test_prop` must be a single number between 0 and 1" =
      length(elargs$test_prop) == 1 && elargs$test_prop > 0 && elargs$test_prop < 1
  )
  if (is.null(elargs$min_match)) elargs$min_match <- 0.5
  stopifnot(
    "`min_match` must be a single number between 0 and 1" =
      length(elargs$min_match) == 1 && elargs$min_match >= 0 && elargs$min_match < 1
  )

  validate_mappings(mappings)
  validate_ml(ml)

  if (mappings$direction == "forward") {
    base_name <- "old"
    target_name <- "new"
  } else if (mappings$direction == "backward") {
    base_name <- "new"
    target_name <- "old"
  }

  mapps <- get_mappings(mappings$trans)
  mapp <- mapps[[paste0("to_", base_name)]]

  cat_var <- ml$data[[ml$cat_var]]
  cat_var_vals <- unlist(mappings$trans[, base_name])
  if (sum(cat_var %in% cat_var_vals) / length(cat_var) < elargs$min_match) {
    stop(
      paste0(
        "There is no mappings to group the cat_var variable.\n",
        "Probably you should change the direction in the mappings argument.\n"
      )
    )
  }

  features <- unique(ml$features)
  methods <- unique(ml$method)

  train_g <- split(
    ml$data[, c(features, ml$cat_var), drop = FALSE],
    factor(ml$data[[ml$cat_var]], exclude = NULL)
  )

  res <- list()
  for (cat in names(mapp)) {
    try(
      {
        matched_cat <- mapp[[match(cat, names(mapp))]]
        cat_nam <- if (cat == "") " " else cat
        res[[cat_nam]] <- list(
          naive = NA_real_,
          acc = stats::setNames(rep(NA_real_, length(methods)), methods),
          freq = NA_real_,
          brier = stats::setNames(rep(NA_real_, length(methods)), methods),
          mean_prob = stats::setNames(rep(NA_real_, length(methods)), methods),
          naive_brier = NA_real_,
          naive_mean_prob = NA_real_,
          freq_brier = NA_real_,
          freq_mean_prob = NA_real_
        )

        data_small_g <- do.call(rbind, train_g[matched_cat])

        if (isTRUE(is.null(data_small_g) || nrow(data_small_g) < 5 ||
                   length(matched_cat) < 2 || sum(matched_cat %in% data_small_g[[ml$cat_var]]) == 1)) {
          next
        }

        n_categories <- length(matched_cat)
        res[[cat_nam]][["naive"]] <- 1 / n_categories
        # Naive baseline: uniform probability = 1/k for each category
        res[[cat_nam]][["naive_mean_prob"]] <- 1 / n_categories
        res[[cat_nam]][["naive_brier"]] <- (1 - 1 / n_categories) / 2

        index_tt <- sample(c(0, 1),
                           nrow(data_small_g),
                           prob = c(1 - elargs$test_prop, elargs$test_prop), replace = TRUE)
        data_test_small <- data_small_g[index_tt == 1, , drop = FALSE]
        data_train_small <- data_small_g[index_tt == 0, , drop = FALSE]
        if (length(features) == 0) {
          cc <- rep(TRUE, nrow(data_test_small))
        } else {
          cc <- complete.cases(data_test_small[, features])
        }

        if (isTRUE(nrow(data_test_small[cc, ]) == 0 || nrow(data_train_small) < 5)) {
          next
        }

        gcounts <- table(data_train_small[[ml$cat_var]])
        gfreq <- names(gcounts)[which.max(gcounts)]
        res[[cat_nam]][["freq"]] <- mean(gfreq == data_test_small[[ml$cat_var]])

        # Frequency baseline probabilities (train frequencies applied to test)
        train_freqs <- gcounts / sum(gcounts)
        true_cats_test <- data_test_small[[ml$cat_var]][cc]
        freq_probs <- vapply(true_cats_test, function(tc) {
          if (tc %in% names(train_freqs)) train_freqs[[tc]] else 0
        }, numeric(1))
        res[[cat_nam]][["freq_mean_prob"]] <- mean(freq_probs)
        freq_prob_matrix <- matrix(
          rep(0, length(true_cats_test) * length(matched_cat)),
          nrow = length(true_cats_test),
          dimnames = list(NULL, matched_cat)
        )
        freq_prob_matrix[, names(train_freqs)] <- rep(
          train_freqs,
          each = length(true_cats_test)
        )
        res[[cat_nam]][["freq_brier"]] <- brier_score(
          freq_prob_matrix,
          true_cats_test,
          matched_cat
        )

        for (m in methods) {
          try(
            {
              if (m == "knn") {
                group_prediction <- suppressWarnings(
                  caret::knn3(
                    x = data_train_small[, features, drop = FALSE],
                    y = factor(data_train_small[[ml$cat_var]]),
                    k = min(ml$args$k, ceiling(nrow(data_train_small) / 4))
                  )
                )
                pred <- stats::predict(
                  group_prediction,
                  data_test_small[cc, features, drop = FALSE],
                  type = "class"
                )
                prob_matrix <- stats::predict(
                  group_prediction,
                  data_test_small[cc, features, drop = FALSE],
                  type = "prob"
                )
              } else if (m == "rf") {
                group_prediction <- suppressWarnings(
                  randomForest::randomForest(
                    y = factor(data_train_small[[ml$cat_var]]),
                    x = data_train_small[, features, drop = FALSE],
                    ntree = min(ml$args$ntree, 100)
                  )
                )
                pred <- stats::predict(
                  group_prediction,
                  data_test_small[cc, features, drop = FALSE]
                )
                prob_matrix <- stats::predict(
                  group_prediction,
                  data_test_small[cc, features, drop = FALSE],
                  type = "prob"
                )
              } else if (m == "lda") {
                group_prediction <- suppressWarnings(
                  MASS::lda(
                    grouping = factor(data_train_small[[ml$cat_var]]),
                    x = as.matrix(data_train_small[, features, drop = FALSE])
                  )
                )
                lda_pred <- stats::predict(
                  group_prediction,
                  as.matrix(data_test_small[cc, features, drop = FALSE])
                )
                pred <- lda_pred$class
                prob_matrix <- lda_pred$posterior
              } else if (m == "nb") {
                group_prediction <- suppressWarnings(
                  e1071::naiveBayes(
                    x = data_train_small[, features, drop = FALSE],
                    y = factor(data_train_small[[ml$cat_var]])
                  )
                )
                pred <- stats::predict(
                  group_prediction,
                  data_test_small[cc, features, drop = FALSE],
                  type = "class"
                )
                prob_matrix <- stats::predict(
                  group_prediction,
                  data_test_small[cc, features, drop = FALSE],
                  type = "raw"
                )
              }
              res[[cat_nam]][["acc"]][m] <- mean(pred == data_test_small[[ml$cat_var]])

              # Extract probability assigned to true class for each test observation
              true_cats <- as.character(data_test_small[[ml$cat_var]][cc])
              prob_true <- vapply(seq_along(true_cats), function(i) {
                tc <- true_cats[i]
                if (tc %in% colnames(prob_matrix)) prob_matrix[i, tc] else 0
              }, numeric(1))
              res[[cat_nam]][["mean_prob"]][m] <- mean(prob_true)
              res[[cat_nam]][["brier"]][m] <- brier_score(
                prob_matrix,
                true_cats,
                matched_cat
              )
            },
            silent = TRUE
          )
        }
      },
      silent = TRUE
    )
  }

  invisible(structure(res, ml_models = methods, class = c("cat2cat_ml_run", "list")))
}


#' @rdname cat2cat_ml_run
#' @param x \code{cat2cat_ml_run} instance created with
#'   \code{\link{cat2cat_ml_run}}.
#' @param ... other arguments (currently unused).
#' @return \code{print} returns \code{x} invisibly, after printing a summary
#'   with average accuracy per method, baseline comparisons, and failure rates.
#' @method print cat2cat_ml_run
#' @export
print.cat2cat_ml_run <- function(x, ...) {
  # Average accuracy - please take into account it is multi-level classification
  ml_models <- attr(x, "ml_models")

  ml_acc_message <- NULL
  ml_brier_message <- NULL
  ml_meanprob_message <- NULL
  how_ml_message_n <- NULL
  how_ml_message_f <- NULL
  na_message <- NULL

  for (m in ml_models) {
    acc <- mean(vapply(x, function(i) i$acc[m], numeric(1)), na.rm = TRUE)
    ml_acc_message <- c(
      ml_acc_message,
      sprintf("  %s: accuracy = %.4f", m, acc)
    )

    brier <- mean(vapply(x, function(i) i$brier[m], numeric(1)), na.rm = TRUE)
    ml_brier_message <- c(
      ml_brier_message,
      sprintf("  %s: brier = %.4f", m, brier)
    )

    meanprob <- mean(vapply(x, function(i) i$mean_prob[m], numeric(1)), na.rm = TRUE)
    ml_meanprob_message <- c(
      ml_meanprob_message,
      sprintf("  %s: mean P(true) = %.4f", m, meanprob)
    )

    howaccn <- mean(vapply(x, function(i) i$naive < i$acc[m], numeric(1)), na.rm = TRUE)
    how_ml_message_n <- c(
      how_ml_message_n,
      sprintf("  %s > naive: %.1f%%", m, howaccn * 100)
    )

    howaccf <- mean(vapply(x, function(i) i$freq < i$acc[m], numeric(1)), na.rm = TRUE)
    how_ml_message_f <- c(
      how_ml_message_f,
      sprintf("  %s > freq: %.1f%%", m, howaccf * 100)
    )

    nna <- vapply(x, function(i) is.na(i$acc[m]), logical(1))
    pna <- sum(nna) / length(nna) * 100
    na_message <- c(
      na_message,
      sprintf("  %s: %.1f%%", m, pna)
    )
  }

  # Baseline stats
  acc_naive <- mean(vapply(x, function(i) i$naive, numeric(1)), na.rm = TRUE)
  acc_freq <- mean(vapply(x, function(i) i$freq, numeric(1)), na.rm = TRUE)
  brier_naive <- mean(vapply(x, function(i) i$naive_brier, numeric(1)), na.rm = TRUE)
  brier_freq <- mean(vapply(x, function(i) i$freq_brier, numeric(1)), na.rm = TRUE)
  meanprob_naive <- mean(vapply(x, function(i) i$naive_mean_prob, numeric(1)), na.rm = TRUE)
  meanprob_freq <- mean(vapply(x, function(i) i$freq_mean_prob, numeric(1)), na.rm = TRUE)

  cat(
    paste(
      c(
        "=== cat2cat ML Cross-Validation Results ===",
        "",
        "ACCURACY (higher is better):",
        sprintf("  naive (1/k): %.4f", acc_naive),
        sprintf("  freq (most common): %.4f", acc_freq),
        ml_acc_message,
        "",
        "BRIER SCORE (lower is better, range 0-1):",
        sprintf("  naive: %.4f", brier_naive),
        sprintf("  freq: %.4f", brier_freq),
        ml_brier_message,
        "",
        "MEAN P(TRUE CLASS) (higher is better):",
        sprintf("  naive: %.4f", meanprob_naive),
        sprintf("  freq: %.4f", meanprob_freq),
        ml_meanprob_message,
        "",
        "ACCURACY: ML vs BASELINES (percent of groups where ML wins):",
        how_ml_message_n,
        how_ml_message_f,
        "",
        "SKIPPED GROUPS (single category or <5 observations):",
        na_message
      ),
      collapse = "\n"
    ),
    "\n"
  )
  invisible(x)
}
