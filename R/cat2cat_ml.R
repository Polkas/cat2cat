#' The internal function used in the cat2cat one
#' @description apply the ml models to the cat2cat data
#' @param ml `list` the same `ml` argument as provided to `cat2cat` function.
#' @param mapp `list` a mapping table
#' @param target_data `data.frame`
#' @param cat_var_target `character(1)` name of the categorical variable
#' in the target period.
#' @keywords internal
cat2cat_ml <- function(ml, mapp, target_data, cat_var_target) {
  validate_ml(ml)

  stopifnot(all(ml$features %in% colnames(target_data)))
  stopifnot(cat_var_target %in% colnames(target_data))

  stopifnot(all(vapply(
    target_data[, ml$features, drop = FALSE],
    function(x) is.numeric(x) || is.logical(x), logical(1)
  )))

  features <- unique(ml$features)
  methods <- unique(ml$method)
  ml_names <- paste0("wei_", methods, "_c2c")

  target_data[, ml_names] <- target_data["wei_freq_c2c"]

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
    try(
      {
        matched_cat <- match(cat, names(target_data_cat_c2c))
        target_data_cat <- target_data_cat_c2c[[matched_cat]]
        dis <- do.call(rbind, cat_ml_year_g[mapp[[match(cat, names(mapp))]]])
        udc <- unique(dis[[ml$cat_var]])
        if (length(udc) <= 1) {
          target_data_cat_c2c[[matched_cat]][ml_names] <-
            target_data_cat$wei_freq_c2c
          next
        }
        if (
          length(unique(target_data_cat$g_new_c2c)) > 1 &&
          length(udc) >= 1 &&
          nrow(target_data_cat) > 0 &&
          any(unique(target_data_cat$g_new_c2c) %in% names(cat_ml_year_g))
        ) {
          base_ml <-
            target_data_cat[
              !duplicated(target_data_cat[["index_c2c"]]),
              c("index_c2c", features)
            ]
          cc <- complete.cases(base_ml[, features])

          for (m in methods) {

            ml_name <- paste0("wei_", m, "_c2c")

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
            target_data_cat_c2c[[
              match(cat, names(target_data_cat_c2c))
            ]][[ml_name]] <- resso$val
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

#" Validate cat2cat ml
#' @keywords internal
validate_ml <- function(ml) {
  stopifnot(all(c("method", "features", "data") %in% names(ml)))
  stopifnot(all(ml$method %in% c("knn", "rf", "lda")))

  if ("rf" %in% ml$method) {
    delayed_package_load("randomForest", "rf")
  }

  if ("knn" %in% ml$method) {
    delayed_package_load("caret", "knn")
  }

  stopifnot(ml$cat_var %in% colnames(ml$data))
  stopifnot(all(ml$features %in% colnames(ml$data)))
  stopifnot(all(vapply(
    ml$data[, ml$features, drop = FALSE],
    function(x) is.numeric(x) || is.logical(x), logical(1)
  )))
}

#" Delayed load of a package
#' @keywords internal
delayed_package_load <- function(package, name) {
  if (isFALSE(suppressPackageStartupMessages(requireNamespace(package, quietly = TRUE)))) {
    stop(
      sprintf(
        "Please install %s package to use the %s model in the cat2cat function.",
        package, name
      )
    )
  }
}


#' @keywords internal
#' @export
#' @examples
#' \dontrun{
#' library("cat2cat")
#' data("occup", package = "cat2cat")
#' data("trans", package = "cat2cat")
#'
#' occup_2006 <- occup[occup$year == 2006,]
#' occup_2008 <- occup[occup$year == 2008,]
#' occup_2010 <- occup[occup$year == 2010,]
#' occup_2012 <- occup[occup$year == 2012,]
#'
#' library("caret")
#' ml_setup <- list(
#'   data = rbind(occup_2010, occup_2012),
#'   cat_var = "code",
#'   method = c("knn", "rf", "lda"),
#'   features = c("age", "sex", "edu", "exp", "parttime", "salary"),
#'   args = list(k = 10, ntree = 50)
#' )
#' data <- list(
#'   old = occup_2008, new = occup_2010,
#'   cat_var_old = "code", cat_var_new = "code", time_var = "year"
#' )
#' mappings <- list(trans = trans, direction = "backward")
#' res <- cat2cat_ml_run(mappings, ml_setup, test_prop = 0.2)
#' res
#' }
#'
cat2cat_ml_run <- function(mappings, ml, ...) {
  stopifnot(is.list(ml))
  stopifnot(is.list(mappings))
  elargs <- list(...)
  if (is.null(elargs$test_prop)) elargs$test_prop <- 0.2
  stopifnot(elargs$test_prop > 0 && elargs$test_prop < 1)

  validate_mappings(mappings)
  validate_ml(ml)

  mapps <- get_mappings(mappings$trans)

  if (mappings$direction == "forward") {
    base_name <- "old"
    target_name <- "new"
  } else if (mappings$direction == "backward") {
    base_name <- "new"
    target_name <- "old"
  }

  mapp <- mapps[[paste0("to_", base_name)]]

  #train, test split
  nobs <- nrow(ml$data)

  features <- unique(ml$features)
  methods <- unique(ml$method)

  train_g <- split(
    ml$data[, c(features, ml$cat_var), drop = FALSE],
    factor(ml$data[[ml$cat_var]], exclude = NULL)
  )

  res <- list()
  for (cat in unique(names(mapp))) {
    try(
      {

        matched_cat <- mapp[[match(cat, names(mapp))]]
        g_name <- paste(matched_cat, collapse = "&")
        res[[g_name]][["ncat"]] <- length(matched_cat)
        res[[g_name]][["naive"]] <- 1 / length(matched_cat)
        res[[g_name]][["acc"]] <- NA_real_
        res[[g_name]][["freq"]]  <- NA_real_

        data_small_g <- do.call(rbind, train_g[matched_cat])

        if (isTRUE(is.null(data_small_g) || nrow(data_small_g) < 5 || length(matched_cat) < 2 || sum(matched_cat %in% data_small_g[[ml$cat_var]]) == 1)) {
          next
        }

        index_tt <- sample(c(0, 1), nrow(data_small_g), prob = c(1 - elargs$test_prop, elargs$test_prop), replace = TRUE)
        data_test_small <- data_small_g[index_tt == 1, ]
        data_train_small <- data_small_g[index_tt == 0, ]

        gcounts <- table(data_train_small[[ml$cat_var]])
        gfreq <- names(gcounts)[which.max(gcounts)]

        res[[g_name]][["freq"]] <- mean(gfreq == data_test_small[[ml$cat_var]])

        if (isTRUE(nrow(data_test_small) == 0 || nrow(data_train_small) < 5)) {
          next
        }

        if (isFALSE(sum(matched_cat %in% data_train_small[[ml$cat_var]]) > 1)) {
          next
        }

          cc <- complete.cases(data_test_small[, features])

          for (m in methods) {

            ml_name <- paste0("wei_", m, "_c2c")

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
                data_test_small[cc, features, drop = FALSE], type = "class"
              )
              res[[g_name]][["acc"]]["knn"] <- mean(pred == data_test_small[[ml$cat_var]])

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
              res[[g_name]][["acc"]]["rf"] <- mean(pred == data_test_small[[ml$cat_var]])
            } else if (m == "lda") {
              group_prediction <- suppressWarnings(
                MASS::lda(
                  grouping = factor(data_train_small[[ml$cat_var]]),
                  x = as.matrix(data_train_small[, features, drop = FALSE])
                )
              )
                pred <- stats::predict(
                  group_prediction,
                  as.matrix(data_test_small[cc, features, drop = FALSE])
                )$class
                res[[g_name]][["acc"]]["lda"] <- mean(pred == data_test_small[[ml$cat_var]])

            }
          }

      }, silent = TRUE
    )
  }

  invisible(structure(res, ml_models = methods, class = c("cat2cat_ml_run", "list")))
}


#' @export
print.cat2cat_ml_run <- function(x, ...) {
  # Average accurecy - please take into account it is multi-level classification
  ml_models <- attr(x, "ml_models")

  ml_message <- NULL
  how_ml_message_n <- NULL
  how_ml_message_f <- NULL
  na_message <- NULL
  for (m in ml_models) {
    acc <- mean(vapply(x, function(i) i$acc[m], numeric(1)), na.rm = T)
    ml_message <- c(ml_message,
                     sprintf("Average (groups) accurecy for %s ml models: %f", m, acc))
    howaccn <- mean(vapply(x, function(i) i$naive < mean(i$acc[m], na.rm = TRUE), numeric(1)), na.rm = T)
    how_ml_message_n <- c(how_ml_message_n,
                        sprintf("How often %s ml model is better than naive guess: %f", m, howaccn))
    howaccf <- mean(vapply(x, function(i) i$freq < mean(i$acc[m], na.rm = TRUE), numeric(1)), na.rm = T)
    how_ml_message_f <- c(how_ml_message_f,
                        sprintf("How often %s ml model is better than most frequent category solution: %f", m, howaccf))
    nna <- vapply(x, function(i) is.na(i$acc[m]), logical(1))
    pna <- sum(nna) / length(nna) * 100
    na_message <- c(na_message,
                    sprintf("Percent of failed %s ml models: %f", m, pna))
  }

  acc_freq <- mean(vapply(x, function(i) i$freq, numeric(1)), na.rm = T)
  acc_naive <- mean(vapply(x, function(i) i$naive, numeric(1)), na.rm = T)

  ml_over_freq <- mean(vapply(x, function(i) i$freq < mean(i$acc, na.rm = TRUE), numeric(1)), na.rm = T)
  cat(
   paste(
     c(
       "Selected prediction stats:",
       "",
       sprintf("Average naive (equal probabilities) guess: %f", acc_naive),
       sprintf("Average (groups) accurecy for most frequent category solution: %f", acc_freq),
       ml_message,
       "",
       na_message,
       "",
       how_ml_message_n,
       "",
       how_ml_message_f
       ),
     collapse = "\n"
   ),
   "\n"
  )
}
