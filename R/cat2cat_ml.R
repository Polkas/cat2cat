#' The internal function used in the cat2cat one
#' @description apply the ml models to the cat2cat data
#' @param ml `list` the same `ml` argument as provided to `cat2cat` function.
#' @param mapp `list` a mapping table
#' @param target_data `data.frame`
#' @param cat_var_target `character(1)` name of the categorical variable
#' in the target period.
#' @keywords internal
cat2cat_ml <- function(ml, mapp, target_data, cat_var_target) {

  stopifnot(all(c("method", "features") %in% names(ml)))
  stopifnot(all(ml$method %in% c("knn", "rf", "lda")))

  if ("rf" %in% ml$method) {
    delayed_package_load("randomForest", "rf")
  }

  if ("knn" %in% ml$method) {
    delayed_package_load("caret", "knn")
  }

  stopifnot(ml$cat_var %in% colnames(ml$data))
  stopifnot(all(ml$features %in% colnames(target_data)))
  stopifnot(all(ml$features %in% colnames(ml$data)))
  stopifnot(cat_var_target %in% colnames(target_data))

  stopifnot(all(vapply(
    target_data[, ml$features, drop = FALSE],
    function(x) is.numeric(x) || is.logical(x), logical(1)
  )))
  stopifnot(all(vapply(
    ml$data[, ml$features, drop = FALSE],
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

#' @keywords internal
cat2cat_ml_run <- function(ml, trans, ...) {
  stopifnot(is.list(ml_setup))
  stopifnot(is.list(trans))
  elargs <- list(...)

  stopifnot(elargs$test_prop > 0 && elargs$test_prop < 1)

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

  mapp <- mapps[[paste0("to_", base_name)]]

  #train, test split
  nobs <- nrow(ml$data)
  index_tt <- sample(c(0, 1), nobs, prob = c(1 - test_prop, test_prop))
  data_test <- ml$data[index_tt == 1, ]
  data_train <- ml$data[index_tt == 0, ]

  #on train
  ml$data <- data_train

  ml_results <- cat2cat_ml(
    ml = ml,
    mapp = mapp,
    target_data = data_test,
    cat_var_target = ml$cat_var_target
  )

  #providate performance on test

}
