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
