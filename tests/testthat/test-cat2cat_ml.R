set.seed(1234)
library("cat2cat")
data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_2010 <- occup[occup$year == 2010,]
occup_2012 <- occup[occup$year == 2012,]

library("caret")
library("randomForest")
ml_setup_run <- list(
  data = rbind(occup_2010, occup_2012),
  cat_var = "code",
  method = c("knn", "rf", "lda"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10, ntree = 50)
)

testthat::test_that("cat2cat_ml_run returns stable diagnostics", {
  mappings <- list(trans = trans, direction = "backward")

  set.seed(1234)
  res <- cat2cat_ml_run(mappings, ml_setup_run)
  set.seed(1234)
  res2 <- cat2cat_ml_run(mappings, ml_setup_run, test_prop = 0.2)
  testthat::expect_equal(res, res2)
  testthat::expect_s3_class(res, c("cat2cat_ml_run", "list"))
  testthat::expect_output(print(res), "cat2cat ML Cross-Validation Results")
  testthat::expect_output(print(res), "BRIER SCORE")
  testthat::expect_output(print(res), "MEAN P\\(TRUE CLASS\\)")
  testthat::expect_output(print(res), "SKIPPED GROUPS")

  # Check new metric fields exist in results
  non_na_idx <- which(!is.na(vapply(res, function(g) g$acc["knn"], numeric(1))))
  testthat::expect_true(length(non_na_idx) > 0)
  non_na_group <- res[[non_na_idx[1]]]
  testthat::expect_true("brier" %in% names(non_na_group))
  testthat::expect_true("mean_prob" %in% names(non_na_group))
  testthat::expect_true("naive_brier" %in% names(non_na_group))
  testthat::expect_true("freq_brier" %in% names(non_na_group))
  testthat::expect_true(all(c("knn", "rf", "lda") %in% names(non_na_group$brier)))
  # Mean stats across all groups should be positive
  avg_brier <- mean(vapply(res, function(g) g$brier["knn"], numeric(1)), na.rm = TRUE)
  avg_mean_prob <- mean(vapply(res, function(g) g$mean_prob["knn"], numeric(1)), na.rm = TRUE)
  testthat::expect_true(avg_brier > 0 && avg_brier <= 1)
  testthat::expect_true(avg_mean_prob > 0 && avg_mean_prob <= 1)
})

testthat::test_that("cat2cat_ml_run errors for unsupported direction", {
  mappings <- list(trans = trans, direction = "forward")

  testthat::expect_error(
    cat2cat_ml_run(mappings, ml_setup_run, test_prop = 0.2),
    "There is no mappings to group the cat_var variable"
  )
})

testthat::test_that("cat2cat_ml_run supports naive bayes metrics", {
  library("e1071")
  mappings <- list(trans = trans, direction = "backward")
  ml_nb <- list(
    data = rbind(occup_2010, occup_2012),
    cat_var = "code",
    method = c("nb", "lda"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary")
  )

  set.seed(1234)
  res <- cat2cat_ml_run(mappings, ml_nb)
  testthat::expect_s3_class(res, c("cat2cat_ml_run", "list"))
  testthat::expect_output(print(res), "nb: accuracy")
  testthat::expect_output(print(res), "nb: brier")
  testthat::expect_output(print(res), "nb: mean P\\(true\\)")

  # Check nb metrics exist
  non_na_idx <- which(!is.na(vapply(res, function(g) g$acc["nb"], numeric(1))))
  testthat::expect_true(length(non_na_idx) > 0)
  avg_acc_nb <- mean(vapply(res, function(g) g$acc["nb"], numeric(1)), na.rm = TRUE)
  testthat::expect_true(avg_acc_nb > 0 && avg_acc_nb <= 1)
})

testthat::test_that("cat2cat_ml_run one-hot encodes factor features", {
  library("e1071")
  mappings <- list(trans = trans, direction = "backward")
  occup_2010_f <- occup_2010
  occup_2012_f <- occup_2012
  occup_2010_f$edu <- factor(occup_2010_f$edu)
  occup_2012_f$edu <- factor(occup_2012_f$edu)

  ml_nb <- list(
    data = rbind(occup_2010_f, occup_2012_f),
    cat_var = "code",
    method = "nb",
    features = c("age", "sex", "edu", "salary")
  )

  set.seed(1234)
  res <- cat2cat_ml_run(mappings, ml_nb)
  non_na_idx <- which(!is.na(vapply(res, function(g) g$acc["nb"], numeric(1))))
  testthat::expect_true(length(non_na_idx) > 0)
})

testthat::test_that("cat2cat_ml_run computes baseline-only diagnostics", {
  mappings <- list(trans = trans, direction = "backward")
  ml_baseline <- list(
    data = rbind(occup_2010, occup_2012),
    cat_var = "code",
    method = character(0),
    features = character(0)
  )

  set.seed(1234)
  res <- cat2cat_ml_run(mappings, ml_baseline)
  non_na_freq <- vapply(res, function(g) !is.na(g$freq), logical(1))
  testthat::expect_true(any(non_na_freq))

  first_group <- res[[which(non_na_freq)[1]]]
  testthat::expect_true(is.numeric(first_group$naive_brier))
  testthat::expect_true(is.numeric(first_group$freq_brier))
  testthat::expect_true(first_group$naive_brier >= 0 && first_group$naive_brier <= 1)
  testthat::expect_true(first_group$freq_brier >= 0 && first_group$freq_brier <= 1)
})

testthat::test_that("cat2cat_ml_run method skips are independent across methods", {
  mappings <- list(trans = trans, direction = "backward")
  ml_features <- c("age", "sex", "edu", "exp", "parttime", "salary")
  methods <- c("knn", "lda", "rf", "nb")

  skipped_rate <- function(res, method) {
    mean(vapply(res, function(g) is.na(g$acc[[method]]), logical(1)))
  }

  set.seed(1234)
  together <- cat2cat_ml_run(
    mappings,
    list(
      data = occup_2010,
      cat_var = "code",
      method = methods,
      features = ml_features,
      args = list(k = 10, ntree = 50)
    ),
    test_prop = 0.2
  )

  alone_rates <- setNames(numeric(length(methods)), methods)
  for (m in methods) {
    set.seed(1234)
    res_single <- cat2cat_ml_run(
      mappings,
      list(
        data = occup_2010,
        cat_var = "code",
        method = m,
        features = ml_features,
        args = list(k = 10, ntree = 50)
      ),
      test_prop = 0.2
    )
    alone_rates[[m]] <- skipped_rate(res_single, m)
  }

  together_rates <- vapply(methods, function(m) skipped_rate(together, m), numeric(1))
  testthat::expect_lt(max(abs(together_rates - alone_rates)), 0.05)
})
