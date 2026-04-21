set.seed(1234)
library("cat2cat")
data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_2006 <- occup[occup$year == 2006,]
occup_2008 <- occup[occup$year == 2008,]
occup_2010 <- occup[occup$year == 2010,]
occup_2012 <- occup[occup$year == 2012,]

library("caret")
library("randomForest")
ml_setup <- list(
  data = rbind(occup_2010, occup_2012),
  cat_var = "code",
  method = c("knn", "rf", "lda"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10, ntree = 50)
)
data <- list(
  old = occup_2008, new = occup_2010,
  cat_var_old = "code", cat_var_new = "code", time_var = "year"
)

testthat::test_that("cat2cat_ml_run", {
  mappings <- list(trans = trans, direction = "backward")

  set.seed(1234)
  res <- cat2cat_ml_run(mappings, ml_setup)
  set.seed(1234)
  res2 <- cat2cat_ml_run(mappings, ml_setup, test_prop = 0.2)
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

testthat::test_that("cat2cat_ml_run wrong direction", {
  mappings <- list(trans = trans, direction = "forward")

  testthat::expect_error(
    cat2cat_ml_run(mappings, ml_setup, test_prop = 0.2),
    "There is no mappings to group the cat_var variable"
  )
})

testthat::test_that("cat2cat_ml_run with naive bayes", {
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

testthat::test_that("cat2cat with naive bayes method", {
  library("e1071")
  ml_nb <- list(
    data = rbind(occup_2010, occup_2012),
    cat_var = "code",
    method = c("nb"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary")
  )

  set.seed(1234)
  result <- cat2cat(
    data = list(
      old = occup_2008, new = occup_2010,
      cat_var = "code", time_var = "year"
    ),
    mappings = list(trans = trans, direction = "backward"),
    ml = ml_nb
  )

  testthat::expect_true(is.data.frame(result$old))
  testthat::expect_true(is.data.frame(result$new))
  testthat::expect_true("wei_nb_c2c" %in% colnames(result$old))
  testthat::expect_true("wei_nb_c2c" %in% colnames(result$new))

  # Check weights are valid probabilities (excluding NA from failed groups)
  non_na_wei <- result$old$wei_nb_c2c[!is.na(result$old$wei_nb_c2c)]
  testthat::expect_true(length(non_na_wei) > 0)
  testthat::expect_true(all(non_na_wei >= 0))
  testthat::expect_true(all(non_na_wei <= 1))

  # Check that most weight sums are close to 1 (some groups may fail)
  wei_sums <- tapply(result$old$wei_nb_c2c, result$old$index_c2c, sum, na.rm = TRUE)
  valid_sums <- wei_sums[wei_sums > 0]
  prop_valid <- mean(abs(valid_sums - 1) < 0.01)
  testthat::expect_true(prop_valid > 0.5)  # At least 50% of groups should sum to ~1
})
