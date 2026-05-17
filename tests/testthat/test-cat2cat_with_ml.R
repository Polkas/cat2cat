set.seed(1234)
library("cat2cat")
data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]

test_data <- list(
  old = occup_2008,
  new = occup_2010,
  cat_var = "code",
  time_var = "year"
)

ml_nb_base <- list(
  data = rbind(occup_2010, occup_2012),
  cat_var = "code",
  method = "nb",
  features = c("age", "sex", "edu", "exp", "parttime", "salary")
)

testthat::test_that("cat2cat with ml adds expected nb weights", {
  library("e1071")

  ml_nb <- ml_nb_base
  ml_nb$fail_warn <- FALSE

  set.seed(1234)
  result <- cat2cat(
    data = test_data,
    mappings = list(trans = trans, direction = "backward"),
    ml = ml_nb
  )

  testthat::expect_s3_class(result$old, "data.frame")
  testthat::expect_s3_class(result$new, "data.frame")
  testthat::expect_true("wei_nb_c2c" %in% colnames(result$old))
  testthat::expect_true("wei_nb_c2c" %in% colnames(result$new))

  non_na_wei <- result$old$wei_nb_c2c[!is.na(result$old$wei_nb_c2c)]
  testthat::expect_gt(length(non_na_wei), 0)
  testthat::expect_true(all(non_na_wei >= 0 & non_na_wei <= 1))

  wei_sums <- tapply(result$old$wei_nb_c2c, result$old$index_c2c, sum, na.rm = TRUE)
  valid_sums <- wei_sums[wei_sums > 0]
  prop_valid <- mean(abs(valid_sums - 1) < 0.01)
  testthat::expect_gt(prop_valid, 0.5)
})

testthat::test_that("cat2cat ml on_fail controls fallback behavior", {
  library("e1071")

  ml_freq <- ml_nb_base
  ml_freq$on_fail <- "freq"
  freq_result <- NULL
  testthat::expect_warning(
    freq_result <- cat2cat(
      data = test_data,
      mappings = list(trans = trans, direction = "backward"),
      ml = ml_freq
    ),
    "[0-9]+\\.?[0-9]*% rows.*[0-9]+\\.?[0-9]*% observations.*on_fail = 'freq'"
  )

  ml_naive <- ml_nb_base
  ml_naive$on_fail <- "naive"
  naive_result <- NULL
  testthat::expect_warning(
    naive_result <- cat2cat(
      data = test_data,
      mappings = list(trans = trans, direction = "backward"),
      ml = ml_naive
    ),
    "on_fail = 'naive'"
  )

  ml_na <- ml_nb_base
  ml_na$on_fail <- "na"
  na_result <- NULL
  testthat::expect_warning(
    na_result <- cat2cat(
      data = test_data,
      mappings = list(trans = trans, direction = "backward"),
      ml = ml_na
    ),
    "on_fail = 'na'"
  )

  failed_rows <- is.na(na_result$old$wei_nb_c2c)
  testthat::expect_true(any(failed_rows))
  testthat::expect_true(all(!is.na(freq_result$old$wei_nb_c2c[failed_rows])))
  testthat::expect_true(all(!is.na(naive_result$old$wei_nb_c2c[failed_rows])))

  on_fail_diff <- abs(
    freq_result$old$wei_nb_c2c[failed_rows] -
      naive_result$old$wei_nb_c2c[failed_rows]
  )
  on_fail_diff <- on_fail_diff[is.finite(on_fail_diff)]
  testthat::expect_true(any(on_fail_diff > 1e-12))

  ml_error <- ml_nb_base
  ml_error$on_fail <- "error"
  testthat::expect_error(
    cat2cat(
      data = test_data,
      mappings = list(trans = trans, direction = "backward"),
      ml = ml_error
    ),
    "[0-9]+\\.?[0-9]*% rows.*[0-9]+\\.?[0-9]*% observations"
  )
})

testthat::test_that("cat2cat ml fail_warn suppresses fallback warnings", {
  library("e1071")

  ml_quiet <- ml_nb_base
  ml_quiet$on_fail <- "freq"
  ml_quiet$fail_warn <- FALSE

  testthat::expect_warning(
    cat2cat(
      data = test_data,
      mappings = list(trans = trans, direction = "backward"),
      ml = ml_quiet
    ),
    NA
  )
})

testthat::test_that("validate_ml rejects unsupported on_fail", {
  ml_bad <- list(
    data = rbind(occup_2010, occup_2012),
    cat_var = "code",
    method = c("knn", "rf", "lda"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 50),
    on_fail = "unsupported"
  )

  testthat::expect_error(
    validate_ml(ml_bad),
    "`ml\\$on_fail` must be one of"
  )
})

testthat::test_that("encode_factor_features one-hot encodes factor features", {
  train <- data.frame(
    age = c(20, 30, 40),
    grp = factor(c("a", "b", "a")),
    code = c("1", "2", "3"),
    stringsAsFactors = FALSE
  )
  target <- data.frame(
    age = c(25, 35),
    grp = factor(c("a", "c"), levels = c("a", "c")),
    stringsAsFactors = FALSE
  )

  ml <- list(
    data = train,
    cat_var = "code",
    method = "nb",
    features = c("age", "grp")
  )

  out <- encode_factor_features(ml, target)

  testthat::expect_setequal(
    out$ml$features,
    c("age", "grp_a", "grp_b", "grp_c")
  )
  testthat::expect_true(all(out$ml$features %in% colnames(out$ml$data)))
  testthat::expect_true(all(out$ml$features %in% colnames(out$target_data)))

  testthat::expect_equal(out$ml$data$grp_a, c(1L, 0L, 1L))
  testthat::expect_equal(out$ml$data$grp_b, c(0L, 1L, 0L))
  testthat::expect_equal(out$ml$data$grp_c, c(0L, 0L, 0L))
  testthat::expect_equal(out$target_data$grp_a, c(1L, 0L))
  testthat::expect_equal(out$target_data$grp_c, c(0L, 1L))

  # original column preserved
  testthat::expect_true(is.factor(out$ml$data$grp))
})

testthat::test_that("encode_factor_features is a no-op when no factor features", {
  train <- data.frame(age = c(1, 2), x = c(TRUE, FALSE), code = c("a", "b"))
  target <- data.frame(age = c(3, 4), x = c(FALSE, TRUE))
  ml <- list(
    data = train, cat_var = "code", method = "nb",
    features = c("age", "x")
  )
  out <- encode_factor_features(ml, target)
  testthat::expect_identical(out$ml$features, c("age", "x"))
  testthat::expect_identical(colnames(out$ml$data), colnames(train))
  testthat::expect_identical(colnames(out$target_data), colnames(target))
})

testthat::test_that("encode_factor_features does not auto-encode character columns", {
  train <- data.frame(
    age = c(1, 2),
    region = c("n", "s"),
    code = c("a", "b"),
    stringsAsFactors = FALSE
  )
  target <- data.frame(age = c(3, 4), region = c("n", "s"), stringsAsFactors = FALSE)
  ml <- list(
    data = train, cat_var = "code", method = "nb",
    features = c("age", "region")
  )
  out <- encode_factor_features(ml, target)
  testthat::expect_identical(out$ml$features, c("age", "region"))
})

testthat::test_that("brier_score uses the full multiclass probability vector", {
  probs <- data.frame(
    a = c(0.8, 0.1),
    b = c(0.1, 0.6),
    c = c(0.1, 0.3)
  )

  expected <- mean(c(
    ((0.8 - 1)^2 + 0.1^2 + 0.1^2) / 2,
    (0.1^2 + (0.6 - 1)^2 + 0.3^2) / 2
  ))

  testthat::expect_equal(brier_score(probs, c("a", "b"), c("a", "b", "c")), expected)

  same_true_prob_1 <- data.frame(a = 0.6, b = 0.2, c = 0.2)
  same_true_prob_2 <- data.frame(a = 0.6, b = 0.4, c = 0.0)
  testthat::expect_gt(
    brier_score(same_true_prob_2, "a", c("a", "b", "c")),
    brier_score(same_true_prob_1, "a", c("a", "b", "c"))
  )
})

testthat::test_that("cat2cat with ml automatically one-hot encodes factor features", {
  library("e1071")

  occup_2008_f <- occup_2008
  occup_2010_f <- occup_2010
  occup_2012_f <- occup_2012

  # Convert a numeric column into a factor
  occup_2008_f$edu <- factor(occup_2008_f$edu)
  occup_2010_f$edu <- factor(occup_2010_f$edu)
  occup_2012_f$edu <- factor(occup_2012_f$edu)

  test_data_f <- list(
    old = occup_2008_f,
    new = occup_2010_f,
    cat_var = "code",
    time_var = "year"
  )

  ml_nb <- list(
    data = rbind(occup_2010_f, occup_2012_f),
    cat_var = "code",
    method = "nb",
    features = c("age", "sex", "edu", "salary"),
    fail_warn = FALSE
  )

  set.seed(1234)
  result <- cat2cat(
    data = test_data_f,
    mappings = list(trans = trans, direction = "backward"),
    ml = ml_nb
  )

  testthat::expect_s3_class(result$old, "data.frame")
  testthat::expect_true("wei_nb_c2c" %in% colnames(result$old))
  # original factor column preserved
  testthat::expect_true(is.factor(result$old$edu))

  non_na_wei <- result$old$wei_nb_c2c[!is.na(result$old$wei_nb_c2c)]
  testthat::expect_gt(length(non_na_wei), 0)
  testthat::expect_true(all(non_na_wei >= 0 & non_na_wei <= 1))
})
