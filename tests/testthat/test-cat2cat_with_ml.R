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
