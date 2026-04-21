data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_old <- occup[occup$year == 2008, ]
occup_new <- occup[occup$year == 2010, ]

testthat::test_that("validate_mappings incorrect", {
  testthat::expect_error(
    validate_mappings(list()), "`mappings` must contain 'trans' and 'direction'"
  )
})

testthat::test_that("validate_mappings correct", {
  mappings_simple_back <- list(trans = trans, direction = "backward")
  mappings_simple_for <- list(trans = trans, direction = "forward")
  testthat::expect_silent(validate_mappings(mappings_simple_back))
  testthat::expect_silent(validate_mappings(mappings_simple_for))
})

testthat::test_that("validate_data incorrect", {
  testthat::expect_error(validate_data(list()), "`data\\$old` must be a data.frame")

  data_simple <- list(old = occup_old, new = occup_new, cat_var_old = "code", cat_var_new = "code", time_var = "WRONG")

  testthat::expect_error(
    validate_data(data_simple),
    "`cat_var_old` and `time_var` must be columns in `data\\$old`"
  )

  data_simple <- list(old = occup_old, new = occup_new, cat_var_old = "WRONG", cat_var_new = "code", time_var = "year")

  testthat::expect_error(
    validate_data(data_simple),
    "`cat_var_old` and `time_var` must be columns in `data\\$old`"
  )

  data_simple <- list(old = occup_old, new = occup_new, cat_var_old = "code", cat_var_new = "WRONG", time_var = "year")

  testthat::expect_error(
    validate_data(data_simple),
    "`cat_var_new` and `time_var` must be columns in `data\\$new`"
  )

})

testthat::test_that("validate_data correct", {
  data_simple <- list(
    old = occup_old, new = occup_new, cat_var_old = "code", cat_var_new = "code", time_var = "year"
  )
  testthat::expect_silent(validate_data(data_simple))
})

testthat::test_that("validate_ml incorrect", {
  testthat::expect_error(
    validate_ml(list()), "`ml` must contain 'method', 'features', and 'data'"
  )
})

testthat::test_that("validate_ml correct", {
  ml <- list(
    data = occup_new,
    cat_var = "code",
    method = c("knn", "rf", "lda"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 30)
  )
  testthat::expect_silent(validate_ml(ml))
})
