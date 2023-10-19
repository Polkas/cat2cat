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

  res <- cat2cat_ml_run(mappings, ml_setup, test_prop = 0.2)
  testthat::expect_s3_class(res, c("cat2cat_ml_run", "list"))
  testthat::expect_output(print(res), "Selected prediction stats:")
  testthat::expect_output(print(res), "Percent of failed knn ml models: 32.73")
})

testthat::test_that("cat2cat_ml_run wrong direction", {
  mappings <- list(trans = trans, direction = "forward")

  testthat::expect_error(
    cat2cat_ml_run(mappings, ml_setup, test_prop = 0.2),
    "There is no mappings to group the cat_var variable"
  )
})
