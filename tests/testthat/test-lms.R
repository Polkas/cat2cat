aa <- airquality
aa2 <- rbind(aa, aa)

testthat::test_that("summary_c2c is properly adjust the std error", {
  ll <- lm(Ozone ~ ., aa)
  ss <- summary(ll)

  ll2 <- lm(Ozone ~ ., aa2)
  ss2 <- summary_c2c(ll2, ll$df.residual, ll2$df.residual)

  expect_equal(unname(ss$coefficients[, 3]), ss2[, 7])
})

data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_old <- occup[occup$year == 2008, ]
occup_new <- occup[occup$year == 2010, ]

data_simple <- list(
  old = occup_old, new = occup_new, cat_var = "code", time_var = "year"
)
mappings_simple_back <- list(trans = trans, direction = "backward")
ml <- list(
  data = occup_new,
  cat_var = "code",
  method = c("knn", "rf", "lda"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10, ntree = 30)
)

testthat::test_that(
  "summary_c2c is properly adjust the std error - cat2cat case", {
  occup <- cat2cat(
    data = data_simple,
    mappings = mappings_simple_back,
    ml = ml
  )

  lms <- lm(
    I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup$old,
    weights = multiplier * wei_freq_c2c
  )

  ss_c2c <- summary_c2c(lms, df_old = nrow(occup_old) - length(lms$assign))
  lms$df.residual <- nrow(occup_old) - length(lms$assign)
  ss1 <- suppressWarnings(summary(lms))

  lms2 <- lm(
    I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old,
    weights = multiplier
  )
  ss2 <- summary(lms2)

  expect_true(sum((ss2$coefficients[, 2] - ss1$coefficients[, 2])**2) < 0.01)
  expect_true(sum((ss2$coefficients[, 2] - ss_c2c$std.error_c)**2) < 0.01)
})
