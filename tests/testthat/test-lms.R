set.seed(1234)

aa <- airquality
aa2 <- rbind(aa, aa)

testthat::test_that("summary_c2c is properly adjust the std error", {
  ll <- lm(Ozone ~ ., aa)
  ss <- summary(ll)

  ll2 <- lm(Ozone ~ ., aa2)
  ss2 <- summary_c2c(ll2, ll$df.residual, ll2$df.residual)

  expect_equal(unname(ss$coefficients[, 3]), ss2[, 7], tolerance = 1e-8)
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
  args = list(k = 10, ntree = 30),
  fail_warn = FALSE
)

testthat::test_that(
  "summary_c2c is properly adjust the std error - cat2cat case",
  {
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
  }
)

# -----------------------------------------------------------------------------
# summary_c2c with occup_panel (id_var case)
# -----------------------------------------------------------------------------

data("occup_panel", package = "cat2cat")

panel_2009Q4 <- occup_panel[occup_panel$quarter == "2009Q4", ]
panel_2010Q1 <- occup_panel[occup_panel$quarter == "2010Q1", ]

testthat::test_that(
  "summary_c2c corrects SE with occup_panel id_var data",
  {
    result <- cat2cat(
      data = list(
        old = panel_2009Q4,
        new = panel_2010Q1,
        id_var = "panel_id",
        cat_var = "code",
        time_var = "quarter"
      ),
      mappings = list(trans = trans, direction = "backward")
    )

    # Fit model on replicated data
    lm_rep <- lm(
      I(log(salary)) ~ age + sex + factor(edu),
      data = result$old,
      weights = wei_freq_c2c
    )

    # Get corrected SE
    df_orig <- nrow(panel_2009Q4) - length(coef(lm_rep))
    ss_c2c <- summary_c2c(lm_rep, df_old = df_orig)

    # Corrected SE should be larger than naive SE (replication inflates df)
    naive_se <- summary(lm_rep)$coefficients[, "Std. Error"]
    expect_true(all(ss_c2c$std.error_c >= naive_se * 0.99))

    # Corrected stats should use original df
    expect_equal(nrow(ss_c2c), length(coef(lm_rep)))
    expect_true(all(c("statistic_c", "p.value_c", "std.error_c") %in% names(ss_c2c)))
  }
)

testthat::test_that(

  "summary_c2c SE correction ratio matches replication factor",
  {
    result <- cat2cat(
      data = list(
        old = panel_2009Q4,
        new = panel_2010Q1,
        cat_var = "code",
        time_var = "quarter"
      ),
      mappings = list(trans = trans, direction = "backward")
    )

    lm_rep <- lm(
      I(log(salary)) ~ age + sex,
      data = result$old,
      weights = wei_freq_c2c
    )

    df_orig <- nrow(panel_2009Q4) - length(coef(lm_rep))
    df_rep <- nrow(result$old) - length(coef(lm_rep))
    ss_c2c <- summary_c2c(lm_rep, df_old = df_orig, df_new = df_rep)

    # SE correction factor should be sqrt(df_rep / df_orig)
    expected_factor <- sqrt(df_rep / df_orig)
    naive_se <- summary(lm_rep)$coefficients[, "Std. Error"]
    actual_factor <- unname(ss_c2c$std.error_c[1] / naive_se[1])

    expect_equal(actual_factor, expected_factor, tolerance = 0.01)
  }
)

testthat::test_that(
  "summary_c2c handles glm gaussian path",
  {
    aa_glm <- na.omit(airquality)
    aa_glm2 <- rbind(aa_glm, aa_glm)

    g1 <- glm(Ozone ~ Solar.R + Wind + Temp,
              data = aa_glm, family = gaussian())
    g2 <- glm(Ozone ~ Solar.R + Wind + Temp,
              data = aa_glm2, family = gaussian())

    ss1 <- summary(g1)$coefficients[, 3]
    ss2 <- summary_c2c(g2, g1$df.residual, g2$df.residual)

    expect_true(all(c("std.error_c", "statistic_c", "p.value_c", "reference_dist") %in% names(ss2)))
    expect_equal(unname(ss1), ss2$statistic_c, tolerance = 0.06)
    expect_true(all(ss2$reference_dist == "t"))
  }
)

testthat::test_that(
  "summary_c2c handles glm binomial path",
  {
    d1 <- mtcars
    d2 <- rbind(d1, d1)

    g1 <- glm(vs ~ mpg + wt,
              data = d1, family = binomial())
    g2 <- glm(vs ~ mpg + wt,
              data = d2, family = binomial())

    ss1 <- summary(g1)$coefficients[, "z value"]
    ss2 <- summary_c2c(g2, g1$df.residual, g2$df.residual)

    expect_true(all(c("std.error_c", "statistic_c", "p.value_c", "reference_dist") %in% names(ss2)))
    expect_equal(unname(ss1), ss2$statistic_c, tolerance = 0.06)
    expect_true(all(ss2$reference_dist == "normal"))
  }
)

testthat::test_that(
  "summary_c2c rejects invalid df inputs",
  {
    ll <- lm(Ozone ~ Solar.R + Wind + Temp, data = na.omit(airquality))

    expect_error(summary_c2c(ll, df_old = 0), "`df_old` must be a single positive finite numeric value.")
    expect_error(summary_c2c(ll, df_old = NA_real_), "`df_old` must be a single positive finite numeric value.")
    expect_error(summary_c2c(ll, df_old = ll$df.residual, df_new = -1), "`df_new` must be a single positive finite numeric value.")
  }
)

testthat::test_that(
  "summary_c2c rejects unsupported coefficient layout",
  {
    summary.badglm <- function(object, ...) {
      list(coefficients = cbind("Estimate" = c(1, 2), "Std. Error" = c(0.1, 0.2)))
    }
    assign("summary.badglm", summary.badglm, envir = .GlobalEnv)
    on.exit(rm("summary.badglm", envir = .GlobalEnv), add = TRUE)

    bad <- structure(list(df.residual = 10), class = c("badglm", "glm", "lm"))

    expect_error(
      summary_c2c(bad, df_old = 10, df_new = 20),
      "must contain either 't value' or 'z value'"
    )
  }
)

testthat::test_that(
  "summary_c2c rejects non-tabular coefficients object",
  {
    summary.badcoef <- function(object, ...) {
      list(coefficients = c(1, 2, 3))
    }
    assign("summary.badcoef", summary.badcoef, envir = .GlobalEnv)
    on.exit(rm("summary.badcoef", envir = .GlobalEnv), add = TRUE)

    bad <- structure(list(df.residual = 10), class = c("badcoef", "lm"))

    expect_error(
      summary_c2c(bad, df_old = 10, df_new = 20),
      "must be a matrix or data.frame"
    )
  }
)

testthat::test_that(
  "summary_c2c requires Std. Error column",
  {
    summary.nose <- function(object, ...) {
      list(coefficients = cbind("Estimate" = c(1, 2), "t value" = c(3, 4)))
    }
    assign("summary.nose", summary.nose, envir = .GlobalEnv)
    on.exit(rm("summary.nose", envir = .GlobalEnv), add = TRUE)

    bad <- structure(list(df.residual = 10), class = c("nose", "lm"))

    expect_error(
      summary_c2c(bad, df_old = 10, df_new = 20),
      "must contain a 'Std. Error' column"
    )
  }
)
