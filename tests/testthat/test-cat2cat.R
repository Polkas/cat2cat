data(occup)
data(trans)

occup_old <- occup[occup$year == 2008, ]
occup_new <- occup[occup$year == 2010, ]

occup_1a <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

expect_true(all(occup_1a$old$wei_freq_c2c <= 1 & occup_1a$old$wei_freq_c2c >= 0))
expect_true(all(occup_1a$new$wei_freq_c2c <= 1 & occup_1a$new$wei_freq_c2c >= 0))

expect_equal(sum(occup_1a$old$wei_naive_c2c), nrow(occup_old))
expect_equal(sum(occup_1a$old$wei_freq_c2c), nrow(occup_old))

occup_2 <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = "knn", features = c("age", "sex", "edu", "exp", "parttime", "salary"), args = list(k = 10))
)

expect_equal(sum(occup_2$old$wei_freq_c2c), nrow(occup_old))
expect_equal(sum(occup_2$old$wei_ml_c2c), nrow(occup_old))
expect_true((all(occup_2$old$wei_ml_c2c <= 1 & occup_2$old$wei_ml_c2c >= 0)))

expect_equal(sum((occup_2$old$wei_ml_c2c + occup_2$old$wei_freq_c2c + occup_2$old$wei_naive_c2c) / 3), nrow(occup_old))

lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_2$old, weights = multipier * wei_freq_c2c)

ss_c2c <- summary_c2c(lms, df_old = nrow(occup_old), df_new = nrow(occup_2$old))

lms2 <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old, weights = multipier)

ss <- summary(lms2)

expect_true(sum((ss$coefficients[, 2] - ss_c2c$std.error_c)**2) < 0.01)

occup_3 <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year", multipier_var = "multipier"),
  mappings = list(trans = trans, direction = "backward")
)

expect_equal(sum(occup_3$old$wei_freq_c2c), nrow(occup_old))

expect_true((all(occup_3$old$wei_freq_c2c <= 1 & occup_3$old$wei_freq_c2c >= 0)))

expect_false(identical(occup_3$old$wei_freq_c2c, occup_1a$old$wei_freq_c2c))

occup_4 <- cat2cat(
  data = list(
    old = occup_old, new = occup_new, cat_var = "code", time_var = "year",
    freqs_df = as.data.frame(table(occup_new$code))
  ),
  mappings = list(trans = trans, direction = "backward")
)

expect_true(identical(occup_4$old$wei_freq_c2c, occup_1a$old$wei_freq_c2c))
