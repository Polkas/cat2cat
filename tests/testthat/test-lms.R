aa <- airquality

aa2 <- rbind(aa, aa)
ll <- lm(Ozone ~ ., aa)
ss <- summary(ll)

ll2 <- lm(Ozone ~ ., aa2)
ss2 <- summary_c2c(ll2, ll$df.residual, ll2$df.residual)

expect_equal(unname(ss$coefficients[, 3]), ss2[, 7])
