data("occup", package = "cat2cat")
# Split the panel dataset to separate years
occup_2006 <- occup[occup$year == 2006, ]
occup_2008 <- occup_old <- occup[occup$year == 2008, ]
occup_2010 <- occup_new <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]

occup_simple_backward <- cat2cat::cat2cat(
  data = list(
    old = occup_old,
    new = occup_new,
    cat_var = "code",
    time_var = "year"
  ),
  mappings = list(
    trans = trans,
    direction = "backward"
  )
)

testthat::test_that("Simple cat2cat example - two periods - backward", {
  testthat::expect_true(all(names(occup_simple_backward) %in% c("old", "new")))
  testthat::expect_true(is.data.frame(occup_simple_backward$old))
  testthat::expect_true(is.data.frame(occup_simple_backward$new))
})

# Adding the dummy level to the mapping table for levels without the candidate
# The best to fill them manually with proper candidates, if possible
trans2 <- rbind(trans, data.frame(
  old = "no_cat",
  new = setdiff(occup_new$code, trans$new)
))
# Forward mapping for the case with two periods
occup_simple_forward <- cat2cat::cat2cat(
  data = list(
    old = occup_old,
    new = occup_new,
    cat_var = "code",
    time_var = "year"
  ),
  mappings = list(
    trans = trans2,
    direction = "forward"
  )
)

testthat::test_that("Simple cat2cat example - two periods - forward", {
  testthat::expect_true(all(names(occup_simple_forward) %in% c("old", "new")))
  testthat::expect_true(is.data.frame(occup_simple_forward$old))
  testthat::expect_true(is.data.frame(occup_simple_forward$new))
})

testthat::test_that("Table with number of replications for both mapping  directions", {
  # Build number of observations before and after unification table
  res <- data.frame(
    `before_mapping` = c(nrow(occup_old), nrow(occup_new)),
    `after_mapping` = c(
      paste0(
        nrow(occup_simple_backward$old),
        " (nonzero ",
        sum(occup_simple_backward$old$wei_freq_c2c > 0),
        ")"
      ),
      paste0(
        nrow(occup_simple_forward$new),
        " (nonzero ",
        sum(occup_simple_forward$new$wei_freq_c2c > 0),
        ")"
      )
    )
  )
  rownames(res) <- c("old (backward)", "new (forward)")
  res_tab <- knitr::kable(
    res,
    "latex",
    caption = "Number of observations before and after unification."
  )
  testthat::expect_identical(
    res_tab,
    structure(
      "\\begin{table}\n\n\\caption{Number of observations before and after unification.}\n\\centering\n\\begin{tabular}[t]{l|r|l}\n\\hline\n  & before\\_mapping & after\\_mapping\\\\\n\\hline\nold (backward) & 17223 & 227662 (nonzero 163262)\\\\\n\\hline\nnew (forward) & 17323 & 18680 (nonzero 18517)\\\\\n\\hline\n\\end{tabular}\n\\end{table}",
      format = "latex",
      class = "knitr_kable"
    )
  )
})

# Set the seed as e.g., randomForest is used
set.seed(1234)
# Statistical models setup
# It could be shared for different iterations for this scenario
ml_setup <- list(
  data = rbind(occup_2010, occup_2012),
  cat_var = "code",
  method = c("knn", "rf"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10)
)
# Use the cat2cat procedure to map 2010 to 2008
occup_back_2008_2010 <- cat2cat::cat2cat(
  data = list(
    old = occup_2008,
    new = occup_2010,
    cat_var = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = ml_setup
)
# Use the cat2cat procedure to map 2008 to 2006
occup_back_2006_2008 <- cat2cat::cat2cat(
  data = list(
    old = occup_2006,
    new = occup_back_2008_2010$old,
    cat_var_new = "g_new_c2c",
    cat_var_old = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = ml_setup
)
# Select a proper dataset for each year
o_2006 <- occup_back_2006_2008$old
o_2008 <- occup_back_2008_2010$old # or occup_back_2006_2008$new
o_2010 <- occup_back_2008_2010$new
# Add default cat2cat procedure columns to not processed dataset
o_2012 <- cat2cat::dummy_c2c(occup_2012, cat_var = "code", ml = c("knn", "rf"))
# Combine datasets
final_data_back_ml <- do.call(rbind, list(o_2006, o_2008, o_2010, o_2012))

testthat::test_that("Backward mapping, with four periods, one mapping table, and ml models", {
  testthat::expect_true(is.data.frame(final_data_back_ml))
  testthat::expect_identical(
    dim(final_data_back_ml),
    c(475543L, 19L)
  )
  testthat::expect_identical(
    colnames(final_data_back_ml),
    c("id", "age", "sex", "edu", "exp", "district", "parttime", "salary",
      "code", "multiplier", "year", "code4", "index_c2c", "g_new_c2c",
      "wei_freq_c2c", "rep_c2c", "wei_naive_c2c", "wei_knn_c2c", "wei_rf_c2c"
    )
  )
})

testthat::test_that("Correlations between ml methods", {
  corr_separate <-
    dplyr::filter(
      dplyr::do(
        dplyr::select(
          dplyr::select(
            dplyr::group_by(
              dplyr::filter(final_data_back_ml, rep_c2c >= 10),
              index_c2c
            ),
            matches("wei.*c2c")
          ),
          -wei_naive_c2c
        ),
        corr = tryCatch(
          cor(.[, -1]),
          error = function(e) NA, warning = function(w) NA
        )
      ), !any(is.na(corr))
    )
  corr_table <- Reduce("+", corr_separate$corr) / length(corr_separate$corr)
  # Build average correlations between different statistical methods table
  res_tab <- knitr::kable(
    corr_table,
    "latex",
    caption = paste(
      "Average correlations between different methods \n",
      "assesed for each subject with at least 10 obs."
    )
  )
  testthat::expect_identical(
    res_tab,
    structure(
      "\\begin{table}\n\n\\caption{Average correlations between different methods \n assesed for each subject with at least 10 obs.}\n\\centering\n\\begin{tabular}[t]{l|r|r|r}\n\\hline\n  & wei\\_freq\\_c2c & wei\\_knn\\_c2c & wei\\_rf\\_c2c\\\\\n\\hline\nwei\\_freq\\_c2c & 1.0000000 & 0.7509772 & 0.7587164\\\\\n\\hline\nwei\\_knn\\_c2c & 0.7509772 & 1.0000000 & 0.7143120\\\\\n\\hline\nwei\\_rf\\_c2c & 0.7587164 & 0.7143120 & 1.0000000\\\\\n\\hline\n\\end{tabular}\n\\end{table}",
      format = "latex",
      class = "knitr_kable"
    )
  )
})

testthat::test_that("Counts for a few random levels in the unified variable over time", {
  data_count_plot <- cat2cat::prune_c2c(df = final_data_back_ml)
  data_count_plot2 <-
    dplyr::summarise_all(
      dplyr::group_by(
        dplyr::select(
          dplyr::mutate(
            dplyr::filter(
              data_count_plot,
              g_new_c2c %in% c("261102", "325502", "352111")
            ),
            g_new_c2c_nams = forcats::fct_recode(
              g_new_c2c,
              `OHS Inspector` = "325502",
              `Sound Engineer` = "352111",
              `Prosecutor` = "261102"
            )
          ),
          "wei_freq_c2c", "wei_rf_c2c", "year", "g_new_c2c_nams", -"g_new_c2c"
        ),
        g_new_c2c_nams, year
      ),
    sum
  )
  # Build counts across a 3 random categories table
  counts_base <-
    tidyr::pivot_wider(
      data_count_plot2,
      names_from = "year",
      values_from = c("wei_freq_c2c", "wei_rf_c2c"),
      names_sep = " "
    )
  res_tab <-
    knitr::kable(
      dplyr::select(
        counts_base,
        g_new_c2c_nams,
        `rf 2006` = `wei_rf_c2c 2006`,
        `freq 2006` = `wei_freq_c2c 2006`,
        `rf 2008` = `wei_rf_c2c 2008`,
        `freq 2008` = `wei_freq_c2c 2008`,
        `2010` = `wei_rf_c2c 2010`,
        `2012` = `wei_rf_c2c 2012`
      ),
      "latex",
      caption = "Counts across a 3 random categories."
    )
  testthat::expect_identical(
    res_tab,
    structure(
      "\\begin{table}\n\n\\caption{Counts across a 3 random categories.}\n\\centering\n\\begin{tabular}[t]{l|r|r|r|r|r|r}\n\\hline\ng\\_new\\_c2c\\_nams & rf 2006 & freq 2006 & rf 2008 & freq 2008 & 2010 & 2012\\\\\n\\hline\nProsecutor & 7.13 & 5.2250000 & 11.98 & 4.8291667 & 19 & 12\\\\\n\\hline\nOHS Inspector & 14.13 & 3.9707031 & 16.49 & 4.7044025 & 16 & 12\\\\\n\\hline\nSound Engineer & 0.21 & 0.0714286 & 0.87 & 0.2142857 & 1 & 1\\\\\n\\hline\n\\end{tabular}\n\\end{table}",
      format = "latex",
      class = "knitr_kable"
    )
  )
})

# Mincerian-like regression formula
formula_micer <- I(log(salary)) ~ sex + parttime + edu + exp + I(exp**2)

testthat::test_that("Regression - neutral impact of the unified variable", {
  # Fit a weighted linear regression on replicated
  lms_replicated <- lm(
    formula = formula_micer,
    data = final_data_back_ml,
    weights = multiplier * wei_freq_c2c
  )
  # Adjust size of stds as the replication process enlarges degrees of freedom
  lms_replicated$df.residual <- nrow(occup) - length(lms_replicated$assign)
  # Fit a linear regression on original dataset
  lms_original <- lm(
    formula = formula_micer,
    data = occup,
    weights = multiplier
  )
  # Build a table with regression results
  res_tab <- capture.output(stargazer::stargazer(
    lms_replicated,
    lms_original,
    header = FALSE,
    column.labels = c("replicated", "original"),
    omit.stat = "adj.rsq",
    label = "regression_res",
    title = "Regressions for original and replicated data."
  ))

  testthat::expect_identical(
    res_tab,
    c("", "\\begin{table}[!htbp] \\centering ", "  \\caption{Regressions for original and replicated data.} ",
      "  \\label{regression_res} ", "\\begin{tabular}{@{\\extracolsep{5pt}}lcc} ",
      "\\\\[-1.8ex]\\hline ", "\\hline \\\\[-1.8ex] ", " & \\multicolumn{2}{c}{\\textit{Dependent variable:}} \\\\ ",
      "\\cline{2-3} ", "\\\\[-1.8ex] & \\multicolumn{2}{c}{I(log(salary))} \\\\ ",
      " & replicated & original \\\\ ", "\\\\[-1.8ex] & (1) & (2)\\\\ ",
      "\\hline \\\\[-1.8ex] ", " sex & 0.254$^{***}$ & 0.254$^{***}$ \\\\ ",
      "  & (0.004) & (0.004) \\\\ ", "  & & \\\\ ", " parttime & 1.964$^{***}$ & 1.964$^{***}$ \\\\ ",
      "  & (0.009) & (0.009) \\\\ ", "  & & \\\\ ", " edu & $-$0.120$^{***}$ & $-$0.120$^{***}$ \\\\ ",
      "  & (0.001) & (0.001) \\\\ ", "  & & \\\\ ", " exp & 0.020$^{***}$ & 0.020$^{***}$ \\\\ ",
      "  & (0.001) & (0.001) \\\\ ", "  & & \\\\ ", " I(exp$\\hat{\\mkern6mu}$2) & $-$0.0002$^{***}$ & $-$0.0002$^{***}$ \\\\ ",
      "  & (0.00001) & (0.00001) \\\\ ", "  & & \\\\ ", " Constant & 8.573$^{***}$ & 8.573$^{***}$ \\\\ ",
      "  & (0.009) & (0.009) \\\\ ", "  & & \\\\ ", "\\hline \\\\[-1.8ex] ",
      "Observations & 475,543 & 69,126 \\\\ ", "R$^{2}$ & 0.566 & 0.566 \\\\ ",
      "Residual Std. Error (df = 69120) & 10.214 & 10.214 \\\\ ", "F Statistic (df = 5; 69120) & 18,019.060$^{***}$ & 18,019.060$^{***}$ \\\\ ",
      "\\hline ", "\\hline \\\\[-1.8ex] ", "\\textit{Note:}  & \\multicolumn{2}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\ ",
      "\\end{tabular} ", "\\end{table} ")
  )
})

testthat::test_that("Regression for each level in the unified variable", {
  # Separate regression for each occupational group under newest classification
  regression_sep <-
    dplyr::filter(
      dplyr::do(
        dplyr::filter(
          dplyr::mutate(
            dplyr::group_by(
              cat2cat::prune_c2c(df = final_data_back_ml, method = "nonzero"),
              g_new_c2c
            ),
            n = dplyr::n()
          ),
          n >= 30
        ),
        lm = tryCatch(
          lm(formula_micer, .data, weights = multiplier * wei_freq_c2c),
          error = function(e) NULL
        )
      ),
      !is.null(lm)
    )
  # Regression results for the first group
  res_tab <- capture.output(stargazer::stargazer(
    regression_sep$lm[[1]],
    header = FALSE,
    label = "regression_sep",
    title = "Regression for one of the unified vairable levels."
  ))
  testthat::expect_identical(
    res_tab,
    c("", "\\begin{table}[!htbp] \\centering ", "  \\caption{Regression for one of the unified vairable levels.} ",
      "  \\label{regression_sep} ", "\\begin{tabular}{@{\\extracolsep{5pt}}lc} ",
      "\\\\[-1.8ex]\\hline ", "\\hline \\\\[-1.8ex] ", " & \\multicolumn{1}{c}{\\textit{Dependent variable:}} \\\\ ",
      "\\cline{2-2} ", "\\\\[-1.8ex] & I(log(salary)) \\\\ ", "\\hline \\\\[-1.8ex] ",
      " sex & 0.515$^{***}$ \\\\ ", "  & (0.100) \\\\ ", "  & \\\\ ",
      " parttime & 1.870$^{***}$ \\\\ ", "  & (0.496) \\\\ ", "  & \\\\ ",
      " edu & $-$0.253$^{***}$ \\\\ ", "  & (0.071) \\\\ ", "  & \\\\ ",
      " exp & 0.001 \\\\ ", "  & (0.018) \\\\ ", "  & \\\\ ", " I(exp$\\hat{\\mkern6mu}$2) & 0.0001 \\\\ ",
      "  & (0.0004) \\\\ ", "  & \\\\ ", " Constant & 9.500$^{***}$ \\\\ ",
      "  & (0.461) \\\\ ", "  & \\\\ ", "\\hline \\\\[-1.8ex] ", "Observations & 66 \\\\ ",
      "R$^{2}$ & 0.601 \\\\ ", "Adjusted R$^{2}$ & 0.568 \\\\ ", "Residual Std. Error & 1.717 (df = 60) \\\\ ",
      "F Statistic & 18.107$^{***}$ (df = 5; 60) \\\\ ", "\\hline ",
      "\\hline \\\\[-1.8ex] ", "\\textit{Note:}  & \\multicolumn{1}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\ ",
      "\\end{tabular} ", "\\end{table} ")
  )
})
