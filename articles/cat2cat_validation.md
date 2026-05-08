# Choosing Weights and Validating ML

This vignette is a decision guide for choosing and checking weights in
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md).

Read it when you want to answer one of these questions:

- Are naive and frequency weights telling the same story?
- Is ML worth trying at all?
- If ML is used, does it improve on the frequency baseline?
- What should I do when different weight methods disagree?
- How should I handle failed ML predictions?

If you only need the basic two-period workflow, go back to [Get
Started](https://polkas.github.io/cat2cat/articles/cat2cat.md). If you
need multi-period, panel, aggregated, or regression workflows, continue
to [Advanced
Workflows](https://polkas.github.io/cat2cat/articles/cat2cat_advanced.md).

``` r

library(cat2cat)
library(dplyr)
library(tidyr)
library(e1071)
library(randomForest)

data(occup, package = "cat2cat")
data(trans, package = "cat2cat")

occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]
```

## Step 1: Understand the competing weight assumptions

`cat2cat` offers several ways to assign probability weights to
replicated observations. Each method encodes a different
**distributional assumption** about how ambiguous observations split
across candidate categories. When a downstream estimand depends on the
mapped category, this is the identifying assumption for that estimand -
so always check sensitivity.

**Naive weights** (`wei_naive_c2c`) are always computed. Each replicated
observation gets uniform probability $`1/k`$ where $`k`$ is the number
of candidate categories.

- *Assumption*: All candidates equally likely (maximum entropy /
  uninformative prior)
- *Requires*: Only the mapping table - no data from either period
- *Use when*: No information favoring any candidate, or as a robustness
  lower bound

**Frequency-based weights** (`wei_freq_c2c`) are the default. They use
category counts from the base period.

- *Assumption*: Ambiguous observations distribute like the base period
  population
- *Requires*: Observed counts in base period (falls back to naive if all
  zero)
- *Use when*: Base period is large and representative; ambiguous cases
  resemble the general population

**ML weights** (`wei_knn_c2c`, `wei_lda_c2c`, `wei_rf_c2c`,
`wei_nb_c2c`) use individual features to predict category membership.

- *Assumption*: Features (age, education, etc.) predict true category:
  $`P(j \mid X, g)`$
- *Requires*: Training data with both category labels and predictive
  features
- *Use when*: Features are informative - verify with
  [`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md)

Available ML methods:

- **knn**: k-Nearest Neighbours. A non-parametric method that handles
  non-linear boundaries. Sensitive to the choice of `k`.
- **lda**: Linear Discriminant Analysis. Fast, interpretable. Assumes
  multivariate normality and equal covariance.
- **rf**: Random Forest. Handles interactions well. Slower, needs
  `ntree` tuning.
- **nb**: Naive Bayes via `e1071`. Fast, handles mixed types. Assumes
  conditional independence of features.

You can run multiple methods at once and compare or combine them:

``` r

occup_2_mix <- cat2cat(
  data = list(
    old = occup_2008, new = occup_2010,
    cat_var = "code", time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_2010,
    cat_var = "code",
    method = c("knn", "rf", "lda", "nb"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 50),
    on_fail = "na"
  )
)
```

Correlations between weight methods:

``` r

occup_2_mix$old %>%
  select(wei_knn_c2c, wei_rf_c2c, wei_lda_c2c, wei_nb_c2c, wei_freq_c2c, wei_naive_c2c) %>%
    cor(use = "pairwise.complete.obs")
#>               wei_knn_c2c wei_rf_c2c wei_lda_c2c wei_nb_c2c wei_freq_c2c
#> wei_knn_c2c     1.0000000  0.8645474   0.8327864  0.6196138    0.8989887
#> wei_rf_c2c      0.8645474  1.0000000   0.8828298  0.6532996    0.8749677
#> wei_lda_c2c     0.8327864  0.8828298   1.0000000  0.6592195    0.8667809
#> wei_nb_c2c      0.6196138  0.6532996   0.6592195  1.0000000    0.6107475
#> wei_freq_c2c    0.8989887  0.8749677   0.8667809  0.6107475    1.0000000
#> wei_naive_c2c   0.4908619  0.4744875   0.4811839  0.5594270    0.5449029
#>               wei_naive_c2c
#> wei_knn_c2c       0.4908619
#> wei_rf_c2c        0.4744875
#> wei_lda_c2c       0.4811839
#> wei_nb_c2c        0.5594270
#> wei_freq_c2c      0.5449029
#> wei_naive_c2c     1.0000000
```

### If ML fails on some rows: `on_fail` and `fail_warn`

Sometimes ML probabilities cannot be produced for a subset of replicated
rows (for example incomplete target features or method-specific
prediction failures).
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md)
exposes explicit policy controls in `ml`:

- `on_fail = "freq"` (default): failed ML rows are filled with
  `wei_freq_c2c`.
- `on_fail = "naive"`: failed ML rows are filled with `wei_naive_c2c`.
- `on_fail = "na"`: failed ML rows are kept as `NA`.
- `on_fail = "error"`: stop immediately when failed rows are detected.
- `fail_warn = TRUE` (default): warn with affected rows/observations per
  method.
- `fail_warn = FALSE`: suppress these warnings.

Important: this failure accounting is specific to
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md) and
the constructed weight columns (`wei_*_c2c`). It is different from
[`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md)
“SKIPPED GROUPS”, which reports mapping groups that were not evaluated
in holdout diagnostics (single category, too few observations, or method
fit/predict error for that group).

``` r

ml_setup <- list(
  data = bind_rows(occup_2010, occup_2012),
  cat_var = "code",
  method = c("knn", "rf", "lda"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10, ntree = 50),
  on_fail = "freq",   # default policy
  fail_warn = TRUE     # default reporting
)

# strict mode for QA pipelines
ml_strict <- ml_setup
ml_strict$on_fail <- "error"

# diagnostic mode to inspect failures directly
ml_diag <- ml_setup
ml_diag$on_fail <- "na"
ml_diag$fail_warn <- FALSE
```

Ensemble weights with
[`cross_c2c()`](https://polkas.github.io/cat2cat/reference/cross_c2c.md)
and pruning with
[`prune_c2c()`](https://polkas.github.io/cat2cat/reference/prune_c2c.md):

``` r

occup_old_mix <- occup_2_mix$old %>%
  cross_c2c(.) %>%
  prune_c2c(., column = "wei_cross_c2c", method = "nonzero")
```

## Step 2: Check whether conclusions are sensitive to the weight choice

Different weight methods affect regression coefficients when you filter
to a specific occupation group and combine both periods. This is the
proper sensitivity analysis: subjects from the base period (new, no
replication) plus subjects from the target period (old, weighted by
probability of belonging to this group).

### Compare weight methods on the same mapped data

Run backward mapping with all ML methods:

``` r

result_all <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_2010, cat_var = "code",
    method = c("knn", "rf", "lda", "nb"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 50)
  )
)
```

**Weighted counts per group** - compare how weight methods redistribute
observations:

``` r

weight_cols <- c("wei_naive_c2c", "wei_freq_c2c", "wei_knn_c2c", "wei_rf_c2c", "wei_lda_c2c", "wei_nb_c2c")

# Pick groups with high replication
top_groups <- result_all$old %>%
  filter(rep_c2c > 1) %>%
  count(g_new_c2c, sort = TRUE) %>%
  head(6) %>%
  pull(g_new_c2c)

# Weighted counts from OLD period (replicated)
old_counts <- lapply(weight_cols, function(wcol) {
  result_all$old %>%
    filter(g_new_c2c %in% top_groups) %>%
    group_by(g_new_c2c) %>%
    summarise(n = sum(.data[[wcol]]), .groups = "drop")
}) %>%
  setNames(gsub("wei_|_c2c", "", weight_cols)) %>%
  bind_rows(.id = "method") %>%
  tidyr::pivot_wider(names_from = method, values_from = n)

# Counts from NEW period (no replication, exact)
new_counts <- result_all$new %>%
  filter(code %in% top_groups) %>%
  count(code, name = "new_period") %>%
  rename(g_new_c2c = code)

# Combine for comparison
left_join(old_counts, new_counts, by = "g_new_c2c")
#> # A tibble: 6 × 8
#>   g_new_c2c naive  freq   knn    rf   lda    nb new_period
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>      <int>
#> 1 232002     23.1 21.9   29.2 23.9  29.7  21.9          30
#> 2 232003     23.1 19.7   23.9 17.2  25.1  19.7          27
#> 3 232004     23.1  5.10   5    5.86  8.05  5.10          7
#> 4 232005     23.1  3.65   5.1  5.02  7.49  3.65          5
#> 5 232006     23.1 16.8   15.2 13.1  18.5  16.8          23
#> 6 232007     23.1  2.92   2.5  3.34  3.74  2.92          4
```

The `new_period` column shows the actual counts in 2010. The other
columns show how the 2008 observations are redistributed under each
weight method. `naive` assigns uniform probability (1/n candidates),
`freq` uses base period frequencies, and ML methods (`knn`, `rf`, `lda`,
`nb`) use predicted probabilities.

Pick a specific group for regression analysis:

``` r

# New-period counts per category (no replication, so plain tally)
new_counts_all <- result_all$new %>%
  count(code, name = "n_new") %>%
  rename(g_new_c2c = code)

# Old-period weighted counts, joined to new-period counts
group_sizes <- result_all$old %>%
  group_by(g_new_c2c) %>%
  summarise(n_old = sum(wei_freq_c2c), .groups = "drop") %>%
  left_join(new_counts_all, by = "g_new_c2c") %>%
  filter(n_old >= 10, n_new >= 10) %>%
  arrange(desc(n_old))

# Pick a group for regression analysis
target_group <- group_sizes$g_new_c2c[1]
cat("Analysing occupation group:", target_group, "\n")
#> Analysing occupation group: 222101
```

**Regression within a single occupation group** - combine both periods
and compare coefficients:

``` r

# Subset old period to target group (with weights)
old_subset <- result_all$old %>%
  filter(g_new_c2c == target_group)

# Subset new period to target group (no replication, weight = 1)
new_subset <- result_all$new %>%
  filter(code == target_group) %>%
  mutate(
    wei_naive_c2c = 1, wei_freq_c2c = 1, wei_knn_c2c = 1,
    wei_rf_c2c = 1, wei_lda_c2c = 1, wei_nb_c2c = 1
  )

# Combine both periods
d <- bind_rows(old_subset, new_subset)

# Compare all regression coefficients across weight methods
f <- I(log(salary)) ~ age + sex + factor(edu) + exp + parttime

coefs <- sapply(weight_cols, function(wcol) {
  d$w <- d$multiplier * d[[wcol]]
  coef(lm(f, data = d, weights = w))
})
colnames(coefs) <- gsub("wei_|_c2c", "", weight_cols)
round(coefs, 4)
#>                naive    freq     knn      rf     lda      nb
#> (Intercept)   9.3225  9.2194  9.2118  9.2148  9.2229  9.2194
#> age          -0.0090 -0.0083 -0.0081 -0.0079 -0.0085 -0.0083
#> sexTRUE      -0.0042 -0.1153 -0.1421 -0.1295 -0.1253 -0.1153
#> factor(edu)2 -0.1317 -0.1131 -0.1157 -0.1141 -0.1108 -0.1131
#> factor(edu)3 -0.1036 -0.1065 -0.1090 -0.1065 -0.1033 -0.1065
#> factor(edu)4 -0.1333 -0.1450 -0.1472 -0.1466 -0.1428 -0.1450
#> factor(edu)5 -0.1884 -0.1370 -0.1326 -0.1419 -0.1303 -0.1370
#> exp           0.0138  0.0131  0.0128  0.0127  0.0132  0.0131
#> parttime      1.3797  1.4348  1.4411  1.4279  1.4343  1.4348
```

All coefficients can vary because weight methods change which old-period
subjects contribute to this occupation group.

### Compare pruning strategies only after comparing full weights

> **Note**: Pruning discards probability information and should be used
> only after analysis with full weights. Prefer
> `prune_c2c(method = "nonzero")` to remove impossible candidates while
> preserving the probability distribution. More aggressive pruning
> (`highest1`) is appropriate only for descriptive tables or when you
> need exactly one category per observation.

``` r

# Compare regression coefficients under different pruning strategies
prune_methods <- c("nonzero", "highest", "highest1")

prune_coefs <- sapply(prune_methods, function(pm) {
  old_pruned <- result_all$old %>%
    prune_c2c(method = pm) %>%
    filter(g_new_c2c == target_group)
  
  d <- bind_rows(old_pruned, new_subset)
  d$w <- d$multiplier * d$wei_freq_c2c
  coef(lm(f, data = d, weights = w))
})
round(prune_coefs, 4)
#>              nonzero highest highest1
#> (Intercept)   9.2194  9.2143   9.2143
#> age          -0.0083 -0.0083  -0.0083
#> sexTRUE      -0.1153 -0.1200  -0.1200
#> factor(edu)2 -0.1131 -0.1122  -0.1122
#> factor(edu)3 -0.1065 -0.1068  -0.1068
#> factor(edu)4 -0.1450 -0.1454  -0.1454
#> factor(edu)5 -0.1370 -0.1337  -0.1337
#> exp           0.0131  0.0131   0.0131
#> parttime      1.4348  1.4384   1.4384
```

### Compare ensemble compositions when no single method dominates

[`cross_c2c()`](https://polkas.github.io/cat2cat/reference/cross_c2c.md)
creates a weighted average of multiple weight columns. Vary the mix:

``` r

configs <- list(
  equal      = c(1, 1) / 2,
  freq_heavy = c(3, 1) / 4,
  ml_heavy   = c(1, 3) / 4
)

ens_coefs <- sapply(names(configs), function(nm) {
  old_ens <- result_all$old %>%
    cross_c2c(c("wei_freq_c2c", "wei_knn_c2c"), configs[[nm]]) %>%
    filter(g_new_c2c == target_group)
  
  new_ens <- new_subset %>% mutate(wei_cross_c2c = 1)
  d <- bind_rows(old_ens, new_ens)
  d$w <- d$multiplier * d$wei_cross_c2c
  coef(lm(f, data = d, weights = w))
})
round(ens_coefs, 4)
#>                equal freq_heavy ml_heavy
#> (Intercept)   9.2155     9.2175   9.2136
#> age          -0.0082    -0.0083  -0.0082
#> sexTRUE      -0.1287    -0.1220  -0.1354
#> factor(edu)2 -0.1144    -0.1138  -0.1151
#> factor(edu)3 -0.1078    -0.1072  -0.1084
#> factor(edu)4 -0.1462    -0.1456  -0.1467
#> factor(edu)5 -0.1348    -0.1359  -0.1337
#> exp           0.0130     0.0131   0.0129
#> parttime      1.4381     1.4365   1.4396
```

When regression coefficients are stable across weight methods, pruning
strategies, and ensemble compositions, report with confidence. When they
diverge, the mapping introduces uncertainty - report the range or
investigate the source.

## Step 3: Validate whether ML actually improves on simpler baselines

The `ml` argument in
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md)
adds ML-based probability weights, but ML is not guaranteed to improve
over simpler baselines.
[`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md)
provides per-group holdout (single train/test split) diagnostics to
answer this question *before* committing to a method.

### What `cat2cat_ml_run()` is doing

For each mapping group (set of candidate categories linked by the
transition table)
[`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md):

1.  Collects all observations from `ml$data` whose category belongs to
    the group.
2.  Randomly splits them into training (`1 - test_prop`) and test
    (`test_prop`) sets.
3.  Computes two baselines on the test set:
    - **naive** - accuracy of a random guess ($`1 / k`$ where $`k`$ is
      the number of candidate categories).
    - **freq** - accuracy of always predicting the most frequent
      training-set category.
4.  Trains each specified ML method on the training set and records
    test-set model performance.

Groups with fewer than 5 observations or only one candidate category are
skipped. Also note that
[`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md)
does not use `on_fail`; it is a diagnostic tool and reports skipped
groups instead of applying row-level fallback weights.

### Minimal validation workflow

``` r

cv_knn <- cat2cat_ml_run(
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = bind_rows(occup_2010, occup_2012),
    cat_var = "code",
    method = "knn",
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)
print(cv_knn)
#> === cat2cat ML Cross-Validation Results ===
#> 
#> ACCURACY (higher is better):
#>   naive (1/k): 0.1805
#>   freq (most common): 0.5215
#>   knn: accuracy = 0.5144
#> 
#> BRIER SCORE (lower is better, range 0-1):
#>   naive: 0.6834
#>   freq: 0.4179
#>   knn: brier = 0.4135
#> 
#> MEAN P(TRUE CLASS) (higher is better):
#>   naive: 0.1805
#>   freq: 0.4194
#>   knn: mean P(true) = 0.4473
#> 
#> ACCURACY: ML vs BASELINES (percent of groups where ML wins):
#>   knn > naive: 88.4%
#>   knn > freq: 25.1%
#> 
#> SKIPPED GROUPS (single category or <5 observations):
#>   knn: 32.1%
```

The [`print()`](https://rdrr.io/r/base/print.html) summary reports:

- **ACCURACY** - average held-out classification accuracy across
  non-skipped groups. `naive (1/k)` is the random-guess baseline, `freq`
  is the majority-class baseline, and each ML line reports top-class
  accuracy for that method.
- **BRIER SCORE** - average probability error across non-skipped groups.
  Lower is better. This matters because `cat2cat` ultimately uses
  probability weights, not just hard classifications.
- **MEAN P(TRUE CLASS)** - average probability assigned to the true
  category. Higher is better. This is often the most directly relevant
  metric for `cat2cat`, because it measures the quality of the
  probability weights themselves.
- **ACCURACY: ML vs BASELINES** - the share of groups in which the ML
  method beats `naive` or beats `freq` on accuracy. This is a win-rate
  summary, not an average accuracy gap.
- **SKIPPED GROUPS** - the percentage of mapping groups for which that
  ML method has no reported result because the group had only one
  candidate category, fewer than 5 observations, or the model could not
  be fit for that group.

So for output like:

- `knn > naive: 87.7%`
- `knn > freq: 18.0%`
- `knn: accuracy = 0.5108` vs `freq (most common): 0.5366`

the right reading is: kNN clearly beats the naive baseline, but it does
**not** beat the frequency baseline on top-class accuracy overall. In
that case, `wei_freq_c2c` remains the default choice if your only goal
is classification accuracy.

At the same time, if kNN has a slightly lower Brier score and a higher
mean P(true class) than `freq`, then it may still be producing
better-calibrated probability weights even though its top prediction is
less often correct. That distinction matters in `cat2cat`, because the
mapped weights are probabilities distributed across candidate categories
rather than single-class assignments.

### Compare multiple ML methods in one run

``` r

cv_all <- cat2cat_ml_run(
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = bind_rows(occup_2010, occup_2012),
    cat_var = "code",
    method = c("knn", "lda", "rf", "nb"),
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10, ntree = 50)
  )
)
print(cv_all)
#> === cat2cat ML Cross-Validation Results ===
#> 
#> ACCURACY (higher is better):
#>   naive (1/k): 0.1805
#>   freq (most common): 0.5414
#>   knn: accuracy = 0.5191
#>   lda: accuracy = 0.5439
#>   rf: accuracy = 0.5451
#>   nb: accuracy = 0.3863
#> 
#> BRIER SCORE (lower is better, range 0-1):
#>   naive: 0.6834
#>   freq: 0.4104
#>   knn: brier = 0.4064
#>   lda: brier = 0.4039
#>   rf: brier = 0.3931
#>   nb: brier = 0.5174
#> 
#> MEAN P(TRUE CLASS) (higher is better):
#>   naive: 0.1805
#>   freq: 0.4255
#>   knn: mean P(true) = 0.4543
#>   lda: mean P(true) = 0.4784
#>   rf: mean P(true) = 0.4658
#>   nb: mean P(true) = 0.4008
#> 
#> ACCURACY: ML vs BASELINES (percent of groups where ML wins):
#>   knn > naive: 87.7%
#>   lda > naive: 94.3%
#>   rf > naive: 92.3%
#>   nb > naive: 76.9%
#>   knn > freq: 24.5%
#>   lda > freq: 35.5%
#>   rf > freq: 32.3%
#>   nb > freq: 16.2%
#> 
#> SKIPPED GROUPS (single category or <5 observations):
#>   knn: 33.6%
#>   lda: 46.3%
#>   rf: 33.8%
#>   nb: 33.8%
```

Interpretation tip for mixed outputs:

- It is possible for a method to have 0 failed rows in
  [`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md)
  but a non-zero skipped-group rate in
  [`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md).
- This is not a contradiction: the first is row-level weight
  construction, the second is group-level holdout evaluation coverage.

### Inspect per-group diagnostics when methods disagree

The returned object is a named list. Each element corresponds to one
mapping group:

``` r

# Pick a group with multiple candidates
group_names <- names(cv_all)
example_group <- group_names[
  which(vapply(cv_all, function(g) !is.na(g$freq) && g$naive < 1, logical(1)))[1]
]
cv_all[[example_group]]
#> $naive
#> [1] 0.3333333
#> 
#> $acc
#> knn lda  rf  nb 
#>   1  NA   1   1 
#> 
#> $freq
#> [1] 1
#> 
#> $brier
#>          knn          lda           rf           nb 
#> 0.0000000000           NA 0.0001333333           NA 
#> 
#> $mean_prob
#>       knn       lda        rf        nb 
#> 1.0000000        NA 0.9933333        NA 
#> 
#> $naive_brier
#> [1] 0.4444444
#> 
#> $naive_mean_prob
#> [1] 0.3333333
#> 
#> $freq_brier
#> [1] 0.003460208
#> 
#> $freq_mean_prob
#> [1] 0.9411765
```

Each group entry contains the group-level diagnostics behind the printed
summary:

- `$naive` - $`1/k`$ random-guess accuracy for that group.
- `$freq` - majority-class accuracy for that group.
- `$acc` - named numeric vector with ML accuracy by method.
- `$naive_brier` and `$freq_brier` - baseline Brier scores.
- `$brier` - named numeric vector with ML Brier scores by method.
- `$naive_mean_prob` and `$freq_mean_prob` - baseline mean P(true
  class).
- `$mean_prob` - named numeric vector with ML mean P(true class) by
  method.

### Decision rules for interpreting the output

**Understanding model performance in context**: This is **multi-class
classification** - each mapping group can have 3-10+ candidate
categories. A naive random guess yields only ~18% accuracy (1/k where k
is the number of candidates). Achieving 50%+ is substantial improvement
over random - do not compare these numbers to binary classification
benchmarks where 80%+ is typical. The key question is whether ML beats
the *frequency* baseline, not whether it reaches some absolute
threshold.

| Scenario | Recommendation |
|----|----|
| ML model performance \>\> freq across most groups | ML weights add genuine signal; use them |
| ML model performance $`\approx`$ freq | ML is no better than frequency; prefer `wei_freq_c2c` (simpler, faster) |
| ML model performance \< freq for many groups | ML is adding noise; do **not** use ML weights |
| High failure rate (\>20%) | Features may have too many missing values or groups are too small |

Because the train/test split is random, results vary between runs. For
more stable estimates, pool more data into `ml$data` (e.g. multiple
survey waves) or run
[`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md)
several times and average the summaries.

> **Caveat**: high
> [`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md)
> model performance means the model discriminates well *within* mapping
> groups. It does not validate the mapping table itself. A perfect model
> with a wrong transition table will still produce wrong results.
