# Advanced Workflows

This vignette collects the advanced `cat2cat` workflows in one place: ML
weights, multi-period chaining, panel data with identifiers, aggregated
data, and regression on replicated data. It assumes you’ve read [Get
Started](https://polkas.github.io/cat2cat/articles/cat2cat.md).

Use this vignette by module:

1.  **ML weights** if you want feature-informed probabilities rather
    than only naive or frequency weights.
2.  **Multi-period chaining** if your harmonisation spans 3 or more
    waves.
3.  **Panel data with subject identifiers** if you have a rotational
    panel or other stable subject IDs.
4.  **Aggregated data and special cases** if you only observe category
    totals or need hierarchical-code mappings.
5.  **Regression on replicated data** if your main task is estimation
    and inference after harmonisation.

``` r

library(cat2cat)
library(dplyr)
library(tidyr)
library(fixest)

data(occup, package = "cat2cat")
data(occup_panel, package = "cat2cat")
data(trans, package = "cat2cat")
data(verticals, package = "cat2cat")
data(verticals2, package = "cat2cat")

occup_2006 <- occup[occup$year == 2006, ]
occup_2008 <- occup[occup$year == 2008, ]
occup_2010 <- occup[occup$year == 2010, ]
occup_2012 <- occup[occup$year == 2012, ]
```

## ML weights

Machine-learning weights are useful when the mapping table alone is too
coarse and observed features help distinguish which target category is
most plausible for a replicated observation.

This section shows:

- when ML weights are worth trying,
- how to specify the `ml` argument in
  [`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md),
- how to validate whether ML improves on simpler baselines,
- and how to handle rows where ML probabilities cannot be produced.

In practice, ML is most helpful when replication is substantial and the
ambiguous categories differ systematically by observed characteristics
such as age, education, experience, or salary. If ML does not improve on
the frequency baseline in
[`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md),
the simpler `wei_freq_c2c` weights are usually the better choice.

``` r

ml_setup <- list(
  data = bind_rows(occup_2010, occup_2012),
  cat_var = "code",
  method = c("knn", "rf", "lda"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10, ntree = 50),
  on_fail = "freq",
  fail_warn = TRUE
)

result_ml <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = ml_setup
)
```

Validate whether ML adds value before using it in production:

``` r

cv <- cat2cat_ml_run(
  mappings = list(trans = trans, direction = "backward"),
  ml = ml_setup
)
print(cv)
```

Baseline-only diagnostics are also available:

``` r

ml_baseline <- list(
  data = bind_rows(occup_2008, occup_2010),
  cat_var = "code",
  method = character(0),
  features = character(0)
)

cv_baseline <- cat2cat_ml_run(
  mappings = list(trans = trans, direction = "forward"),
  ml = ml_baseline
)
print(cv_baseline)
```

If ML probabilities cannot be produced for some replicated rows, use
`on_fail` and `fail_warn`:

- `on_fail = "freq"` (default): replace failed ML rows with
  `wei_freq_c2c`
- `on_fail = "naive"`: replace failed ML rows with `wei_naive_c2c`
- `on_fail = "na"`: keep failed ML rows as `NA`
- `on_fail = "error"`: stop immediately
- `fail_warn = TRUE` (default): warn with affected rows/observations per
  method
- `fail_warn = FALSE`: silence the warning

## Multi-period chaining

Repeated cross-sections or longitudinal datasets often contain more than
two waves. To bring all waves onto the same encoding, apply
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md)
iteratively, feeding the output of one step into the next.

Use this module when you need one harmonised categorical variable across
3 or more periods. The main design choice is whether to chain backward
or forward, and whether truncating hierarchical codes can reduce
replication before chaining.

In the examples below, `occup` is a repeated cross-section dataset, not
a panel. The chained outputs therefore combine harmonised cross-sections
from multiple years.

### Optional: passing `mappings$freqs_df`

In most cases you do **not** need to pass `mappings$freqs_df`. If it is
omitted,
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md)
computes base-period frequencies internally from the provided data.

Pass `mappings$freqs_df` only when you need explicit control over base
frequencies.

The object should be a two-column data frame: category name in the first
column and counts in the second.

### Mapping table stability and truncation

Before chaining, inspect how disruptive the transition table is:

``` r

max_digits <- max(nchar(as.character(trans[[1]])), nchar(as.character(trans[[2]])))

stability <- sapply(1:max_digits, function(d) {
  old_trunc <- substr(as.character(trans[[1]]), 1, d)
  new_trunc <- substr(as.character(trans[[2]]), 1, d)
  mean(old_trunc != new_trunc) * 100
})

data.frame(
  digits = 1:max_digits,
  pct_changed = round(stability, 1)
)
#>   digits pct_changed
#> 1      1         6.5
#> 2      2        40.4
#> 3      3        77.5
#> 4      4        85.7
#> 5      5       100.0
#> 6      6       100.0
```

Truncating codes to fewer digits can reduce replication:

For backward mapping this is often true, but for forward mapping
truncation can move in the opposite direction. Collapsing detailed codes
into broader prefixes can create additional many-to-one and one-to-many
ties on the old-code side, which may increase replication in the mapped
period.

``` r

occup_2008_trunc <- occup_2008
occup_2008_trunc$code <- substr(occup_2008_trunc$code, 1, 4)
occup_2010_trunc <- occup_2010
occup_2010_trunc$code <- substr(occup_2010_trunc$code, 1, 4)
trans_trunc <- unique(data.frame(
  old = substr(trans$old, 1, 4),
  new = substr(trans$new, 1, 4)
))

back_full <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)
back_trunc <- cat2cat(
  data = list(old = occup_2008_trunc, new = occup_2010_trunc,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans_trunc, direction = "backward")
)

fwd_full <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "forward")
)
fwd_trunc <- cat2cat(
  data = list(old = occup_2008_trunc, new = occup_2010_trunc,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans_trunc, direction = "forward")
)

data.frame(
  mapping = c("backward full (4->6)", "backward trunc (4->4)",
              "forward full (6->4)", "forward trunc (4->4)"),
  mean_rep = c(mean(back_full$old$rep_c2c), mean(back_trunc$old$rep_c2c),
               mean(fwd_full$new$rep_c2c), mean(fwd_trunc$new$rep_c2c))
)
#>                 mapping  mean_rep
#> 1  backward full (4->6) 23.479114
#> 2 backward trunc (4->4)  4.697948
#> 3   forward full (6->4)  1.363676
#> 4  forward trunc (4->4)  3.268608
```

### Backward chaining

``` r

step1 <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

step2 <- cat2cat(
  data = list(old = occup_2006, new = step1$old,
              cat_var_old = "code", cat_var_new = "g_new_c2c",
              time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

harmonised_back <- bind_rows(
  step2$old,
  step1$old,
  step1$new,
  dummy_c2c(occup_2012, "code")
)
```

Validation: weighted counts should match the original counts within each
year.

``` r

harmonised_back %>%
  group_by(year) %>%
  summarise(weighted_n = round(sum(wei_freq_c2c)), .groups = "drop") %>%
  left_join(count(occup, year), by = "year")
#> # A tibble: 4 × 3
#>    year weighted_n     n
#>   <int>      <dbl> <int>
#> 1  2006      16540 16540
#> 2  2008      17223 17223
#> 3  2010      17323 17323
#> 4  2012      18040 18040
```

### Forward chaining

``` r

trans_fwd <- rbind(
  trans,
  data.frame(old = "no_cat",
             new = setdiff(c(occup_2010$code, occup_2012$code), trans$new))
)

fwd1 <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans_fwd, direction = "forward")
)

fwd2 <- cat2cat(
  data = list(old = fwd1$new, new = occup_2012,
              cat_var_old = "g_new_c2c", cat_var_new = "code",
              time_var = "year"),
  mappings = list(trans = trans_fwd, direction = "forward")
)

harmonised_fwd <- bind_rows(
  dummy_c2c(occup_2006, "code"),
  fwd1$old,
  fwd1$new,
  fwd2$new
)
```

### Adding ML to the chain

``` r

step1_ml <- cat2cat(
  data = list(old = occup_2008, new = occup_2010,
              cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = ml_setup
)

step2_ml <- cat2cat(
  data = list(old = occup_2006, new = step1_ml$old,
              cat_var_old = "code", cat_var_new = "g_new_c2c",
              time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = ml_setup
)
```

## Panel data with subject identifiers

If subjects have stable identifiers across waves, `id_var` can reduce
unnecessary replication by directly matching returning subjects.

Use this module only when the identifier truly tracks the same subject
across adjacent waves and short-run category changes are unlikely to
represent genuine transitions rather than coding changes.

If you have a complete panel with every subject observed in both periods
and no missing category values, you may not need probabilistic
harmonisation at all. In that case, the target-period category can often
be joined back to the earlier record by `id_var`, and the task is mostly
a deterministic join.
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md) is
more useful when the panel is incomplete, rotational, or mixed with new
entrants and leavers, so some observations still need the mapping-table
replication path.

``` r

panel_old <- occup_panel[occup_panel$quarter == "2009Q4", ]
panel_new <- occup_panel[occup_panel$quarter == "2010Q1", ]
shared_ids <- intersect(panel_old$panel_id, panel_new$panel_id)
length(shared_ids)
#> [1] 450
```

``` r

result_id <- cat2cat(
  data = list(
    old = panel_old,
    new = panel_new,
    id_var = "panel_id",
    cat_var = "code",
    time_var = "quarter"
  ),
  mappings = list(trans = trans, direction = "backward")
)
```

How `id_var` works:

- direct match: workers observed in both periods receive `rep_c2c = 1`
  and weight 1,
- replication path: workers observed in only one period go through the
  standard mapping-table replication.

``` r

table(result_id$old$rep_c2c)
#> 
#>   1   2   3   4   5   6   7   8   9  10  11  13  16  18  19  21  22  23  24  25 
#> 457   6  12  56  75  54  84  88  81  30 165  26  64 180 190  42  44  46  48  25 
#>  33  34  46  70 
#>  33 170 276  70
sum(result_id$old$wei_freq_c2c)
#> [1] 600
nrow(panel_old)
#> [1] 600
```

Compare with and without identifiers:

``` r

result_no_id <- cat2cat(
  data = list(
    old = panel_old,
    new = panel_new,
    cat_var = "code",
    time_var = "quarter"
  ),
  mappings = list(trans = trans, direction = "backward")
)

cat("WITH id_var average replication:", round(mean(result_id$old$rep_c2c), 2), "\n")
#> WITH id_var average replication: 18.49
cat("WITHOUT id_var average replication:", round(mean(result_no_id$old$rep_c2c), 2), "\n")
#> WITHOUT id_var average replication: 23.49
```

Use `id_var` when:

- identifiers are reliable,
- the time gap is short relative to true mobility,
- and direct matches are informative about the coding change.

## Aggregated data and special cases

When only aggregate counts are available, use
[`cat2cat_agg()`](https://polkas.github.io/cat2cat/reference/cat2cat_agg.md)
with mapping equations rather than micro-data replication.

Use this module when you do not have person-level data, or when the
classification itself has a hierarchical code structure that can be
exploited to build coarser mappings.

``` r

agg_old <- verticals[verticals$v_date == "2020-04-01", ]
agg_new <- verticals[verticals$v_date == "2020-05-01", ]
```

``` r

agg <- cat2cat_agg(
  data = list(
    old = agg_old,
    new = agg_new,
    cat_var = "vertical",
    time_var = "v_date",
    freq_var = "counts"
  ),
  Automotive %<% c(Automotive1, Automotive2),
  c(Kids1, Kids2) %>% c(Kids),
  Home %>% c(Home, Supermarket)
)
```

Inspect how categories were proportionally redistributed:

``` r

agg$old[agg$old$vertical %in% c("Automotive1", "Automotive2"), ]
#>        vertical    sales counts     v_date  prop_c2c
#> 4   Automotive1 76.54302    135 2020-04-01 0.6452772
#> 4.1 Automotive2 76.54302    135 2020-04-01 0.3547228
agg$new[agg$new$vertical %in% c("Kids1", "Kids2"), ]
#>      vertical    sales counts     v_date  prop_c2c
#> 13      Kids1 105.4317    874 2020-05-01 0.3534726
#> 13.1    Kids2 105.4317    874 2020-05-01 0.6465274
```

Hierarchical codes can also be used to build coarser mapping tables when
an official transition table is unavailable:

``` r

trans_2digit <- data.frame(
  old = substr(trans$old, 1, 2),
  new = substr(trans$new, 1, 2)
)
trans_2digit <- unique(trans_2digit)

cat("2-digit mapping rows:", nrow(trans_2digit),
    "vs full mapping rows:", nrow(trans))
#> 2-digit mapping rows: 122 vs full mapping rows: 2666
```

This works for classifications with stable prefix hierarchies such as
ISCO, ICD, NACE, CPC, or HS codes.

## Regression on replicated data

The replication is neutral for regressions on non-mapped covariates
because per-subject weights sum to one. Standard errors, however, must
be corrected because replication inflates the row count.

Use this module when your end goal is estimation rather than descriptive
harmonisation. The key issue is not coefficient bias for non-mapped
regressors, but valid inference after replication.

### Building a 4-period harmonised repeated cross-section dataset

``` r

cat2cat_data_back <- bind_rows(
  step2$old,
  step1$old,
  step1$new,
  dummy_c2c(occup_2012, "code")
)
```

### Neutral impact demonstration

``` r

lms_orig <- lm(
  I(log(salary)) ~ age + sex + factor(edu) + parttime + exp,
  data = occup,
  weights = multiplier
)

lms_harmonised <- lm(
  I(log(salary)) ~ age + sex + factor(edu) + parttime + exp,
  data = cat2cat_data_back,
  weights = multiplier * wei_freq_c2c
)

summary_c2c(lms_harmonised, df_old = nrow(occup))
#>                  Estimate   Std. Error    t value      Pr(>|t|)  correct
#> (Intercept)   8.567134022 0.0055759904 1536.43270  0.000000e+00 2.246708
#> age          -0.001669601 0.0001429794  -11.67722  1.689342e-31 2.246708
#> sexTRUE       0.254854849 0.0015855706  160.73384  0.000000e+00 2.246708
#> factor(edu)2 -0.123208217 0.0031187751  -39.50532  0.000000e+00 2.246708
#> factor(edu)3 -0.390643025 0.0037961258 -102.90571  0.000000e+00 2.246708
#> factor(edu)4 -0.465471793 0.0022196689 -209.70325  0.000000e+00 2.246708
#> factor(edu)5 -0.443598202 0.0031191398 -142.21812  0.000000e+00 2.246708
#> factor(edu)6 -0.678797186 0.0022404213 -302.97747  0.000000e+00 2.246708
#> factor(edu)7 -0.617843013 0.0192393288  -32.11354 6.112599e-226 2.246708
#> factor(edu)8 -0.717563371 0.0035222572 -203.72259  0.000000e+00 2.246708
#> parttime      1.999007607 0.0037223872  537.02301  0.000000e+00 2.246708
#> exp           0.011337142 0.0001368157   82.86435  0.000000e+00 2.246708
#>               std.error_c statistic_c     p.value_c reference_dist
#> (Intercept)  0.0125276207  683.859629  0.000000e+00              t
#> age          0.0003212329   -5.197479  2.025825e-07              t
#> sexTRUE      0.0035623137   71.541945  0.000000e+00              t
#> factor(edu)2 0.0070069760  -17.583650  4.650384e-69              t
#> factor(edu)3 0.0085287850  -45.802893  0.000000e+00              t
#> factor(edu)4 0.0049869472  -93.338022  0.000000e+00              t
#> factor(edu)5 0.0070077954  -63.300678  0.000000e+00              t
#> factor(edu)6 0.0050335718 -134.853979  0.000000e+00              t
#> factor(edu)7 0.0432251482  -14.293601  2.792952e-46              t
#> factor(edu)8 0.0079134824  -90.676056  0.000000e+00              t
#> parttime     0.0083631160  239.026649  0.000000e+00              t
#> exp          0.0003073848   36.882568 6.567139e-295              t
```

[`summary_c2c()`](https://polkas.github.io/cat2cat/reference/summary_c2c.md)
scales naive standard errors by the replication factor:

``` math
\text{SE}_{\text{corrected}} = \text{SE}_{\text{naive}} \times \sqrt{\frac{n_{\text{rep}}}{n_{\text{orig}}}}
```

### Fixed effects regression

``` r

harmonised_fe <- cat2cat_data_back %>%
  prune_c2c(method = "nonzero") %>%
  mutate(orig_obs_id = interaction(year, index_c2c, drop = TRUE, lex.order = TRUE)) %>%
  filter(!is.na(g_new_c2c), !is.na(salary), salary > 0)

fe_model_cluster <- feols(
  log(salary) ~ age + sex + factor(edu) + parttime + exp | g_new_c2c + year,
  data = harmonised_fe,
  weights = ~multiplier * wei_freq_c2c,
  cluster = ~orig_obs_id
)
summary(fe_model_cluster)
#> OLS estimation, Dep. Var.: log(salary)
#> Observations: 348,744
#> Weights: multiplier * wei_freq_c2c
#> Fixed-effects: g_new_c2c: 1,561,  year: 4
#> Standard-errors: Clustered (orig_obs_id) 
#>               Estimate Std. Error   t value  Pr(>|t|)    
#> age          -0.000589   0.000353  -1.67043  0.094839 .  
#> sexTRUE       0.127013   0.005018  25.30943 < 2.2e-16 ***
#> factor(edu)2 -0.122142   0.009111 -13.40599 < 2.2e-16 ***
#> factor(edu)3 -0.220058   0.009472 -23.23189 < 2.2e-16 ***
#> factor(edu)4 -0.254005   0.007823 -32.46795 < 2.2e-16 ***
#> factor(edu)5 -0.236455   0.009999 -23.64695 < 2.2e-16 ***
#> factor(edu)6 -0.329733   0.008764 -37.62281 < 2.2e-16 ***
#> factor(edu)7 -0.299026   0.027005 -11.07290 < 2.2e-16 ***
#> factor(edu)8 -0.335429   0.010119 -33.14734 < 2.2e-16 ***
#> parttime      1.871971   0.011668 160.44062 < 2.2e-16 ***
#> exp           0.008152   0.000332  24.51840 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> RMSE: 0.36761     Adj. R2: 0.713868
#>                 Within R2: 0.555095
```

Do not cluster directly on `index_c2c` after binding multiple waves,
because `index_c2c` is created separately inside each
[`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md)
call and can repeat across years. Instead, build a wave-specific
original-observation identifier such as `interaction(year, index_c2c)`
and cluster on that. This treats all replications of the same source row
as one cluster without incorrectly merging different people from
different waves.

## Choosing the right advanced workflow

| Problem | Recommended tool |
|----|----|
| Need feature-informed weights | `cat2cat(..., ml = ...)` + [`cat2cat_ml_run()`](https://polkas.github.io/cat2cat/reference/cat2cat_ml_run.md) |
| Need 3+ wave harmonisation | iterative [`cat2cat()`](https://polkas.github.io/cat2cat/reference/cat2cat.md) chaining |
| Stable subject identifiers across waves | `id_var` |
| Only aggregated counts available | [`cat2cat_agg()`](https://polkas.github.io/cat2cat/reference/cat2cat_agg.md) |
| Regression on replicated data | [`summary_c2c()`](https://polkas.github.io/cat2cat/reference/summary_c2c.md) or clustered inference |

## Next steps

- [Get Started](https://polkas.github.io/cat2cat/articles/cat2cat.md)
  for core concepts and the two-period workflow
- [Choosing Weights and Validating
  ML](https://polkas.github.io/cat2cat/articles/cat2cat_validation.md)
  for weight comparisons and robustness checks
