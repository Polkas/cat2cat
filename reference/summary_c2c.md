# Adjusted summary for regressions on replicated datasets

adjusting lm/glm object results according to original number of degree
of freedom. The standard errors, test statistics and p values have to be
adjusted because of replicated observations.

## Usage

``` r
summary_c2c(x, df_old, df_new = x$df.residual)
```

## Arguments

- x:

  lm or glm object

- df_old:

  integer number of d.f in original dataset. For bigger datasets
  \`nrow\` should be sufficient.

- df_new:

  integer number of d.f in dataset with replicated rows, Default:
  x\$df.residual

## Value

data.frame with additional columns over a regular summary output, like
correct and statistics adjusted by it.

## Details

The replication step in `cat2cat` inflates the nominal sample size: the
model sees `n_rep` rows but only `n_orig` are independent observations.
Naive OLS therefore under-estimates standard errors.

The correction factor is `sqrt(df_new / df_old)`, where `df_new` is the
residual d.f. from the replicated model and `df_old` the residual d.f.
from the original dataset. Standard errors are multiplied by this
factor, test statistics divided by it, and p-values recomputed from the
appropriate reference distribution: `t(df_old)` for t-based summaries
and standard normal for z-based summaries.

This is a pragmatic d.f. adjustment. It works well when per-subject
weights sum to one and no extreme weights dominate.

Note: Goodness-of-fit statistics (R-squared, AIC, BIC) from the
replicated model are **not meaningful** — they reflect the inflated
sample size, not explanatory power. Report only coefficient estimates
and the corrected SEs/p-values from this function.

## Examples

``` r
data("occup_small", package = "cat2cat")
data("trans", package = "cat2cat")

occup_old <- occup_small[occup_small$year == 2008, ]
occup_new <- occup_small[occup_small$year == 2010, ]

occup_2 <- cat2cat(
  data = list(
    old = occup_old,
    new = occup_new,
    cat_var = "code",
    time_var = "year"
  ),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(
    data = occup_new,
    cat_var = "code",
    method = "knn",
    features = c("age", "sex", "edu", "exp", "parttime", "salary"),
    args = list(k = 10)
  )
)

# Regression
# we have to adjust size of std as we artificialy enlarge degrees of freedom
lms <- lm(
  formula = I(log(salary)) ~ age + sex + factor(edu) + parttime + exp,
  data = occup_2$old,
  weights = multiplier * wei_freq_c2c
)

summary_c2c(lms, df_old = nrow(occup_old))
#>                  Estimate   Std. Error    t value      Pr(>|t|)  correct
#> (Intercept)   8.642709060 0.0305125691 283.250782  0.000000e+00 2.412389
#> age          -0.002354694 0.0007645294  -3.079926  2.075450e-03 2.412389
#> sexTRUE       0.291655854 0.0086207208  33.831957 3.607064e-239 2.412389
#> factor(edu)2 -0.063704597 0.0161174380  -3.952526  7.779765e-05 2.412389
#> factor(edu)3 -0.338008780 0.0201287688 -16.792323  1.565695e-62 2.412389
#> factor(edu)4 -0.381827051 0.0123532071 -30.909143 1.641476e-201 2.412389
#> factor(edu)5 -0.366016945 0.0170324446 -21.489396 1.904297e-100 2.412389
#> factor(edu)6 -0.579703259 0.0123940057 -46.772873  0.000000e+00 2.412389
#> factor(edu)7 -0.523761245 0.0890371147  -5.882505  4.154668e-09 2.412389
#> factor(edu)8 -0.630390830 0.0184889170 -34.095606 1.054062e-242 2.412389
#> parttime      1.875598705 0.0200106821  93.729874  0.000000e+00 2.412389
#> exp           0.010307126 0.0007306681  14.106440  8.256218e-45 2.412389
#>              std.error_c statistic_c     p.value_c reference_dist
#> (Intercept)  0.073608193  117.415041  0.000000e+00              t
#> age          0.001844343   -1.276712  2.018554e-01              t
#> sexTRUE      0.020796534   14.024253  1.195445e-42              t
#> factor(edu)2 0.038881534   -1.638428  1.014932e-01              t
#> factor(edu)3 0.048558425   -6.960868  4.597874e-12              t
#> factor(edu)4 0.029800744  -12.812668  3.782334e-36              t
#> factor(edu)5 0.041088886   -8.907931  1.160653e-18              t
#> factor(edu)6 0.029899166  -19.388610  9.622762e-77              t
#> factor(edu)7 0.214792178   -2.438456  1.483856e-02              t
#> factor(edu)8 0.044602464  -14.133543  2.935764e-43              t
#> parttime     0.048273554   38.853545 2.899983e-245              t
#> exp          0.001762656    5.847498  5.833868e-09              t
```
