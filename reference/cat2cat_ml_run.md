# Cross-validation diagnostics for cat2cat ML models

Evaluates whether machine-learning models used in the `ml` argument of
[`cat2cat`](https://polkas.github.io/cat2cat/reference/cat2cat.md)
actually improve category assignment over simpler baselines. The
function runs a per-group train/test split across every mapping group
defined by the transition table.

## Usage

``` r
cat2cat_ml_run(mappings, ml, ...)

# S3 method for class 'cat2cat_ml_run'
print(x, ...)
```

## Arguments

- mappings:

  \`named list\` with 3 fields \`trans\`, \`direction\` and optional
  \`freqs_df\`.

- ml:

  \`named list\` (optional) with fields \`data\`, \`cat_var\`,
  \`method\`, \`features\` and optional \`args\`, \`on_fail\`,
  \`fail_warn\`.

- ...:

  other arguments (currently unused).

- x:

  `cat2cat_ml_run` instance created with `cat2cat_ml_run`.

## Value

An object of class `"cat2cat_ml_run"` (a named list). Each element
corresponds to one mapping group and contains:

- `naive`:

  `numeric(1)` — random-guess baseline accuracy (1/k where k is number
  of categories). Theoretical lower bound.

- `freq`:

  `numeric(1)` — most-frequent-category baseline accuracy. A simple but
  often strong baseline.

- `acc`:

  Named `numeric` vector — test-set accuracy for each ML method. Higher
  is better; compare to `freq`.

- `brier`:

  Named `numeric` vector — Brier score for each ML method. Computed as
  `mean((1 - P(true))^2)`. Lower is better; range is \[0, 1\].

- `mean_prob`:

  Named `numeric` vector — average probability assigned to the true
  class. Higher is better. This directly measures the quality of
  probability weights used by cat2cat.

- `naive_brier`:

  `numeric(1)` — Brier score for uniform baseline (= (1 - 1/k)^2).
  Serves as a calibration reference.

- `naive_mean_prob`:

  `numeric(1)` — mean P(true) for uniform baseline (= 1/k). Equals
  `naive` by definition.

- `freq_brier`:

  `numeric(1)` — Brier score using training set category frequencies as
  probabilities.

- `freq_mean_prob`:

  `numeric(1)` — mean P(true) using training set category frequencies.

The object also carries an `"ml_models"` attribute listing the methods
evaluated. Use [`print()`](https://rdrr.io/r/base/print.html) for a
human-readable summary.

`print` returns `x` invisibly, after printing a summary with average
accuracy per method, baseline comparisons, and failure rates.

## Details

For each mapping group (set of candidate categories linked by the
transition table) the function:

1.  Collects all observations from `ml$data` whose category belongs to
    the group.

2.  Randomly splits them into training (`1 - test_prop`) and test
    (`test_prop`) sets.

3.  Computes two baselines on the test set:

    - **naive**: accuracy of a random guess (\\1 / k\\ where \\k\\ is
      the number of candidate categories).

    - **freq**: accuracy of always predicting the most frequent category
      in the training set.

4.  Trains each ML model specified in `ml$method` on the training set
    and records its classification accuracy on the test set.

Groups with fewer than 5 observations or only one candidate category are
skipped (their accuracy is recorded as `NA`).

### Baseline-Only Diagnostics

To inspect only baseline diagnostics (`naive`, `freq`, and their
Brier/mean-probability variants), pass empty model and feature vectors:
`ml$method = character(0)` and `ml$features = character(0)`. In this
mode, no ML models are trained, but baseline diagnostics are still
computed for each mapping group.

### Understanding the Metrics

Three complementary metrics evaluate model quality:

**Accuracy** measures how often the model's top prediction matches the
true category. Use this when you only care about the single most likely
assignment. Higher is better; theoretical maximum is 1.0.

**Mean P(true class)** measures the average probability the model
assigns to the correct category. This evaluates the full probability
distribution, not just the top prediction. For cat2cat, where weights
ARE probabilities distributed across candidates, this metric directly
measures weight quality. Higher is better; range is \\\[0, 1\]\\.

**Brier score** measures the squared error between predicted probability
and the true outcome: \\(1 - P(true))^2\\. Unlike log-loss, Brier score
is bounded \\\[0, 1\]\\ and does not explode when P(true) is near zero.
Lower is better; 0 means perfect prediction. For k categories, the naive
baseline (uniform 1/k) gives Brier = \\(1 - 1/k)^2\\.

### Choosing a Method

- If accuracy and mean P(true) are similar across methods, prefer
  simpler methods (freq, lda) over complex ones (rf, knn).

- If ML methods rarely beat the frequency baseline, use `wei_freq_c2c` —
  ML adds complexity without benefit.

- If Brier score for ML is similar to or worse than naive, the model is
  not well-calibrated. Consider `wei_freq_c2c` or a different ML method.

- Use
  [`cross_c2c`](https://polkas.github.io/cat2cat/reference/cross_c2c.md)
  to ensemble multiple methods if no single method dominates.

Because the split is random, results will vary between runs. For more
stable estimates, call the function several times or use a larger
`ml$data` set (e.g. pool multiple survey waves).

## See also

[`cat2cat`](https://polkas.github.io/cat2cat/reference/cat2cat.md) for
the main mapping function,
[`cross_c2c`](https://polkas.github.io/cat2cat/reference/cross_c2c.md)
for ensembling weights from multiple methods.

## Examples

``` r
# \donttest{
library("cat2cat")
data("occup", package = "cat2cat")
data("trans", package = "cat2cat")

occup_2006 <- occup[occup$year == 2006, ]
occup_2008 <- occup[occup$year == 2008, ]

# Forward direction: old encoding -> new encoding
# Use data from OLD encoding periods (2006, 2008)
ml_setup <- list(
  data = rbind(occup_2006, occup_2008),
  cat_var = "code",
  method = c("knn", "rf", "lda", "nb"),
  features = c("age", "sex", "edu", "exp", "parttime", "salary"),
  args = list(k = 10, ntree = 50)
)
mappings <- list(trans = trans, direction = "forward")

set.seed(1234)
res <- cat2cat_ml_run(mappings, ml_setup, test_prop = 0.2)
print(res)
#> === cat2cat ML Cross-Validation Results ===
#> 
#> ACCURACY (higher is better):
#>   naive (1/k): 0.4380
#>   freq (most common): 0.6447
#>   knn: accuracy = 0.6424
#>   rf: accuracy = 0.7026
#>   lda: accuracy = 0.7075
#>   nb: accuracy = 0.6870
#> 
#> BRIER SCORE (lower is better, range 0-1):
#>   naive: 0.3277
#>   freq: 0.2512
#>   knn: brier = 0.2687
#>   rf: brier = 0.2191
#>   lda: brier = 0.2141
#>   nb: brier = 0.2414
#> 
#> MEAN P(TRUE CLASS) (higher is better):
#>   naive: 0.4380
#>   freq: 0.5866
#>   knn: mean P(true) = 0.5919
#>   rf: mean P(true) = 0.6430
#>   lda: mean P(true) = 0.6588
#>   nb: mean P(true) = 0.6478
#> 
#> ACCURACY: ML vs BASELINES (percent of groups where ML wins):
#>   knn > naive: 80.0%
#>   rf > naive: 97.7%
#>   lda > naive: 90.5%
#>   nb > naive: 90.7%
#>   knn > freq: 20.0%
#>   rf > freq: 41.9%
#>   lda > freq: 40.5%
#>   nb > freq: 39.5%
#> 
#> SKIPPED GROUPS (single category or <5 observations):
#>   knn: 98.3%
#>   rf: 98.3%
#>   lda: 98.4%
#>   nb: 98.3% 

# Typical good results show:
# - ML accuracy > freq baseline (ML adds value)
# - ML Brier < naive (well-calibrated probabilities)
# - ML mean P(true) > freq (better probability weights)
#
# If Brier(ML) >= Brier(naive), the model is poorly calibrated
# and wei_freq_c2c may be safer. Use cross_c2c() to ensemble.

# High failure rate is normal - most groups have <5 observations

# Baseline-only diagnostics (no ML models):
ml_baseline <- list(
  data = rbind(occup_2006, occup_2008),
  cat_var = "code",
  method = character(0),
  features = character(0)
)
baseline_cv <- cat2cat_ml_run(mappings, ml_baseline)
print(baseline_cv)
#> === cat2cat ML Cross-Validation Results ===
#> 
#> ACCURACY (higher is better):
#>   naive (1/k): 0.4380
#>   freq (most common): 0.7045
#> 
#> BRIER SCORE (lower is better, range 0-1):
#>   naive: 0.3277
#>   freq: 0.2194
#> 
#> MEAN P(TRUE CLASS) (higher is better):
#>   naive: 0.4380
#>   freq: 0.6136
#> 
#> ACCURACY: ML vs BASELINES (percent of groups where ML wins):
#> 
#> SKIPPED GROUPS (single category or <5 observations): 
# }
```
