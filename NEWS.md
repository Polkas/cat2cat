# cat2cat 0.6.1

This release incorporates feedback from the PhD dissertation reviewers: dr hab. Andrzej Dudek, dr hab. Joanna Landmesser-Rusek, and dr hab. Paweł Andrzej Strzelecki.

## New features

* New `occup_panel` dataset - a rotational panel covering 2009Q1--2010Q4 - used to illustrate `id_var` direct matching and to exercise the panel-data code path in tests.
* Naive Bayes (`"nb"`) added to the supported ML methods, via `e1071` (Suggests).
* `summary_c2c()` now supports both `lm` and `glm`, adds input/shape validation (`df_old`/`df_new`, coefficient table checks), and reports the reference distribution used for p-values (`t` or normal). Added edge-case tests and aligned regression/panel vignette examples with current `summary_c2c()` output and fixed-effects inference guidance.
* `cat2cat_ml_run()` now reports the Brier score and mean P(true class) in addition to accuracy. A proper scoring rule matters because `cat2cat` weights are *probabilities*, not classifications - a model can be accurate and still be poorly calibrated.
* All `stopifnot()` assertions now carry descriptive messages, so failures point the user at the offending argument instead of printing the raw expression.
* `cat2cat()` ML fallback is now configurable via `ml$on_fail` (`"freq"`, `"naive"`, `"na"`, `"error"`) with optional warning control via `ml$fail_warn`. Failed ML weights are now explicitly handled according to this policy instead of always silently falling back to frequency weights.
* `cat2cat()` ML now accepts `factor` features in addition to numeric/logical: factor columns listed in `ml$features` are automatically one-hot encoded using the union of levels observed in `ml$data` and the target period.

## Documentation

* Vignettes reorganised into three clearer guides: *Get Started*, *Choosing Weights and Validating ML*, and *Advanced Workflows*. The goal is a cleaner reader path: core concepts first, method choice second, advanced workflows third.
* *Get Started* now focuses on the two-period workflow, core assumptions, and the value added of cat2cat for group-level longitudinal analysis, with sharper pointers to the more advanced guides.
* *Choosing Weights and Validating ML* is now structured as a decision guide: understanding weight assumptions, checking robustness across methods, validating ML against naive/frequency baselines, and handling failed ML predictions.
* *Advanced Workflows* now collects ML setup, multi-period chaining, rotational panels with `id_var`, aggregated-data workflows, hierarchical-code mappings, and regression/inference after harmonisation into one better-structured advanced reference.
* Added a *When cat2cat won't help* section distinguishing hard blockers (no mapping table, unobserved category) from method-specific limitations with available workarounds.
* Corrected the $R^2$ guidance: ordinary $R^2$ is preserved in neutral replication cases when replicated copies keep the same response/covariates and weights sum back to the source observation; adjusted $R^2$, AIC, and BIC require original-scale degrees-of-freedom care, and fit statistics with harmonised-category covariates are conditional on the chosen weights.
* Roxygen for `cat2cat()` and `cat2cat_agg()` trimmed - argument documentation kept, conceptual material moved to the vignettes where it belongs.

## Bug fixes

* Fixed the `nomnoml` diagram that previously mislabelled the base/target sides under forward mapping.
* Fixed `get_freqs()` for `r-devel`/`R CMD check`: replaced `as.data.frame(table(...))` conversion with direct `data.frame(input = names(tab), Freq = as.integer(tab))` construction to avoid failures when `NA` appears in table names (`row names contain missing values`).

# cat2cat 0.4.7

* New `cat2cat_ml_run` function to check the ml models performance before `cat2cat` with ml option is run. Now, the ml models are more transparent.
* Add tests for cat2cat related journal (softwarex) paper.
* Add CITATION file.
* Internal changes to make the code base more clear.
* Replace itemize with describe in the latex documentation.

# cat2cat 0.4.6

* Add some `ropensci` standards, like CONTRIBUTING file and `testthat` version 3.
* Add LICENSE file.
* Update DESCRIPTION file.
* Update outdated URLs.
* `cat2cat_agg` has updated `cat_var` argument to two new ones, `cat_var_old` and `cat_var_new`.
* Rename the `master` to `main` branch.
* Improve tests and lintr related issues.

# cat2cat 0.4.5

* The `freqs_df` argument in the `cat2cat` function is moved from data to mappings part, it is backward compatible. 
Now it is consistent with the python cat2cat implementation.
* `pkgcheck` related fixes, like 80 chars per line.
* Improve `data` and `library` calls style.

# cat2cat 0.4.4

* Fix example in the `cat2cat` function.
* Improve documentation.
* Improve validation.

# cat2cat 0.4.3

* Fix `dummy_c2c` to be backward compatible.
* Add more tests for base utils functions.
* Improve `cat_apply_freq` function performance.
* Improve documentation.
* Improve validation.

# cat2cat 0.4.2

* The `ml` argument in the `dummy_c2c` function is redefined, shorter names for a simpler usage.
* The `cat2cat` ml part is using direct `cat_var` for target (for an update) dataset now, not the one from the `ml` argument list.
* Improve procedure graphs.

# cat2cat 0.4.1

* additional `cat2cat` validation, if the `trans` table covers all needed levels.
* documentation and tests improvements.

# cat2cat 0.4.0

* new syntax of the `ml` and `data` argument in the `cat2cat::cat2cat` function, two additional arguments each.
* `prune_c2c` scales the weights now, so still sum to one for each subject.
* new function `dummy_c2c` to add a default `cat2cat` columns to a `data.frame`.
* `occup`and `occup_small` datasets have 4 periods now.
* add the 4 periods example to the vignette.
* custom `pkgdown` reference.

# cat2cat 0.3.3

* improve the `pkgdown` website.
* `deparse` instead of `deparse1`.

# cat2cat 0.3.2

* `tinyverse` world, even less dependencies.

# cat2cat 0.3.1

* updated README file.
* improved covr and tests.
* updated vignette.

# cat2cat 0.3.0

* fixed the `cat2cat` function, the ml part is assuming that categorical variable is always named "code". 
* fixed problems when using only one feature in the ml part of `cat2cat` function.
* persist NA values in a categorical variable in the cat2cat function.
* updated vignette.
* improved roxygen2 docs.
* transferred caret and `randomForest` packages to Suggests, they are delayed loaded now.
* styler of the code.
* more readable code.
* removed data.table from Imports.

# cat2cat 0.2.1

* usage of summary_c2c with the default df_new argument.
* plot_c2c roxygen2

# cat2cat 0.2.0

* plot_c2c

# cat2cat 0.1.9

* vignette improvements

# cat2cat 0.1.8

* pt inside summary_c2c
* VignetteIndexEntry

# cat2cat 0.1.7

* URL for doi in vignettes

# cat2cat 0.1.6

* basic vignette
* rm .internal.selfref attr for mapped df
* assertthat instead of stopifnot

# cat2cat 0.1.5

* data.R adding set.seed
* lack of a one bracket in description file

# cat2cat 0.1.4

* remove the unnecessary imports in the DESCRIPTION file

# cat2cat 0.1.3

* Title reduce to less than 65 characters.
* doi of journal paper added to a description section in the DESCRIPTION file.
* unnecessary dontrun in data.R were removed.
  
# cat2cat 0.1.2

* `occup_small` dataset to pass checks in terms of computation time of examples.
* data.table rbindlist.
* optimize for loop for ml models.

# cat2cat 0.1.1

* Title format in Description file.
* spelling.

# cat2cat 0.1.0

* More ml methods for `cat2cat` function - "knn", "rf", "lda".
* `prune_c2c` and `cross_c2c` to improve processing of results.
* Enable cat2cat for panel with unique identifier.
* Turn on github actions - covr, pkgdown, check.
* Preparation before R CRAN.
