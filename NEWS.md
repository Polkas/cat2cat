# cat2cat 0.4.6.9006

* Add tests for cat2cat related journal (softwarex) paper.
* Internal changes to make the code base more clear.

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
