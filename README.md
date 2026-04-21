# cat2cat <a href='https://github.com/polkas/cat2cat'><img src='man/figures/cat2cat_logo.png' alt="cat2cat logo" align="right" width="200px" /></a>

[![R build status](https://github.com/polkas/cat2cat/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/cat2cat/actions)
[![CRAN](http://www.r-pkg.org/badges/version/cat2cat)](https://cran.r-project.org/package=cat2cat)
[![codecov](https://codecov.io/gh/Polkas/cat2cat/branch/main/graph/badge.svg)](https://app.codecov.io/gh/Polkas/cat2cat)
[![Dependencies](https://tinyverse.netlify.com/badge/cat2cat)](https://cran.r-project.org/package=cat2cat)

## Handling an Inconsistent Coded Categorical Variable in a Longitudinal Dataset

**cat2cat** provides a statistical solution for harmonising categorical variables whose encoding changes between survey waves or data releases. 
If you work with longitudinal data where classification schemes evolve—occupations (ISCO), diseases (ICD), industries (NACE), products, education fields—this package enables valid cross-temporal analysis.

### The Problem

Real-world classifications change.
When ISCO-88 becomes ISCO-08, or ICD-9 becomes ICD-10, a single old code may map to multiple new codes (and vice versa).
Naive approaches require separate analyses for each period, forcing manual mappings on high-level aggregates, or ignoring the problem altogether—leading to limited analysis.

### The Solution

**cat2cat** procedure maps a categorical variable according to a mapping (transition) table between two different time points. 
The mapping (transition) table should to have a candidate for each category from the targeted for an update period. 
The main rule is to replicate the observation if it could be assigned to a few categories, then using simple frequencies or modern statistical methods to approximate probabilities of being assigned to each of them.

**cat2cat** implements a **replication-and-weighting** algorithm that:

1. Replicates each observation onto all candidate categories from the mapping table
2. Assigns probability weights (summing to 1 per subject) based on category frequencies or ML predictions
3. Preserves the central moments of non-mapped variables—coefficients remain unbiased

The result: a unified categorical variable across all periods, ready for longitudinal analysis or trend studies

### Direction

With cat2cat, you can harmonize in both directions:

#### Forward Mapping (Old → New)

![](man/figures/for_nom.png)

#### Backward Mapping (New → Old)

![](man/figures/back_nom.png)

For evolutionary classifications (new one is more detailed), forward mapping will produce fewer replications.
For hierarchical classifications (each digit adds detail), we can consider to truncate mapping table to fewer digits to reduce replication for backward mapping.

### Key Features

| Feature | Benefit |
|---------|---------|
| **Moment-preserving weights** | Regression coefficients for non-mapped variables remain unbiased |
| **Multiple weight methods** | Frequency-based, knn, random forest, LDA—compare and ensemble |
| **Multi-period chaining** | Handle 3, 4, or more waves with iterative mapping |
| **SE correction** | `summary_c2c()` adjusts standard errors for replicated data |
| **Fixed effects ready** | Unified `g_new_c2c` variable enables occupation/industry FE across time |
| **Aggregated data support** | `cat2cat_agg()` handles pre-aggregated counts with equation syntax |
| **Cross-validation** | `cat2cat_ml_run()` validates weights before committing |
| **Minimal dependencies** | Base R only in Imports; ML methods are in Suggests |

### References

- **Method**: [Nasinski, Majchrowska & Broniatowska (2020)](https://doi.org/10.24425/cejeme.2020.134747) — *Central European Journal of Economic Modelling and Econometrics*
- **Software**: [Nasinski & Gajowniczek (2023)](https://doi.org/10.1016/j.softx.2023.101525) — *SoftwareX*

### Ecosystem

| | |
|---|---|
| [**R Package**](https://cran.r-project.org/package=cat2cat) | CRAN, production-ready |
| [**Python Package**](https://pypi.org/project/cat2cat/) | PyPI, equivalent functionality |
| [**Documentation**](https://polkas.github.io/cat2cat/) | Full API reference and vignettes |

## Documentation

For guidance on when cat2cat is appropriate (and when it isn't), see the [When cat2cat won't help](https://polkas.github.io/cat2cat/articles/cat2cat.html#when-cat2cat-wont-help) section in the Get Started vignette.


- [Get Started](https://polkas.github.io/cat2cat/articles/cat2cat.html) - Core concepts, assumptions, and a step-by-step example with the `cat2cat()` function
- [Multi-Period Chaining](https://polkas.github.io/cat2cat/articles/cat2cat_multi_period.html) — chaining `cat2cat()` across 3+ survey waves, building 4-period panels
- [Sensitivity Analysis & Holdout Validation](https://polkas.github.io/cat2cat/articles/cat2cat_validation.html) — comparing weight methods, pruning strategies, ensembles, ML validation with `cat2cat_ml_run()`
- [Regression on Replicated Data](https://polkas.github.io/cat2cat/articles/cat2cat_regression.html) — SE correction with `summary_c2c()`, fixed effects models, unbiasedness proof
- [Panel Data with Subject Identifiers](https://polkas.github.io/cat2cat/articles/cat2cat_panel.html) — `id_var` for rotational panels with consistent subject IDs across waves
- [Aggregated Data & Special Cases](https://polkas.github.io/cat2cat/articles/cat2cat_aggregated.html) — `cat2cat_agg()` for count-level data with mapping equations, building mapping tables from hierarchical codes

## Installation

```r
# Stable release from CRAN
install.packages("cat2cat")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("polkas/cat2cat")
```

## Citation

If you use cat2cat in your research, please cite:

```
Nasinski M, Gajowniczek K (2023). "cat2cat: Handling an Inconsistently Coded 
Categorical Variable in a Longitudinal Dataset." SoftwareX, 24, 101525. 
doi:10.1016/j.softx.2023.101525
```

```bibtex
@article{nasinski2023cat2cat,
  title={cat2cat: Handling an Inconsistently Coded Categorical Variable in a Longitudinal Dataset},
  author={Nasinski, Maciej and Gajowniczek, Krzysztof},
  journal={SoftwareX},
  volume={24},
  pages={101525},
  year={2023},
  doi={10.1016/j.softx.2023.101525}
}
```
