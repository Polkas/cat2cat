# cat2cat <a href='https://github.com/polkas/cat2cat'><img src='man/figures/cat2cat_logo.png' align="right" width="200px" /></a>
[![R build status](https://github.com/polkas/cat2cat/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/cat2cat/actions)
[![CRAN](http://www.r-pkg.org/badges/version/cat2cat)](https://cran.r-project.org/package=cat2cat)
[![codecov](https://codecov.io/gh/Polkas/cat2cat/branch/master/graph/badge.svg)](https://codecov.io/gh/Polkas/cat2cat)
[![Dependencies](https://tinyverse.netlify.com/badge/cat2cat)](https://cran.r-project.org/package=cat2cat)

## Handling an Inconsistent Coded Categorical Variable in a Panel Dataset

Unifying an inconsistent coded categorical variable in a panel/longtitudal dataset.  
There is offered the `cat2cat` procedure to map a categorical variable according to a transition table between two different time points.
The transition table should to have a candidate for each category from the targeted for an update period. The main rule is to replicate the observation if it could be assigned to a few categories, then using simple frequencies or statistical methods to approximate probabilities of being assigned to each of them.

**This algorithm was invented and implemented in the paper by (Nasinski, Majchrowska and Broniatowska (2020) <doi:10.24425/cejeme.2020.134747>).**

[**Please visit the cat2cat webpage for more information**](https://polkas.github.io/cat2cat/articles/cat2cat.html)

## Installation

```r
# install.packages("remotes")
remotes::install_github("polkas/cat2cat")
# or
install.packages("cat2cat")
```

## Example

```{r}
library(cat2cat)
data(occup)
data(trans)

occup_old <- occup[occup$year == 2008, ]
occup_new <- occup[occup$year == 2010, ]

occup_simple <- cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)
```

**The 4 period example is presented in the "Get Started" vignette.**

## UML

The graphs present how the `cat2cat::cat2cat` function works, in this case under a panel dataset without the unique identifiers and only two periods.

![Backward Mapping](./man/figures/back_nom.png)

![Forward Mapping](./man/figures/for_nom.png)


