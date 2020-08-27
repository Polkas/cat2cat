# cat2cat

[![R build status](https://github.com/polkas/cat2cat/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/cat2cat/actions)
[![codecov](https://codecov.io/gh/Polkas/cat2cat/branch/master/graph/badge.svg)](https://codecov.io/gh/Polkas/cat2cat)

## transform a categorical variable according to a new encoding

Why cat2cat:
- stop removing variables for ml models because categories are not the same across time
- use a statistical modelling to join datasets from different time points and retain caterogical variable structure
- visualize any factor variable across time
- universal algorithm which could be used in different science fields

In many projects where dataset contains a categorical variable one of the biggest obstacle is that 
the data provider during internal processes was changing an encoding of this variable during a time.
Thus some categories were grouped and other were separated or a new one is added or an old one is removed.

## Installation

```r
# install.packages("devtools")
devtools::install_github("polkas/cat2cat")
```

There should be stated a 3 clear questions:

1. Do i have a transition table. 
2. Type of the data - panel balanced or unbalanced or aggregate data vs individual data.
3. Direction of a transition, forward or backward - use a new or an old encoding

For more advance usage check the vignette.

Quick intro:

```r
library(cat2cat)

# Manual transitions

## Simulate datasets
agg_old <- data.frame(
  vertical = c("Electronics", "Kids1", "Kids2", "Automotive", "Books",
               "Clothes", "Home", "Fashion", "Health", "Sport"),
  sales = rnorm(10, 100, 10),
  counts = rgeom(10, 0.0001),
  v_date = rep("2020-04-01", 10), stringsAsFactors = F
)

agg_new <- data.frame(
  vertical = c("Electronics", "Supermarket", "Kids", "Automotive1", 
               "Automotive2", "Books", "Clothes", "Home", "Fashion", "Health", "Sport"),
  sales = rnorm(11, 100, 10),
  counts = rgeom(11, 0.0001),
  v_date = rep("2020-05-01", 11), stringsAsFactors = F
)

#

agg = cat2cat_man(data = list(old = agg_old, 
                              new = agg_new, 
                              cat_var = "vertical", 
                              time_var = "v_date",
                              freq_var = "counts"), 
                  Automotive %<% c(Automotive1, Automotive2),
                  c(Kids1, Kids2) %>% c(Kids),
                  Home %>% c(Home, Supermarket))

# possible processing
library(dplyr)
  
agg$old %>% 
group_by(vertical) %>% 
summarise(sales = sum(sales*prop), counts = sum(counts*prop), v_date = first(v_date))

agg$new %>% 
group_by(vertical) %>%
summarise(sales = sum(sales*prop), counts = sum(counts*prop), v_date = first(v_date))

#

data(occup)
data(trans)

occup_old = occup[occup$year == 2008,]
occup_new = occup[occup$year == 2010,]

# Automatic using trans table

cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

# with informative features it might be usefull to run ml algorithm - currently only knn
# where probability will be assessed as fraction of closest points.

occup_2 = cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = "knn", features = c("age", "sex", "edu", "exp", "parttime", "salary"), args = list(k = 10))
)

occup_old_2 <- occup_2$old %>% prune_cat2cat(method = "nonzero") #many prune methods like highest

# Regression

# we have to adjust size of stds as we artificialy enlarge degrees of freedom

lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_2$old, weights = multipier * wei_freq_c2c)

summary_c2c(lms, df_old = nrow(occup_old), df_new = nrow(occup_old_2))

# orginal dataset 

lms2 <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old, weights = multipier)

summary(lms2)

```
