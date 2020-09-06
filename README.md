# cat2cat <a href='https://github.com/polkas/cat2cat'><img src='./docs/cat2cat_logo.png' align="right" height="200" /></a>

[![R build status](https://github.com/polkas/cat2cat/workflows/R-CMD-check/badge.svg)](https://github.com/polkas/cat2cat/actions)
[![codecov](https://codecov.io/gh/Polkas/cat2cat/branch/master/graph/badge.svg)](https://codecov.io/gh/Polkas/cat2cat)

## Mapping of a categorical variable in a panel dataset according to a new encoding

**The main rule is to replicate the observation if it could be assign to a few categories**
**then using simple freqencies or ml model to approximate probabilities of being assign to each of them.**

Why cat2cat:  
- universal algorithm which could be used in different science fields  
- stop removing variables for ml models because variable categories are not the same across time  
- use a statistical modelling to join datasets from different time points and retain caterogical variable structure  
- visualize any factor variable across time  

In many projects where dataset contains a categorical variable one of the biggest obstacle is that 
the data provider during internal processes was changing an encoding of this variable during a time.
Thus some categories were grouped and other were separated or a new one is added or an old one is removed.

**Quick Reference Table** 

|  Function | Description |
|----------------------|----------------------|
| `cat2cat` |  |
| `cat2cat_agg`  |   |
| `prune_cat2cat` |  |
| `cross_cat2cat` |  |
| `get_mappings` | |  
| `cat_apply_freq` | |
| `get_freqs` | |

## Installation

```r
# install.packages("devtools")
devtools::install_github("polkas/cat2cat")
```

There should be stated a 3 clear questions:

1. Do i have a transition table. 
2. Type of the data - panel dataset with unique identifiers vs panel dataset without unique identifiers, aggregate data vs individual data.
3. Direction of a transition, forward or backward - use a new or an old encoding

For more advance usage check the vignette.

Quick intro:

## Manual transitions
## Aggragate dataset
```r
library(cat2cat)
library(dplyr)

data(verticals)
agg_old <- verticals[verticals$v_date == "2020-04-01", ]
agg_new <- verticals[verticals$v_date == "2020-05-01", ]

## cat2cat_agg - could map in both directions at once although 
## usually we want to have old or new representation

agg = cat2cat_agg(data = list(old = agg_old, 
                              new = agg_new, 
                              cat_var = "vertical", 
                              time_var = "v_date",
                              freq_var = "counts"), 
                  Automotive %<% c(Automotive1, Automotive2),
                  c(Kids1, Kids2) %>% c(Kids),
                  Home %>% c(Home, Supermarket))
            
## possible processing
  
agg$old %>% 
group_by(vertical) %>% 
summarise(sales = sum(sales*prop), counts = sum(counts*prop), v_date = first(v_date))

agg$new %>% 
group_by(vertical) %>%
summarise(sales = sum(sales*prop), counts = sum(counts*prop), v_date = first(v_date))
```
## Automatic using trans table
## Dataset with unique identifiers
```r
## the ean variable is a unique identifier
data(verticals2)

vert_old <- verticals2[verticals2$v_date == "2020-04-01", ]
vert_new <- verticals2[verticals2$v_date == "2020-05-01", ]

## get transitions table
trans_v <- vert_old %>% 
inner_join(vert_new, by = "ean") %>%
select(vertical.x, vertical.y) %>% distinct()

# 
## cat2cat
## it is important to set id_var as then we merging categories 1 to 1 
## for this identifier which exists in both periods.
verts = cat2cat(
  data = list(old = vert_old, new = vert_new, id_var = "ean", cat_var = "vertical", time_var = "v_date"),
  mappings = list(trans = trans_v, direction = "backward")
)
```
## Dataset without unique identifiers
```r
data(occup)
data(trans)

occup_old = occup[occup$year == 2008,]
occup_new = occup[occup$year == 2010,]

## cat2cat
cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

## with informative features it might be usefull to run ml algorithm
## currently only knn, lda or rf (randomForest),  a few methods could be specified at once 
## where probability will be assessed as fraction of closest points.
occup_2 = cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = "knn", features = c("age", "sex", "edu", "exp", "parttime", "salary"), 
            args = list(k = 10))
)
# mix of methods
occup_2_mix = cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = c("knn", "rf", "lda"), features = c("age", "sex", "edu", "exp", "parttime", "salary"), 
            args = list(k = 10, ntree = 50))
)
# correlation between ml models and simple fequencies
occup_2_mix$old %>% select(wei_knn_c2c, wei_rf_c2c, wei_lda_c2c, wei_freq_c2c) %>% cor()
# cross all methods and subset one highest probability category for each subject
occup_old_mix_highest1occup_2_mix <- occup_2_mix$old %>% 
                cross_cat2cat(.) %>% 
                prune_cat2cat(.,column = "wei_cross_c2c", method = "highest1") 
```
## Regression
```r
## orginal dataset 
lms2 <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old, weights = multipier)
summary(lms2)

## using one highest cross weights
## cross_cat2cat to cross differen methods weights
## prune_cat2cat - highest1 leave only one the highest probability obs for each subject
occup_old_2 <- occup_2$old %>% 
                cross_cat2cat(., c("wei_freq_c2c", "wei_knn_c2c"), c(1/2,1/2)) %>% 
                prune_cat2cat(.,column = "wei_cross_c2c", method = "highest1") 
lms <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old_2, weights = multipier)
summary(lms)

## we have to adjust size of stds as we artificialy enlarge degrees of freedom
occup_old_3 <- occup_2$old %>% 
                prune_cat2cat(method = "nonzero") #many prune methods like highest
lms1 <- lm(I(log(salary)) ~ age + sex + factor(edu) + parttime + exp, occup_old_3, weights = multipier * wei_freq_c2c)
## summary_c2c
summary_c2c(lms1, df_old = nrow(occup_old), df_new = nrow(occup_old_3))

```
