# cat2cat
## transform a categorical variable according to a new encoding

In many projects where dataset contains a caterogical variable one of the biggest obstacle is that 
the data provider during internal proceesses was changing an encoding of this variable during a time.
Thus some categories were grouped and other were separated or a new one is added or an old one is removed.

There should be stated a 3 clear questions:

1. Do i have a transition table. 
2. Type of the data - panel balanced or unbalanced or aggregate data vs individual data.
3. Direction of a transition, forward or backward - use a new or an old encoding

For more advance usage check the vigniette.

Quick intro:

```r
data(occup)
data(trans)

occup_old = occup[occup$year == 2008,]
occup_new = occup[occup$year == 2010,]

# Automatic using trans table

cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward")
)

cat2cat(
  data = list(old = occup_old, new = occup_new, cat_var = "code", time_var = "year"),
  mappings = list(trans = trans, direction = "backward"),
  ml = list(method = "knn", features = c("age", "sex", "edu", "exp", "parttime", "salary"), args = list(k = 10))
)

# Manual transitions

## Simulate dataset
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

agg = cat2cat_man(data = list(old = agg_old, 
                              new = agg_new, 
                              cat_var = "vertical", 
                              freq_var = "counts"), 
                  Automotive %<% c(Automotive1, Automotive2),
                  c(Kids1, Kids2) %>% c(Kids),
                  Home %>% c(Home, Supermarket))

agg$old %>% 
group_by(vertical) %>% 
summarise(sales = sum(sales*prop), counts = sum(counts*prop), v_date = first(v_date))

agg$new %>% 
group_by(vertical) %>%
summarise(sales = sum(sales*prop), counts = sum(counts*prop), v_date = first(v_date))
```
