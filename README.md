
<!-- README.md is generated from README.Rmd. Please edit that file -->
gghighlight
===========

Highlight points and lines in ggplot2.

Installation
------------

You can install gghighlight from github with:

``` r
# install.packages("devtools")
devtools::install_github("yutannihilation/gghighlight")
```

Example
-------

Suppose the data has a lot of series.

``` r
library(tidyverse)

set.seed(1)
d <- tibble(
  idx = 1:10000,
  value = runif(idx, -1, 1),
  type = sample(letters, size = length(idx), replace = TRUE)
) %>%
  group_by(type) %>%
  mutate(value = cumsum(value)) %>%
  ungroup()
```

It is difficult to distinguish them by colour.

``` r
ggplot(d) +
  geom_line(aes(idx, value, colour = type))
```

![](images/ggplot-too-many-1.png)

So we are motivated to highlight only important series, like this:

``` r
library(gghighlight)

gghighlight_line(d, aes(idx, value, colour = type), max(value) > 20)
```

![](images/gghighlight-line-1.png)
