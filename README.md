
<!-- README.md is generated from README.Rmd. Please edit that file -->
gghighlight
===========

[![Travis-CI Build Status](https://travis-ci.org/yutannihilation/gghighlight.svg?branch=master)](https://travis-ci.org/yutannihilation/gghighlight)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/gghighlight)](https://cran.r-project.org/package=gghighlight)

Highlight geoms in ggplot2.

Installation
------------

``` r
install.packages("gghighlight")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("yutannihilation/gghighlight")
```

Example
-------

Suppose we have a data that has so many series that it is hard to identify them by their colours as the differences are so subtle.

``` r
library(ggplot2)

ggplot(d) +
  geom_line(aes(idx, value, colour = type))
```

![](man/figures/README-ggplot2-simple-1.png)

With `gghighlight()`, we can highlight the lines whose max values are larger than 20:

``` r
library(gghighlight)

ggplot(d) +
  geom_line(aes(idx, value, colour = type)) +
  gghighlight(max(value) > 20)
#> label_key: type
```

![](man/figures/README-gghighlight-simple-1.png)

`gghighlight()` can highlight almost any geoms. For more info, please read [Introduction to gghighlight](https://yutannihilation.github.io/gghighlight/docs/index.html).
