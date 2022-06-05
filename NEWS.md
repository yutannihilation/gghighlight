# gghighlight 0.3.3

* Fix the deprecated use of `dplyr::across()` in `dplyr::filter()` (#177).

# gghighlight 0.3.2

* `gghighlight()` now accepts `n()` so that you can highlight based on the
  number of rows within each group (#154).
* `gghighlight()` now preserves `colour` or `fill` when explicit `NULL` is
  specified on `unhighlighted_params` (i.e. `unhighlighted_params = list(colour = NULL)`)
  (#152).
* Fix a longstanding bug of wrong calculation when the data has the same name of
  columns as aesthetics, e.g. `x` (#168).

# gghighlight 0.3.1

* `gghighlight()` now can add labels on discrete scales (#160).

# gghighlight 0.3.0

## Breaking changes

* `gghighlight_point()` and `gghighlight_line()`, deprecated in gghighlight 0.1.0, are now defunct (#132).

# gghighlight 0.2.0

## New features

* `gghighlight()` gets a new argument `unhighlighted_params`, which accepts a
  list of parameters for the unhighlighted layer (e.g. `colour`, `fill`, `shape`,
  and `size`). Accordingly, `unhighlighted_colour` is deprecated (#76).

* `gghighlight()` gets a new argument `keep_scales` to choose whether to keep the
  original scale with the shadowed data (#72).

* `gghighlight()` gets a new (experimental) argument `calculate_per_facet` to
  choose whether to calculate highlighting per facet or not (#14).

## Bug fixes

* If the mapping has `group`, use it as grouping variable, which is consistent
  with the logic of ggplot2 (#77).

* `gghighlight()` now ignores if the calculation fails over some layers. This
  is useful to combine with such layers as `annotate()` (#78).

* `gghighlight()` now allows to highlight 0-layer plots, which means just
  filtering the plot data (#81).

* `gghighlight()` now ignores `NA`s in numeric predicates (#86).

# gghighlight 0.1.0

* Add `gghighlight()`, which replaces the current `gghighlight_line()` and `gghighlight_point()`; these functions are now deprecated.
* Add a introductory vignette.

# gghighlight 0.0.1

* First release


