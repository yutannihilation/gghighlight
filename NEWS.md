# gghighlight 0.1.0.9000

* `gghighlight()` gets a new argument `unhighlighted_params`, which accepts a
  list of parameters for the unhighlighted layer (e.g. `colour`, `fill`, `shape`,
  and `size`). Accordingly, `unhighlighted_colour` is deprecated (#76).

* If the mapping has `group`, use it as grouping variable, which is consistent
  with the logic of ggplot2 (#77).

# gghighlight 0.1.0

* Add `gghighlight()`, which replaces the current `gghighlight_line()` and `gghighlight_point()`; these functions are now deprecated.
* Add a introductory vignette.

# gghighlight 0.0.1

* First release


