## Test environments
* local Arch Linux install: release
* travis-ci: 3.5, release and devel
* win-builder: devel

## R CMD check results

0 errors | 0 warnings | 0 note

* This is mainly a maintenance release to fix compatibility issue with upcoming
  release of dplyr.
* Accordingly, `gghighlight_point()` and `gghighlight_line()` became defunct.
  Considering that these functions have been deprecated for 1.5 years, I expect
  there will be little impact on users.
