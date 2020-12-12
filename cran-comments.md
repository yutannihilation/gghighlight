## Test environments
* local Ubuntu Linux install: release
* GitHub Actions CI: 3.6, release and devel
* win-builder: devel

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a maintenance release to address the following minor things.
    * Conditional use of suggested package vdiffr
    * Fix test breakage on R-devel due to the change on all.equal()
