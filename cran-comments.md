Reason for early update: fixes errors on solaris and debian clang

## Test environments
* x86_64-pc-linux-gnu (64-bit); Ubuntu 14.04.6 LTS;  R 3.6.1 - OK
* x86_64-w64-apple-darwin15.6.0 (64-bit); macOS Mojave 10.14.4; R 3.6.1 - OK
* x86_64-w64_mingw32 (64-bit); R devel (2019-12-02 r77499) - NOTE
* x86_64-w64_mingw32 (64-bit); R 3.5.3 - NOTE
* x86_64-w64_mingw32 (64-bit); R 3.6.1 - NOTE
* Ubuntu Linux 16.04 LTS, R-release, GCC - NOTE
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit - NOTE

## R CMD check results
There were no ERRORs or WARNINGs.

There were 3 NOTEs:

New submission

Possibly mis-spelled words in DESCRIPTION:
  mlr (20:19)

Found the following (possibly) invalid URLs:
  URL: https://dl.acm.org/citation.cfm?id=1354603
    From: man/LearnerSurvGBM.Rd
    Status: 403
    Message: Forbidden
    
Found the following (possibly) invalid URLs:
   URL: https://cran.r-project.org/web/checks/check_results_mlr3proba.html
   From: README.md
   Status: 404
   Message: Not Found
    
  * `mlr` is a package name;
  * The CRAN link will be valid once accepted
  * The URL is valid
  
## Downstream dependencies
There are currently no downstream dependencies for this package.
