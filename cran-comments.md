## Resubmission

radiant.model was recently archived and removed from CRAN. The message at https://cran.r-project.org/web/packages/radiant.data/index.html shows: "Archived on 2018-10-03 as check problems were not corrected despite reminders." Uwe Ligges confirmed that multiple emails were sent to my radiant@rady.ucsd.edu email address but, for some, reason I cannot find any such emails in my inbox.

radiant.model showed errors on linux and windows CRAN (see link below). Since these errors were not present when the packages were last submitted to CRAN I assume they are related to updated dependencies that were posted to CRAN since then. I cannot replicate these errors on macOS, linux, or Windows however.

* ftp://ftp.math.ethz.ch/sfs/pub/Software/R-CRAN/web/checks/check_results_radiant.model.html

In sum, this is resubmission. In this version I have fixed some (minor) bugs and added several new features (see NEWS.md for details).

radiant.model depends on the simultaneously submitted radiant.data package and the radiant.basics that was also archived but showed no errors or warnings. 

## Test environments

* local OS X install, R 3.5.1
* local Windows install, R 3.5.1
* Ubuntu "trusty" (on travis-ci), R oldrel, release, and devel

I normally also test using win-builder, however, this gave the error below. This seems odd because radiant.data requires dplyr 7.6 which has been available on CRAN for all platforms for several months (https://cran.r-project.org/web/packages/dplyr/index.html). 

"Package required and available but unsuitable version: 'dplyr'"

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


# Previous cran-comments

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

radiant.model depends on the simultaneously submitted radiant.data and radiant.basics packages. From the last time I submitted updates to CRAN I seem to recall Uwe Ligges suggested I submit all radiant packages to be updated at the same time. I hope my recollection is correct and that this is indeed the preferred approach that will minimize workload for CRAN.

## Test environments

* local OS X install, R 3.5.0
* local Windows install, R 3.5.0
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs.

The [previous submission to CRAN](https://cran.rstudio.com/web/checks/check_results_radiant.model.html) showed a note: "Check: data for non-ASCII characters". However, I am not able to reproduce this during testing on Windows, macOS, or Linux

## Resubmission

This is a resubmission (0.8.0). In this version I have fixed several bugs and added 
several new features (see NEWS.md for details).

Please note that this version addresses the reverse dependency check warnings from radiant.data for radiant.model. Deprecating the `*_each` commands used in the 0.6.0 versions of the `radiant.*` packages is related to the deprecation of the `*_each` functions in dplyr. I will update the remaining `radiant.*` packages asap after radiant.model is available on CRAN.

## Test environments

* local OS X install, R 3.4
* local Windows install, R 3.4
* ubuntu 14.04 (on travis-ci), R 3.3.3 and R-dev
* win-builder (release)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE about a possibly mis-spelled word ("Analytics"). The spelling is correct however. Could this word be added to the spell check database perhaps?

## Resubmission

This is a resubmission. In this version I have:

* Updated a link to https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0) to fix a libcurl error. I prefer to keep this link in the README.md file because it provides a convenient summary of the terms of the AGPL3 license. The link is valid but perhaps there is something else that is required?
* Added a link to  https://www.r-project.org/Licenses/AGPL-3 in the LICENSE file.

## Test environments

* local OS X install, R 3.3.1
* local Windows install, R 3.3.1
* ubuntu 12.04 (on travis-ci), R 3.3.1
* win-builder (devel and release)

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE: New submission
