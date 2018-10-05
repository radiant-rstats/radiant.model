## Resubmission

radiant.model was recently archived and removed from CRAN. Uwe Ligges confirmed that multiple emails were sent to my radiant@rady.ucsd.edu email address but, unfortunately, I cannot find any such emails in my inbox. I hope you will accept radiant.model as a resubmission.

## Test environments

* local OS X install, R 3.5.1
* local Windows install, R 3.5.1
* Ubuntu "trusty" (on travis-ci), R oldrel, release, and devel
* win-builder

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
