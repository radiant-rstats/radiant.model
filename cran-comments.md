## Resubmission

This is a resubmission. In this version is have added a feature to the shiny interface to create screenshots of application settings. See NEWS.md. 

## Test environments

* macOS, R 4.2.1
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.

# Previous cran-comments

## Resubmission

This is a resubmission. In this version I have fixed bugs and updated documentation (see NEWS.md for details).

## Test environments

* macOS, R 4.2.0
* win-builder (devel)

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.data.


## Resubmission

This is a resubmission. In this version I addressed the `as.vector` issue communicated by Martin Maechler due to a breaking changed in R-devel. The key element of the email is shown below. This submissions should address the issue in radiant.model that was causing errors.

The reason is that we/I have introduced a new
as.vector.data.frame() method, and saying in NEWS

    • as.vector() gains a data.frame method which returns a
      simple named list, also obeying a long standing     
      ‘FIXME’ to enable as.vector(<data.frame>, mode="list").
      This will break code relying on 'as.vector(<data.frame>)' to
      return the unchanged data frame.

## Test environments

* local Ubuntu 20.04 through WSL2, R 4.0.5
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Resubmission

This is a resubmission. In this version is addressed a function clash with `rlang` (i.e., `is_empty`) and made adjustments to work with the latest version of `shiny` and `bootstrap4`

Update: Fixed the package size issue. Resubmitting version 1.4.2

## Test environments

* local Ubuntu 20.04 install, R 4.1.0
* local Ubuntu 20.04 through WSL2, R 4.0.5
* win-builder (devel)

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this version I have fixed a few bugs (see NEWS.md for details) and addressed a URL forwarding issues uncovered by CRAN.

## Test environments

* Ubuntu Linux through WSL2: R 4.0.5
* Windows (development) through devtools::check_win_devel()

## R CMD check result

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.model.

## Resubmission

This is a resubmission. In this version I have fixed a few bugs (see NEWS.md for details) and addressed a URL forwarding issues uncovered by CRAN.

## Test environments

* Ubuntu Linux through WSL2: R 4.0.5
* Windows (development) through devtools::check_win_devel()

## R CMD check result

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.model.


## Resubmission

This is a resubmission. Thanks for Kurt I was able to fix an error uncovered in CRAN testing.

## Test environments

* Local Windows install, R 4.0.3
* Windows (development) through devtools::check_win_devel()

## R CMD check result

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.model.


## Resubmission

This is a resubmission. In this version I have updated to URL that were being forwarded from https://www.rstudio.com to https://rstudio.com. I also added single quotes around software names such as 'radiant.data' in the Description field of the DESCRIPTION file.

## Test environments

* local OS X install, R 4.0.2
* Windows (development) through devtools::check_win_devel()

## R CMD check result

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.model.


## Resubmission

This is a resubmission. In this version I have tried to address the NoLD issue in a test uncovered by CRAN on Solaris. The tests all pass on a linux based NoLD set. However, I do not have access to a Solaris system to double check.

## Test environments

* local OS X install, R 4.0.2
* ubuntu 20.04, R Under development (unstable) (2020-08-03 r78964) with NoLD option set

## R CMD check result

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.model.

## Resubmission

This is a resubmission. In this version I have addressed an issue introduced by a change in broom 0.7.0 and attempted to address the NoLD issue identified by CRAN. 

The NoLD message I received previously refers to "xgb.attr which is a function from the XGBoost package that is not used explicitly in radiant.model. I have now confirmed this is an upstream issue in the xgboost package and I created an issue with a reproducible example (see https://github.com/dmlc/xgboost/issues/5935). As a workaround, until the issue is addressed in xgboost, I set `early_stopping_rounds` to 0 in the relevant examples in radiant.model. I hope this is acceptable.

Since testing on R-devel with NoLD was a bit of a challenge, at least for me, I created a docker image (vnijs/nold) with R-devel NoLD on Ubuntu with Rstudio and build tools available. See the repo (vnijs/NoLD/README.md) and dockerhub repo linked below. 

https://github.com/vnijs/NoLD
https://hub.docker.com/r/vnijs/nold

## Test environments

* local OS X install, R 4.0.2
* local Windows install, R 4.0.2
* ubuntu 20.04, R Under development (unstable) (2020-08-03 r78964) with NoLD option set
* ubuntu "bionic" (on travis-ci), R release and devel
* win-builder (devel)

## R CMD check result

There were no ERRORs or WARNINGs. There was one NOTE related to the number of non-standard dependencies. However, this note is not easily addressed without substantially inconveniencing users that rely on the web (shiny) interface available for radiant.model.


## Resubmission

This is a resubmission. In this version I have removed screenshots used in the documentation and linked to images on the documentation website instead to reduce package size. 

## Test environments

* local OS X install, R 3.6.3
* local Windows install, R 3.6.3
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder
* rhub

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 



## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.6.3
* local Windows install, R 3.6.2
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder
* rhub

## R CMD check results

There were no ERRORs or WARNINGs. There was one NOTE related to the size of the app/ directory. This is mostly due to screenshot used in the documentation. If this is problematic, please let me know and I'll try for a work-around. 

# Previous cran-comments

## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.6.1
* local Windows install, R 3.6.1
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder
* rhub

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 


## Resubmission

This is a resubmission. In this version I have fixed several bugs and added several new features (see NEWS.md for details).

## Test environments

* local OS X install, R 3.5.2
* local Windows install, R 3.5.2
* ubuntu "trusty" (on travis-ci), R release and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

## Resubmission

radiant.model was recently archived and removed from CRAN. Uwe Ligges confirmed that multiple emails were sent to my radiant@rady.ucsd.edu email address but, unfortunately, I cannot find any such emails in my inbox. I hope you will accept radiant.model as a resubmission.

## Test environments

* local OS X install, R 3.5.1
* local Windows install, R 3.5.1
* Ubuntu "trusty" (on travis-ci), R oldrel, release, and devel
* win-builder

## R CMD check results

There were no ERRORs, WARNINGs, or NOTEs. 

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
