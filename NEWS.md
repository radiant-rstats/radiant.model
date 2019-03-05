# radiant.model 0.9.9.0

* Allow any variable in the prediction dataset to be used to customize a prediction when using _Predict > Data & Command_
* Fix for `write.coeff` when interactions, quadratic, and/or cubic terms are included in a linear or logistic regression
* Rescale predictions in `cv.nn` so RMSE and MAE are in the original scale even if the data were standardized for estimation
* Rename `scaledf` to `scale_df` for consistency
* Fix for plot sizing and printing of missing values in collaborative filtering
* Fix for `cv.nn` when weights are used in estimation
* Improve documentation for cross-validation of `nn` and `crtree` models (i.e., `cv.nn` and `cv.crtree`)
* Fixes for breaking changes in dplyr 0.8.0
* Fix to download tables from _Model > Evaluate classificiation_
* Use an expandable `shinyAce` input for the formula and function inputs in _Model > Simulate_
* Fixes for repeated simulation with grid-search
* Use `test` instead of `validation` 

# radiant.model 0.9.8.0

* Option to add user defined function to simulations. This dramatically increases the flexibility of the simulation tool
* Ensure variable and dataset names are valid for R (i.e., no spaces or symbols), "fixing" the input as needed
* Cross validation functions for decision trees (`cv.crtree`) and neural networks(`cv.nn`) that can use various performance metrics for during evaluation e.g., `auc` or `profit`
* Option to add square and cube terms in _Model > Linear regression_ and _Model > Logistic regression_.
* Option to pass additional arguments to `shiny::runApp` when starting radiant such as the port to use. For example, radiant.model::radiant.model("https://github.com/radiant-rstats/docs/raw/gh-pages/examples/demo-dvd-rnd.state.rda", port = 8080) 
* Avoid empty string showing up in auto-generated code for model prediction (i.e., `pred_data` or `pred_cmd`) 
* Fix for VIF based on `car` for `regress` and `logistic`
* Load a state file on startup by providing a (relative) file path or a url. For example, radiant.model::radiant.model("https://github.com/radiant-rstats/docs/raw/gh-pages/examples/demo-dvd-rnd.state.rda") 
* Don't  live-update the active tree input to make it easier to save edits to a new tree without adding edits to the existing tree (Model > Decision analysis)
* Fix for NA error when last line of a decision analysis input is a node without a payoff or probability
* Load input (CMD + O) and Save input (CMD + S) keyboard shortcuts for decision analysis

# radiant.model 0.9.7.0

## Major changes

* Using [`shinyFiles`](https://github.com/thomasp85/shinyFiles) to provide convenient access to data located on a server

## Minor changes

* Fix for simulations that use a data set as part of the analysis
* Replace non-ASCII characters in example datasets
* Remove `rstudioapi` as a direct import
* Revert from `svg` to `png` for plots in `_Report > Rmd_ and _Report > R_. `svg` scatter plots with many point get to big for practical use on servers that have to transfer images to a local browser
* Removed dependency on `methods` package

# radiant.model 0.9.5.0

## Major changes

* Various changes to the code to accomodate the use of `shiny::makeReactiveBinding`. The advantage is that the code generated for _Report > Rmd_ and _Report > R_ will no longer have to use `r_data` to store and access data. This means that code generated and used in the Radiant browser interface will be directly usable without the browser interface as well.
* Improved documentation and examples

# radiant.model 0.9.2.3

## Bug fixes

* Fix for https://github.com/radiant-rstats/radiant/issues/53

# radiant.model 0.9.2.2

## Major changes

* Show the interval used in prediction for _Model > Regression_ and _Model > logistic_ (e.g., "prediction" or "confidence" for linear regression)
* Auto complete in _Model > Decision analysis_ now provides hints based on the current tree input and any others defined in the app. It also provides suggestions for the basic element of the tree (e.g., `type: decision`, `type: chance`, `payoff`, etc.)
* Updated user messages for _Model > Decision analysis_ when input has errors

# radiant.model 0.9.2.1

## Major changes

* Default interval for predictions from a linear regression is now "confidence" rather than "prediction"
* `Estimate model` button indicates when the output has been invalidated and the model should be re-estimated
* Combined _Evaluate classification_ Summary and Plot into Evaluate tab
* Upload and download data using the Rstudio file browser. Allows using relative paths to files (e.g., data or images inside an Rstudio project)

## Minor changes

* Require `shinyAce` 0.3.0 in `radiant.data` and `useSoftTabs` for _Model > Decision Analysis_

# radiant.model 0.9.1.0

## Major changes

* Add Poisson as an option for _Model > Simulate_

## Bug fixes

* Fix for [#43](https://github.com/radiant-rstats/radiant/issues/43) where scatter plot was not shown for a dataset with less than 1,000 rows
* Fixed example for logistic regression prediction plot
* Fix for case weights when minimum response value is 0

# radiant.model 0.9.0.15

## Minor changes

* Allow character variables in estimation and prediction
* Depend on DiagrammeR 1.0.0

# radiant.model 0.9.0.13

## Major changes

* Residual diagnostic plot for Neural Network regression
* Improved handling of case weights for logistic regression and neural networks 

## Minor changes
* Show number of observations used in training and validation in _Model > Evaluate classification_
* Use Elkan's formula to adjust probabilities when using `priors` in `crtree` (`rpart`)
* Added options to customize tree generated using `crtree` (based on `rpart`)
* Better control of tree plot size in `plot.crtree`
* Cleanup of `crtree` code
* Improved printing of NN weights
* Option to change font size in NN plots 
* Keyboard shortcut: Press return when cursor is in textInput to store residuals or predictions

## Bug fixes

* Fix for tree labels when (negative) integers are used

# radiant.model 0.9.0.8

## Minor changes

* Cleanup of lists returned by `evalbin` and `confusion`
* Add intercept in coefficient tables that can be downloaded for linear and logistic regression or using `write.coeff`
* Convert logicals to factors in `crtree` to avoid labels < 0.5 and >= 0.5
* Improved labeling of decision tree splits in `crtree`. The tooltip (aka hover-over) will contain all levels used, but the tree label may be truncated as needed

## Bug fixes

* Fix input reset when screen size or zoom level is changed

# radiant.model 0.9.0.4

* Renamed `ann` to `nn`. The `ann` function is now deprecated

# radiant.model 0.9.0.3

## Major changes

* Prediction confidence interval provided for logistic regression based on blog post by [Gavin Simpson] (https://www.fromthebottomoftheheap.net/2017/05/01/glm-prediction-intervals-i/)
* Argument added to `logistic` to specify if profiling or the Wald method should be used for confidence intervals. Profiling will be used by default for datasets with fewer than 5,000 rows

# radiant.model 0.9.0.2

## Minor changes

* Left align tooltip in DiagrammeR plots (i.e., _Model >Decision Analysis_ and _Model > Classification and regression trees_)
* Add information about levels in tree splits to tooltips (_Model > Classification and regression trees_)

## Bug fixes

* Fix to ensure DiagrammeR plots are shown in Rmarkdown report generate in _Report > Rmd_ or _Report > R_

# radiant.model 0.9.0.1

## Major changes

* Added option to generate normally distributed correlated data in Model > Simulate
* Added option to generate normally distributed simulated data with exact mean and standard deviation in Model > Simulate
* Long lines of code generated for _Report > Rmd_ will be wrapped to enhance readability 

## Minor changes

* Default names when saving Decision Analysis input and output are now based on tree name
* Allow browser zoom for tree plots in Model > Decision Analysis and Model > Classification and Regression Trees
* Enhanced keyboard shortcuts for estimation and reporting
* Applied `styler` to code

## Bug fixes

* Grid search specs ignored when _Model > Simulate > Repeat_ is set to `Simulate`
* The number of repetitions in Model > Simulate was NA when grid search was used
* Fix for large weights that may cause an integer overflow
* Minor fix for coefficient plot in `plot.logistic`
* Fixed state setting for decision analysis sensitivity input
* Fixed for special characters (e.g., curly quote) in input for Model > Decision Analysis
* Check that costs are not assigned to terminal nodes in Decision Analysis Trees.  Specifying a cost is only useful if it applies to multiple nodes in a branch. If the cost only applies to a terminal node adjust the payoff instead 
* Ensure : are followed by a space in the YAML input to Model > Decision Analysis

# radiant.model 0.8.7.4

## Minor change

* Upgraded dplyr dependency to 0.7.1
* Upgraded tidyr dependency to 0.7

## Bug fix

* Fix in `crs` when a tibble is passed

# radiant.model 0.8.3.0

## Major change
* Added option to use robust standard errors in _Linear regression_ and _Logistic regression_. The `HC1` covariance matrix is used to produce results consistent with Stata

## Minor changes
* Moved coefficient formatting from summary.regress and summary.logistic to make result$coeff more easily accessible
* Added F-score to _Model > Evaluate classification > Confusion_

## Bug fixes

* Fixed RSME typo
* Don't calculate VIFs when stepwise regression selects only one explanatory variable

# radiant.model 0.8.0.0

## Major changes

* Added Model > Naive Bayes based on e1071
* Added Model > Classification and regression trees based on rpart
* Added Model > Collaborative Filtering and example dataset (data/cf.rda)
* Various enhancements to evaluate (binary) classification models
* Added Garson plot and moved all plots to the ANN > Plot tab

## Minor changes

* Improved plot sizing for Model > Decision Analysis
* Show progress indicators if variable acquisition takes some time
* Expanded coefficient csv file for linear and logistic regression
* Show dataset name in output if dataframe passed directly to analysis function 
* As an alternative to using the Estimate button to run a model you can now also use CTRL-enter (CMD-enter on mac)
* Use ALT-enter as a keyboard short-cut to generate code and sent to _Report > Rmd_ or _Report > R_
* Improved documentation on how to customize plots in _Report > Rmd_ or _Report > R_

## Bug fixes

* Multiple tooltips in sequence in Decision Analysis
* Decision Analysis plot size in PDF was too small
* Replace histogram by distribution in regression plots
* Fix bug in regex for overlapping labels in variables section of Model > Decision Analysis
* Fixes for model with only an intercept (e.g., after stepwise regression)
* Update Predict settings when dataset is changed
* Fix for predict when using center or standardize with a command to generate the predictions
* Show full confusion matrix even if some elements are missing
* Fix for warnings when creating profit and gains charts
* Product dropdown for Model > Collaborative filtering did not list all variables

## Deprecated

* Use of *_each is deprecated
