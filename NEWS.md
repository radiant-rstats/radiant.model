# CHANGES IN radiant.model 0.9.0.2

* Left align tooltip in DiagrammeR plots (i.e., _Model >Decision Analysis_ and _Model > Classification and regression trees_)
* Add information about levels in tree splits to tooltips (_Model > Classification and regression trees_)
* Fix to ensure DiagrammeR plots are shown in Rmarkdown report generate in _Report > Rmd_ or _Report > R_

# CHANGES IN radiant.model 0.9.0.1

* Minor fix for coefficient plot in `plot.logistic`
* Default names when saving Decision Analysis input and output are now based on tree name
* The number of repetitions in Model > Simulate was NA when grid search was used
* Fixed state setting for decision analysis sensitivity input
* Fixed for special characters (e.g., curly quote) in input for Model > Decision Analysis
* Allow browser zoom for tree plots in Model > Decision Analysis and Model > Classification and Regression Trees
* Check that costs are not assigned to terminal nodes in Decision Analysis Trees.  Specifying a cost is only useful if it applies to multiple nodes in a branch. If the cost only applies to a terminal node adjust the payoff instead 
* Ensure : are followed by a space in the YAML input to Model > Decision Analysis
* Added option to generate normally distributed correlated data in Model > Simulate
* Added option to generate normally distributed simulated data with exact mean and standard deviation in Model > Simulate
* Fix for large weights that may cause an integer overflow
* Grid search specs ignored when _Model > Simulate > Repeat_ is set to `Simulate`
* Long lines of code generated for _Report > Rmd_ will be wrapped to enhance readability 
* Applied `styler` to code
* Enhanced keyboard shortcuts for estimation and reporting

# CHANGES IN radiant.model 0.8.7.4

* Upgraded tidyr dependency to 0.7
* Fix in `crs` when a tibble is passed

# CHANGES IN radiant.model 0.8.7.1

* Upgraded dplyr dependency to 0.7.1

# CHANGES IN radiant.model 0.8.3

* Moved coefficient formatting from summary.regress and summary.logistic to make result$coeff more easily accessible
* Added F-score to _Model > Evaluate classification > Confusion_
* Added option to use robust standard errors in _Linear regression_ and _Logistic regression_. The `HC1` covariance matrix is used to produce results consistent with Stata

## BUG FIXES

* Fixed RSME typo
* Don't calculate VIFs when stepwise regression selects only one explanatory variable

# CHANGES IN radiant.model 0.8.0

## NEW FEATURES

- Added Model > Naive Bayes based on e1071
- Added Model > Classification and regression trees based on rpart
- Added Model > Collaborative Filtering and example dataset (data/cf.rda)
- Various enhancements to evaluate (binary) classification models
- Improved plot sizing for Model > Decision Analysis
- Added Garson plot and moved all plots to the ANN > Plot tab
- Show progress indicators if variable acquisition takes some time
- Expanded coefficient csv file for linear and logistic regression
- Show dataset name in output if dataframe passed directly to analysis function 
- As an alternative to using the Estimate button to run a model you can now also use CTRL-enter (CMD-enter on mac)
- Use ALT-enter as a keyboard short-cut to generate code and sent to _Report > Rmd_ or _Report > R_
- Improved documentation on how to customize plots in _Report > Rmd_ or _Report > R_

## BUG FIXES

- Multiple tooltips in sequence in Decision Analysis
- Decision Analysis plot size in PDF was too small
- Replace histogram by distribution in regression plots
- Fix bug in regex for overlapping labels in variables section of Model > Decision Analysis
- Fixes for model with only an intercept (e.g., after stepwise regression)
- Update Predict settings when dataset is changed
- Fix for predict when using center or standardize with a command to generate the predictions
- Show full confusion matrix even if some elements are missing
- Fix for warnings when creating profit and gains charts
- Product dropdown for Model > Collaborative filtering did not list all variables

## Deprecated

- Use of *_each is deprecated
