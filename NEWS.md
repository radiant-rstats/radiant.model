# CHANGES IN radiant.model VERSION 0.6.6 (unreleased)

## NEW FEATURES

- Added Model > Naive Bayes based on e1071

## BUG FIXES

- Remove \\r and special characters from strings in r_data and r_state 
- Multiple tooltips in sequence in Decision Analysis
- Decision Analysis plot size in PDF was too small
- Replace histogram by distribution in regression plots
- Fix bug in regex for overlapping labels in variables section of Model > Decision Analysis
- Fixes for model with only an intercept (e.g., after stepwise regression)
- Update Predict settings when dataset is changed
- Fix for predict when using center or standardize with a command to generate the predictions
