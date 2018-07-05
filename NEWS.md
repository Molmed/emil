# News and Changelog

## emil 2.2.10
Compatibility update for *survival* v2.42-5.

Improved build tools (only for developer convenice, not included in the package).

## emil 2.2.7
Compatibility update for *dplyr* v0.6.0.

## emil 2.2.6
* Fixed broken cross-references in the documentation.
* Corrected bugs in the tests.

## emil 2.2.3
### New features
* Functions for generating dummy variables for unordered and ordered factors.
* Added wrappers for SVM and naive Bayes classifiers (using the *e1071* package).

### Bug fixes and minor improvements
* Updated package for *ggplot2* v1.0.1.9003 and the coming update in december (#3).

## emil 2.2.1
### New features
* Added the news and changelog.
* Changed the representation of selected features from logical to integer.
  This was to allow multiple features to be generated from a single original
  feature by the pre-processing functions (analogously to the representation of
  oversampled observations in resampling schemes).
* Added a function for generating binary features from factors 
  (`factor_to_logical`) and a pre-processing wrapper for it
  (`pre_factor_to_logical`).
  
### Bug fixes and minor improvements
* Changed all unqualified non-default package imports to qualified imports.
* Changed the dependency on *dplyr* to an import and re-export the `select` 
  function (#2). The initial purpose of having *dplyr* as a dependency rather
  than an import was to automatically attach it upon loading *emil* to allow
  class-specific extension to `select`. The current solution provides the same
  functionality without affecting the global search path.
