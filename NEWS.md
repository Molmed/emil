# News and Changelog

## emil 2.2.1
### New features
* Added the news and changelog.
* Changed the representation of selected features from logical to integer.
  This was to allow multiple features to be generated from a single original
  feature by the pre-processing functions (analogously to the representation of
  oversampled observations in resampling schemes).
* Added a pre-processing function for generating binary features from factors
  (`pre_factor_to_logical`).
  
### Bug fixes and minor improvements
* Changed all unqualified non-default package imports to qualified imports ().
* Changed the dependency on *dplyr* to an import and re-export the `select` 
  function (#2). The initial purpose of having *dplyr* as a dependency rather
  than an import was to automatically attach it upon loading *emil* to allow
  class-specific extension to `select`. The current solution provides the same
  functionality without affecting the global search path.
