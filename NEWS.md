# Emil News and Changelog

## Version 2.2.0:
- Changed the representation of selected features from logical to integer.
  This was to allow multiple features to be generated from a single original
  feature by the pre-processing functions (analogously to the representation of
  oversampled observations in resampling schemes).
- Added a pre-processing function for generating binary features from factors
  (`pre_factor_to_logical`).
- Added the news and changelog.