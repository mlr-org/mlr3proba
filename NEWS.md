# mlr3proba dev

* fix: allow cloning of measure objects
* refactor: `TaskSurv` uses only right, left or interval censoring, simplified code a lot in the methods
* feat: add `TaskCompRisks` class and `as_task_cmprk()` S3 methods (support for right-censored data only)
* fix: as.data.table() for `PredictionSurv` objects holds now a survival curve per observation as it should

# mlr3proba 0.7.4

* fix + update `MeasureSurv`: survival measure labels are now printed and the `obs_loss` property is now supported
* feat: add `na.rm` parameter to `msr("surv.calib_index")` to avoid `NaN` scores

# mlr3proba 0.7.3

* feat: added new calibration measure => `msr("surv.calib_index")`
* refactor + feat: `autoplot.PredictionSurv`
  * The default `"calib"` plot uses the survival matrix directly now which is faster
  * `"dcalib"` has extra barplot + better documentation
  * Added new `type = "scalib"` which constructs the smoothed calibration plots as in Austin et al. (2020)
  * **BREAKING CHANGE**: `"preds"` is now called `"isd"` (individual survival distribution). `row_ids` can now be used to filter the observations for which you draw the survival curves.

# mlr3proba 0.7.2

* fix: `lrn("surv.coxph")` is now trained with `model=TRUE` which fixes an issue with using observation weights [stackoverflow link](https://stackoverflow.com/questions/79297386/mlr3-predicted-values-for-surv-coxph-learner-with-case-weights).
* cleanup: remove `tsk("unemployment")` and associated files
* cleanup: remove unused references

# mlr3proba 0.7.1

* cleanup: removed all `PipeOp`s and pipelines related to survival => regression reduction techniques (see #414)
* fix: `$predict_type` of `survtoclassif_disctime` and `survtoclassif_IPCW` was `prob` (classification type) and not `crank` (survival type)
* fix: G(t) is not filtered when `t_max|p_max` is specified in scoring rules (didn't influence evaluation at all)
* docs: Clarified the use and impact of using `t_max` in scoring rules, added examples in scoring rules and AUC scores
* feat: Added new argument `remove_obs` in scoring rules to remove observations with observed time `t > t_max` as a processing step to alleviate IPCW issues.
This was before 'hard-coded' which made the Integrated Brier Score (`msr("surv.graf")`) differ minimally from other implementations and the original definition.

# mlr3proba 0.7.0

* Add `mlr3pipelines` to `Imports` and set minimum latest version from CRAN (`0.7.0`)
* Refactor code to minimize namespace calling and imports such as `mlr3pipelines::` or `R6::`
* Doc updates: add experimental badge in a some PipeOps + add references in others
* Add argument `scale_lp` for AFT `distrcompose` pipeop + respective pipeline

# mlr3proba 0.6.9

* New `PipeOp`s: `PipeOpTaskSurvClassifIPCW`, `PipeOpPredClassifSurvIPCW`
* New pipeline (**reduction method**): `pipeline_survtoclassif_IPCW`
* Improved the way Integrated Brier score handles the `times` argument and the `t_max`, especially when the survival matrix has one time point (column)
* Improved documentation of integrated survival scores
* Improved documentation of all pipelines
* Temp fix of math-rendering issue in package website
* Add experimental `lifecycle` badge for 3 pipelines (`survtoregr`, `distrcompositor` and `probregr`) - these are currently either not supported by literature or tested enough.

# mlr3proba 0.6.8

* `Rcpp` code optimizations
* Fixed ERV scoring to comply with `mlr3` dev version (no bugs before)
* Skipping `survtoregr` pipelines due to bugs (to be refactored in the future)

# mlr3proba 0.6.7

* Deprecate `crank` to `distr` composition in `distrcompose` pipeop (only from `lp` => `distr` works now)
* Add `get_mortality()` function (from `survivalmodels::surv_to_risk()`
* Add Rcpp function `assert_surv_matrix()`
* Update and simplify `crankcompose` pipeop and respective pipeline (no `response` is created anymore)
* Add `responsecompositor` pipeline with `rmst` and `median`

# mlr3proba 0.6.6

* Small fixes and refactoring to the discrete-time pipeops

# mlr3proba 0.6.5

* Add support for discrete-time survival analysis
* New `PipeOp`s: `PipeOpTaskSurvClassifDiscTime`, `PipeOpPredClassifSurvDiscTime`
* New pipeline (**reduction method**): `pipeline_survtoclassif_disctime`

# mlr3proba 0.6.4

* Add useR! 2024 tutorial
* Lots of refactoring, improving code quality, migration to testthat v3, etc. (thanks to @m-muecke)

# mlr3proba 0.6.3

* Add new tasks from `survival` package: `veteran`, `pbc`, `mgus`, `gbsg`
  * Refined docs for loaded tasks, `task$help()` works as it should now
  * All loaded tasks have now **complete cases** (no features with missing values)
* Refined docs for task generators
* Added task generator from `coxed` package
* Added new methods for `TaskSurv`: `cens_prop()`, `admin_cens_prop()`, `dep_cens_prop()` and `prop_haz()`

# mlr3proba 0.6.2

* Updates in `surv.cindex` measure
  * added `p_max` (same as `surv.graf`)
  * refactor `cutoff` to `t_max`

# mlr3proba 0.6.1

* Compatibility with upcoming 'paradox' release.
* Fix bug when applying `t_max` in `surv.graf` and similar time-integrated scores.

# mlr3proba 0.6.0

* Optimized `surv.logloss` and `calib_alpha` measures (bypassing `distr6`)
* Update/refine all measure docs (naming conventions from upcoming scoring rules paper) + doc templates
* fix very rare bugs in `calib_alpha`, `surv.logloss` and `surv.graf` (version with proper = FALSE)

# mlr3proba 0.5.9

* Fix several old issues (#348, #301, #281)
* `distrcompositor` and `crankcompositor` deprecated functions were completely removed

# mlr3proba 0.5.8

* Fix Breslow issues (`phash` warning and `Inf` lp predictions)

# mlr3proba 0.5.7

* Add `breslow` function for estimating the cumulative baseline hazard of proportional hazard models
* Add `PipeOpBreslow` to wrap a survival learner and generate `distr` predictions from `lp` predictions
* Add option `breslow` estimator option in `distrcompositor`

# mlr3proba 0.5.6

* Add `extend_quantile` to `autoplot.PredictionSurv` for `type = "dcalib"`, which imputes NAs with the maximum observed survival time
* Fixes default in `autoplot.PredictionSurv`, now `"calib"`
* Update `msr("surv.dcalib")` default for `truncate` to `Inf`

# mlr3proba 0.5.5

* Add `$reverse()` method to `TaskSurv`, which returns the same task but with 1-status.
* Add `reverse` parameter to `TaskSurv$kaplan()` method, which calculates Kaplan-Meier on the censoring distribution of the task (1-status).

# mlr3proba 0.5.4

* Fix bottlenecks in Dcalib and RCLL

# mlr3proba 0.5.3

* Add support for learners that can predict multiple posterior distributions by using `distr6::Arrdist`

# mlr3proba 0.5.2

* Add `plot_probregr` for plotting probabilistic regression distribution predictions
* Fix big bug in `surv.rcll` creating erroneous results as distributions were not being subsetted correctly

# mlr3proba 0.5.1

* Add `regr.logloss`

# mlr3proba 0.5.0

* Possibly small breaking change, renamed `PipeOpProbregrCompositor` to `PipeOpProbregr` and default distribution now `"Uniform"`.
* Renamed `probregrcompositor` pipeline to `probregr` and default distribution now `"Uniform"`.
* Bumped minor version to highlight all fixes in measures below
* Bugfix in setting Uno's AUC parameters
* Bugfix in RCLL when no censoring
* Bugfix in all learners when making single predictions

# mlr3proba 0.4.17

* Fix bug in `surv.rcll` when individual scores are `NA`.

# mlr3proba 0.4.16

* Remove `theme_mlr3` from plots.
* Fix bug in `surv.logloss` when using `IPCW = TRUE`

# mlr3proba 0.4.15

* Fix bug in `surv.cindex` caused when probability of censoring was 0. Added `eps` parameter to control this.

# mlr3proba 0.4.14

* Fix bug in converting distr6 distributions to matrices when creating `PredictionSurv`
* Fix bug in RCLL

# mlr3proba 0.4.13

* Fix minor bug in 'SG' cindex method

# mlr3proba 0.4.12

* Fix bug in survavg pipeline causing unequal weights not to be applied

# mlr3proba 0.4.11

* Fix minor bug in scoring rules

# mlr3proba 0.4.10

* Added `ERV` parameter to scoring rule measures to return more interpretable scoring rules. Explained Residual Variation is the percentage decrease between a scoring rule comparing a Kaplan-Meier baseline to the learner of interest.

# mlr3proba 0.4.9

* Fixed bug in surv.logloss causing IPCW weighting to not be applied correctly

# mlr3proba 0.4.8

* Bug fixes in AUC measures

# mlr3proba 0.4.7

* Add right-censored log loss
* Fix bug in {rpart} where model was being discarded when set to be kept. Parameter `model` now called `keep_model`.

# mlr3proba 0.4.6

* Patch for upstream breakages
* Add `TaskSurv$kaplan` method
* {survivalmodels} now imported (previously suggested)

# mlr3proba 0.4.5

* Improved reduction from survival matrix predictions to ranking predictions
* Fixed cindex bug when all predictions equal
* Fix for valgrind

# mlr3proba 0.4.4

* Minor change to how distributions are created to better support improper distributions
* Fixed bug in `simsurv` task that made it impossible to predict the target

# mlr3proba 0.4.3

* Massive speed-up in distrcompositor PipeOp/pipeline
* More informative error given if `$distr` called for a learner that does not support this return type
* Fix massive bottleneck in scoring rule measures
* Add Density coercions `as_task_dens` and `as_prediction_dens`
* Measures now use parameter sets like learners. This streamlines the interface but unfortunately means ids can no longer be set dynamically.
* Add parameters `t_max` and `p_max` to Graf, Schmid and Integrated Log-loss as an alternative to `times`. `t_max` is equivalent to `times = seq(t_max)` and `p_max` is the proportion of censoring to integrate up to in the dataset.
* Fix bug in Rcpp code that was causing erroneous values for calculating the cindex in datasets greater than 20,000 observations.

# mlr3proba 0.4.2

* Patch for linux

# mlr3proba 0.4.1

* Remove `mlr3extralearners` from Suggests
* Add `response` to `as_prediction_surv`
* Now exported a couple cpp functions and `assert_surv`
* `mlr3` is now in `Depends` not `imports`
* `distr` predictions are now internally stored as matrices to significantly reduce prediction object sizes
* Tasks now support strata property

# mlr3proba 0.4.0

* Deprecated measures from 0.2.0 have now been deleted.
* IPCW measures such as `surv.graf`, `surv.schmid`, and `surv.intlogloss` now allow training data to be passed to the score function with `task` and `train_set` to allow the censoring distribution to be estimated on the training data. This is automatically applied for resample and benchmark results.
* IPCW measures such as `surv.graf`, `surv.schmid`, and `surv.intlogloss` now include a parameter `proper` to determine what weighting scheme should be applied by the estimated censoring distribution, The current method (Graf, 1999) `proper = FALSE`, weights observations either by their event time or 'current' time depending if they're dead or not, the new method `proper = TRUE` weights observations by event time. The `proper = TRUE` method is strictly proper when censoring and survival times are independent and G is estimated on large enough data. The `proper = FALSE` method is never proper. The default is currently `proper = FALSE` to enable backward compatibility, this will be changed to `proper = TRUE` in v0.6.0.
* The `rm_cens` parameter in `surv.logloss` has been deprecated in favour of `IPCW`. `rm_cens` will be removed in v0.6.0. If `rm_cens` or `IPCW` are `TRUE` then censored observations are removed and the score is weighted by an estimate of the censoring distribution at individual event times. Otherwise if `rm_cens` and `IPCW` are `FALSE` then no deletion or weighting takes place. The `IPCW = TRUE` method is strictly proper when censoring and survival times are independent and G is estimated on large enough data. The `ipcw = FALSE` method is never proper.
* Add `surv.dcalib` for the D-Calibration measure from Haider et al. (2020).

# mlr3proba 0.3.2

* Patched bug causing ``"interval2"`` task type not to work
* Fixed bug causing pipelines not to function correctly in `$aggregate`

# mlr3proba 0.3.1

* Reverted removal of `"interval2"`

# mlr3proba 0.3.0

* Commonly used survival quantities have been added as active bindings to `TaskSurv` including `times` (observed survival times), `status` (observed survival indicator), `unique_times` (set of sorted unique outcome times), `unique_event_times` (set of sorted unique failure times), `risk_set` (set of observations alive 'just before' a given time)
* `"interval2"` censoring type has been removed from `TaskSurv` as this is covered by the other types
* Default values have now been given to the `time` and `event` arguments in `TaskSurv`
* `PredictionDens` can now include `distr` return type (equivalent to `learner$model`)

# mlr3proba 0.2.6

* Minor internal fixes

# mlr3proba 0.2.5

* `PipeOpCrankCompositor` updated to fix bottleneck in computation via `mean`. Now `Inf` or `NA` is replaced by `0` for `response` and imputed with the median for `crank`
* Bug in `distr` predict types fixed that lead to fitting degenerate distributions and returning incorrect values for mean survival time and `crank`

# mlr3proba 0.2.4

* CRITICAL BUG FIX - `compose_crank` was previously returning ranks with the reverse ordering so that higher ranks implied higher risk not lower.

# mlr3proba 0.2.3

* All learners that previously lived in the mlr3learners organisation are now in the [mlr3extralearners](https://github.com/mlr-org/mlr3extralearners) repository.
* Fixed bottleneck in `MeasureSurvLogloss`
* Bugfix in `MeasureSurvCalibrationAlpha`
* Patch for valgrind
* `TaskDens` now inherits from `TaskUnsupervised` which means `target`/`truth` has been removed. No specification of a `target` column is required, instead a one-column matrix-like object or numeric vector should be passed to the task `backend` and the density will be estimated for this column, or two columns and one set as `weight`.
* Fixed bug in  `load_eruption` to fix name of data columns
* Added calibration plot for comparing average predicted survival distribution to Kaplan-Meier to [mlr3viz](https://github.com/mlr-org/mlr3viz)
* Removed unneccessary `pracma` dependency in learners
* Fix in `PipeOpDistrCompositor`, previously base distribution was only using the first predicted distribution, now the baseline is taken by averaging over all predictions with uniform weights

# mlr3proba 0.2.2

* Default kernel for `LearnerDensityKDE` is now `Epan` to reduce imports
* Minor internal patches for mlr3 0.6.0
* Bug fix in `MeasureSurvCalibrationBeta` now returns `NA` not error if `lp` predict type not available

# mlr3proba 0.2.1

* Removed `PredictionRegr` causing masking issues with `{mlr3}`
* Bug fix in `PipeOpDistrCompositor` causing some `cdf` predictions to be lost
* Internal fixes for `mlr3pipelines`: public train and predict methods to private
* Added four datasets and tasks: `grace`, `actg`, `gbcs`, `whas`
* Add `overwrite` to `crankcompositor` pipeop and pipeline
* Bug fix in `surv.kaplan` `crank` prediction

# mlr3proba 0.2.0

### Added Functionality

* `MeasureSurvCindex` added. Generalises all c-index measures with a fast C++ implementation
* Akritas estimator added to `mlr3learners/mlr3learners.proba`
* Added scoring rule `MeasureSurvSchmid`
* Addd calibration measures `MeasureSurvCalibrationBeta` and `MeasureSurvCalibrationAlpha`
* `surv.brier` alias added for `surv.graf`
* `response` parameter added to `PipeOpCrankCompositor` and `crankcompositor` to now optionally fill `response` predict type with same values as `crank`
* Added `PipeOpProbregrCompostior` and `compose_probregr` for composition to `distr` return type from (a) regression learner(s) predicting `response` and `se`
* Added `PipeOpSurvAvg` and `surv_averager` pipeline for weighted model averaging of distr, lp, crank, and response predictions.

### Deprecated Functionality

* The following measures are deprecated use `MeasureSurvCindex` instead with following parameters: `MeasureSurvBeggC`, use defaults; `MeasureSurvHarrellC`, use defaults; `MeasureSurvUnoC`, use `weight_meth = 'G/2'`; `MeasureSurvGonenC`, use `weight_method = 'GH'`
* `MeasureSurvGrafSE`, `MeasureSurvLoglossSE`, `MeasureSurvIntLoglossSE`, `MeasureSurvRMSESE`, `MeasureSurvMSESE`, and `MeasureSurvMAESE` all deprecated and will be deleted in v0.4.0. Use `msr("surv.graf", se = TRUE)` instead (for example).
* Measures renamed such that `surv.nagelkR2` is now `surv.nagelk_r2`, analogously for all R2, AUC, TPR, and TNR measures. Old constructors will be deleted in v0.4.0.
* Renamed `distrcompose` and `crankcompose` to `distr_compose` and `crank_compose`. Old ids will be deleted in v0.4.0.

### Edited Functionality

* Measures renamed such that `surv.nagelkR2` is now `surv.nagelk_r2`, analogously for all R2, AUC, TPR, and TNR measures. Old constructors will be deleted in v0.4.0.
* `MeasureSurvGraf` and `MeasureSurvIntLogloss` now have much faster C++ implementation

### Moved Functionality

- `LearnerSurvGlmnet`, `LearnerSurvCVGlmnet`, `LearnerSurvXgboost` and `LearnerSurvRanger` have been moved to `mlr-org/mlr3learners`

- `LearnerSurvGBM` has been moved to https://www.github.com/mlr3learners/mlr3learners.gbm

- `LearnerSurvMboost`, `LearnerSurvGlmBoost`, `LearnerSurvGamboost`, `LearnerSurvBlackboost` have been moved to https://www.github.com/mlr3learners/mlr3learners.mboost



# mlr3proba 0.1.6

* Early release due to backward compatibility error introduced by an upstream dependency
* Minor updates to `mboost` family of learners: added `gehan` family, fixed parameters for `cindex`, added support for:  `weights`, `response` predict type, `importance`, `selected_features`
* Minor internal changes
* All density learners except `LearnerDensHist` and `LearnerDensKDE` have been moved to the `mlr3learners org`
* The following survival learners have been moved to  the `mlr3learners org`, LearnerSurv: `Flexible`, `ObliqueRSF`, `Penalized`, `RandomForestSRC`
* Bugfix in `LearnerSurvXgboost` previously `lp` was erroneously returned as `exp(lp)`
* Now licenced under LPGL-3

# mlr3proba 0.1.5

* `LearnerSurvParametric` and `LearnerSurvNelson` moved to `mlr3learners/mlr3learners.survival ` repo
* `LearnerSurvCoxboost` and `LearnerSurvCVCoxboost` moved to `mlr3learners/mlr3learners.coxboost ` repo
* `LearnerSurvSVM` moved to `mlr3learners/mlr3learners.survivalsvm` repo
* In the next release, all learners except for `LearnerSurvKaplan`, `LearnerSurvCoxPH`, and `LearnerDensHist` will be moved to the `mlr3learners` org
* Minor internal changes

# mlr3proba 0.1.4

* Density estimation has now been added to mlr3proba, see `TaskDens`, `LearnerDens`, `PredictionDens`, and `MeasureDens`.
* Added `mlr_tasks_faithful` and `mlr_tasks_precip` for density task examples
* Added `mlr_task_generators_simdens` for generating density tasks
* Added learners for density estimation, see `mlr3::mlr_learners$keys("^dens")` for the full list
* In line with mlr3 0.1.7, public methods `train_internal`, `predict_internal`, `score_internal` are now private methods `.train`,`.predict`,`.score`
* Converted to roxygen2 R6 documentation

# mlr3proba 0.1.3

* Changed `lp` in `surv.parametric` to include the intercept, which is in line with `survival::survreg`. Now `exp(pred$lp)` is equal to the predicted survival time for AFTs
* Moved `mboost` to `suggests`
* Added `response` predict type, which predicts the time until event. Currently only supported for AFT models in `surv.parametric`
* Added measures for `response` predict type: `MeasureSurvMAE, MeasureSurvMAESE, MeasureSurvMSE, MeasureSurvMSESE, MeasureSurvRMSE, MeasureSurvRMSESE`

# mlr3proba 0.1.2

* Fixed error in r-patched-solaris
* Added `mode` option to `crankcompositor`
* Fixes bug resulting from `R62S3` incompatibility

# mlr3proba 0.1.1

* Added `method` argument to integrated scores and added weighting by bin-width
* Added notes to IGS documentation regarding default methods and comparison to other packages
* Added `method` to `MeasureSurvIntegrated` constructor and fields
* Fixed mistake in documentation of: `TaskSurv`, `MeasureSurvUnoC`
* Added missing `LearnerSurvRpart` parameter `parms` and `cost`
* Fixed errors in r-patched-solaris and r-devel debian-clang

# mlr3proba 0.1.0

* Initial upload to CRAN.
