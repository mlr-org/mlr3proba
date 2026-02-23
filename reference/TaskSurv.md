# Survival Task

This task specializes
[mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html) and
[mlr3::TaskSupervised](https://mlr3.mlr-org.com/reference/TaskSupervised.html)
for **single-event survival** problems. The target is comprised of
survival times and an event indicator (\\0\\ represents censored
observations, \\1\\ represents observations that had the event). Every
row corresponds to one subject/observation.

Predefined tasks are stored in
[mlr3::mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.html).

The `task_type` is set to `"surv"`.

**Note:** Currently only right-censoring is supported, though it
possible to create tasks with left and interval censoring using the
[Surv](https://rdrr.io/pkg/survival/man/Surv.html) interface.

## References

Grambsch, Patricia, Therneau, Terry (1994). “Proportional hazards tests
and diagnostics based on weighted residuals.” *Biometrika*, **81**(3),
515–526.
[doi:10.1093/biomet/81.3.515](https://doi.org/10.1093/biomet/81.3.515) ,
<https://doi.org/10.1093/biomet/81.3.515>.

## See also

Other Task:
[`TaskDens`](https://mlr3proba.mlr-org.com/reference/TaskDens.md),
[`mlr_tasks_actg`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_actg.md),
[`mlr_tasks_faithful`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_faithful.md),
[`mlr_tasks_gbcs`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_gbcs.md),
[`mlr_tasks_gbsg`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_gbsg.md),
[`mlr_tasks_grace`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_grace.md),
[`mlr_tasks_lung`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_lung.md),
[`mlr_tasks_mgus`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_mgus.md),
[`mlr_tasks_precip`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_precip.md),
[`mlr_tasks_rats`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_rats.md),
[`mlr_tasks_veteran`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_veteran.md),
[`mlr_tasks_whas`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_whas.md)

## Super classes

[`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html) -\>
[`mlr3::TaskSupervised`](https://mlr3.mlr-org.com/reference/TaskSupervised.html)
-\> `TaskSurv`

## Active bindings

- `cens_type`:

  (`character(1)`)  
  Returns the type of censoring, one of `"right"`, `"left"` or
  `"interval"`.

  Currently, only the `"right"` censoring type is fully supported, the
  rest are experimental and the API might change in the future.

## Methods

### Public methods

- [`TaskSurv$new()`](#method-TaskSurv-new)

- [`TaskSurv$truth()`](#method-TaskSurv-truth)

- [`TaskSurv$formula()`](#method-TaskSurv-formula)

- [`TaskSurv$times()`](#method-TaskSurv-times)

- [`TaskSurv$status()`](#method-TaskSurv-status)

- [`TaskSurv$unique_times()`](#method-TaskSurv-unique_times)

- [`TaskSurv$unique_event_times()`](#method-TaskSurv-unique_event_times)

- [`TaskSurv$kaplan()`](#method-TaskSurv-kaplan)

- [`TaskSurv$reverse()`](#method-TaskSurv-reverse)

- [`TaskSurv$cens_prop()`](#method-TaskSurv-cens_prop)

- [`TaskSurv$admin_cens_prop()`](#method-TaskSurv-admin_cens_prop)

- [`TaskSurv$dep_cens_prop()`](#method-TaskSurv-dep_cens_prop)

- [`TaskSurv$prop_haz()`](#method-TaskSurv-prop_haz)

- [`TaskSurv$clone()`](#method-TaskSurv-clone)

Inherited methods

- [`mlr3::Task$add_strata()`](https://mlr3.mlr-org.com/reference/Task.html#method-add_strata)
- [`mlr3::Task$cbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-cbind)
- [`mlr3::Task$data()`](https://mlr3.mlr-org.com/reference/Task.html#method-data)
- [`mlr3::Task$divide()`](https://mlr3.mlr-org.com/reference/Task.html#method-divide)
- [`mlr3::Task$droplevels()`](https://mlr3.mlr-org.com/reference/Task.html#method-droplevels)
- [`mlr3::Task$filter()`](https://mlr3.mlr-org.com/reference/Task.html#method-filter)
- [`mlr3::Task$format()`](https://mlr3.mlr-org.com/reference/Task.html#method-format)
- [`mlr3::Task$head()`](https://mlr3.mlr-org.com/reference/Task.html#method-head)
- [`mlr3::Task$help()`](https://mlr3.mlr-org.com/reference/Task.html#method-help)
- [`mlr3::Task$levels()`](https://mlr3.mlr-org.com/reference/Task.html#method-levels)
- [`mlr3::Task$materialize_view()`](https://mlr3.mlr-org.com/reference/Task.html#method-materialize_view)
- [`mlr3::Task$missings()`](https://mlr3.mlr-org.com/reference/Task.html#method-missings)
- [`mlr3::Task$print()`](https://mlr3.mlr-org.com/reference/Task.html#method-print)
- [`mlr3::Task$rbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-rbind)
- [`mlr3::Task$rename()`](https://mlr3.mlr-org.com/reference/Task.html#method-rename)
- [`mlr3::Task$select()`](https://mlr3.mlr-org.com/reference/Task.html#method-select)
- [`mlr3::Task$set_col_roles()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_col_roles)
- [`mlr3::Task$set_levels()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_levels)
- [`mlr3::Task$set_row_roles()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_row_roles)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    TaskSurv$new(
      id,
      backend,
      time = "time",
      event = "event",
      time2 = "time2",
      type = "right",
      label = NA_character_
    )

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `backend`:

  ([mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html))  
  Either a
  [DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html), or
  any object which is convertible to a
  [DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html)
  with
  [`as_data_backend()`](https://mlr3.mlr-org.com/reference/as_data_backend.html).
  E.g., a [`data.frame()`](https://rdrr.io/r/base/data.frame.html) will
  be converted to a
  [DataBackendDataTable](https://mlr3.mlr-org.com/reference/DataBackendDataTable.html).

- `time`:

  (`character(1)`)  
  Name of the column for event time if data is right censored, otherwise
  starting time if interval censored.

- `event`:

  (`character(1)`)  
  Name of the column giving the event indicator. If data is right
  censored then `0` means alive (no event) and `1` means dead (event).
  If `type` is `"interval"` then `event` is ignored.

- `time2`:

  (`character(1)`)  
  Name of the column for ending time of the interval for interval
  censored data, otherwise ignored.

- `type`:

  (`character(1)`)  
  The type of censoring. Can be `"right"` (default), `"left"` or
  `"interval"` censoring.

- `label`:

  (`character(1)`)  
  Label for the new instance.

#### Details

Depending on the censoring type (`"type"`), the output of a survival
task's `"$target_names"` is a
[`character()`](https://rdrr.io/r/base/character.html) vector with
values the names of the target columns. Specifically, the output is as
follows (and in the specified order):

- For `type` = `"right"` or `"left"`: (`"time"`, `"event"`)

- For `type` = `"interval"`: (`"time"`, `"time2"`)

------------------------------------------------------------------------

### Method `truth()`

True response for specified `row_ids`. This is the survival outcome
using the [Surv](https://rdrr.io/pkg/survival/man/Surv.html) format and
depends on the censoring type. Defaults to all rows with role `"use"`.

For censoring type:

- `"right|left"`: `Surv(time, event, type = "right|left")`

- `"interval"`: `Surv(time, time2, type = "interval2")`

#### Usage

    TaskSurv$truth(rows = NULL)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices.

#### Returns

[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).

------------------------------------------------------------------------

### Method [`formula()`](https://rdrr.io/r/stats/formula.html)

Creates a formula for survival models with
[`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html) on the
LHS (left hand side).

#### Usage

    TaskSurv$formula(rhs = NULL, reverse = FALSE)

#### Arguments

- `rhs`:

  If `NULL`, RHS (right hand side) is `"."`, otherwise RHS is `"rhs"`.

- `reverse`:

  If `TRUE` then formula calculated with 1 - status. Only applicable to
  `"right"` or `"left"` censoring.

#### Returns

[`stats::formula()`](https://rdrr.io/r/stats/formula.html).

------------------------------------------------------------------------

### Method `times()`

Returns the (unsorted) outcome times.

#### Usage

    TaskSurv$times(rows = NULL)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices.

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html)

------------------------------------------------------------------------

### Method `status()`

Returns the event indicator (aka censoring/survival indicator). If
censoring type is `"right"` or `"left"` then `1` is event and `0` is
censored. If censoring type is `"interval"` then `0` means
right-censored, `1` is event, `2` is left-censored and `3` is
interval-censored.

See [`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html).

#### Usage

    TaskSurv$status(rows = NULL)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices.

#### Returns

[`integer()`](https://rdrr.io/r/base/integer.html)

------------------------------------------------------------------------

### Method `unique_times()`

Returns the sorted unique outcome times.

#### Usage

    TaskSurv$unique_times(rows = NULL)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices.

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html)

------------------------------------------------------------------------

### Method `unique_event_times()`

Returns the sorted unique event (or failure) outcome times.

#### Usage

    TaskSurv$unique_event_times(rows = NULL)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices.

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html)

------------------------------------------------------------------------

### Method `kaplan()`

Calls
[`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html)
to calculate the Kaplan-Meier estimator.

#### Usage

    TaskSurv$kaplan(strata = NULL, rows = NULL, reverse = FALSE, ...)

#### Arguments

- `strata`:

  ([`character()`](https://rdrr.io/r/base/character.html))  
  Stratification variables to use.

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Subset of row indices.

- `reverse`:

  ([`logical()`](https://rdrr.io/r/base/logical.html))  
  If `TRUE` calculates Kaplan-Meier of censoring distribution
  (1-status). Default `FALSE`.

- `...`:

  (any)  
  Additional arguments passed down to
  [`survival::survfit.formula()`](https://rdrr.io/pkg/survival/man/survfit.formula.html).

#### Returns

[survival::survfit.object](https://rdrr.io/pkg/survival/man/survfit.object.html).

------------------------------------------------------------------------

### Method `reverse()`

Returns the same task with the status variable reversed, i.e., 1 -
status.

#### Usage

    TaskSurv$reverse()

#### Returns

TaskSurv.

------------------------------------------------------------------------

### Method `cens_prop()`

Returns the **proportion of censoring** for this survival task. This the
proportion of censored observations in case of `"right"` or `"left"`
censoring, otherwise the proportion of left (2), right (0) and interval
censored (3) observations when censoring type is `"interval"`.

By default, this is returned for all observations, otherwise only the
specified ones (`rows`).

#### Usage

    TaskSurv$cens_prop(rows = NULL)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices.

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html)

------------------------------------------------------------------------

### Method `admin_cens_prop()`

Returns an estimated proportion of **administratively censored
observations** (i.e. censored at or after a user-specified time point).
Our main assumption here is that in an administratively censored
dataset, the maximum censoring time is likely close to the maximum event
time and so we expect higher proportion of censored subjects near the
study end date.

Only designed for `"right"` censoring.

#### Usage

    TaskSurv$admin_cens_prop(rows = NULL, admin_time = NULL, quantile_prob = 0.99)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices.

- `admin_time`:

  (`numeric(1)`)  
  Administrative censoring time (in case it is known *a priori*).

- `quantile_prob`:

  (`numeric(1)`)  
  Quantile probability value with which we calculate the cutoff time for
  administrative censoring. Ignored, if `admin_time` is given. By
  default, `quantile_prob` is equal to \\0.99\\, which translates to a
  time point very close to the maximum outcome time in the dataset. A
  lower value will result in an earlier time point and therefore in a
  more *relaxed* definition (i.e. higher proportion) of administrative
  censoring.

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html)

------------------------------------------------------------------------

### Method `dep_cens_prop()`

Returns the proportion of covariates (task features) that are found to
be significantly associated with censoring. This function fits a
logistic regression model via [glm](https://rdrr.io/r/stats/glm.html)
with the censoring status as the response and using all features as
predictors. If a covariate is significantly associated with the
censoring status, it suggests that censoring may be *informative*
(dependent) rather than *random* (non-informative). This methodology is
more suitable for **low-dimensional datasets** where the number of
features is relatively small compared to the number of observations.

Only designed for `"right"` censoring.

#### Usage

    TaskSurv$dep_cens_prop(rows = NULL, method = "holm", sign_level = 0.05)

#### Arguments

- `rows`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row indices.

- `method`:

  (`character(1)`)  
  Method to adjust p-values for multiple comparisons, see
  [p.adjust.methods](https://rdrr.io/r/stats/p.adjust.html). Default is
  `"holm"`.

- `sign_level`:

  (`numeric(1)`)  
  Significance level for each coefficient's p-value from the logistic
  regression model. Default is \\0.05\\.

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html)

------------------------------------------------------------------------

### Method `prop_haz()`

Checks if the data satisfy the *proportional hazards (PH)* assumption
using the Grambsch-Therneau test, Grambsch (1994). Uses
[cox.zph](https://rdrr.io/pkg/survival/man/cox.zph.html). This method
should be used only for **low-dimensional datasets** where the number of
features is relatively small compared to the number of observations.

Only designed for `"right"` censoring.

#### Usage

    TaskSurv$prop_haz()

#### Returns

[`numeric()`](https://rdrr.io/r/base/numeric.html)  
If no errors, the p-value of the global chi-square test. A p-value \\\<
0.05\\ is an indication of possible PH violation.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskSurv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tsk("lung")

# meta data
task$target_names # target is always (time, status) for right-censoring tasks
#> [1] "time"   "status"
task$feature_names
#> [1] "age"       "meal.cal"  "pat.karno" "ph.ecog"   "ph.karno"  "sex"      
#> [7] "wt.loss"  
task$formula()
#> Surv(time, status, type = "right") ~ .
#> <environment: namespace:survival>

# survival data
task$truth() # survival::Surv() object
#>   [1]  455   210  1022+  310   361   218   166   170   567   613   707    61 
#>  [13]  301    81   371   520   574   118   390    12   473    26   107    53 
#>  [25]  814   965+   93   731   460   153   433   583    95   303   519   643 
#>  [37]  765    53   246   689     5   687   345   444   223    60   163    65 
#>  [49]  821+  428   230   840+  305    11   226   426   705   363   176   791 
#>  [61]   95   196+  167   806+  284   641   147   740+  163   655    88   245 
#>  [73]   30   477   559+  450   156   529+  429   351    15   181   283    13 
#>  [85]  212   524   288   363   199   550    54   558   207    92    60   551+
#>  [97]  293   353   267   511+  457   337   201   404+  222    62   458+  353 
#> [109]  163    31   229   156   329   291   179   376+  384+  268   292+  142 
#> [121]  413+  266+  320   181   285   301+  348   197   382+  303+  296+  180 
#> [133]  145   269+  300+  284+  292+  332+  285   259+  110   286   270   225+
#> [145]  269   225+  243+  276+  135    79    59   240+  202+  235+  239   252+
#> [157]  221+  185+  222+  183   211+  175+  197+  203+  191+  105+  174+  177+
task$times() # (unsorted) times
#>   [1]  455  210 1022  310  361  218  166  170  567  613  707   61  301   81  371
#>  [16]  520  574  118  390   12  473   26  107   53  814  965   93  731  460  153
#>  [31]  433  583   95  303  519  643  765   53  246  689    5  687  345  444  223
#>  [46]   60  163   65  821  428  230  840  305   11  226  426  705  363  176  791
#>  [61]   95  196  167  806  284  641  147  740  163  655   88  245   30  477  559
#>  [76]  450  156  529  429  351   15  181  283   13  212  524  288  363  199  550
#>  [91]   54  558  207   92   60  551  293  353  267  511  457  337  201  404  222
#> [106]   62  458  353  163   31  229  156  329  291  179  376  384  268  292  142
#> [121]  413  266  320  181  285  301  348  197  382  303  296  180  145  269  300
#> [136]  284  292  332  285  259  110  286  270  225  269  225  243  276  135   79
#> [151]   59  240  202  235  239  252  221  185  222  183  211  175  197  203  191
#> [166]  105  174  177
task$status() # event indicators (1 = death, 0 = censored)
#>   [1] 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1
#>  [38] 1 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 0 1 0 1 1 1 0 1 1 1 1 1 1
#>  [75] 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 0 1 1 1 0 1 1 0 1 1 1 1
#> [112] 1 1 1 1 0 0 1 0 1 0 0 1 1 1 0 1 1 0 0 0 1 1 0 0 0 0 0 1 0 1 1 1 0 1 0 0 0
#> [149] 1 1 1 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0
task$unique_times() # sorted unique times
#>   [1]    5   11   12   13   15   26   30   31   53   54   59   60   61   62   65
#>  [16]   79   81   88   92   93   95  105  107  110  118  135  142  145  147  153
#>  [31]  156  163  166  167  170  174  175  176  177  179  180  181  183  185  191
#>  [46]  196  197  199  201  202  203  207  210  211  212  218  221  222  223  225
#>  [61]  226  229  230  235  239  240  243  245  246  252  259  266  267  268  269
#>  [76]  270  276  283  284  285  286  288  291  292  293  296  300  301  303  305
#>  [91]  310  320  329  332  337  345  348  351  353  361  363  371  376  382  384
#> [106]  390  404  413  426  428  429  433  444  450  455  457  458  460  473  477
#> [121]  511  519  520  524  529  550  551  558  559  567  574  583  613  641  643
#> [136]  655  687  689  705  707  731  740  765  791  806  814  821  840  965 1022
task$unique_event_times() # sorted unique event times
#>   [1]   5  11  12  13  15  26  30  31  53  54  59  60  61  62  65  79  81  88
#>  [19]  92  93  95 107 110 118 135 142 145 147 153 156 163 166 167 170 176 179
#>  [37] 180 181 183 197 199 201 207 210 212 218 222 223 226 229 230 239 245 246
#>  [55] 267 268 269 270 283 284 285 286 288 291 293 301 303 305 310 320 329 337
#>  [73] 345 348 351 353 361 363 371 390 426 428 429 433 444 450 455 457 460 473
#>  [91] 477 519 520 524 550 558 567 574 583 613 641 643 655 687 689 705 707 731
#> [109] 765 791 814
task$kaplan(strata = "sex") # stratified Kaplan-Meier
#> Call: survfit(formula = f, data = data)
#> 
#>         n events median 0.95LCL 0.95UCL
#> sex=f  64     38    426     345     641
#> sex=m 104     83    284     229     353
task$kaplan(reverse = TRUE) # Kaplan-Meier of the censoring distribution
#> Call: survfit(formula = f, data = data)
#> 
#>        n events median 0.95LCL 0.95UCL
#> [1,] 168     47    740     511      NA

# proportion of censored observations across all dataset
task$cens_prop()
#> [1] 0.2797619
# proportion of censored observations at or after the 95% time quantile
task$admin_cens_prop(quantile_prob = 0.95)
#> [1] 0.1276596
# proportion of variables that are significantly associated with the
# censoring status via a logistic regression model
task$dep_cens_prop() # 0 indicates independent censoring
#> [1] 0
# data barely satisfies proportional hazards assumption (p > 0.05)
task$prop_haz()
#> [1] 0.0608371
# veteran data is definitely non-PH (p << 0.05)
tsk("veteran")$prop_haz()
#> [1] 3.225193e-05
```
