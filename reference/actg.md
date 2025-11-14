# ACTG 320 Clinical Trial Dataset

actg dataset from Hosmer et al. (2008)

## Usage

``` r
actg
```

## Format

- id:

  Identification Code

- time:

  Time to AIDS diagnosis or death (days).

- censor:

  Event indicator. 1 = AIDS defining diagnosis, 0 = Otherwise.

- time_d:

  Time to death (days)

- censor_d:

  Event indicator for death (only). 1 = Death, 0 = Otherwise.

- tx:

  Treatment indicator. 1 = Treatment includes IDV, 0 = Control group.

- txgrp:

  Treatment group indicator. 1 = ZDV + 3TC. 2 = ZDV + 3TC + IDV. 3 =
  d4T + 3TC. 4 = d4T + 3TC + IDV.

- strat2:

  CD4 stratum at screening. 0 = CD4 \<= 50. 1 = CD4 \> 50.

- sexF:

  0 = Male. 1 = Female.

- raceth:

  Race/Ethnicity. 1 = White Non-Hispanic. 2 = Black Non-Hispanic. 3 =
  Hispanic. 4 = Asian, Pacific Islander. 5 = American Indian, Alaskan
  Native. 6 = Other/unknown.

- ivdrug:

  IV drug use history. 1 = Never. 2 = Currently. 3 = Previously.

- hemophil:

  Hemophiliac. 1 = Yes. 0 = No.

- karnof:

  Karnofsky Performance Scale. 100 = Normal; no complaint no evidence of
  disease. 90 = Normal activity possible; minor signs/symptoms of
  disease. 80 = Normal activity with effort; some signs/symptoms of
  disease. 70 = Cares for self; normal activity/active work not
  possible.

- cd4:

  Baseline CD4 count (Cells/Milliliter).

- priorzdv:

  Months of prior ZDV use (months).

- age:

  Age at Enrollment (years).

## Source

<https://onlinelibrary.wiley.com/doi/book/10.1002/9780470258019>

## References

Hosmer, D.W. and Lemeshow, S. and May, S. (2008) Applied Survival
Analysis: Regression Modeling of Time to Event Data: Second Edition,
John Wiley and Sons Inc., New York, NY
