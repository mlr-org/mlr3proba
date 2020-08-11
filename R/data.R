#' @title ACTG 320 Clinical Trial Dataset
#' @template dataset
#' @templateVar id actg
#' @format
#' \describe{
#' \item{id}{Identification Code}
#' \item{time}{Time to AIDS diagnosis or death (days).}
#' \item{censor}{Event indicator. 1 = AIDS defining diagnosis, 0 = Otherwise.}
#' \item{time_d}{Time to death (days)}
#' \item{censor_d}{Event indicator for death (only). 1 = Death, 0 = Otherwise.}
#' \item{tx}{Treatment indicator. 1 = Treatment includes IDV, 0 = Control group.}
#' \item{txgrp}{Treatment group indicator. 1 = ZDV + 3TC. 2 = ZDV + 3TC + IDV. 3 = d4T + 3TC.
#' 4 = d4T + 3TC + IDV.}
#' \item{strat2}{CD4 stratum at screening. 0 = CD4 <= 50. 1 = CD4 > 50.}
#' \item{sexF}{0 = Male. 1 = Female.}
#' \item{raceth}{Race/Ethnicity. 1 = White Non-Hispanic. 2 = Black Non-Hispanic. 3 = Hispanic.
#' 4 = Asian, Pacific Islander. 5 = American Indian, Alaskan Native. 6 = Other/unknown.}
#' \item{ivdrug}{IV drug use history. 1 = Never. 2 = Currently. 3 = Previously.}
#' \item{hemophil}{Hemophiliac. 1 = Yes. 0 = No.}
#' \item{karnof}{Karnofsky Performance Scale. 100 = Normal; no complaint no evidence of disease.
#' 90 = Normal activity possible; minor signs/symptoms of disease. 80 = Normal activity with
#' effort; some signs/symptoms of disease. 70 = Cares for self; normal activity/active work not
#' possible.}
#' \item{cd4}{Baseline CD4 count (Cells/Milliliter).}
#' \item{priorzdv}{Months of prior ZDV use (months).}
#' \item{age}{Age at Enrollment (years).}
#' }
"actg"

#' @title German Breast Cancer Study (GBCS) Dataset
#' @template dataset
#' @templateVar id gbcs
#' @format
#' \describe{
#' \item{id}{Identification Code}
#' \item{diagdate}{Date of diagnosis.}
#' \item{recdate}{Date of recurrence free survival.}
#' \item{deathdate}{Date of death.}
#' \item{age}{Age at diagnosis (years).}
#' \item{menopause}{Menopausal status. 1 = Yes, 0 = No.}
#' \item{hormone}{Hormone therapy. 1 = Yes. 0 = No.}
#' \item{size}{Tumor size (mm).}
#' \item{grade}{Tumor grade (1-3).}
#' \item{nodes}{Number of nodes.}
#' \item{prog_recp}{Number of progesterone receptors.}
#' \item{estrg_recp}{Number of estrogen receptors.}
#' \item{rectime}{Time to recurrence (days).}
#' \item{censrec}{Recurrence status. 1 = Recurrence. 0 = Censored.}
#' \item{survtime}{Time to death (days).}
#' \item{censdead}{Censoring status. 1 = Death. 0 = Censored.}
#' }
"gbcs"

#' @title GRACE 1000 Dataset
#' @template dataset
#' @templateVar id grace
#' @format
#' \describe{
#' \item{id}{Identification Code}
#' \item{days}{Follow up time.}
#' \item{death}{Censoring indicator. 1 = Death. 0 = Censored.}
#' \item{revasc}{Revascularization Performed. 1 = Yes. 0 = No.}
#' \item{revascdays}{Days to revascularization after admission.}
#' \item{los}{Length of hospital stay (days).}
#' \item{age}{Age at admission (years).}
#' \item{sysbp}{Systolic blood pressure on admission (mm Hg).}
#' \item{stchange}{ST-segment deviation on index ECG. 1 = Yes. 0 = No.}
#' }
"grace"

#' @title Worcester Heart Attack Study (WHAS) Dataset
#' @template dataset
#' @templateVar id whas
#' @format
#' \describe{
#' \item{id}{Identification Code}
#' \item{age}{Age (per chart) (years).}
#' \item{sex}{Sex. 0 = Male. 1 = Female.}
#' \item{cpk}{Peak cardiac enzyme (iu).}
#' \item{sho}{Cardiogenic shock complications. 1 = Yes. 0 = No.}
#' \item{chf}{Left heart failure complications. 1 = Yes. 0 = No.}
#' \item{miord}{MI Order. 1 = Recurrent. 0 = First.}
#' \item{mitype}{MI Type. 1 = Q-wave. 2 = Not Q-wave. 3 = Indeterminate.}
#' \item{year}{Cohort year.}
#' \item{yrgrp}{Grouped cohort year.}
#' \item{lenstay}{Days in hospital.}
#' \item{dstat}{Discharge status from hospital. 1 = Dead. 0 = Alive.}
#' \item{lenfol}{Total length of follow-up from hospital admission (days).}
#' \item{fstat}{Status as of last follow-up. 1 = Dead. 0 = Alive.}
#' }
"whas"
