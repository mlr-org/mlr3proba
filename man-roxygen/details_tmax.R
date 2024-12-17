#' @section Time Cutoff Details:
#' `r lifecycle::badge("experimental")`
#'
#' If `t_max` or `p_max` is given, then the predicted survival function \eqn{S(t)} is
#' truncated at the time cutoff for all observations.
#'
#' Also, if `remove_obs = TRUE`, **observations with observed times** \eqn{t > t_{max}} **are removed**.
#' This data preprocessing step mitigates issues that arise when using IPCW
#' in cases of administrative censoring, see Kvamme et al. (2023).
#' Practically, this step, along with setting a time cutoff `t_max`, helps mitigate
#' the **inflation of the score** observed when an observation is censored at the
#' final time point. In such cases, \eqn{G(t) = 0}, triggering the use of a
#' small constant `eps` instead.
#' This inflation particularly impacts the proper version of the score, see Sonabend et al. (2024)
#' for more details.
#' Note that the `t_max` and `remove_obs` parameters do not affect the estimation
#' of the censoring distribution, i.e. **always all the observations are used for estimating** \eqn{G(t)}.
#'
#' If `remove_obs = FALSE`, inflated scores may occur. While this aligns more closely
#' with the definitions presented in the original papers, it can lead to misleading
#' evaluation and poor optimization outcomes when using this score for model tuning.
#'
