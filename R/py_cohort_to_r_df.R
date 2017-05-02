
#' Convenience function to convert a (python) Cohort
#' into an R data.frame for use in further analysis
#'
#' Uses \code{reticulate} to interface with python libraries.
#' Depends on having `cohorts` installed in local python environment.
#'
#' @param py_cohort Python object representing a Cohort
#' @param dtypes which dtypes to convert
#' @param ... (other parameters to `as_dataframe()`
#'
#' @returns R data.frame containing cohort data
#' @export
py_cohort_to_r_df <- function(py_cohort,
                         dtypes = c('inexact', 'bool_', 'object', 'integer'),
                         ...) {
  py_df <- py_cohort$as_dataframe(...)
  py_to_r_df(py_df, dtypes = dtypes)
}
