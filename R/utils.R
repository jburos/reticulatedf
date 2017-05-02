
#' Helper function to drop empty columns in a dataframe,
#' since converted objects typically have 1+ completely empty columns.
#'
#' @param r_df R data.frame object
#' @return data.frame with columns that are entirely NA removed
#' @export
drop_empty_cols <- function(r_df) {
  r_df %>% dplyr::select_if(function(x) any(!is.na(x)))
}

