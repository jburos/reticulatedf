
ALLOWED_DTYPES <- c('inexact', 'bool_', 'object', 'integer')

#' Helper function to convert (ideally, filtered) py_df
#' to a data.frame in R.
#'
#' Converts object to a matrix -> data.frame & then applies
#' column names to the data.frame
#'
#' Note: does not handle any type-conversion; types are
#' cast to their lowest-denominator type, which might
#' be that of a python object
#'
#' @param py_obj Reference to a python object (pandas.DataFrame) to convert
#'
#' @return R data.frame
#' @import reticulate
convert_filtered_to_df <- function(py_obj, dtype) {
  # check inputs
  if (!inherits(py_obj, 'python.builtin.object'))
    stop('Bug found: py_obj is not a python.builtin.object. Please report this error on http://github.com/jburos/reticulatedf')
  if (!(dtype %in% ALLOWED_DTYPES))
    stop(str_c('invalid dtype provided: ', dtype))

  # convert py_obj to matrix (mat)
  if (dtype == 'object') {
    mat <- try(reticulate::py_to_r(py_obj$astype('unicode')$values), silent = TRUE)
    if (grepl(mat[1:1], pattern='^b\'')) {
        py_obj <- py_obj$apply(function(x) x$str$decode('utf-8'))
        mat <- try(reticulate::py_to_r(py_obj$astype('unicode')$values), silent = TRUE)
    }
    # handle string/char conversions
    if (inherits(mat[1:1], 'list')) {
      # sometimes each object gets translated to a 'list'
      # here we simplify to `chr` type
      mat1 <- mat
      mat <- as.list(data.frame(mat1))
      mat <- lapply(mat, FUN=as.character)
    }
  } else {
    mat <- try(reticulate::py_to_r(py_obj$values), silent = TRUE)
  }

  # convert matrix (mat) to df
  if (inherits(mat, 'try-error'))
    return(NULL)
  df <- data.frame(mat, stringsAsFactors = FALSE)
  try({
    #retain column names
    colnames(df) <- reticulate::py_to_r(py_obj$columns$values)
  })
  df
}

#' Convert pandas.DataFrame to an R DataFrame
#'
#' Note: not a perfect conversion! Does not handle dates, lists, etc.
#' Not likely to work except for the simplest dataframes. For each dtype,
#' converts pandas DataFrame to an R data.frame via a matrix.
#'
#' @param py_df Python object (pandas.DataFrame) to be converted
#' @param dtypes which dtypes to convert, as character vector
#'
#' @return R data.frame
#'
#' @importFrom dplyr bind_cols
#'
#' @export
py_to_r_df <- function(py_df, dtypes = ALLOWED_DTYPES) {

  # check object type
  if (!inherits(py_df, 'python.builtin.object'))
    stop('Bug found: py_df is not a python.builtin.object. Please report this error on http://github.com/jburos/reticulatedf')
  if (inherits(py_df, 'numpy.ndarray')) {
    warning("py_df is a python numpy.ndarray, not a pandas.DataFrame. Using default `py_to_r` method.")
    mdf <- try(reticulate::py_to_r(py_df), silent = TRUE)
    if (!inherits(mdf, 'try-error')) {
      return(mdf)
    } else {
      ## try to convert to pandas df
      if (reticulate::py_module_available('pandas')) {
        pandas <- reticulate::import('pandas', convert = FALSE, delay_load = TRUE)
        py_df <- pandas$DataFrame(py_df)
      } else {
        stop('Unable to convert object.')
      }
    }
  }
  if (!inherits(py_df, 'pandas.core.frame.DataFrame'))
    stop('py_df is not a a python pandas.DataFrame.')

  # convert to df
  mdf <- NULL
  for (dtype in dtypes) {
    py_df_filtered <- py_df$select_dtypes(include=list(dtype))
  	df <- convert_filtered_to_df(py_df_filtered, dtype = dtype)
  	if (!is.null(df) && is.null(mdf)) {
  		mdf <- df
  	}
  	else if (!is.null(df)) {
  		mdf <- dplyr::bind_cols(mdf, df)
  	}
  }
  mdf
}
