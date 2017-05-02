
library(reticulate)
library(testthat)
library(dplyr)
reticulate::use_condaenv('root', required = TRUE, conda = '/home/analysis/miniconda3/bin/conda')

skip_if_no_numpy <- function() {
  have_numpy <- reticulate::py_module_available("numpy")
  if (!have_numpy)
    skip("numpy not available for testing")
}

skip_if_no_pandas <- function() {
  have_pandas <- reticulate::py_module_available("pandas")
  if (!have_pandas)
    skip("pandas not available for testing")
}

test_python_nparray <- "
import numpy as np
npdata = np.zeros((2,), dtype=[('A', 'i4'),('B', 'f4'),('C', 'a10'), ('D', 'bool_')])
npdata[:] = [(1,2.,b'Hello', True), (2,3.,\"World\", False)]
"

test_python_pandas <- "
import pandas as pd
pddata = pd.DataFrame(npdata)
"

expected_df <- data.frame(A=c(1L, 2L), B=c(2.0, 3.0), C=c('Hello', 'World'), D=c(TRUE, FALSE),
                          stringsAsFactors = F)
expected_df_type <- list()
expected_df_type['integer'] <- expected_df %>% dplyr::select_if(is.integer)
expected_df_type['inexact'] <- expected_df %>% dplyr::select_if(is.double)
expected_df_type['object'] <- expected_df %>% dplyr::select_if(is.character)
expected_df_type['bool_'] <- expected_df %>% dplyr::select_if(is.logical)

testthat::test_that('pandas dataframes of each type are converted correctly', {
  skip_if_no_pandas()
  main <- reticulate::py_run_string(test_python_nparray, convert = FALSE)
  main <- reticulate::py_run_string(test_python_pandas, convert = FALSE)
  for (dtype in ALLOWED_DTYPES) {
    r_df <- py_to_r_df(main$pddata, dtypes = dtype)
    testthat::expect_is(r_df, 'data.frame', info = paste0('Result is not data.frame (dtype = ', dtype))
    testthat::expect_equivalent(r_df, expected_df_type[dtype], info = paste0('Data.frame was not converted correctly for dtype ',dtype))
  }
})


testthat::test_that('pandas dataframes of mixed type are converted correctly', {
  skip_if_no_pandas()
  main <- reticulate::py_run_string(test_python_nparray, convert = FALSE)
  main <- reticulate::py_run_string(test_python_pandas, convert = FALSE)
  r_df <- py_to_r_df(main$pddata) %>% dplyr::select(order(colnames(.)))
  testthat::expect_is(r_df, 'data.frame', info = 'Result is data.frame')
  testthat::expect_equivalent(r_df, expected_df, info = 'Data.frame was not converted correctly')
})

testthat::test_that('numpy arrays of mixed type are converted correctly', {
  skip_if_no_numpy()
  main <- reticulate::py_run_string(test_python_nparray, convert = FALSE)
  r_df <- py_to_r_df(main$npdata) %>% dplyr::select(order(colnames(.)))
  testthat::expect_is(r_df, 'data.frame', info = 'Result is not a data.frame')
  testthat::expect_equivalent(r_df, expected_df, info = 'Data.frame was not converted correctly')
})

