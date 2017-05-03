
library(reticulate)
library(testthat)
library(dplyr)
reticulate::use_condaenv('root', conda = '/home/analysis/miniconda3/bin/conda', required = T) # local testing

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
npdata = np.zeros((2,), dtype=[('A', 'i4'),('B', 'f4'),('C', 'a10'), ('D', 'bool_'), ('E', 'a10')])
npdata[:] = [(1,2., 'Hello', True, 'A'), (2,3.,\"World\", False, 'B')]
"

test_python_pandas <- "
import pandas as pd
pddata = pd.DataFrame(npdata)
"

expected_df <- data.frame(A=c(1L, 2L), B=c(2.0, 3.0), C=c("Hello", "World"), D=c(TRUE, FALSE), E=c("A", "B"),
                          stringsAsFactors = F)
prep_df_by_type <- function(df) {
  expected_df_type <- list()
  expected_df_type[['integer']] <- df %>% dplyr::select_if(is.integer)
  expected_df_type[['inexact']] <- df %>% dplyr::select_if(is.double)
  expected_df_type[['object']] <- df %>% dplyr::select_if(is.character)
  expected_df_type[['bool_']] <- df %>% dplyr::select_if(is.logical)
  expected_df_type
}

expected_df_type <- prep_df_by_type(expected_df)

# test case for chars with missing / NA values
test_python_pandas2 <- "
import numpy as np
import pandas as pd
npdata2 = np.zeros((4,), dtype=[('A', 'i4'),('B', 'f4'),('C', 'a10'), ('D', 'bool_'), ('E', 'a10')])
npdata2[:] = [(1,2., 'Hello', True, 'A'), (2,3.,\"World\", False, 'B'), (3, 4., b'', False, 'C'), (4, 5., None, True, 'D')]
pddata2 = pd.DataFrame(npdata2)
"

expected_df2 <- data.frame(A=c(1L, 2L, 3L, 4L),
                           B=c(2.0, 3.0, 4.0, 5.0),
                           C=c("Hello", "World", "", "None"),
                           D=c(TRUE, FALSE, FALSE, TRUE),
                           E=c('A', 'B', 'C', 'D'),
                          stringsAsFactors = F)
expected_df_type2 <- prep_df_by_type(expected_df2)


testthat::test_that('pandas dataframes of each type are converted correctly', {
  skip_if_no_numpy()
  skip_if_no_pandas()
  main <- reticulate::py_run_string(test_python_nparray, convert = FALSE)
  main <- reticulate::py_run_string(test_python_pandas, convert = FALSE)
  for (dtype in ALLOWED_DTYPES) {
    r_df <- py_to_r_df(main$pddata, dtypes = dtype)
    testthat::expect_is(r_df, 'data.frame', info = paste0('Result is not data.frame (dtype = ', dtype))
    testthat::expect_equivalent(r_df, expected_df_type[[dtype]], info = paste0('Data.frame was not converted correctly for dtype ',dtype))
  }
})

testthat::test_that('pandas dataframes convert each type correctly', {
  skip_if_no_numpy()
  skip_if_no_pandas()
  main <- reticulate::py_run_string(test_python_nparray, convert = FALSE)
  main <- reticulate::py_run_string(test_python_pandas, convert = FALSE)
  r_df <- py_to_r_df(main$pddata) %>% dplyr::select(order(colnames(.)))
  testthat::expect_is(r_df, 'data.frame', info = 'Result is data.frame')
  testthat::expect_equivalent(r_df, expected_df, info = 'Data.frame was not converted correctly')
})

testthat::test_that('numpy arrays of mixed type are converted correctly', {
  skip_if_no_numpy()
  skip_if_no_pandas()
  main <- reticulate::py_run_string(test_python_nparray, convert = FALSE)
  r_df <- py_to_r_df(main$npdata) %>% dplyr::select(order(colnames(.)))
  testthat::expect_is(r_df, 'data.frame', info = 'Result is not a data.frame')
  testthat::expect_equivalent(r_df, expected_df, info = 'Data.frame was not converted correctly')
})


testthat::test_that('pandas dataframes with NA char values convert each type correctly', {
  skip_if_no_numpy()
  skip_if_no_pandas()
  main <- reticulate::py_run_string(test_python_pandas2, convert = FALSE)
  for (dtype in ALLOWED_DTYPES) {
    r_df <- py_to_r_df(main$pddata2, dtypes = dtype)
    testthat::expect_is(r_df, 'data.frame', info = paste0('Result is not data.frame (dtype = ', dtype))
    testthat::expect_equivalent(r_df, expected_df_type2[[dtype]], info = paste0('Data.frame was not converted correctly for dtype ',dtype))
  }
})

testthat::test_that('pandas dataframes with character NA values are converted correctly', {
  skip_if_no_numpy()
  skip_if_no_pandas()
  main <- reticulate::py_run_string(test_python_pandas2, convert = FALSE)
  r_df2 <- py_to_r_df(main$pddata2) %>% dplyr::select(order(colnames(.)))
  testthat::expect_is(r_df2, 'data.frame', info = 'Result is data.frame')
  testthat::expect_equivalent(r_df2, expected_df2, info = 'Data.frame was not converted correctly')
})

