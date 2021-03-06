% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{drop_empty_cols}
\alias{drop_empty_cols}
\title{Helper function to drop empty columns in a dataframe,
since converted objects typically have 1+ completely empty columns.}
\usage{
drop_empty_cols(r_df)
}
\arguments{
\item{r_df}{R data.frame object}
}
\value{
data.frame with columns that are entirely NA removed
}
\description{
Helper function to drop empty columns in a dataframe,
since converted objects typically have 1+ completely empty columns.
}
