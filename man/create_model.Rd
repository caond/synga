% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_model.R
\name{create_model}
\alias{create_model}
\title{Create a Data Transformation Model}
\usage{
create_model(df, data_type)
}
\arguments{
\item{df}{A data frame containing the original dataset to be processed.}

\item{data_type}{A named list specifying column data type overrides: factor, datetime, date, integer, numeric, text, key}
}
\value{
A list containing the following components:
\itemize{
\item \code{original}: The original input data frame.
\item \code{metadata}: A data frame describing column types after applying overrides.
\item \code{data_converted}: The input data frame with converted data types according to metadata.
\item \code{transformed}: A list of distribution functions for each column.
\item \code{data_uniform}: The data transformed into a uniform \link{0, 1} space based on transform functions.
}
}
\description{
This function takes a data frame (\code{df}) and applies a series of transformations
to prepare the data for modeling. It generates metadata, converts data types,
fits empirical CDFs, and transforms the data into a uniform \link{0, 1} space.
}
\examples{
library(synga)
ed_model <- create_model(ed,
  data_type<-list(
           event_id="key",
           patient_name="text",
           establishment_code = "factor",
           arrival_datetime = "datetime",
           sex = "factor",
           ethnicity = "factor",
           triage_category = "factor",
           mode_of_arrival = "factor",
           presenting_complaint = "factor",
           admission = "factor",
           attendance_length_of_episode = "integer"
        )
)

}
