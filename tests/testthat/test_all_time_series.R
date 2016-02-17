library(validate)
library(testthat)
context("all time series")

all_time_series_names <- nardata::get_time_series_data_frame_names()
all_time_series <- Map(get, all_time_series_names)

expect_at_least_one_site_id_with_a_leading_zero <- function(dataframe) {
	expect_true(any(substr(dataframe$SITE_QW_ID, 1, 1) == '0'))
}

test_that('there is at least one site with a leading zero in all the time series data frames', {
	Map(expect_at_least_one_site_id_with_a_leading_zero, all_time_series)
})

expect_consistent_missing_value_representation <- function(dataframe) {
	mapply(function(colname){
		col <- dataframe[[colname]]
		if (!is.null(col)) {
			fail(paste('Column "', colname, '" contained one or more NULLs. NaNs expected'))
		}
		if (is.character(col) && all(nchar(col) <= 0)) {
			fail(paste('Column "', colname, '" contained one or more blank strings.'))
		}
	}, colnames(dataframe))
}

test_that('there are consistent missing value representations in all time series data frames', {
	Map(expect_consistent_missing_value_representation, unlist(all_time_series, recursive = FALSE))
})
