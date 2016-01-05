library(validate)
library(testthat)
context("all time series")

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
	Map(expect_consistent_missing_value_representation, c(aloads, aflow, dflow, mloads, mflow))
})
