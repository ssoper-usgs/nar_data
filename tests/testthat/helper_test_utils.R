library(validate)
library(testthat)

# Determines whether the validation has errors. If it does, create a helpful 
# failure message and fail the present test.
# @param validation validate::validation object
expect_no_errors <- function(validation) {
	the_summary <- validate::summary(validation)
	error_count <- sum(the_summary$fails)
	if (error_count) {
		msg <- paste(error_count, "errors detected", "\n")
		msg <- paste(msg, paste(capture.output(the_summary), collapse = "\n"))
		fail(msg)
	} else {
		expect_true(TRUE)
	}
	

}

# @param dataframe data.frame to check for same columns
# @param names character vector of column names
expect_has_names <- function(dataframe, names) {
	dataframe_names <- names(dataframe)
	expect_equal(length(names), length(dataframe_names))
	same_names <- all(names %in% dataframe_names)
	if (same_names) {
		expect_true(TRUE)
	} else {
		expected_names <- paste(sort(names), collapse = "\n")
		actual_names <- paste(sort(dataframe_names), collapse = "\n")
		error_message <- paste('expected names', expected_names, '\nactual names', actual_names, sep = "\n")
		fail(error_message)
	}
}

# @param column - the column whose entries should 
#        have their significant figures counted
count_sig_figs <- function(column) {
	nchar(sub("^[0]+", "",sub("[.]","",column)))
}