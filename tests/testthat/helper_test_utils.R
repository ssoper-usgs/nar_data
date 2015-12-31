library(validate)
library(testthat)
library(compare)

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
	
	barplot(validation)
}

# @param dataframe data.frame to check for same columns
# @param names character vector of column names
expect_has_names <- function(dataframe, names) {
	same_names <- compare::isTRUE(compare::compareIgnoreOrder(names, names(dataframe)))
	if (same_names) {
		expect_true(TRUE)
	} else {
		expected_names <- paste(sort(names), collapse = "\n")
		actual_names <- paste(sort(names(dataframe)), collapse = "\n")
		error_message <- paste('expected names', expected_names, '\nactual names', actual_names, sep = "\n")
		fail(error_message)
	}
}