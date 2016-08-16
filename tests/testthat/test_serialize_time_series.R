library(testthat)
library(validate)
context("serialize_time_series")

# creates some mock input, runs the function, returns serialized file name
run_serialization <- function(){
		alpha_values <- c('a', 'ebe', 'ece', 'ede') #using lots of 'e's in the test data to try to expose corner cases in the regex
		num_values <- c(0, 200000, 300000, 400000)
		more_alpha_values <- c('eee', 'f', 'ege', 'ehe')
		test_frame <- data.frame(alpha_values, num_values, more_alpha_values)
		test_data_frames_and_names <- list(test_frame = test_frame)
		base_path <- paste0(tempdir(), .Platform$file.sep)
		extension <- '.fake.csv'
		nardata::serialize_specific_time_series(test_data_frames_and_names, base_path, extension)
		file_name <- paste0(base_path, 'test_frame', extension)
		return(file_name)
}


test_that("serialization does not change the global scipen option", {
	pre_test_scipen <- options()$scipen
	tryCatch({
		#set to a known state
		expected_scipen <- 10
		options(scipen = expected_scipen)
		run_serialization()
		actual_scipen <- options()$scipen
		#assert that the known state is restored after the function is called
		expect_equal(actual_scipen, expected_scipen)
	}, finally = {
		options(scipen = pre_test_scipen)
	})
});

test_that("numbers in time series are serialized without using scientific notation", {
	pre_test_scipen <- options()$scipen
	tryCatch({
		match_sci_notation <- "[[:digit:]]e\\+[[:digit:]]" # look for things like 4e+05
		file_name <- run_serialization()
		matches <- grep(match_sci_notation, readLines(file_name), value = TRUE)
		if ( 0 != length(matches)) {
			message <- paste("The following lines had scientific notation in file", file_name, "\n")
			message <- paste0(message, paste(matches, collapse = '\n'))
			fail(message)
		} else {
			succeed()
		}
	}, finally = {
		options(scipen = pre_test_scipen)
	})
	
})
