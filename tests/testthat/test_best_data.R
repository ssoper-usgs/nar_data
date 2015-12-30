library(testthat)
library(validate)
context("best data")

#looking for more thorough explanation of the 'validate' library capabilities?
#Run:
# vignette("intro", package="validate")

expect_no_errors <- function(validation) {
	the_summary <- validate::summary(validation)
	error_count <- sum(the_summary$fails)
	if(error_count){
		msg <- paste(error_count, "errors detected", "\n")
		msg <- paste(msg, paste(capture.output(the_summary), collapse = "\n"))
		fail(msg)
	} else {
		expect_equal(TRUE, TRUE)
	}
	
	barplot(validation, main = "check out dat data")
}

test_that("best data has a reasonable range of values", {
	result <- validate::check_that(best_data, x > 0, x < 4, nchar(y) == 1)
	#print(str(summary(result)))
	#print(str(errors(result)))
	expect_no_errors(result)
	
})

test_that("annual flow has a reasonable range of values", {
	result <- validate::check_that(aflow, 
																 FLOW > 0,
																 FLOW < 1E10,
																 nchar(SITE_ABB) == 4,
																 WY < 2020,
																 WY > 1950
	)
	expect_no_errors(result)
})