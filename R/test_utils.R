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