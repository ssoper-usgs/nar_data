library(validate)
library(testthat)
context("non-pesticide time series")

test_that("constituent abbreviations are consistent", {
	
	all_non_pesticide_data_frame_names_with_constituents <- c('aloads', 'mloads', 'discqw')
	
	expected_constituents <- sort(c("TP", "TN", "SSC", "NO3_NO2"))
	
	actual_constituents_list <- Map(function(data_frame_name){
		returnList <- levels(get(data_frame_name)$CONSTIT)
		attr(returnList, 'data_frame_name') <- data_frame_name
		return(returnList)
	}, all_non_pesticide_data_frame_names_with_constituents)
	
	Map(function(actual_constituents){
		matches <- actual_constituents %in% expected_constituents
		if (!all(matches)) {
			msg <- "Did not find all of ("
			msg <- paste(msg, paste(sort(unlist(actual_constituents)), collapse = ", "))
			msg <- paste(msg, ") in the expected list of constituents (")
			msg <- paste(msg, paste(expected_constituents, collapse = ", "))
			msg <- paste(msg, ") for the data frame", attr(actual_constituents, 'data_frame_name'))
			fail(msg)
		}
		
	}, actual_constituents_list)
})