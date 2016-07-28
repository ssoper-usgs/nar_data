library(validate)
library(testthat)
context("all time series")
all_time_series_names <- nardata::get_time_series_data_frame_names()
all_time_series <- list()
for (time_series_name in all_time_series_names) {
	all_time_series[[time_series_name]] <- get(time_series_name)
}

expect_at_least_one_site_id_with_a_leading_zero <- function(name, dataframe) {
	first_char_of_ids <- substr(dataframe$SITE_QW_ID, 1, 1)
	if (any( first_char_of_ids == '0')) {
		succeed()
	} else {
		message <- paste("Could not find any sites with a leading zero in data frame '", name, "'")
		fail(message)
	}
	
}

# given a list of dataframe names mapped to data frames, iterate
# through every one, passing the dataframe name and dataframe to
# the specified function. The specified function must accept two
# parameters. The first is a character name. The second is a 
# data frame.

for_each_data_frame <- function(data_frames, fn) {
	for(data_frame_name in names(data_frames)) {
		data_frame <- data_frames[[data_frame_name]]
		fn(data_frame_name, data_frame)
	}
}

test_that('there is at least one site with a leading zero in all the time series data frames', {
	for_each_data_frame(all_time_series, expect_at_least_one_site_id_with_a_leading_zero)
})

expect_consistent_missing_value_representation <- function(name, dataframe) {
	mapply(function(colname){
		col <- dataframe[[colname]]
		if (!is.null(col)) {
			fail(paste('Column "', colname, '" from dataframe "', name, '" contained one or more NULLs. NaNs expected'))
		}
		if (is.character(col) && all(nchar(col) <= 0)) {
			fail(paste('Column "', colname, '" from dataframe "', name, '" contained one or more blank strings.'))
		}
	}, colnames(dataframe))
}

test_that('there are consistent missing value representations in all time series data frames', {
	for_each_data_frame(unlist(all_time_series, recursive = FALSE), expect_consistent_missing_value_representation)
})

reference_sites <- FALSE

same_sites <- function(name, dataframe) {
	sites_from_this_data_frame <- sort(unique(dataframe$SITE_QW_ID))
	
	#if this is the first time the function has been run
	if ( identical(reference_sites, FALSE)) {
		#use the first data frame's sites as the reference sites
		print(paste("using", length(sites_from_this_data_frame), "sites from dataframe '", name, "' as the reference site list"))
		reference_sites <<- sites_from_this_data_frame
	} else {
		if (setequal(sites_from_this_data_frame, reference_sites)) {
			#register that the test passed
			succeed()
		} else {
			#if they are not equal, provide helpful information about the differences in the 
			#error message
			in_refs_but_not_this_dataframe <- setdiff(reference_sites, sites_from_this_data_frame)
			in_this_dataframe_but_not_in_refs <- setdiff(sites_from_this_data_frame, reference_sites)
			message <- paste("The following", length(in_refs_but_not_this_dataframe), "sites were in the reference site list, but not in '", name, "'.")
			message <- paste(message, "\n", paste(in_refs_but_not_this_dataframe, collapse = ', '), "\n\n")
			message <- paste(message, "The following", length(in_this_dataframe_but_not_in_refs), "sites were in '", name ,"', but not in the reference sites.")
			message <- paste(message, "\n", paste(in_this_dataframe_but_not_in_refs, collapse = ', '))
			fail(message)
		}
	}
}

test_that('the same sites are present in all time series data frames', {
	for_each_data_frame(all_time_series, same_sites)
})