library(validate)
library(testthat)
context("all time series")
all_time_series <- nardata::get_time_series_data_frames_and_names()

expect_at_least_one_site_id_with_a_leading_zero <- function(name, dataframe) {
	first_char_of_ids <- substr(dataframe$SITE_QW_ID, 1, 1)
	if (any( first_char_of_ids == '0')) {
		succeed()
	} else {
		message <- paste("Could not find any sites with a leading zero in data frame '", name, "'")
		fail(message)
	}
	
}
for_each_data_frame <- nardata::for_each_data_frame

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

test_that('the same sites are present in pestsites and pestsamp', {
	pestsites_sites <- sort(unique(pestsites$SITE_QW_ID))
	pestsamp_sites <- sort(unique(pestsamp$SITE_QW_ID))
	
	if (setequal(pestsites_sites, pestsamp_sites)) {
		#register that the test passed
		succeed()
	} else {
		#if they are not equal, provide helpful information about the differences in the 
		#error message
		in_pestsites_but_not_pestsamp <- setdiff(pestsites_sites, pestsamp_sites)
		in_pest_samp_but_not_pestsites <- setdiff(pestsamp_sites, pestsites_sites)
		message <- paste("The following", length(in_pestsites_but_not_pestsamp), "sites were in pestsites but not in pestsamp.")
		message <- paste(message, "\n", paste(in_pestsites_but_not_pestsamp, collapse = ', '), "\n\n")
		message <- paste(message, "The following", length(in_pest_samp_but_not_pestsites), "sites were in pestsamp but not in pestsites.")
		message <- paste(message, "\n", paste(in_pest_samp_but_not_pestsites, collapse = ', '))
		fail(message)
	}
})

test_that('site abbreviations correspond to only one water quality site id', {
	message <- ''
	success <- TRUE
	required_columns <- c('SITE_ABB', 'SITE_QW_ID')
	for_each_data_frame(data_frames=get_time_series_data_frames_and_names(), function(name, dataframe){
		if(all(required_columns %in% names(dataframe))){
			problematic_matches <- dataframe %>% distinct(SITE_ABB, SITE_QW_ID) %>% group_by(SITE_ABB) %>% filter(n()>1) %>% arrange(SITE_ABB)
			if(nrow(problematic_matches) > 0){
				success <<- FALSE
				
				# R's handy native representation of the data frame
				str_problematic_matches <- capture.output(problematic_matches)
				flat_str_problematic_matches <- paste(str_problematic_matches, "\n", sep = "", collapse = "\n")
				
				message <<- paste(message, "\n" ,"The dataframe '", name, 
								 "' had the following site abbreviations that corresponded to more than one water quality site id",
								 "\n", flat_str_problematic_matches
				)
			}
		}
	})
	if(success){
		succeed()	
	} else {
		fail(message)
	}
})

test_that('water quality site ids correspond to only one site abbreviation', {
	message <- ''
	success <- TRUE
	required_columns <- c('SITE_ABB', 'SITE_QW_ID')
	for_each_data_frame(data_frames=get_time_series_data_frames_and_names(), function(name, dataframe){
		if(all(required_columns %in% names(dataframe))){
			problematic_matches <- dataframe %>% distinct(SITE_QW_ID, SITE_ABB) %>% group_by(SITE_QW_ID) %>% filter(n()>1) %>% arrange(SITE_QW_ID)
			if(nrow(problematic_matches) > 0){
				success <<- FALSE
				
				# R's handy native representation of the data frame
				str_problematic_matches <- capture.output(problematic_matches)
				flat_str_problematic_matches <- paste(str_problematic_matches, "\n", sep = "", collapse = "\n")
				
				message <<- paste(message, "\n" ,"The dataframe '", name, 
								  "' had the following water quality site ids that corresponded to more than one site abbreviations",
								  "\n", flat_str_problematic_matches
				)
			}
		}
	})
	if(success){
		succeed()	
	} else {
		fail(message)
	}
})
