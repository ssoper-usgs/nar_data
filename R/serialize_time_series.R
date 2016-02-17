BASE_PATH <- 'inst/extdata/'
EXTENSION <- '.csv'
write_out <- function(dataframe_name){
	dataframe <- get(dataframe_name)
	file_name <- paste(BASE_PATH, dataframe_name, EXTENSION, sep = "")
	write.csv(dataframe, file_name, quote = TRUE, na = "NULL")
	return(file_name);
}

#'@export
get_time_series_data_frame_names <- function(){
	return(list(
		'discqw',
		'aloads',
		'mloads',
		'aflow',
		'mflow',
		'dflow',
		'pest21day',
		'pest60day',
		'pestsamp',
		'pestsites',
		'pestweightave'
	))
}

#'@export
serialize_time_series <- function(){
	lapply(get_time_series_data_frame_names(), write_out)
}