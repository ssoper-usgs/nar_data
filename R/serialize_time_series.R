BASE_PATH <- 'inst/extdata/'
EXTENSION <- '.csv'

write_out <- function(dataframe){
		dataframe_name <- deparse(substitute(dataframe))
		file_name <- paste(BASE_PATH, dataframe_name, EXTENSION, sep = "")
		write.csv(dataframe, file_name, quote = TRUE, na = "NULL")
}

serialize_time_series <- function(){
	write_out(discqw)
	write_out(aloads)
	write_out(mloads)
	write_out(aflow)
	write_out(mflow)
	write_out(dflow)
}