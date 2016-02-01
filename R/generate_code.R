#'@export
generate_code <- function(){
  lapply(get_time_series_data_frame_names(), write_out_ddl)
  lapply(get_time_series_data_frame_names(), write_out_liquibase)
}

write_out_ddl <- function(dataframe_name){
  dataframe <- get(dataframe_name)
  file_name <- paste0(BASE_PATH, dataframe_name, '.sql')
  sink(file=file_name)
  ddl(dataframe)
  sink()
  return(file_name);
}

write_out_liquibase <- function(dataframe_name){
  dataframe <- get(dataframe_name)
  file_name <- paste0(BASE_PATH, dataframe_name, '.xml')
  sink(file=file_name)
  liquibase(dataframe)
  sink()
  return(file_name);
}

ddl <- function(df) {
  names <- colnames(df)
  cat('  id serial NOT NULL,\n')
  for (i in 1:ncol(df)) {
    type <- sqltype(df[[i]])
    size <- sqlsize(df[[i]])
    cat(paste0('  ', tolower(names[i]), ' ', type))
    if(!is.null(size)) {
      cat(paste0('(', size, ')'))
    }
    if(i != ncol(df)) {
      cat(',')
    }
    cat('\n')
  }
}

liquibase <- function(df) {
  names <- colnames(df)
  cat('\t\t\t<column name="rownum" header="" type="skip" />\n')
  for (i in 1:ncol(df)) {
    type <- liquibasetype(df[[i]])
    column <- paste0('\t\t\t<column name="', tolower(names[i]), '" ')
    column <- paste0(column, 'header="', names[i], '" ')
    column <- paste0(column, 'type="', type, '" />\n')
    cat(column)
  }
}

sqltype <- function(column) {
  switch(class(column),
    character="varchar",
    numeric="numeric",
    integer="integer",
    factor="varchar",
    Date="date"
  )
}

sqlsize <- function(column) {
  switch(class(column),
    character=2 * max(nchar(unique(column))),
    numeric="22,11",
    integer=NULL,
    factor=2 * max(nchar(unique(as.character(column)))),
    Date=NULL
  )
}

liquibasetype <- function(column) {
  switch(class(column),
    character="STRING",
    numeric="NUMERIC",
    integer="NUMERIC",
    factor="STRING",
    Date="DATE"
  )
}