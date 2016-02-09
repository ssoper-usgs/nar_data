#' Generate code from data frames
#' This is not part of the data pipeline, but used to more quickly create
#' code that is used on the service-side of the application.  If columns
#' change in the data frames or new data frames are added this can be
#' run to update the code.
#' 
#' This function will place several files in the BASE_PATH.
#'
#'@importFrom R.utils toCamelCase
#'@export
generate_code <- function(){
  lapply(get_time_series_data_frame_names(), write_out_ddl)
  lapply(get_time_series_data_frame_names(), write_out_liquibase)
  lapply(get_time_series_data_frame_names(), write_out_mapper)
  lapply(get_time_series_data_frame_names(), write_out_java)
}

write_out_ddl <- function(dataframe_name){
  dataframe <- get(dataframe_name)
  file_name <- paste0(BASE_PATH, dataframe_name, '.sql')
  sink(file=file_name)
  ddl(dataframe)
  sink()
  return(file_name)
}

write_out_liquibase <- function(dataframe_name){
  dataframe <- get(dataframe_name)
  file_name <- paste0(BASE_PATH, dataframe_name, '.xml')
  sink(file=file_name)
  mapresults(dataframe)
  sink()
  return(file_name)
}

write_out_mapper <- function(dataframe_name){
  dataframe <- get(dataframe_name)
  file_name <- paste0(BASE_PATH, dataframe_name, 'Mapper.xml')
  sink(file=file_name)
  mapresults(dataframe)
  columns(dataframe)
  sink()
  return(file_name)
}

write_out_java <- function(dataframe_name) {
  dataframe <- get(dataframe_name)
  file_name <- paste0(BASE_PATH, dataframe_name, ".java")
  sink(file=file_name)
  javadeclaration(dataframe)
  sink()
  return(file_name)
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

mapresults <- function(df) {
  names <- colnames(df)
  for (i in 1:ncol(df)) {
    column <- paste0('\t\t<result property="', javaname(names[i]), '" ')
    column <- paste0(column, 'column="', tolower(names[i]), '" />\n')
    cat(column)
  }
}

columns <- function(df) {
  names <- colnames(df)
  for (i in 1:ncol(df)) {
    column <- paste0('\t\t', tolower(names[i]))
    if(i != ncol(df)) {
      column <- paste0(column, ',')
    }
    column <- paste0(column, '\n')
    cat(column)
  }
}

javadeclaration <- function(df) {
  names <- colnames(df)
  for (i in 1:ncol(df)) {
    type <- javatype(df[[i]])
    column <- paste0('\tprivate ', type, ' ', javaname(names[i]), ';\n')
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

javatype <- function(column) {
  switch(class(column),
    character="String",
    numeric="double",
    integer="int",
    factor="String",
    Date="Date"
  )
}

javaname <- function(name) {
  return(toCamelCase(tolower(name), split="_"))
}