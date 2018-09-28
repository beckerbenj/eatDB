
## check for sqlite3
onLoad <- function(){
  sqlite_test <- tryCatch(shell(cmd = "sqlite2", mustWork = TRUE, intern = TRUE))
  if(!is.null(attributes(sqlite_test))) warning("SQLite3 must be installed and correct PATH must be specified for eatDB to create data bases. \n Usage of existing data bases works without SQLite3.")
}
