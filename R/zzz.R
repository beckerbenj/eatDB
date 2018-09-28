
## check for sqlite3
onLoad <- function(){
  sqlite_test <- try(shell(cmd = "sqlite3", mustWork = TRUE, intern = TRUE))
  if(class(sqlite_test) == "try-error") stop("SQLite3 must be installed and correct PATH must be specified for eatDB to create data bases.
                                             Usage of existing data bases works without SQLite3.")
}
