
#### Viewing Interface to data base
#############################################################################
#' The variable names in a relational data base.
#'
#' Function to get the names of the variables included in the relational data base.
#'
#' Extracts names of all variables included in the relational data base, structured as list with the individual data tables as list elements.
#'
#'@param filePath Path of an existing db file.
#'@param includeMeta Should the variable names of the Meta_Data table should be included.
#'
#'@return Returns a named list of character vectors with the variables names included in the data tables.
#'
#'@examples
#' # not run:
#' # dbNames(filePath = 'exampleDB.db')
#'
#'@export
dbNames <- function(filePath, includeMeta = FALSE) {
  # check input
  check_dbPath(dbPath = filePath)

  # Establish Connection, disconnect when function exits
  con <- dbConnect(RSQLite::SQLite(), dbname = filePath)
  on.exit(dbDisconnect(con))

  # get data table names
  dtNames <- dbListTables(con)
  # get all variable names in these tables
  nameList <- lapply(dtNames, dbListFields, conn = con)
  names(nameList) <- dtNames

  # drop information about meta data tables
  if(!includeMeta) nameList$Meta_Information <- nameList$Meta_Data <- NULL

  nameList
}

#############################################################################
#' The keys in a relational data base.
#'
#' Function to get the primary and foreign keys of the data frames in the relational data base.
#'
#' Data in a relational data base are indexed by primary and foreign keys. Primary keys are unique indentifiers
#' inside the same data table. Foreign keys reference (link) to other data tables. This function returns the
#' key structure of a relational data base.
#'
#'@param filePath Path of the existing db file.
#'@param includeMeta Should information about the Meta_Data table be included.
#'
#'@return Returns a list named as the data tables in the db. Each elements contains a list with the primary key, the
#' data table it references to and the corresponding foreign keys.
#'
#'@examples
#' # See vignette.
#' # not run:
#' # dbKeys(filePath = 'exampleDB.db')
#'
#'@export
dbKeys <- function(filePath, includeMeta = FALSE) {
  # check input
  check_dbPath(dbPath = filePath)

  # Establish Connection, disconnect when function exits
  con <- dbConnect(RSQLite::SQLite(), dbname = filePath)
  on.exit(dbDisconnect(con))

  # get db names
  dtNames <- names(dbNames(filePath, includeMeta = includeMeta))

  ## Primary keys
  # create query
  pkQueries <- paste("PRAGMA table_info(", dtNames, ")")
  # execute query and transform info
  pk_table <- lapply(pkQueries, dbGetQuery, conn = con)
  pk_list <- lapply(pk_table, extract_PKs)

  ## foreign keys
  fkQueries <- paste("PRAGMA foreign_key_list(", dtNames, ")")
  # execute query and transform info
  fk_table <- lapply(fkQueries, dbGetQuery, conn = con)
  fk_list <- lapply(fk_table, extract_FKs)

  ## structure and name output
  names(pk_list) <- dtNames
  names(fk_list) <- dtNames

  list(pkList = pk_list, fkList = fk_list)
}

### Check dbpath ---------------------------------------------------------
check_dbPath <- function(dbPath) {
  if(!file.exists(dbPath)){
    stop(paste(dbPath, " is not a valid path to a data base"))
  }
}

### Extract output ---------------------------------------------------------
## extract Primary Keys from SCHEMA output
extract_PKs <- function(table_info) {
  table_info[table_info$pk != 0, "name"]
}

## extract foreign Keys from SCHEMA output
extract_FKs <- function(table_info) {
  if(nrow(table_info) == 0) return(list(References = NULL, Keys = NULL))
  ref <- unique(table_info$table)
  if(length(ref) > 1) stop("Foreign keys for more than 1 table defined, check data base creation.")
  if(!identical(table_info$from, table_info$to)) stop("Foreign and primary key have different names, error was made during creation of data base!")
  keys <- table_info$from
  list(References = ref, Keys = keys)
}


#############################################################################
#' Extract a single data table from a relational data base.
#'
#' Function to extract a single, complete data table from a relational data base. Especially useful for the extraction of the meta information stored in Meta_Data.
#'
#' This function makes use of the DBI::dbReadTable function and extracts a complete data table from a data base. All variables are extracted and all rows are used. For extracting only some variables or merging data tables see dbPull.
#'
#'@param dfName Name of the data table which should be extracted
#'@param filePath Path of the existing db file.
#'
#'@return Returns a data frame with all variables and cases as in the corresponding data table.
#'
#'@examples
#' # not run:
#' # Extract Meta Data
#' # dbSingleDF(df_name = 'Meta_Data', filePath = 'exampleDB.db')
#'
#'@export
dbSingleDF <- function(dfName = 'Meta_Data', filePath) {
  # check input
  check_dbPath(dbPath = filePath)

  # Establish Connection, disconnect when function exits
  con <- dbConnect(RSQLite::SQLite(), dbname = filePath)
  on.exit(dbDisconnect(con))

  # extract single table from data base
  out <- dbReadTable(conn = con, name = dfName)

  out
}











