

#### Creating DB from list of data frames
#############################################################################
#' Create a relational data base file.
#'
#' Creates a relational data base from a list of data.frames (\code{dfList}). The list structure including the naming of \code{dfList}, \code{pkList} and \code{fkList} needs to be exactly the same. Keys (\code{pkList} and \code{fkList$Keys}) can either be character vectors with a single variable name or multiple variable names. Primary keys (\code{pkList}) have to be unique within a single data.frame. Foreign Keys (\code{fkList}) have to consist of a list with the referenced data frame (\code{fkList$References}) and the referencing keys (\code{fkList$Keys}). If a single data frame is to be converted to a data base, \code{pkList} can be dropped. Otherwise, both elements of \code{fkList} need to be set to \code{NULL}.
#'
#' Primary keys guarantee uniqueness of cases within a single data.frame, and are single variables or combinations of variables. Foreign keys are used to merge data.frames. The foreign key for the first data set always has to be set to \code{list(References = NULL, Keys = NULL)}. The order in which the data.frames are supplied determines the merge order. Currently, left joins are performed when merging data.frames. However, data.frames are stored seperatly in the relational data base and are only merged if pulled from the data base. \\
#' Conventions for naming variables (columns) follow naming conventions of SQLite3. '.' and \code{\link{sqlite_keywords}} are prohibited. Two additional tables within the SQLite3 data base are created: \code{Meta_Information}, which contains a single character with the merge order that is used by \code{\link{dbPull}} and \code{Meta_Data}, which contains the meta data.frame supplied to the argument \code{metaData}.
#'
#'@param dfList Named list of data frames. The order of the data.frames determines the merge order.
#'@param pkList Named list of the primary keys corresponding to the data.frames.
#'@param fkList Named list of a list per data.frame, including referenced data frame (\code{fkList$References}) and the corresponding keys \code{fkList$Keys}). Default is \code{NULL}, which should be used if only a single data frame is supplied. For multiple data.frames, \code{fkList$References} and \code{fkList$Keys} should be \code{NULL} for the first data.frame.
#'@param metaData [optional] Data.frame including meta data information about the other data.frames.
#'@param filePath Path to the db file to write (including name); has to end on \code{.db}.
#'
#'@return Creates a data base in the given path, returns NULL.
#'
#'@examples
#' # Set up data frames
#' df1 <- data.frame(v1 = 1, ID2 = 1)
#' df2 <- data.frame(ID2 = 1:1, v2 = 1:2)
#' dfList <- list(df1 = df1, df2 = df2)
#'
#' # Specify primary keys
#' pkList <- list(df1 = "v1", df2 = c("ID2", "v2"))
#' # Specify foreign keys
#' fkList <- list(df1 = list(References = NULL, Keys = NULL),
#'                df2 = list(References = "df1", Keys = "ID2"))
#'
#' # Create in memory data base
#' createDB(dfList = dfList, pkList = pkList, fkList = fkList, metaData = NULL, filePath = ":memory:")
#'
#'@export
createDB <- function(dfList, pkList, fkList = NULL, metaData = NULL, filePath) {
  if(is.null(fkList)) {
    fkList <- list(list(References = NULL, Keys = NULL))
    names(fkList) <- names(dfList)[1]
  }
  check_input(dfList = dfList, pkList = pkList, fkList = fkList)

  ### 1) Create queries for database creation
  # Queries for data
  dtQueries <- Map(writeQ_create, df = dfList, primeKey = pkList, foreignKey = fkList, df_name = names(dfList))
  # Query for table with merging information
  metaInfQuery <- writeQ_mergeOrder(dfMergeOrder = names(dfList))
  # Query for meta data table
  metaDatQuery <- character(0)
  if(!is.null(metaData)) {
    if(!is.data.frame(metaData)) stop("metaData must be a data frame.")
    labelDT_name <- "Meta_Data"
    metaDatQuery <- writeQ_create(df = metaData, df_name = labelDT_name,
                                primeKey = NULL, foreignKey = NULL)
  }
  # all queries into one object
  createQueries <- c(metaInfQuery, metaDatQuery, dtQueries)

  ### 2) Create empty Data base
  # check path / file
  check_filePath(filePath)
  # Establish Connection, disconnect when function exits
  con <- dbConnect_default(dbName = filePath)
  on.exit(DBI::dbDisconnect(con))

  ### 3) Execute "create Queries"
  lapply(createQueries, dbExecute_safe, conn = con)

  ### 4) fill data base tables with data
  # a) normale data tables
  lapply(seq_along(dfList), function(i)
    DBI::dbWriteTable(conn = con, name = names(dfList)[i], value = dfList[[i]], append = TRUE))
  # b) meta data table
  if(!is.null(metaData)) DBI::dbWriteTable(conn = con, name = labelDT_name, value = metaData, append = TRUE)

  return()
}



# 01a) Create Queries actual data tables ---------------------------------------------------------
## write query for data tables (without foreign keys)
writeQ_create <- function(df, primeKey, foreignKey, df_name) {
  # write string for variable definitions
  varDefinitions <- write_varDef(df)
  # write partial query for primary definition
  if(is.null(primeKey)) {
    pkDefinition <- ""
  } else {
    pkDefinition <- write_primeKey(primeKey)
  }
  # write partial query for foreign Key definition if fk is defined
  if(is.null(foreignKey$References)) {
    fkDefinition <- ""
  } else {
    fkDefinition <- write_foreignKey(foreignKey)
  }
  # write query including create, variable definitions and primary key
  paste("CREATE TABLE", paste(df_name), "(",
        varDefinitions,
        pkDefinition,
        fkDefinition,
        ");")
}

# variable definitions
write_varDef <- function(df) {
  varList <- vector(mode = "character", length = ncol(df))
  # determine type of all variables
  for(i in seq(ncol(df))) {
    if(is.double(df[, i]) || is.integer(df[, i])) varType = "REAL"
    else if(is.character(df[, i])) varType = "TEXT"
    else if(is.factor(df[, i])) varType = "TEXT"
    else stop("invalid variable type")
  # write syntax per variable
  varList[i]  <- paste(names(df)[i], varType)
  }
  # paste all together
  paste(varList, collapse = ", ")
}

# primary key definition
write_primeKey <- function(primeKey) {
  pk <- paste(primeKey, collapse = ", ")
  paste(", PRIMARY KEY (", pk, ")")
}

# foreign key definition
write_foreignKey <- function(foreignKey) {
  ref <- foreignKey$References
  fk <- paste(foreignKey$Keys, collapse = ", ")
  paste(", FOREIGN KEY (", fk,")", "REFERENCES", ref, "(", fk,")")
}

# 01b) Create Queries meta information data tables ---------------------------------------------------------
## query which saves the order of data frame merging
writeQ_mergeOrder <- function(dfMergeOrder) {
  createQ <- "CREATE TABLE Meta_Information ( mergeOrder TEXT );"
  insertQ <- paste("INSERT INTO Meta_Information (mergeOrder)",
                   "VALUES ( '", paste(dfMergeOrder, collapse = " "),"' );")
  c(createQ, insertQ)
}


# 02) Create Empty Database ---------------------------------------------------------
## create DB via Shell, deprecated because not needed!
# initiate via dbConnect
init_DB_shell <- function(filePath) {
  # create DB, throws an error if sqlite3 not in Path!
  shell(cmd = paste("sqlite3", filePath, ".databases", sep = " "), mustWork = TRUE)
}

# use default driver troughout package
#dbConnect_default <- function(dbName, drv = MonetDBLite::MonetDBLite()) {
dbConnect_default <- function(dbName, drv = RSQLite::SQLite()) {
  DBI::dbConnect(drv = drv, dbName)
}

## check path for db
check_filePath <- function(filePath){
  if(identical(filePath, ":memory:")) {
    message("filePath points to work memory")
    return()
  }
  lastSL <- rev(unlist(gregexpr("/", filePath)))[[1]]
  lastDot <- rev(unlist(gregexpr("\\.", filePath)))[[1]]
  # divide string
  directory <- substr(filePath, 1, lastSL)
  fileFormat <- substr(filePath, lastDot, nchar(filePath))
  # check directory
  if(!dir.exists(directory)) stop(paste(directory, "is not an existing directory"))
  # check file name
  if(file.exists(filePath)) stop(paste(filePath, "is an existing data base"))
  if(!identical(fileFormat, ".db")) stop("Filename does not end on .db")

  return()
}


# 03) Execute Create Queries ---------------------------------------------------------
# safe version of dbExecute with verbose error
dbExecute_safe <- function(conn, statement) {
  check <- try(DBI::dbExecute(conn = conn, statement = statement))
  if(class(check) == "try-error") {
    stop(paste("Error while trying to execute the following query: \n", statement))
  }
}

# 05) Index creation ---------------------------------------------------------
# not necessary, if joins are performed on primary keys, as for these auto indexes are generated
#writeQ_indexes <- function() {
#  out <- paste("CREATE INDEX", some_index_same, "ON", datatable, "(", variables, ");")
#}
