

#### Creating DB from list of data frames
#############################################################################
#' Create a (relational) data base.
#'
#' Creates a relational data base from a list of data frames. The list structure of dfList, pkList and fkList needs to be exactly the same. Keys (pkList and fkList) can either be single variables or combinations of variables. Primary keys (pkList) have to be unique within a single data table. Foreign Keys (fkList) have to consist of a list with the referenced data frame and the referencing keys. All list elements have to be named and in the same order. If a single data frame is to be converted to a data base, fkList can be dropped.
#'
#' Primary keys guarantee uniqueness of cases within a single data table, and are single variables or combinations of variables. Foreign keys are used to merge data tables. The foreign key for the first data set is always list(References = NULL, Keys = NULL) The order in which the data frames are supplied determines the merge order. Currently, only Left Joins are implemented. Primary keys have to be variables or combinations of variables that are unique within the data frame.
#' Data frames are stored seperatly as a relational data base and are merged if pulled from the data base. Then, data frames are joined in the order in which the data frames were supplied in the list. Left Joints are used for merging. SQLite3 is used as a database system.
#'
#'@param dfList Named list of data frames. The order of the data frames determines the merge order.
#'@param pkList Named list of the primary keys corresponding to the data frames.
#'@param fkList Named list of a list per data frame, including referenced data frame ("References") and the corresponding keys ("Keys"). NULL if only a single data frame is supplied.
#'@param metaData Data frame including meta data information about the other data frames.
#'@param filePath Path to the db file to write (including name); has to end on '.db'.
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
  ### checks
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
  con <- dbConnect(RSQLite::SQLite(), dbname = filePath)
  on.exit(dbDisconnect(con))

  ### 3) Execute "create Queries"
  lapply(createQueries, dbExecute_safe, conn = con)

  ### 4) fill data base tables with data
  # a) normale data tables
  lapply(seq_along(dfList), function(i)
    dbWriteTable(conn = con, name = names(dfList)[i], value = dfList[[i]], append = TRUE))

  # b) meta data table
  if(!is.null(metaData)) dbWriteTable(conn = con, name = labelDT_name, value = metaData, append = TRUE)
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
  check <- try(dbExecute(conn = conn, statement = statement))
  if(class(check) == "try-error") {
    stop(paste("Error while trying to execute the following query: \n", statement))
  }
}


