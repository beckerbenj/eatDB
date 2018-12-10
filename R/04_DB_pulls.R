
#### Extracting data from relational data base
#############################################################################
#' Pull data from relational data base.
#'
#' Function to extract specific (or all) variables from various data tables. Variables are merged in the specified merge order via left joins and using the foreign keys.
#'
#' Note that the exact merging process is determined when the data base is created via createDB and can not be altered posthoc. Further options (e.g. including all variables of a single data table, filtering cases, full joins) are still under development. If you want to use the package and have specific requests, please contact the package author.
#'
#'@param vSelect Character vector of variables that should be pulled from data base. If vSelect is NULL, all variables from the data base are selected.
#'@param filePath Path of the existing db file.
#'
#'@return Returns a data frame, including the selected variables. Variables, that exist in more than one data table are only selected from the first data table in the merge order.
#'
#'@examples
#' # not run:
#' # dbSingleDF(vSelect = c("var1", "var5"), filePath = "exampleDB.db")
#'
#'@export
dbPull <- function(vSelect = NULL, filePath) {
  # 1) check input
  check_dbPath(dbPath = filePath)
  # check names and sort to data tables
  varList <- prep_vSelect(vSelect = vSelect, filePath = filePath)

  # Establish Connection, disconnect when function exits
  con <- dbConnect_default(dbName = filePath)
  on.exit(dbDisconnect(con))

  # 2) get names/structure/mergeorder for data base
  keyList <- dbKeys(filePath, includeMeta = FALSE)
  mergeOrder <- get_mergeOrder(con)

  # 3) create query
  pullQ <- writeQ_pull(mergeOrder = mergeOrder, fkList = keyList$fkList, varList = varList)

  # 4) execute query
  df <- dbGet_safe(conn = con, statement = pullQ)

  df
}

### 1) Input check ---------------------------------------------------------
prep_vSelect <- function(vSelect, filePath) {
  allNames <- dbNames(filePath = filePath)

  # all variables are selected
  if(is.null(vSelect)) vSelect <- unique(unlist(allNames))
  # create List with data table attribution
  varList <- list()
  for(df_name in names(allNames)) {
    varList[[df_name]] <- order_vSelect(vSelect = vSelect, allNames_df = allNames[[df_name]])
    vSelect <- vSelect[!vSelect %in% unlist(varList)]
  }

  # check if all names anywhere in data set
  if(length(vSelect) != 0) {
    vSelect <- paste(vSelect, collapse = ", ")
    stop(paste("Variables ", vSelect, "are not in the data base"))
  }

  varList
}

#
order_vSelect <- function(allNames_df, vSelect) {
  if(is.null(vSelect)) return(allNames_df)
  allNames_df[allNames_df %in% vSelect]
}



### 2) Get Meta-Information from DB ---------------------------------------------------------
get_mergeOrder <- function(con) {
  q <- "SELECT * FROM Meta_Information;"
  mergeOrder <- dbGetQuery(conn = con, statement = q)
  # restore original format
  mO <- unlist(strsplit(unlist(mergeOrder), " "))
  names(mO) <- NULL
  mO[mO != ""]
}


### 3) Create Pull Query ---------------------------------------------------------
writeQ_pull <- function(varList, mergeOrder, fkList) {
  ljoins <- write_LJoins(mergeOrder = mergeOrder, fkList = fkList)
  selVars <- write_SEL(varList = varList)
  # put together query
  paste("SELECT DISTINCT", selVars ,
        "FROM",
        mergeOrder[1],
        ljoins, ";")
}
# part of query for left joins
write_LJoins <- function(mergeOrder, fkList) {
  joins <- vector("character")
  for(i in 2:length(mergeOrder)) {
    keyName <- fkList[[mergeOrder[i]]]$Keys
    joins[i-1] <- paste("LEFT JOIN", mergeOrder[i],
                        "using (", paste(keyName, collapse = ", "), " )", sep = " ")
  }
  paste(joins, collapse = " ")
}
# part of query for variable selection
write_SEL <- function(varList = NULL) {
  # default: selects all variables
  if(is.null(varList)) return(" * ")

  # otherwise:
  varVec <- unlist(Map(write_varNames, dfName = names(varList), vars = varList))
  varVec <- paste(varVec, collapse = ", ")
  varVec
}
# create variables names for single data frame
write_varNames <- function(dfName, vars) {
  if(length(vars) == 0) return(character(0))
  paste(dfName, ".", vars, sep = "")
}



# 04) Execute Queries ---------------------------------------------------------
# safe version of dbExecute with verbose error
dbGet_safe <- function(conn, statement) {
  check <- try(dbGetQuery(conn = conn, statement = statement))
  if(class(check) == "try-error") {
    stop(paste("Error while trying to execute the following query", statement))
  }
  check
}







