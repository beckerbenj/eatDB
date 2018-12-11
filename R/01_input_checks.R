

#### check input
#############################################################################
check_input <- function(dfList, pkList, fkList) {
  if(is.null(names(dfList)) || is.null(names(pkList)) || is.null(names(fkList)) ) stop("All input lists have to be named")
  if(!is.list(dfList)) stop("dfList has to be a list.")
  if(!identical(names(dfList), names(pkList))) stop("dfList and pkList have to have identical structure and namings.")
  if(!identical(names(dfList), names(fkList))) stop("dfList and fkList have to have identical structure and namings.")

  # check all data frames, primary and foreign keys
  Map(check_df, df = dfList, df_name = names(dfList))
  Map(check_pk, df = dfList, primeKey = pkList, df_name = names(dfList))
  Map(function(fk, df_n) check_fk(foreignKey = fk, df_name = df_n, dfList = dfList, pkList = pkList),
      fk = fkList, df_n = names(dfList))

  return()
}


#
# 01) input checks for single data frame ---------------------------------------------------------
check_df <- function(df, primeKey, df_name) {
  if(!is.data.frame(df)) stop(paste(df_name), " is not a data frame.")
  if(ncol(df) < 1 || nrow(df) < 1) stop(paste(df_name), " is a data.frame with zero rows or columns.")
  if(identical(df_name, "")) stop("Data frame with empty name.")

  # names data frame
  if(any(duplicated(names(df)))) stop(paste("Some variables names in ", df_name, "are duplicates"))
  if(any(identical(names(df), ""))) stop(paste("Some variables names in ", df_name, "are empty"))
  illegal_names <- c("Meta_Data", "Meta_Information")
  if(any(names(df) %in% illegal_names)) stop(paste("One of the following forbidden data frame names has been used:",
                                                   paste(illegal_names, collapse = ", ")))
  if(any(grepl("\\.", names(df)))) stop(paste("Variable names are not allowed to contain '.' in SQLite."))
  return()
}

# 02) input checks for primary key ---------------------------------------------------------
check_pk <- function(primeKey, df, df_name) {
  print_keys <- paste(primeKey, collapse = " ,")
  if(!any(primeKey %in% names(df))) stop(paste(print_keys, "are not variables in", df_name))
}

# 03) input checks for primary key ---------------------------------------------------------
check_fk <- function(foreignKey, df_name, dfList, pkList) {
  print_keys <- paste(foreignKey[[2]], collapse = " ,")
  if(length(foreignKey) != 2 || !names(foreignKey) %in% c("References", "Keys")) {
    stop(paste("Foreign Keys for ", df_name, " have incorrect format, check help for correct formatting."))
  }

  # foreign keys for first data frame has to be NULL, then stop testing
  if(df_name == names(dfList)[1]) {
    if(!is.null(foreignKey$References) || !is.null(foreignKey$Keys)) {
      stop("Foreign key defined for first data frame.")
    }
    return()
  }

  ref <- foreignKey$References
  keys <- foreignKey$Keys
  # formal checks
  if(length(ref) != 1) stop(paste("Foreign Key reference for", df_name, "must be exactly one other data frame."))
  if(length(keys) < 1) stop(paste("Foreign Keys for", df_name, "are undefined."))
  # reference
  if(!ref %in% names(dfList)) stop(paste("Foreign Key for", df_name, "references not an existing data frame."))
  # keys in both data frames?
  if(any(!keys %in% names(dfList[[df_name]]))) stop(paste(print_keys, "are not variables in", df_name, "."))
  if(any(!keys %in% names(dfList[[ref]]))) stop(paste(print_keys, "are not variables in", ref, "."))
  # all keys must be numerics, strings slow down SQLite A LOT
  #browser()
  if(any(!is.numeric(dfList[[df_name]][, keys]))) stop(paste("All foreign keys have to be numeric. Check keys in ", df_name, "."))
  if(any(!is.numeric(dfList[[ref]][, keys]))) stop(paste("All foreign keys have to be numeric. Check keys in ", ref, "."))
  return()
}





