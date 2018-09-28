
context("Check input via check_input")
library(eatDB)


# load test data (df1, df2, pkList, fkList)
load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatDB/tests/testthat/helper_dbdata.rda")
# load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatGADS/tests/testthat/helper_dbdata.rda")


### Checking primary and foreign keys
test_that("No errors if called correctly ", {
  expect_silent(check_input(dfList = dfList, pkList = pkList, fkList = fkList))
})


## expected errors
pkList2 <- list(df1 = "v1",
                df3 = "ID2")
pkList3 <- list(df1 = "v1",
                df2 = "ID2")
fkList2 <- list(df1 = list(References = "lala", Keys = NULL),
                df2 = list(References = "df1", Keys = "ID2"))
fkList3 <- list(df1 = list(References = NULL, Keys = NULL),
                df2 = list(References = NULL, Keys = "ID2"))


test_that("Errors are called correctly ", {
  expect_error(check_input(dfList = dfList, pkList = pkList2, fkList = fkList),
               "dfList and pkList have to have identical structure and namings")
  expect_error(check_input(dfList = dfList, pkList = pkList, fkList = fkList2),
               "Foreign key defined for first data frame")
  expect_error(check_input(dfList = dfList, pkList = pkList, fkList = fkList3),
               "Foreign Key reference for df2 must be exactly one other data frame")
})

names(dfList$df1) <- c("v.1", "ID2")
test_that("Errors are called correctly ", {
  expect_error(check_input(dfList = dfList, pkList = pkList, fkList = fkList),
               "Variable names are not allowed to contain '.' in SQLite.")
})

# tbd: further tests?


# should this test pass?
#   expect_error(addKeys(bigList = bigList, pkList = pkList3, fkList = fkList), "Foreign Key reference for df2 must be exactly one other data frame")
