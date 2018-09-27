

context("Pull and merge data from DB")
library(eatDB)

# load test data (df1, df2, pkList, fkList)
load(file = "c:/Benjamin_Becker/02_Repositories/packages/eatDB/tests/testthat/helper_dbdata.rda")


### variable input check
test_that("Variable selection checked correctly", {
  expect_error(prep_vSelect(c("ID2", "ID3"), filePath = "helper_database.db"),
               "ID3 are not in the data base")
  expect_silent(prep_vSelect(c("ID2", "v1"), filePath = "helper_database.db"))
})

test_that("Variable selection prepared correctly", {
  expect_identical(prep_vSelect(c("v1", "ID2"), filePath = "helper_database.db"), list(df1 = c("v1", "ID2"), df2 = character(0)))
})

test_that("Variable selection prepared correctly for output all variables", {
  expect_identical(prep_vSelect(vSelect = NULL, filePath = "helper_database.db"), list(df1 = c("v1", "ID2"), df2 = c("v2")))
})

### Query creation
test_that("Left Joins formulated correctly based on foreign keys and merge Order", {
  expect_identical(write_LJoins(c("df1", "df2"), fkList = fkList),
                   "LEFT JOIN df2 using ( ID2  )")
})

test_that("Variable selection pasted correctly for single data frame", {
  expect_identical(write_varNames(dfName = "df1", vars = c("v1", "ID2")),
                   c("df1.v1",  "df1.ID2"))
  expect_identical(write_varNames(dfName = "df2", vars = character(0)),
                    character(0))
})

test_that("Variable selection pasted correctly", {
  expect_identical(write_SEL(list(df1 = c("v1", "ID2"), df2 = ("v2"))),
                   "df1.v1, df1.ID2, df2.v2")
  expect_identical(write_SEL(list(df1 = c("v1", "ID2"), df2 = character(0))),
                   "df1.v1, df1.ID2")
})


test_that("Complete Query pasted correctly", {
  expect_identical(writeQ_pull(list(df1 = c("v1", "ID2"), df2 = c("ID2", "v2")), mergeOrder = c("df1", "df2"),fkList = fkList),
                   "SELECT DISTINCT df1.v1, df1.ID2, df2.ID2, df2.v2 FROM df1 LEFT JOIN df2 using ( ID2  ) ;")
})


### Merged result
# create expected data result
m1 <- merge(dfList$df1, dfList$df2, all = TRUE)
expected <- m1[, c(2, 1, 3)]


test_that("Merged results are correct for complete pulls", {
  expect_equal(dbPull(filePath = "helper_database.db"), expected)
})

test_that("Merged results are correct if one data table has no variables in output", {
  expect_equal(dbPull(vSelect = c("v1", "ID2"), filePath = "helper_database.db"), expected[1, -3])
})



#dbPull(vSelect = c("v1", "ID2"), filePath = "c:/Benjamin_Becker/02_Repositories/packages/eatDB/tests/testthat/helper_database.db")
#dbPull(vSelect = NULL, filePath = "c:/Benjamin_Becker/02_Repositories/packages/eatDB/tests/testthat/helper_database.db")
