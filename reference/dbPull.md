# Pull data from a relational data base.

Function to extract specific variables from various data tables.
Variables are merged in the specified merge order via left joins and
using the foreign keys. If variables are selected from a specific data
table, the corresponding primary keys are also always extracted. If no
variables from the first data tables in the `mergeOrder` are selected,
these data tables are skipped (up till the first variable - data table
match). If only variables of a single data table are selected, this data
table is extracted with all variables and sub setting is performed in
`R`.

## Usage

``` r
dbPull(vSelect = NULL, filePath)
```

## Arguments

- vSelect:

  Character vector of variables that should be pulled from data base. If
  `vSelect` is `NULL`, all variables from the data base are selected.

- filePath:

  Path to an the existing db file.

## Value

Returns a data frame, including the selected variables.

## Details

Note that the exact merging process is determined when the data base is
created via
[`createDB`](https://beckerbenj.github.io/eatDB/reference/createDB.md)
and can not be altered post hoc. Further options (e.g. filtering cases,
full joins) are still under development. If you want to use the package
and have specific requests, please contact the package author.

## Examples

``` r
db_path <- system.file("extdata", "example_dataBase.db", package = "eatDB")

## Extract variables from the first data table by name
# primary and foreign keys are added as required
dat1 <- dbPull(vSelect = c("age"), filePath = db_path)

## Extract all variables from the first data table
varNames <- dbNames(db_path)
dat2 <- dbPull(vSelect = varNames$NoImp, filePath = db_path)

## Extract variables from different data table (merged automatically)
dat3 <- dbPull(vSelect = c("weight", "noBooks", "pv"), filePath = db_path)

## Extract all variables from the data base
dat4 <- dbPull(filePath = db_path)
```
