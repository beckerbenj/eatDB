# Get variable names from a relational data base.

Function to get the names of the variables included in the relational
data base.

## Usage

``` r
dbNames(filePath, includeMeta = FALSE)
```

## Arguments

- filePath:

  Path of an existing `.db` file.

- includeMeta:

  Should the variable names of the `Meta_Data` table be included.

## Value

Returns a named list of character vectors with the variables names
included in the data tables.

## Details

Extracts names of all variables included in the relational data base,
structured as a list with the individual data tables as elements. The
ordering in the list is equivalent to the merge order used when data is
pulled from the data base.

## Examples

``` r
db_path <- system.file("extdata", "example_dataBase.db", package = "eatDB")
varNames <- dbNames(db_path)

## Names of data tables
names(varNames)
#> [1] "NoImp" "Imp"   "PVs"  

## Variable names in data table "NoImp"
varNames$NoImp
#> [1] "ID"     "age"    "weight"
```
