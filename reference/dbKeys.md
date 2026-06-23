# Get keys from a relational data base.

Function to get the primary and foreign keys of the data frames in the
relational data base.

## Usage

``` r
dbKeys(filePath, includeMeta = FALSE)
```

## Arguments

- filePath:

  Path of the existing db file.

- includeMeta:

  Should information about the `Meta_Data` table be included.

## Value

Returns a list named as the data tables in the db. Each elements
contains a list with the primary key, the data table it references to
and the corresponding foreign keys.

## Details

Data in a relational data base are indexed by primary and foreign keys.
Primary keys are unique identifiers inside a single data table. Foreign
keys reference (link) to other data tables. This function returns the
key structure of a relational data base.

## Examples

``` r
db_path <- system.file("extdata", "example_dataBase.db", package = "eatDB")
keys <- dbKeys(db_path)

## primary key structure of the database
keys$pkList
#> $NoImp
#> [1] "ID"
#> 
#> $Imp
#> [1] "ID"  "imp"
#> 
#> $PVs
#> [1] "ID"      "imp"     "subject"
#> 

## foreign key structure of the database
keys$fkList
#> $NoImp
#> $NoImp$References
#> NULL
#> 
#> $NoImp$Keys
#> NULL
#> 
#> 
#> $Imp
#> $Imp$References
#> [1] "NoImp"
#> 
#> $Imp$Keys
#> [1] "ID"
#> 
#> 
#> $PVs
#> $PVs$References
#> [1] "Imp"
#> 
#> $PVs$Keys
#> [1] "ID"  "imp"
#> 
#> 
```
