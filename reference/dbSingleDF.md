# Extract a single data table from a relational data base.

Function to extract a single, complete data table from a relational data
base. Especially useful for the extraction of the meta information
stored in `Meta_Data`.

## Usage

``` r
dbSingleDF(dfName = "Meta_Data", filePath)
```

## Arguments

- dfName:

  Name of the data table which should be extracted.

- filePath:

  Path of the existing db file.

## Value

Returns a data frame with all variables and cases as in the
corresponding data table.

## Details

This function makes use of the
[`DBI::dbReadTable`](https://dbi.r-dbi.org/reference/dbReadTable.html)
function and extracts a complete data table from a data base. All
variables are extracted and all rows are used. For extracting only some
variables or merging data tables see
[`dbPull`](https://beckerbenj.github.io/eatDB/reference/dbPull.md).

## Examples

``` r
db_path <- system.file("extdata", "example_dataBase.db", package = "eatDB")

## Extract all meta information
meta_data <- dbSingleDF(dfName = "Meta_Data", filePath = db_path)
meta_data
#>   varName                                                  varLabel data_table
#> 1      ID                                               ID variable      NoImp
#> 2     age                                              Age in years      NoImp
#> 3  weight                                   Body weight in kilogram      NoImp
#> 4     imp                                Multiple Imputation number        Imp
#> 5 noBooks                   Number of books at home (self reported)        Imp
#> 6 subject Competence domain (Mathematical Literacy/Reading Literacy        PVs
#> 7      pv                                           Plausible value        PVs

## Extract a specific data table
NoImp <- dbSingleDF(dfName = "NoImp", filePath = db_path)
NoImp
#>   ID age weight
#> 1  1  12     55
#> 2  2  17     40
#> 3  3  13     41
#> 4  4  15     42
#> 5  5  14     58
```
