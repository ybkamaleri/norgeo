
# norgeo

<!-- badges: start -->

<!-- badges: end -->

The aim of `norgeo` package is to be able to track changes of geo codes
for different granularity levels in Norway. This is based on information
and tables available on SSB
[website](https://www.ssb.no/befolkning/artikler-og-publikasjoner/regionale-endringer-2020).
Therefore, the use of this package might be limited to the data
available from SSB.

## Installation

`norgeo` package can be installed directly from **GitHub** page of
[Folkehelseprofil](https://github.com/folkehelseprofil). You can run the
code below for installation. It will use `remotes` package to access to
the **GitHub**. If you haven’t installed it before, the package will be
installed automatically prior to installing `norgeo`.

``` r
if(!require(remotes)) install.packages("remotes")
remotes::install.packages("folkehelseprofil/norgeo")
```

## Track code changes

You have to download the files from SSB and save it as a `csv` file. For
the code changes, they have to be copied pasted in an Excel. These code
changes can be found under *Endringer* tab in the website. It’s
advisable to name the file with a specific word to differentiate the
file from the `csv` files. I would recommed to add the word `change`.
For example `fylke_change_jan2018.xlsx` and the downloaded `csv` file
will be `fylke_jan2028.csv`. It’s also advisable to add the year to all
files to easily differentiate them when running the code.

### Add changes

The `add_change()` function is the most important function to start
with. This function will create an object that is easily used by other
available functions in `norgeo`. This is where raw data in `csv` and
`xlsx` formats will be merged. Additional information will also be
added. The codes below show how the function is used:

``` r
## specify the folder where kommune files are
folder <- "F:/org/kommune"

kom2018 <- add_change("jan2018",
                      "change",
                      year = 2018,
                      type = "kommune",
                      folder.path = folder)

kom2019 <- add_change("jan2019",
                      "change",
                      year = 2019,
                      type = "kommune",
                      folder.path = folder)

kom2020 <- add_change("jan2020",
                      "change",
                      year = 2020,
                      type = "kommune",
                      folder.path = folder)
```

### Merge changes

To merge all these files into a big dataset, use `merge_geo()` function.
Important to note that input for `merge_geo()` must be a **list** and
they are arranged from oldest to most recent codes. Available output
includes:

  - `all` : All merged codes. This is the default
  - `change` : Only codes that have changed
  - `split` : Codes that have been divided to at least two codes in the
    current list
  - `merge` : At least two codes are merged into one in the current list

<!-- end list -->

``` r

komfiles <- list(kom2018, kom2019, kom2020)
DF <- merge_geo(files = komfiles)
merge_geo(files = komfiles, output = "change")
```

### Save codes

The output created from `merge_geo()` i.e `DF`, can be saved in a
database management system (DBMS) which include Access and SQLite, or as
ordinary files such as Excel or text file.

``` r
fpath = "C:/Users/ybka/dbms"

save_geo(tblname = "tblTest",
         obj = DT,
         des.path = fpath,
         file.type = "Excel")
```

## Add granularity

The function `cast_all()` will add lower granularity to all files when
applicable. It means for *kommune* files, a new `fylke` column will be
added, while column `fylke` and `kommune` will be added to a *bydel* and
*grunnkrets* files.

``` r

## Geo 2019
file2019 <- c("F:/Geo/fylke/fylke_jan2019.csv",
              "F:/Geo/kommune/kommune_jan2019.csv",
              "F:/Geo/bydel/bydel_jan2019.csv",
              "F:/Geo/grunnkrets/grunnkrets_jan2019.csv")

fileTyp2019 <- c("fylke", "kommune", "bydel", "grunnkrets")


DF2019 <- cast_all(file = file2019,
                   type = fileTyp2019,
                   year = 2019)

## Geo 2020
file2020 <- c("F:/Geo/fylke/fylke_jan2020.csv",
              "F:/Geo/kommune/kommune_jan2020.csv",
              "F:/Geo/bydel/bydel_jan2020.csv",
              "F:/Geo/grunnkrets/grunnkrets_jan2020.csv")

fileTyp2020 <- c("fylke", "kommune", "bydel", "grunnkrets")


DF2020 <- cast_all(file = file2020,
                   type = fileTyp2020,
                   year = 2020)
```

Running `cast_all()` will also add a new row for granularity `land`
which has code `0`. The output from the above example can then be merged
to a file and saved to the file type one wishes. To bind all the output,
you can use the base R `rbind()` function or `rbindlist()` function from
`data.table` package.

``` r

bigDF <- rbind(DF2019, DF2020)
```

To save the final result, `save_geo()` function can be used as explained
above. Database file must be available prior to saving it as a database
table. In the example below it’s `geo_ssb.accdb`.

``` r
dbpath = "C:/Users/ybka/dbms"
dbname = "geo_ssb.accdb"

save_geo(tblname = "tblGeo",
         obj = bigDT,
         des.path = dbpath,
         file.type = "Access",
         db.name = dbname)
```
