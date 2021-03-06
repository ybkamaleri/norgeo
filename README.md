
[![R-CMD-check](https://github.com/helseprofil/norgeo/workflows/R-CMD-check/badge.svg)](https://github.com/helseprofil/norgeo/actions)

# norgeo

Regional granularity levels in Norway which are depicted by different
codes, have undergone several changes over the years. Identifying when
codes have changed and how many changes have taken place can be
troublesome. This package will help to identify these changes and when
the changes have taken place. One of the limitation of this package is
that it is heavily depending on the codes available from SSB which can
be accessed from their
[website](https://www.ssb.no/befolkning/artikler-og-publikasjoner/regionale-endringer-2020).
To use other data than the data structure should mimic those from SSB.

## Installation

`norgeo` package can be installed directly from **GitHub** page of
[Helseprofil](https://github.com/helseprofil). You can run the code
below for installation. It will use `remotes` package to access to the
**GitHub**. If you haven’t installed it before, the package will be
installed automatically prior to installing `norgeo`.

``` r
if(!require(remotes)) install.packages("remotes")
remotes::install_github("helseprofil/norgeo")
```

If you want to use the development version then use:

``` r
remotes::install_github("helseprofil/norgeo@dev")
```

## Usage

Data can either be downloaded directly from SSB website or via API. When
you download the files manually then use the functions with `geo_`
prefix while functions with `get_` prefix are used to get data via API.

All functions with `geo_` prefix are the main function for `norgeo`
downloaded files. Among them, `geo_set()` is the most important one to
set up object accordingly for further use. Other available functions
aren’t really necessary to know, but they are available when needed.
They are mostly for internal use. To learn how to use these function,
please read the tutorial under
[Guides](https://helseprofil.github.io/norgeo/articles/use-api.html)

To get all code changes directly via API can be done with `get_change()`
function, while `geo_change()` is used when you have downloaded the
files manually.

## Output

Among the output produced by the function `geo_merge()` is as follows:

![output-result](man/figures/kommune_merge.PNG)

The data elucidate the complexity of all the codes change. For Larvik
for instance, the manucipality has grown in 2020 with the inclusion of
Lardal. Therefore the code for Larvik has changed twice. How about
Holmestrand? When there are more than 350 manucipalities with different
changes, then tracking these can be a nightmare. The same with
enumeration units ie. *grunnkrets* with 14000 units\!

## File structure

I would suggest to structure your files in different folders as shown
below:

![File structure](man/figures/geo_dir2.PNG)
