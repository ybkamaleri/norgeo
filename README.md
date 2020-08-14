
# norgeo

<!-- badges: start -->

[![Build
Status](https://travis-ci.org/ybkamaleri/norgeo.svg?branch=master)](https://travis-ci.org/ybkamaleri/norgeo)
<!-- badges: end -->

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
[Folkehelseprofil](https://github.com/folkehelseprofil). You can run the
code below for installation. It will use `remotes` package to access to
the **GitHub**. If you haven’t installed it before, the package will be
installed automatically prior to installing `norgeo`.

``` r
if(!require(remotes)) install.packages("remotes")
remotes::install_github("folkehelseprofil/norgeo")
```

## Usage

All functions with `geo_` prefix are the main function for `norgeo`.
Among them, `geo_set()` is the most important one for setting up object
for further use. Other available functions aren’t really necessary to
know, but they are available when needed. To learn how to use these
function, please read the tutorial under
[Guides](articles/code-change.html)

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

I would suggest to structure your files in different folders as show
below:

![File structure](man/figures/geo_dir2.PNG)
