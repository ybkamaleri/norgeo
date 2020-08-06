
<!-- README.md is generated from README.Rmd. Please edit that file -->

# norgeo

<!-- badges: start -->

<!-- badges: end -->

R pakke `norgeo` skal hjelpe til å spore geokoder endringer basert på
informasjon som er tilgjengelig på SSB
[hjemmeside](https://www.ssb.no/befolkning/artikler-og-publikasjoner/regionale-endringer-2020).

## Installasjon

Pakken kan installere fra
[Folkehelseprofil](https://github.com/folkehelseprofil) **GitHub** side.
Følgende kode kan kjøres i R console. Hvis du ikke allerede har
installert `remotes` pakke, skal den installeres først automatisk.

``` r
if(!require(remotes)) install.packages("remotes")
remotes::install.packages("folkehelseprofil/norgeo")
```

## Eksampel

This is a basic example which shows you how to solve a common problem:

``` r
library(norgeo)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
