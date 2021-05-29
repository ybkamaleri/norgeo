library(norgeo)
dlist <- get_list("fylke", 2019)
dlist
dchg <- get_change("fylke", year = 2020, from = 2010)
dchg
dset <- geo_set("fylke", 2020)
dset

dc <- get_change("fylke", 2020)
get_change("f", 2020, 2017)[]

get_code("b", 2000, 2020)[]
get_code("b", 2020)
get_change("b", 2020, 2019)[]

geo_set("fylke", 2021, 2018)

## Kommune
komchg <- get_change("kom", year = 2020, from = 2019)
komchg
uniqueN(komchg$newCode)
unique(komchg$newCode)
komchg[duplicated(newCode), .N]




## install
remotes::install_github("helseprofil/norgeo@dev")
