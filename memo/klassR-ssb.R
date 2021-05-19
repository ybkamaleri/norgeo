devtools::install_github("statisticsnorway/klassR")
## install.packages("klassR")

library(klassR)
head(GetKlass(klass = "7"))
head(GetKlass(103, correspond = 1, date = "2018-01-01"))
head(GetKlass(131, correspond = 1, date = "2002-01-01"))
head(GetKlass(131, correspond = 1, date = "2020-01-01"))

head(GetKlass(klass = "131", date = c("2012-01-01", "2020-08-01")))

SearchKlass("fylke")
SearchKlass("bydel")
SearchKlass("kommune")

GetVersion(103)
GetName("1168")
GetFamily("103")

ListFamily(15)
ListKlass(TRUE)
