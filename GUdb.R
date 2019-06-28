library(RMariaDB)

localuserpassword <- "tSG7RWiMj8uK9vD97X2wUZf2kw0gOvV4"
GUdb <- dbConnect(RMariaDB::MariaDB(), user='surveyuser', password=localuserpassword, dbname='surveydatabase', host='api.gieffektivt.no')
dbListTables(GUdb)



dbDisconnect(GUdb)
