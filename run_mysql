library(RMySQL)

mydb = dbConnect(MySQL(), user='', password='',dbname='',host='')
dbListTables(mydb)

rs = dbSendQuery(mydb,
"select *
from  "
)

data = fetch(rs, n=-1)
