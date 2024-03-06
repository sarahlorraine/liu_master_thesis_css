library(httr)
library(jsonlite)


age.request_body <- data.frame(
  code = "Region",
  selection = data.frame(filter = "vs:DeSoHE", values = c("0114C1030", "0114C1030")),
  code = "UtlBakgrund")
age.request_body <- toJSON(list(query = age.request_body), auto_unbox = TRUE)

age.request_body <- '{"query": [{"code": "Region","selection": { "filter": "vs:DeSoHE","values": [ "0114C1030"]}},{"code": "UtlBakgrund", "selection": {"filter": "item","values": ["2","1"]}},{"code": "Tid","selection": {"filter": "item","values": ["2022"]}}],"response": { "format": "px"}}'
age.res = POST("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101Y/FolkmDesoBakgrKonN",
               body=age.request_body)
age.data <- fromJSON(rawToChar(age.res$content))
age.data