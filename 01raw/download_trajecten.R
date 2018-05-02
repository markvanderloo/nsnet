file_prefix <- format(Sys.time(),format="%Y%m%d_")
library(rvest)

html <- read_html("https://www.rijdendetreinen.nl/trajecten")

list_items <- html_nodes(html, xpath="//li")
trajecten <- html_text(list_items)
trajecten <- trajecten[grepl("-",trajecten)]

link_list <- html_nodes(list_items, xpath="//a")
links <- html_attr(link_list,"href")

links <- links[grepl("-",links)]
d <- data.frame(traject=trajecten,link=paste0("https://www.rijdendetreinen.nl",links),stringsAsFactors = FALSE)

write.csv(d,paste0("01raw/",file_prefix,"trajecten_en_links.csv"),row.names=FALSE)

L <- lapply(seq_len(nrow(d)), function(i){
  html <- read_html(d$link[i])
  traject_nodes <- html_nodes(html,css="body > div.wrap > div > div:nth-child(5) > div:nth-child(2) > ul")
  traject_nodes <- html_nodes(traject_nodes,xpath="//li")
  traject <- html_text(traject_nodes)
  traject <- traject[-(1:16)]
})

K <- lapply(seq_along(L), function(i){
  u <- L[[i]]
  data.frame(start=u[1:(length(u)-1)],stop=u[2:length(u)],traject=d$traject[i])
})

out <- do.call("rbind",K)

write.csv(out,paste0("01raw/",file_prefix,"trajecten.csv"),row.names=FALSE)


