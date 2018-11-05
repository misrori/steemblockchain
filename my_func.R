library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(rvest)
get_all_tr <- function(name) {
  r = POST('http://127.0.0.1:54321/get_all_tr', body = list("name" = name), encode = 'json')
  adat <- fromJSON( content(r, 'text' ))
  return(adat)
}

all_tr<- get_all_tr("misrori")



get_alltr_df <- function(all_tr) {
  get_tr_lapply <- function(x) {
    tryCatch({
      if(x[[1]][[2]]$op[[1]]=='transfer'){
        return(x)
      }
    }, error = function(e) {
      NULL
    })
    
    
  }
  
  all_filtered_tr <- NULL
  
  for(i in c(1:length(all_tr))){
     t<- get_tr_lapply(all_tr[i])
     if(is.null(t)==FALSE){
       #print(t)
       
      k<-  data.frame('amount'=t[[1]][[2]]$op[[2]]$amount,
                      'from'= t[[1]][[2]]$op[[2]]$from, 
                      'memo' = t[[1]][[2]]$op[[2]]$memo,
                      'to' = t[[1]][[2]]$op[[2]]$to,
                      'timestamp' = t[[1]][[2]]$timestamp, stringsAsFactors = F)
       
       all_filtered_tr[[i]]=k
       
     }
  }
  eredmeny<- rbindlist(all_filtered_tr)
  eredmeny$currency_amount <- as.numeric(lapply(strsplit(as.character(eredmeny$amount), ' '),'[[', 1))
  eredmeny$currency_type <- as.character(lapply(strsplit(as.character(eredmeny$amount), ' '),'[[', 2))
  
  return(eredmeny)
}


eredmeny <- get_alltr_df(all_tr)


get_one_coin <- function(coin,agg_time){
  
  link<- paste('https://min-api.cryptocompare.com/data/', agg_time,'?fsym=',coin,'&tsym=USD&limit=2000',sep ="")
  adat <- fromJSON(link)
  if(adat$Response=="Success"){
    adat<- data.table(adat$Data)
    adat<- adat[high!=0&close!=0&low!=0,]
    adat$time <- as.POSIXct(adat$time, origin="1970-01-01")
    adat$symbol <- coin
    adat <- adat[,c("symbol","time",'close')]
    return(adat)
  }else{
    return(data.frame())
  }
}

get_one_coin('STEEM', 'histoday')
