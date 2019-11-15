library(rvest)
library(XML)
library(httr)
library(RCurl)
library(rjson)
library(dplyr)
library(tidyr)

get_cmc_coins=function(){
url=paste0("https://coinmarketcap.com/all/views/all/")
html_data=xml2::read_html(url)
url_slug=html_data %>% html_nodes(".link-secondary") %>% html_attr('href')
doc=htmlParse(html_data)
df1=readHTMLTable(doc)
url_symbol=as.character(as.data.frame(df1[1])[,3])
url_slug=unique(html_data %>% html_nodes(".link-secondary") %>% html_attr('href'))
url_info=data.frame(url_slug=url_slug,symbol=url_symbol)%>%mutate(slug=gsub("/currencies/","",url_slug))%>%mutate(slug=gsub("/","",slug))
return(url_info)
}

cmc_coins=get_cmc_coins()

Bittrex_API_Coins=httr::GET("https://api.bittrex.com/v3/currencies")
Bittrex_API_Coins=data.table::rbindlist(httr::content(Bittrex_API_Coins, as="parsed"),fill=T)%>%
    filter(status%in%"ONLINE")%>%select(symbol,name)%>%left_join(cmc_coins)
 

  
crypto_history=function(coin,start_date,end_date){
  coin=toupper(coin)
  desired_coin=Bittrex_API_Coins%>%filter(symbol%in%coin)
  daily_prices=NULL
  if(nrow(desired_coin)>0 & !is.na(desired_coin[1,3])){
  for (cmc_currency in unique(desired_coin$url_slug)){
  url=paste0("https://coinmarketcap.com",cmc_currency,"historical-data/?start=",start_date,"&end=",end_date)
  html_data=xml2::read_html(url)
  doc=htmlParse(html_data)
  df1=readHTMLTable(doc)
  prices=as.data.frame(df1[3])
  colnames(prices)=c("date","open","high","low","close","volume","market")
   prices$date=as.character(as.Date(as.character(prices$date),"%B %d, %Y"))
   prices[,2:7]=prices[,2:7]%>%mutate_all(extract_numeric)
   prices=prices%>%mutate(symbol=as.character(coin),
                         name=desired_coin$name[desired_coin$url_slug==cmc_currency],
                         slug=desired_coin$slug[desired_coin$url_slug==cmc_currency],spread=0,close_ratio=0)
   daily_prices=daily_prices%>%bind_rows(prices)
   return(daily_prices)
  }
  }else{
    message(paste(coin,"is not an online BITTREX coin listed on coinmarketcap"))
    return(NULL)}
}
    


#####coin is any token listed on bittrex
####start_date can be any date in this format "20191101"
####end_date can be any date in this format "20191111"
ohlvc=crypto_history(coin="BTC",start_date,end_date)
