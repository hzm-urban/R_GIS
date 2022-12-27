#2022/01 作成 2022/12 最終変更 coded by: https://github.com/hzm-urban
#Adlistには、日本語表記の住所を格納。表記ゆれや大字とは異なる地名も、ある程度は柔軟に対応してくれる。
#このコード以外に必要なものはないはず。

library("httr")
library("data.table")
library(dplyr)

geocoding<-function(AdList){
  addr<-AdList
  lati<-numeric(length(addr))
  long<-numeric(length(addr))
  
  # Request object from API
  for (i in 1:length(addr)){
    r<-GET(paste0(
      "https://msearch.gsi.go.jp/address-search/AddressSearch?q=",
      addr[i]
    ),
    encoding="UTF-8"
    )
    
    dataquery<-content(r)
    #distance given in meters
    tryCatch(lati[i]<-dataquery[[1]][["geometry"]][["coordinates"]][[2]], error=function(e) NULL)
    
    #time given in seconds
    tryCatch(long[i]<-dataquery[[1]][["geometry"]][["coordinates"]][[1]], error=function(e) NULL)
    print(i)
    Sys.sleep(0.2) #地理院APIにアクセス間隔の要求はないが、念のため。4万地点の取得はこれでも拒否されなかった。
  }
  
  output<-data.table(Address=addr,latitude=lati,longitude=long)
}



#data.frameに緯度経度を追加するコード。こちらの使用を推奨。

geocoding_merge<-function(dat,column=NULL,error.detect=F){
  # Column name retrieve
  if (is.null(column)) {
    print(head(dat))
    print("Input column name.")
    column <- readline()
  }
  addr<-unique(dat[,column])
  lati<-numeric(length(addr))
  long<-numeric(length(addr))
  
  # Request object from API
  for (i in 1:length(addr)){
    r<-GET(paste0(
      "https://msearch.gsi.go.jp/address-search/AddressSearch?q=",
      addr[i]
    ),
    encoding="UTF-8"
    )
    
    dataquery<-content(r)
    #distance given in meters
    tryCatch(lati[i]<-dataquery[[1]][["geometry"]][["coordinates"]][[2]], error=function(e) NULL)
    
    #time given in seconds
    tryCatch(long[i]<-dataquery[[1]][["geometry"]][["coordinates"]][[1]], error=function(e) NULL)
    print(paste0(i,"/",length(addr)))
    Sys.sleep(0.2) #地理院APIにアクセス間隔の要求はないが、念のため。4万地点の取得はこれでも拒否されなかった。
  }
  address<-data.table(Address=addr,latitude=lati,longitude=long)
  output <- merge(dat,address,by.x=print(column),by.y = "Address")
  
  #Error detection
  if (error.detect==T) {
    print("Failed geocoding;")
    print(address[is.null(address[,"latitude"]),"Address"])
    print("May Incorrectly geocoded;")
    ingeo <- data.frame(cbind(address$Address,paste0(address$latitude,address$longitude)))
    ingeo %>% group_by(X2) %>% filter(n()>1)
    print(ingeo)
    output <- list(output,ingeo)
  }
  return(output)
}
  
