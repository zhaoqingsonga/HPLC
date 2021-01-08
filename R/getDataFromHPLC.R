
####################
#
####################
# library(stringr)
get_data_from_hplc<-function(filename="e:/new 9.txt"){
  mydata<-read.csv(filename,sep = "\t",fileEncoding = "UTF-8")
  mydata<-as.vector(as.matrix(mydata))
  mydata<-mydata[nchar(mydata)>20]

  location<-mydata[grep("仪器",mydata)]
  location<-unlist(strsplit(location,""))
  location<-paste(location[-grep(" ",location)],collapse="")

  myse<-NULL
  for(i in 1:length(mydata)){

    myline<-str_trim(mydata[i], side = c("both"))

    #isna<-as.numeric(substr(myline,1,3))
    myjudge<-all(unlist(strsplit(str_trim(substr(myline,1,3)),""))%in%c("0","1","2","3","4","5","6","7","8","9","0"))

    if(myjudge) myse<-rbind(myse,myline)
  }

  ####
  myse2<-NULL
  for(i in 1:nrow(myse)){
    unit<-unlist(strsplit(myse[i]," "))
    unit<-unit[nchar(unit)>0]
    unit<-unit[-grep("[A-Z]",unit)]
    myse2<-rbind(myse2,unit)
  }
  ####
  myse3<-as.data.frame(myse2)
  colnames(myse3)<-c("峰#","保留时间min","峰宽min","峰面积mAUs","峰高mAU","峰面积%")
  rownames(myse3)<-1:nrow(myse3)
  myse3$位置<-location
  return(myse3)
}

######################
#
######################




