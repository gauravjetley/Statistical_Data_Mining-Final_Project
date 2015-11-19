## Dr. Daniel's R Script for Cleaning and getting one integrated file

setwd("C:…\fabric_softener")
purdata<-read.table("D1PUR.DAT")
purdata$IRIWeek<-substring(purdata$V2,1,3)
purdata$Store<-as.numeric(substring(purdata$V2,4,6))
purdata$SKU<-as.numeric(substring(purdata$V2,7,9))
purdata<-purdata[,c("V1","IRIWeek", "Store", "SKU")]
names(purdata)<-c("HHId","IRIWeek","Store","SKU")
merchdata<-read.table("MERCH.DAT")
       
       
for(i in 1:length(merchdata$V5))
  {
  if(nchar(merchdata[i,"V5"])<6)
    {
    ZeroString<-character()
    for(j in 1:(6-nchar(merchdata[i,"V5"])))
      {
      ZeroString<-paste(ZeroString,0,sep='')
      }
    merchdata[i,"V5"]<-paste(ZeroString,merchdata[i,"V5"],sep='')
    }
  }

merchdata$Price<-as.numeric(substring(merchdata$V5,1,3))
merchdata$Display<-as.numeric(substring(merchdata$V5,5,5))
merchdata$Feature<-as.numeric(substring(merchdata$V5,6,6))
merchdata$Price<-merchdata$Price/100
merchdata<-merchdata[,-5]
merchdata<-merchdata[,c("V1","V2","V3","V4","Price","Display","Feature")]
names(merchdata)<-c("SKU","Store","IRIWeek","PricePaid","RegPrice","Display","Feature")
merchdata$IRIWeek<-as.numeric(merchdata$IRIWeek)
purplusmerch <- merge(purdata, merchdata, by=c("IRIWeek", "Store","SKU"))
attrdata<-read.csv("Membership panel Data.csv")
attrdata<-attrdata[,-1]
attrplusmerch <- merge(purplusmerch, attrdata, by=c("SKU"))
arspdata<-read.table("ARSP.DAT")
names(arspdata)<-c("SKU","Store","ARSP")
finaldata <- merge(attrplusmerch, arspdata, by=c("SKU","Store"))
finaldata<-finaldata[,c("HHId","SKU","IRIWeek","ARM","BNC","CLF","DWN","FNT","GEN","PRL","SNG","STP","TSN","B","F","L","S","LT","RG","ST","UN","LR","MD","SM","XL","PricePaid","RegPrice","ARSP","Display","Feature")]
finaldata$PriceCut<-finaldata$RegPrice-finaldata$PricePaid
finaldata<-finaldata[,c("HHId","SKU","IRIWeek","ARM","BNC","CLF","DWN","FNT","GEN","PRL","SNG","STP","TSN","B","F","L","S","LT","RG","ST","UN","LR","MD","SM","XL","RegPrice","PriceCut","ARSP","Display","Feature")]
names(finaldata)<-c("HHId","SKU","IRIWeek","ARM","BNC","CLF","DWN","FNT","GEN","PRL","SNG","STP","TSN","B","F","L","S","LT","RG","ST","UN","LR","MD","SM","XL","Price","PriceCut","AveragePrice","Display","Feature")
write.csv(finaldata,”finalized_data.csv",row.names=FALSE)