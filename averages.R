{{library(ggplot2)
library(reshape2)
library(readxl)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(openxlsx)}
 #Read in file with data by state
{data <- read_excel("Discount Tracking By Region.xlsx", 
                   range = "a3:Cn134")
#Read in file with OOA data
 OOAdata<-read_excel("OOA Tracking.xlsx")
#Melt, get rid of NA, delete unnecessary columns and rows
dat<-melt(data)
dat<-dat[is.na(dat$FCAST1),]
dat<-dat[,c(1,2,8,9)]
dat1<-dat[-c(1:860),]
OOA<-melt(OOAdata)
 #Make an identifier
dat$id <- paste(dat$MEMSTATE, dat$PRODUCT, sep=":")
#Separate bthe setlements into new data frames
sttlmnt2015<-dat[grepl("Settlement 2015",dat$variable),]
sttlmnt2016<-dat[grepl("Settlement 2016",dat$variable),]
sttlmnt2017<-dat[grepl("Settlement 2017",dat$variable),]
sttlmnt2018<-dat[grepl("Settlement 2018",dat$variable),]
#Take non settlement years into df
k15<-dat[grepl("2015",dat$variable),]
k15$year<-c(2015)
k16<-dat[grepl("2016",dat$variable),]
k16$year<-c(2016)
k17<-dat[grepl("2017",dat$variable),]
k17$year<-c(2017)
k18<-dat[grepl("2018",dat$variable),]
k18$year<-c(2018)
#Match up the discount for the settlements
k15$Settlement <- sttlmnt2015$value[match(k15$id, sttlmnt2015$id)]
k16$Settlement <- sttlmnt2016$value[match(k16$id, sttlmnt2016$id)]
k17$Settlement <- sttlmnt2017$value[match(k17$id, sttlmnt2017$id)]
k18$Settlement <- sttlmnt2018$value[match(k18$id, sttlmnt2018$id)]
dat<-rbind(k15,k16,k17,k18)
dat<-dat[!grepl("Settlement",dat$variable),]

#Add months in new column
dat<-dat[,-c(5)]
dat$value<-((dat$value)*100)
dat$Settlement<-((dat$Settlement)*100)
{
  q1<-dat[grepl("Q1",dat$variable),]
  q1$month<-"Q1"
  april<-dat[grep("Apr",dat$variable),]
  april$month<-"april"
  may<-dat[grepl("May",dat$variable),]
  may$month<-"may"
  june<-dat[grepl("June",dat$variable),]
  june$month<-"june"
  july<-dat[grepl("July",dat$variable),]
  july$month<-"july"
  aug<-dat[grepl("Aug",dat$variable),]
  aug$month<-"aug"
  sept<-dat[grepl("Sept",dat$variable),]
  sept$month<-"sept"
  oct<-dat[grepl("Oct",dat$variable),]
  oct$month<-"oct"
  nov<-dat[grepl("Nov",dat$variable),]
  nov$month<-"nov"
  dec<-dat[grepl("Dec",dat$variable),]
  dec$month<-"dec"
  dat<-rbind(q1,april,may,june,july,aug,sept,oct,nov,dec)
  dat$month <- factor(dat$month, levels=c("Q1", "april", "may","june","july","aug","sept","oct","nov","dec"))
  
}
 #Calculate discount difference from end of the year settlement
dat$diff<-(dat$value)-(dat$Settlement)
attach(dat)
k15<-dat[grepl("2015",dat$year),]
k16<-dat[grepl("2016",dat$year),]
k17<-dat[grepl("2017",dat$year),]
k18<-dat[grepl("2018",dat$year),]}


{q1<-dat[grepl("Q1",dat$variable),]
april<-dat[grep("Apr",dat$variable),]
may<-dat[grepl("May",dat$variable),]
june<-dat[grepl("June",dat$variable),]
july<-dat[grepl("July",dat$variable),]
aug<-dat[grepl("Aug",dat$variable),]
sept<-dat[grepl("Sept",dat$variable),]
oct<-dat[grepl("Oct",dat$variable),]
nov<-dat[grepl("Nov",dat$variable),]
dec<-dat[grepl("Dec",dat$variable),]}
#Find average difference in percentage from the end of the year
#Q1
{
  avgQ1M<-tapply(q1$diff,list(q1$MEMSTATE,q1$year),mean)
  q1m<-melt(avgQ1M,value.name="avgMemstate")
  q1m$month<-"Q1"
  avgQ1P<-tapply(q1$diff,list(q1$PRODUCT,q1$year),mean)
  q1p<-melt(avgQ1P,value.name="avgProduct")
  q1p$month<-"Q1"
}
{
  avgaprM<-tapply(april$diff,list(april$MEMSTATE,april$year),mean)
  aprilm<-melt(avgaprM,value.name="avgMemstate")
  aprilm$month<-"april"
  avgaprilP<-tapply(april$diff,list(april$PRODUCT,april$year),mean)
  aprilp<-melt(avgaprilP,value.name="avgProduct")
  aprilp$month<-"april"
}

{
  avgmayM<-tapply(may$diff,list(may$MEMSTATE,may$year),mean)
  maym<-melt(avgmayM,value.name="avgMemstate")
  maym$month<-"may"
  avgmayP<-tapply(may$diff,list(may$PRODUCT,may$year),mean)
  mayp<-melt(avgmayP,value.name="avgProduct")
  mayp$month<-"may"
}

{
  avgjuneM<-tapply(june$diff,list(june$MEMSTATE,june$year),mean)
  junem<-melt(avgjuneM,value.name="avgMemstate")
  junem$month<-"june"
  avgjuneP<-tapply(june$diff,list(june$PRODUCT,june$year),mean)
  junep<-melt(avgjuneP,value.name="avgProduct")
  junep$month<-"june"
}

{
  avgjulyM<-tapply(july$diff,list(july$MEMSTATE,july$year),mean)
  julym<-melt(avgjulyM,value.name="avgMemstate")
  julym$month<-"july"
  avgjulyP<-tapply(july$diff,list(july$PRODUCT,july$year),mean)
  julyp<-melt(avgjulyP,value.name="avgProduct")
  julyp$month<-"july"
}

{
  avgaugM<-tapply(aug$diff,list(aug$MEMSTATE,aug$year),mean)
  augm<-melt(avgaugM,value.name="avgMemstate")
  augm$month<-"aug"
  avgaugP<-tapply(aug$diff,list(aug$PRODUCT,aug$year),mean)
  augp<-melt(avgaugP,value.name="avgProduct")
  augp$month<-"aug"
}
{
  avgseptM<-tapply(sept$diff,list(sept$MEMSTATE,sept$year),mean)
  septm<-melt(avgseptM,value.name="avgMemstate")
  septm$month<-"sept"
  avgseptP<-tapply(sept$diff,list(sept$PRODUCT,sept$year),mean)
  septp<-melt(avgseptP,value.name="avgProduct")
  septp$month<-"sept"
}
{
  avgoctM<-tapply(oct$diff,list(oct$MEMSTATE,oct$year),mean)
  octm<-melt(avgoctM,value.name="avgMemstate")
  octm$month<-"oct"
  avgoctP<-tapply(oct$diff,list(oct$PRODUCT,oct$year),mean)
  octp<-melt(avgoctP,value.name="avgProduct")
  octp$month<-"oct"
}
{
  avgnovM<-tapply(nov$diff,list(nov$MEMSTATE,nov$year),mean)
  novm<-melt(avgnovM,value.name="avgMemstate")
  novm$month<-"nov"
  avgnovP<-tapply(nov$diff,list(nov$PRODUCT,nov$year),mean)
  novp<-melt(avgnovP,value.name="avgProduct")
  novp$month<-"nov"
}
{
  avgdecM<-tapply(dec$diff,list(dec$MEMSTATE,dec$year),mean)
  decm<-melt(avgdecM,value.name="avgMemstate")
  decm$month<-"dec"
  avgdecP<-tapply(dec$diff,list(dec$PRODUCT,dec$year),mean)
  decp<-melt(avgdecP,value.name="avgProduct")
  decp$month<-"dec"
}

avgProduct<-rbind(q1p,aprilp,mayp,junep,julyp,augp,septp,octp,novp,decp)
avgProduct$month <- factor(avgProduct$month, levels=c("Q1", "april", "may","june","july","aug","sept","oct","nov","dec"))
avgRegion<-rbind(q1m,aprilm,maym,junem,julym,augm,septm,octm,novm,decm)
avgRegion$month <- factor(avgRegion$month, levels=c("Q1", "april", "may","june","july","aug","sept","oct","nov","dec"))
}
#Line plot by Product
{avgP<-ggplot(avgRegion, aes(x=avgRegion$month,y=avgRegion$avgMemstate,color=Var1,group=avgRegion$Var1))+geom_line()+
 labs(y="percentage difference from settlement on average", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))
avgP+facet_grid(avgRegion$Var2~.)+ggtitle("Average Percentage Difference from Settlement by Region")+ theme(plot.title = element_text(hjust = 0.5))
}
{avgM <-ggplot(avgProduct, aes(x=avgProduct$month,y=avgProduct$avgMemstate,color=Var1,group=avgRegion$Var1))+geom_line()+
    labs(y="percentage difference from settlement on average", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  avgM+facet_grid(avgRegion$Var2~.)+ggtitle("Average Percentage Difference from Settlement by Region")+ theme(plot.title = element_text(hjust = 0.5))
}
#Boxplots

prods<-ggplot(data =k15, aes(x=month,y=diff,group=PRODUCT,color=PRODUCT))+geom_line()+
labs(y="difference(percent)", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))
plotprod<-prods+facet_grid(MEMSTATE~.,scale="free_y")+ggtitle("Difference from Settlement in Percentage by Month")+ theme(plot.title = element_text(hjust = 0.5))
plotprod  

bp<-boxplot(diff~month,data=dat,main="Difference from Settlement - All Years", xlab="Months",ylab="Percent Difference from Settlement",abline(h=0))
bp15<-boxplot(diff~month,data=k15,main="Difference from Settlement - 2015", xlab="Months",ylab="Percent Difference from Settlement",abline(h=0))
bp16<-boxplot(diff~month,data=k16,main="Difference from Settlement - 2016", xlab="Months",ylab="Percent Difference from Settlement")
bp17<-boxplot(diff~month,data=k17,main="Difference from Settlement - 2017", xlab="Months",ylab="Percent Difference from Settlement")
bp18<-boxplot(diff~month,data=k18,main="Difference from Settlement - 2018", xlab="Months",ylab="Percent Difference from Settlement")
lm1<-lm(diff~month-1,data=k15)
summary(lm1)
#write.xlsx(dat,'datafile.xlsx')
