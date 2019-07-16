library(ggplot2)
library(reshape2)
library(readxl)
library(grid)
library(gridExtra)
library(dplyr)
library(tidyr)
library(openxlsx)
library(car)
data <- read_excel("X:/Reimbursement/RFPs/2018/Discounts/Tracking/By Region/Copy of Discount Tracking By Region KK.xlsx", 
                   range = "a3:Cn134")
OOAdata<-read_excel("X:/Reimbursement/RFPs/2018/Discounts/Tracking/By Region/OOA Tracking.xlsx")
dat<-melt(data)
dat<-dat[is.na(dat$FCAST1),]
dat<-dat[,c(1,2,8,9)]
dat1<-dat[-c(1:860),]
OOA<-melt(OOAdata)
dat$id <- paste(dat$MEMSTATE, dat$PRODUCT, sep=":")
sttlmnt2015<-dat[grepl("Settlement 2015",dat$variable),]
sttlmnt2016<-dat[grepl("Settlement 2016",dat$variable),]
sttlmnt2017<-dat[grepl("Settlement 2017",dat$variable),]
sttlmnt2018<-dat[grepl("Settlement 2018",dat$variable),]

k15<-dat[grepl("2015",dat$variable),]
k15$year<-c(2015)
k16<-dat[grepl("2016",dat$variable),]
k16$year<-c(2016)
k17<-dat[grepl("2017",dat$variable),]
k17$year<-c(2017)
k18<-dat[grepl("2018",dat$variable),]
k18$year<-c(2018)

k15$Settlement <- sttlmnt2015$value[match(k15$id, sttlmnt2015$id)]
k16$Settlement <- sttlmnt2016$value[match(k16$id, sttlmnt2016$id)]
k17$Settlement <- sttlmnt2017$value[match(k17$id, sttlmnt2017$id)]
k18$Settlement <- sttlmnt2018$value[match(k18$id, sttlmnt2018$id)]
dat<-rbind(k15,k16,k17,k18)
dat<-dat[!grepl("Settlement",dat$variable),]

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
dat$diff<-(dat$value)-(dat$Settlement)
attach(dat)
k15<-dat[grepl("2015",dat$year),]
k16<-dat[grepl("2016",dat$year),]
k17<-dat[grepl("2017",dat$year),]
k18<-dat[grepl("2018",dat$year),]
{{dg<-ggplot(data =k15, aes(x=variable,y=diff,group=PRODUCT,color=PRODUCT))+geom_line()+
  labs(y="difference(percent)", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))+abline(h=0)
  plot15<-dg+facet_grid(k15$MEMSTATE~., scale = "free_y")+ggtitle("Discounts by Product")+ theme(plot.title = element_text(hjust = 0.5))
}
{dh<-ggplot(data =k16, aes(x=variable,y=diff,group=PRODUCT,color=PRODUCT))+geom_line()+
    labs(y="difference(percent)", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))+abline(h=0)
  plot16<-dh+facet_grid(k16$MEMSTATE~., scale = "free_y")+ggtitle("Discounts by Product")+ theme(plot.title = element_text(hjust = 0.5))
}
{di<-ggplot(data =k17, aes(x=variable,y=diff,group=PRODUCT,color=PRODUCT))+geom_line()+
    labs(y="difference(percent)", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))+abline(h=0)
  plot17<-di+facet_grid(k17$MEMSTATE~., scale = "free_y")+ggtitle("Discounts by Product")+ theme(plot.title = element_text(hjust = 0.5))
}
{dj<-ggplot(data =k18, aes(x=variable,y=diff,group=PRODUCT,color=PRODUCT))+geom_line()+
    labs(y="difference(percent)", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))+abline(h=0)
  plot18<-dj+facet_grid(k18$MEMSTATE~.)+ggtitle("Discounts by Product")+ theme(plot.title = element_text(hjust = 0.5))
}}

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
#write.xlsx(dat,'file.xlsx')
