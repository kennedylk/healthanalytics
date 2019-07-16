
#Libraries and Data Manipulation
{library(ggplot2)
  library(reshape2)
  library(readxl)
  library(grid)
  library(gridExtra)
  library(dplyr)
  subsetData<-function(datframe){
    datadf<- melt(data,value.name = 'MEMSTATE')
    names(datadf)[9] = "percent"
    datadf<-datadf[,c(1,2,8,9)]
    df<-subset(datadf,(MEMSTATE!= 'OS'))
    df<-df[df$PRODUCT!="CL" & df$PRODUCT!="HMO" & df$PRODUCT!="EPO"& df$PRODUCT!="POS"& df$PRODUCT!="PPO" , ]
    datadf<-df[!grepl("Settlement",df$variable),]
    #datadf<-df[!grepl("Q1",df$variable),]
    datadf<-datadf[datadf$PRODUCT!="CL" & datadf$PRODUCT!="HMO" & datadf$PRODUCT!="EPO"& datadf$PRODUCT!="POS"& datadf$PRODUCT!="PPO" , ]
    datadf<-datadf[-c(1:646),]}
  
}
#Data Manipulation
{data <- read_excel("X:/Reimbursement/RFPs/2018/Discounts/Tracking/KK/Discount Project/Copy of Discount Tracking By Region KK.xlsx", 
                    range = "a3:Cn134")
  OOAdata<-read_excel("X:/Reimbursement/RFPs/2018/Discounts/Tracking/By Region/OOA Tracking.xlsx")
  datadf<-subsetData(data)
  df<-datadf
  OOA<-melt(OOAdata)}
#Product Plot
{
  dg<-ggplot(data =datadf, aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE))+
    geom_line()+scale_x_discrete(breaks=datadf$variable[seq(1, length(datadf$variable), by = 30)])+
    labs(y="Discount", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))+scale_y_continuous(labels=scales::percent)
  Prod1<-dg+facet_grid(datadf$PRODUCT~., scale = "free_y")+ggtitle("Discounts by Product")+ theme(plot.title = element_text(hjust = 0.5))
Prod1}
#State Plot
{ds<-ggplot(data =datadf, aes(x=variable,y=percent,group=PRODUCT,color=PRODUCT,shape=PRODUCT))+
    geom_line()+scale_x_discrete(breaks=datadf$variable[seq(1, length(datadf$variable), by = 30)])+
    labs(y="Discount", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  State1<-ds+facet_grid(datadf$MEMSTATE~.)+ggtitle("Discounts by State")+ theme(plot.title = element_text(hjust = 0.5))}
#Trend by State
{DS<-ggplot(datadf, aes(x=variable,y=percent,group=PRODUCT,color=PRODUCT,shape=PRODUCT))+
    scale_x_discrete(breaks=datadf$variable[seq(1, length(datadf$variable), by = 0)])+axis.text.x = element_text(angle = 40, hjust = 1)
    geom_smooth(method = "lm",se = FALSE,linetype = "longdash")+ theme(axis.text.x = element_text(angle = 40, hjust = 1))
  StateTrend<-DS+facet_grid(datadf$MEMSTATE~.)+ggtitle("Discount Trends by State")+ theme(plot.title = element_text(hjust = 0.5))

}
#Trend By Product
{G<-ggplot(datadf, aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE))+
    scale_x_discrete(breaks=datadf$variable[seq(1, length(datadf$variable), by = 50)])+
    geom_smooth(method = "lm",se = FALSE,linetype = "longdash")+ theme(axis.text.x = element_text(angle = 40, hjust = 1))
  ProdTrend<-G+facet_grid(datadf$PRODUCT~.)+ggtitle("Discount Trends by Product")+ theme(plot.title = element_text(hjust = 0.5))}
#OOA Trend by Provcat
{
  O<-ggplot(OOA, aes(x=variable,y=value,group=PRODUCT,color=PRODUCT))+coord_cartesian(expand=FALSE)+
    geom_smooth(method = "lm",se = FALSE,linetype = "longdash")+ theme(axis.text.x = element_text(angle = 40, hjust = 1))
  ooa1<-O+facet_grid(OOA$PROVCAT~.)+ggtitle("OOA Discounts by Provcat")+ theme(plot.title = element_text(hjust = 0.5))}
#OOA Trend by Product
{o2<-ggplot(OOA, aes(x=variable,y=value,group=PROVCAT,color=PROVCAT))+coord_cartesian(expand=FALSE)+
    geom_smooth(method = "lm",se = FALSE,linetype = "longdash")+ theme(axis.text.x = element_text(angle = 40, hjust = 1))
  ooa2<-o2+facet_grid(OOA$PRODUCT~.)+ggtitle("OOA Discounts by Product")+ theme(plot.title = element_text(hjust = 0.5))
}
#OOA by Provcat
{
  e<-ggplot(OOA, aes(x=variable,y=value,group=PRODUCT,color=PRODUCT))+coord_cartesian(expand=FALSE)+
    geom_line()+scale_y_continuous(labels=scales::percent)+
    labs(y="Discount", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  ooa3<-e+facet_grid(OOA$PROVCAT~.)+ggtitle("Discounts by Provcat")+ theme(plot.title = element_text(hjust = 0.5))
}
#OOA by Product
{
  d<-ggplot(OOA, aes(x=variable,y=value,group=PROVCAT,color=PROVCAT))+coord_cartesian(expand=FALSE)+
    geom_line()+scale_y_continuous(labels=scales::percent)+
    labs(y="Discount", x = "Months") + theme(axis.text.x = element_text(angle = 40, hjust = 1))
  ooa4<-d+facet_grid(OOA$PRODUCT~.)+ggtitle("OOA Discounts by Product")+ theme(plot.title = element_text(hjust = 0.5))
}
#CL
{CLdf<-subset(df,(PRODUCT=='CL TOTAL'))
  
cl1<-ggplot(data =CLdf, aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE))+
  geom_line()+ggtitle("CL by Region")+
  labs(y="Discount (Percentage)", x = "Months") +
  scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))
cl2<- ggplot(CLdf)+ggtitle("CL Summary")+labs(y="Discount (Percentage)", x = "Months") +
  scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 30)])+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  geom_smooth( aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE),se = FALSE, linetype = "longdash")+theme(plot.title = element_text(hjust = 0.5))
cl<-grid.arrange(cl1,cl2,ncol = 1)
}
#EPO
{EPOd<-subset(df,(PRODUCT=='EPO Total'))
EPOdf<-EPOd[!grepl("2015",EPOd$variable),]
epo1<-  ggplot(data =EPOdf, aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE))+
    geom_line()+ggtitle("EPO by Region")+
    labs(y="Discount", x = "Months") + 
  scale_x_discrete(breaks=EPOdf$variable[seq(1, length(EPOdf$variable), by = 6)])+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    theme(plot.title = element_text(hjust = 0.5))+scale_y_continuous(labels=scales::percent)
epo2<- ggplot(EPOdf)+ggtitle("EPO Summary")+labs(y="Discount (Percentage)", x = "Months") +
  scale_x_discrete(breaks=EPOdf$variable[seq(1, length(EPOdf$variable), by = 6)])+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+
  geom_smooth( aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE),se = FALSE, linetype = "longdash") +theme(plot.title = element_text(hjust = 0.5))

epo<-grid.arrange(epo1,epo2,ncol = 1)
}
#HMO
{HMOdf<-subset(df,(PRODUCT=='HMO Total'))

hmo1 <- ggplot(data =HMOdf, aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE))+
  geom_line()+ggtitle("HMO by Region")+labs(y="Discount (Percentage)", x = "Months") +
  scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
  theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
  theme(plot.title = element_text(hjust = 0.5))
hmo2<-ggplot(HMOdf)+ggtitle("HMO Summary")+labs(y="Discount (Percentage)", x = "Months") +
  scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
  theme(axis.text.x = element_text(angle = 40, hjust = 1))+geom_smooth(aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE),se = FALSE, linetype = "longdash")+theme(plot.title = element_text(hjust = 0.5))

hmo<-grid.arrange(hmo1,hmo2,ncol = 1)
}
#POS
{POSdf<-subset(df,(PRODUCT=='POS Total'))
    pos1 <- ggplot(data =POSdf, aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE))+
    geom_line()+ggtitle("POS by Region")+
      labs(y="Discount (Percentage)", x = "Months") +
      scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
      theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    theme(plot.title = element_text(hjust = 0.5))
    pos2<- ggplot(POSdf)+ggtitle("POS Summary")+labs(y="Discount (Percentage)", x = "Months") +
      scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
      theme(axis.text.x = element_text(angle = 40, hjust = 1))+
      geom_smooth( aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE),se = FALSE, linetype = "longdash") +theme(plot.title = element_text(hjust = 0.5))
  
     pos<-grid.arrange(pos1,pos2,ncol = 1)
    }
#PPO
{PPOdf<-subset(df,(PRODUCT=='PPO Total'))
  ppo1 <- ggplot(data =PPOdf, aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE))+
    geom_line()+ggtitle("PPO by Region")+
    labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    theme(plot.title = element_text(hjust = 0.5))
  ppo2<- ggplot(PPOdf)+ggtitle("PPO Summary")+labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_smooth( aes(x=variable,y=percent,group=MEMSTATE,color=MEMSTATE,shape=MEMSTATE),se = FALSE, linetype = "longdash") +theme(plot.title = element_text(hjust = 0.5))

   ppo<-grid.arrange(ppo1,ppo2,ncol = 1)
  }
#MA
{MAdf<-subset(df,(MEMSTATE=='MA'))
  ma1 <-ggplot(data =MAdf, aes(x=variable,y=percent,group=PRODUCT,color=PRODUCT,shape=PRODUCT))+
    geom_line()+ggtitle("MA by Product")+
    labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    theme(plot.title = element_text(hjust = 0.5))
  ma2<- ggplot(MAdf)+ggtitle("MA Summary")+labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_smooth( aes(x=variable,y=percent,group=PRODUCT,color=PRODUCT,shape=PRODUCT),se = FALSE, linetype = "longdash")+theme(plot.title = element_text(hjust = 0.5))
  ma<-grid.arrange(ma1,ma2,ncol = 1)
}
#RI
{RIdf<-subset(datadf,(MEMSTATE=='RI'))
  ri1 <- ggplot(data =RIdf, aes(x=variable,y=percent,group=PRODUCT,color=PRODUCT,shape=PRODUCT))+
    geom_line()+ggtitle("RI by Product")+
    labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=RIdf$variable[seq(1, length(RIdf$variable), by = 6)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+theme(plot.title = element_text(hjust = 0.5))
  ri2<- ggplot(RIdf)+ggtitle("RI Summary")+labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=RIdf$variable[seq(1, length(RIdf$variable), by = 6)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_smooth( aes(x=variable,y=percent,group=PRODUCT,color=PRODUCT,shape=PRODUCT),se = FALSE, linetype = "longdash")+theme(plot.title = element_text(hjust = 0.5))
  ri<-grid.arrange(ri1,ri2,ncol = 1)
}
#NH
{NHdf<-subset(df,(MEMSTATE=='NH'))
  nh1 <- ggplot(data =NHdf, aes(x=variable,y=percent,group=PRODUCT,color=PRODUCT,shape=PRODUCT))+
    geom_line()+ggtitle("NH by Product")+
    labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+theme(plot.title = element_text(hjust = 0.5))
  nh2<- ggplot(NHdf)+ggtitle("NH Summary")+labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=CLdf$variable[seq(1, length(CLdf$variable), by = 6)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_smooth( aes(x=variable,y=percent,group=PRODUCT,color=PRODUCT,shape=PRODUCT),se = FALSE, linetype = "longdash")+theme(plot.title = element_text(hjust = 0.5))
 nh<-grid.arrange(nh1,nh2,ncol = 1)
}
#rawdata
{
    dat<-melt(data)
    dat<-dat[,c(1,2,8,9)]
    dat<-dat[dat$PRODUCT!="CL" & dat$PRODUCT!="HMO" & dat$PRODUCT!="EPO"& dat$PRODUCT!="POS"& dat$PRODUCT!="PPO" , ]
dat<-dat[-c(1:1200),]
    dat1 <- ggplot(data= dat, aes(x=variable,y=value,group=PRODUCT,color=PRODUCT))+
    geom_line()+
    ggtitle("Raw Data")+
    labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=dat$variable[seq(1, length(dat$variable), by = 70)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+theme(plot.title = element_text(hjust = 0.5))
  dat2<- ggplot(dat)+ggtitle("Raw Data Summary")+labs(y="Discount (Percentage)", x = "Months") +
    scale_x_discrete(breaks=dat$variable[seq(1, length(dat$variable), by = 70)])+
    theme(axis.text.x = element_text(angle = 40, hjust = 1))+
    geom_smooth( aes(x=variable,y=value,group=PRODUCT,color=PRODUCT,shape=PRODUCT),se = FALSE, linetype = "longdash")+theme(plot.title = element_text(hjust = 0.5))
  dat3<-grid.arrange(dat1,dat2,ncol = 1)
}
