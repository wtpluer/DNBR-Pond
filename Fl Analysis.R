# Import dataset
setwd("~/Projects/DNBR Pond/Analysis")
Fl.Data=read.csv("~/Projects/DNBR Pond/Analysis/Fl Data.txt")
colnames(Fl.Data)=c("Date","Site","Lake","Transect","Distance","Analyte","Value","Unit")
Fl.Data$Analyte=as.character(Fl.Data$Analyte)
Fl.Data$Analyte[which(substr(Fl.Data$Analyte,1,2)=="NO")]="NOx"

# Replace minimum values with detection limit/sqrt(2)
Fl.Data$Value[which(Fl.Data$Analyte=="NOx" & Fl.Data$Value<0.05)]=0.05/sqrt(2)
Fl.Data$Value[which(Fl.Data$Analyte=="Cl" & Fl.Data$Value<0.05)]=0.05/sqrt(2)
Fl.Data$Value[which(Fl.Data$Analyte=="SO4" & Fl.Data$Value<0.05)]=0.05/sqrt(2)
Fl.Data$Value[which(Fl.Data$Analyte=="TOC" & Fl.Data$Value<0.1)]=0.1/sqrt(2)
Fl.Data$Value[which(Fl.Data$Analyte=="Chla" & Fl.Data$Value<0.001)]=0.001/sqrt(2)

Fl.Data=sqldf("Select Date,Site,Lake,Transect,Distance,Analyte,sum(Value) 'Value'
      from 'Fl.Data' group by Date,Site,Lake,Transect,Distance,Analyte")

Fl.Sum=sqldf("Select Lake,Distance,Analyte,avg(Value) 'mean',sqrt(variance(Value)/count(Value)) 'se'
      from 'Fl.Data' group by Lake,Distance,Analyte")

baselines=data.frame(Site=rep(c("Riverwood","Stillwater"),5),
                     Analyte=c("Cl","Cl","NOx","NOx","TOC","TOC","SO4","SO4","Chla","Chla"),
                     Value=c(241.68,294.32,7.85,4.85,0,0,54.97,32.15,0.059,0.036),
                     Limits=c(0,300,0,8,0,20,0,60,0,0.06))

ggplot(Fl.Sum[which(Fl.Sum$Analyte!="Cl"),],aes(x=Distance, y=mean, color=Lake)) +
  theme_bw(20)+
  geom_hline(data=baselines[which(baselines$Analyte!="Cl"),],aes(yintercept=Limits)) +
  geom_hline(data=baselines[which(baselines$Analyte!="Cl"),],aes(yintercept=Value,color=Site),size=.5,lty=2) +
  scale_colour_manual(values=rep(colfun(2,T),2)) +
  geom_smooth(method = 'lm',se=FALSE,size=.5) +
  facet_wrap(~Analyte,scales="free_y") +
  geom_errorbar(width=0,aes(ymin=mean-se,ymax=mean+se)) +
  geom_point(size=3) +
  theme(legend.key=element_blank(),legend.position="bottom") +
  labs(x="Distance from Reactor (ft)",y="Analyte Concentration (mg/L)") + 
  ggtitle("Concentrations in Transects from Pond Reactors") 

ggplot(Fl.Sum[which(Fl.Sum$Analyte=="NOx"),],aes(x=Distance, y=mean)) +
  theme_bw(20)+
  geom_hline(data=baselines[which(baselines$Analyte=="NOx"),],
             aes(yintercept=Value,color=Site),size=1,lty=2) +
  geom_smooth(method = 'lm',se=FALSE,size=1,color="black") +
  geom_errorbar(width=0,aes(ymin=mean-se,ymax=mean+se)) +
  geom_point(size=5,aes(color=Lake)) +
  theme(legend.key=element_blank(),legend.position=c(0.5,0.5)) +
  labs(x="Distance from Reactor (ft)",y="NOx-N Concentration (mg/L)") + 
  ggtitle("NOx Concentration in Pond Transects") 