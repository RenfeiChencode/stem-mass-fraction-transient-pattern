setwd("C:\\Users\\lenovo\\Desktop\\Figures")
library(ggplot2)
library(gridExtra)
library(basicTrendline)
library(lmodel2)
library(scales)
library(tseries)# test the stability of time series
library(grid)
library(tseries)# test the stability of time series
library(sqldf);library(dplyr);library(patchwork)
######################################################################
#calculating carrying capacity through constant final yield
pcorn=ggplot(data=df,aes(x=Dense,y=wholemass,group=class,color=class))+scale_colour_gradientn(colours = hcl.colors(length(1:6)))+geom_point(size=3)+ 
  geom_smooth(method = "nls", se = FALSE, method.args = list(formula= y~x*W/(1+a*x), start = list(W=6000/2.25,a=1),trace = T))+
  labs(title="Corn 2011",x=expression("Density (no./" ~ m^{2}~")"),y=expression("Whole biomass (g/" ~ m^{2}~")"))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.1,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0.0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.7, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,900,300),limits=c(0,900))+
  scale_y_continuous(breaks=seq(0,4000,1000),limits=c(0,4000))

pwheat=ggplot(data=df,aes(x=Dense,y=wholemass,group=class,color=class))+scale_colour_gradientn(colours = hcl.colors(length(1:5)))+geom_point(size=3)+ 
  geom_smooth(method = "nls", se = FALSE, method.args = list(formula= y~x*W/(1+a*x), start = list(W=600/2.25,a=1),trace = T))+
  labs(title="Wheat 2011",x=expression("Density (no./" ~ m^{2}~")"),y=expression("Whole biomass (g/" ~ m^{2}~")"))+#theme(plot.title = element_text(hjust = 0.5))
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.15,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0.0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.7, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,800,200),limits=c(0,800))+
  scale_y_continuous(breaks=seq(0,600,200),limits=c(0,600))

pflax=ggplot(data=df,aes(x=Dense,y=wholemass,group=class,color=class))+scale_colour_gradientn(colours = hcl.colors(length(1:6)))+geom_point(size=3)+ 
  geom_smooth(method = "nls", se = FALSE, method.args = list(formula= y~x*W/(1+a*x), start = list(W=600/2.25,a=1),trace = T))+
  labs(title="Flax 2011",x=expression("Density (no./" ~ m^{2}~")"),y=expression("Whole biomass (g/" ~ m^{2}~")"))+#theme(plot.title = element_text(hjust = 0.5))
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.15,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0.0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.7, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,4000,1000),limits=c(0,4000))+
  scale_y_continuous(breaks=seq(0,1500,500),limits=c(0,1500))

KTcorn=c(0.3944298/ 0.0001042902,2.697651/ 0.0005533997,4.724898 /0.0009294475,6.715837 /0.001197204,30.41216 /0.008298492,63.61039/ 0.01184737)
KTwheat=c(0.7351187 /0.002737025,0.8173058 /0.000125521,1.49474/ 0.0008362794,0.7530507 /0.0004692125,1.045022/ 0.0004251083)
KTflax=c(0.02541527/ 3.662604e-06,0.6095139 /0.001206913,1.229125/ 0.000765852,1.09179 /0.0007150112,1.810612 /0.0006060676,1.947565/ 0.0006021042)


######################################################################
#study variation of stem mass fraction in response to biotic factors.
#study transient variation during plant ontogeny.
#flax wheat mixture
together1=together2=0;MLT=matrix(nrow=5,ncol=18);GAO=matrix(nrow=5,ncol=18);MIDUwheat=matrix(nrow=5,ncol=18);MIDUflax=matrix(nrow=5,ncol=18)
together3=together4=together5=together6=together7=together8=together9=NA
for (i in 1:5){
  #body size corrected carring capacity
  Kflax=KTflax[i]/Mtotalflax
  Kwheat=KTwheat[i]/Mtotalwheat
  q1=(Kflax-denseflax)*Kwheat/(densewheat*Kflax)
  q2=(Kwheat-densewheat)*Kflax/(denseflax*Kwheat)
  together1=together1+q1
  together2=together2+q2
  
  together3=c(together3,df$stem);together4=c(together4,df$total);together5=c(together5,df$stemfraction);together6=c(together6,df$densityflaxwheat);  
  together7=c(together7,df$id1);together8=c(together8,df$id2);together9=c(together9,df$heightflaxwheat)
  result=lmodel2(log(df$stem)~log(df$total)) #for each timepoint
  print(result)
}
mlt=as.data.frame(MLT)
gao=as.data.frame(GAO)
miduwheat=as.data.frame(MIDUwheat)
miduflax=as.data.frame(MIDUflax)
df2=data.frame(stem=together3,total=together4,stemfraction=together5,height=together9,density=together6,id1=together7,id2=together8)
stemflaxwheat=df2$stem;totalflaxwheat=df2$total;classflaxwheat=length(df2$total)
df2=df2[complete.cases(df2),]

lmodel2(log(df2$stem)~log(df2$total)) #for pooled data

competition4=exponent4=CVST4=MMEANH=MMEANTO=0;j=1
for (i in 1:length(position)){
  dfsub=subset(df2,df2$id1==position[i]);
  total=dfsub$total;stem=dfsub$stem;density=dfsub$density;stemfraction=dfsub$stemfraction;height=dfsub$height
  competition4[j]=mean(density)
  result=lmodel2(log(stem)~log(total))
  exponent4[j]=result$regression.results[1,3]
  CVST4[j]=sd(stemfraction,na.rm=T)/mean(stemfraction,na.rm=T) 
  MMEANH[j]=mean(height,na.rm=T) 
  MMEANTO[j]=mean(total,na.rm=T)
  j=j+1} 

ontogeny=stemfraction=exponent=comp=Height=Ddenwheat=Ddenflax=0
for (i in 1:length(mlt[1,])){
  ontogeny[((i-1)*5+1):((i-1)*5+5)]=c(30,40,50,60,70)
  stemfraction[((i-1)*5+1):((i-1)*5+5)]=mlt[,i]
  Height[((i-1)*5+1):((i-1)*5+5)]=gao[,i]
  Ddenwheat[((i-1)*5+1):((i-1)*5+5)]=miduwheat[,i]
  Ddenflax[((i-1)*5+1):((i-1)*5+5)]=miduflax[,i]
  exponent[((i-1)*5+1):((i-1)*5+5)]=rep(exponent4[i],5)
  comp[((i-1)*5+1):((i-1)*5+5)]=rep(competition4[i],5)
}
df=data.frame(ontogeny=ontogeny,stemfraction=stemfraction,Height=Height,exponent=exponent,density=comp/2.25,Ddenwheat=Ddenwheat,Ddenflax=Ddenflax)
# effect of plant ontogeny on stem mass fraction
fig4A=ggplot(data = df,aes(x=ontogeny,y=stemfraction,group=density,color=density))+
  scale_colour_gradientn(breaks=c(70,360,650),limits=c(70,650),colours = hcl.colors(length(density)))+geom_point(shape=NA)+geom_path()+
  labs(title="Flax wheat mixture",x="Plant ontogeny (days)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.75,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(30,70,10),limits=c(30,70))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1),10^(-0.0))
  )

# effect of plant height on stem mass fraction
figadd5A=ggplot(data = df,aes(x=Height,y=stemfraction,group=density,color=density))+
  scale_colour_gradientn(breaks=c(70,360,650),limits=c(70,650),colours = hcl.colors(length(density)))+geom_point(shape=NA)+geom_line()+
  labs(title="Flax wheat mixture",x="Plant height (cm)",y="Mean stem mass fraction",color="Density")+ 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.75,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,50,10),limits=c(0,50))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1),10^(-0.0))
  )


class=c(rep("Wheat",length(df$ontogeny)),rep("Flax",length(df$ontogeny)))
TIME=rep(df$ontogeny,2)
dn=c(df$Ddenwheat,df$Ddenflax)
subforDN=data.frame(class=class,TIME=TIME,dn=dn)
# variation of population density during plant ontogeny
figadd6A=ggplot(data = subforDN,aes(x=TIME,y=dn,group=class,color=class))+geom_point(size=3)+
  labs(title="Flax wheat mixture",x="Plant ontogeny (days)",y=expression("Density (no./" ~ m^{2}~")"),color="")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.75,0.7), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(30,70,10),limits=c(30,70))+
  scale_y_continuous(breaks=seq(0,4000,1000),limits=c(0,4000))


q1=together1/5;q2=together2/5;asymmetryC=abs(q1-q2) 
df=data.frame(exponent4=round(exponent4,1),CVST4=round(CVST4,1),asymmetryC=round(log(asymmetryC),0))
Flaxwheat=ggplot(df,aes(x=exponent4,y=asymmetryC,fill=CVST4))+scale_fill_gradient(breaks=seq(0.5,1.1,0.2),limits=c(0.5,1.1),low="blue",high="red")+geom_tile()

class=c(rep("q1",length(q1)),rep("q2",length(q2)))
qq=c(q1,q2);CVST=c(CVST4,CVST4)
df=data.frame(class=class,qq=qq,CVST=CVST)
#effect of plant interaction on the coefficient of variation of stem mass fraction
fig5A=ggplot(data = df,aes(x=qq,y=CVST,group=class,color=class))+geom_point(size=5)+geom_smooth(method="lm",span=1,size=3)+
  labs(title="Flax wheat mixture",colour="",x="Plant interaction",y="CVST")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.8,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(-105,195,100),limits=c(-105,195))+
  scale_y_continuous(breaks=seq(0,2.5,0.5),limits=c(0.0,2.5))

df=data.frame(MMEANH=MMEANH,CVST4=CVST4,MMEANTO=MMEANTO)
figadd7A=ggplot(data = df,aes(x=MMEANH,y=CVST4))+geom_point(size=5)+geom_smooth(method="lm",span=1,size=3)+
  labs(title="Flax wheat mixture",colour="",x="Mean height (cm)",y="CVST")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.9,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(20,35,5),limits=c(20,35))+
  scale_y_continuous(breaks=seq(0,2,0.5),limits=c(0,2))
fit=lm(CVST4~MMEANH,data = df)
summary(fit)

figadd8A=ggplot(data = df,aes(x=log10(MMEANTO),y=CVST4))+geom_point(size=3)+geom_smooth(method="lm",span=1)+
  labs(title="Flax wheat mixture",colour="",x="Log mean total biomass (g)",y="CVST")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.9,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(-1,2,1),limits=c(-1,2))+
  scale_y_continuous(breaks=seq(0,2,0.5),limits=c(0,2))
fit=lm(CVST4~log10(MMEANTO),data = df)
summary(fit)
###########################################################################################################################################################
#corn wheat mixture
together1=together2=0;MLT=matrix(nrow=5,ncol=8);GAO=matrix(nrow=5,ncol=8);MIDUwheat=matrix(nrow=5,ncol=8);MIDUcorn=matrix(nrow=5,ncol=8)
together3=together4=together5=together6=together7=together8=together9=NA
for (i in 1:5){
  #生物量修正的carring capacity
  Kcorn=KTcorn[i]/Mtotalcorn
  Kwheat=KTwheat[i]/Mtotalwheat
  q1=(Kcorn-densecorn)*Kwheat/(densewheat*Kcorn)
  q2=(Kwheat-densewheat)*Kcorn/(densecorn*Kwheat)
  together1=together1+q1
  together2=together2+q2
  
  together3=c(together3,df$stem);together4=c(together4,df$total);together5=c(together5,df$stemfraction);together6=c(together6,df$densitycornwheat);  
  together7=c(together7,df$id1);together8=c(together8,df$id2);together9=c(together9,df$heightcornwheat)
}
mlt=as.data.frame(MLT)
gao=as.data.frame(GAO)
miduwheat=as.data.frame(MIDUwheat)
miducorn=as.data.frame(MIDUcorn)
df2=data.frame(stem=together3,total=together4,stemfraction=together5,height=together9,density=together6,id1=together7,id2=together8)
stemcornwheat=df2$stem;totalcornwheat=df2$total;classcornwheat=length(df2$total)
df2=df2[complete.cases(df2),]
competition4=exponent4=CVST4=MMEANH=MMEANTO=0;j=1
for (i in 1:length(position)){ 
  dfsub=subset(df2,df2$id1==position[i]);
  total=dfsub$total;stem=dfsub$stem;density=dfsub$density;stemfraction=dfsub$stemfraction;height=dfsub$height
  
  competition4[j]=mean(density)
  result=lmodel2(log(stem)~log(total))
  exponent4[j]=result$regression.results[1,3]
  CVST4[j]=sd(stemfraction,na.rm=T)/mean(stemfraction,na.rm=T)
  MMEANH[j]=mean(height,na.rm=T) 
  MMEANTO[j]=mean(total,na.rm=T)
  j=j+1
  #  }
} 

ontogeny=stemfraction=exponent=comp=Height=Ddenwheat=Ddencorn=0
for (i in 1:length(mlt[1,])){
  ontogeny[((i-1)*5+1):((i-1)*5+5)]=c(30,40,50,60,70)
  stemfraction[((i-1)*5+1):((i-1)*5+5)]=mlt[,i]
  Height[((i-1)*5+1):((i-1)*5+5)]=gao[,i]
  Ddenwheat[((i-1)*5+1):((i-1)*5+5)]=miduwheat[,i]
  Ddencorn[((i-1)*5+1):((i-1)*5+5)]=miducorn[,i]
  exponent[((i-1)*5+1):((i-1)*5+5)]=rep(exponent4[i],5)
  comp[((i-1)*5+1):((i-1)*5+5)]=rep(competition4[i],5)
}
df=data.frame(ontogeny=ontogeny,stemfraction=stemfraction,Height=Height,exponent=exponent,density=comp/2.25,Ddenwheat=Ddenwheat,Ddencorn=Ddencorn)
fig4B=ggplot(data = df,aes(x=ontogeny,y=stemfraction,group=density,color=density))+
  scale_colour_gradientn(breaks=c(70,400,870),limits=c(70,870),colours = hcl.colors(length(density)))+geom_point(shape=NA)+geom_path()+
  labs(title="Corn wheat mixture",x="Plant ontogeny (days)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.7,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.3, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(30,70,10),limits=c(30,70))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1),10^(-0.0)))


figadd5B=ggplot(data = df,aes(x=Height,y=stemfraction,group=density,color=density))+
  scale_colour_gradientn(breaks=c(70,400,870),limits=c(70,870),colours = hcl.colors(length(density)))+geom_point(shape=NA)+geom_line()+
  labs(title="Corn wheat mixture",x="Plant height (cm)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.7,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.3, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,50,10),limits=c(0,50))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1),10^(-0.0)))

class=c(rep("Wheat",length(df$ontogeny)),rep("Corn",length(df$ontogeny)))
TIME=rep(df$ontogeny,2)
dn=c(df$Ddenwheat,df$Ddencorn)
subforDN=data.frame(class=class,TIME=TIME,dn=dn)
figadd6B=ggplot(data = subforDN,aes(x=TIME,y=dn,group=class,color=class))+geom_point(size=3)+
  labs(title="Corn wheat mixture",x="Plant ontogeny (days)",y=expression("Density (no./" ~ m^{2}~")"),color="")+ 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.7,0.7), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.3, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(30,70,10),limits=c(30,70))+
  scale_y_continuous(breaks=seq(0,6000,2000),limits=c(0,6000))


q1=together1/5;q2=together2/5;asymmetryC=abs(q1-q2) 
df=data.frame(exponent4=round(exponent4,1),CVST4=round(CVST4,1),asymmetryC=round(log(asymmetryC),0))
cornwheat=ggplot(df,aes(x=exponent4,y=asymmetryC,fill=CVST4))+scale_fill_gradient(low="blue",high="red")+geom_tile()+
  labs(fill="CVST",title="Corn wheat mixture",x="Scaling exponent",y="Relative interaction")+theme(plot.title = element_text(hjust = 0.5))
class=c(rep("q1",length(q1)),rep("q2",length(q2)))
qq=c(q1,q2);CVST=c(CVST4,CVST4)
df=data.frame(class=class,qq=qq,CVST=CVST)
fig5B=ggplot(data = df,aes(x=qq,y=CVST,group=class,color=class))+geom_point(size=5)+geom_smooth(method="lm",span=1,size=3)+
  labs(title="Corn wheat mixture",colour="",x="Plant interaction",y="CVST")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.8,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,45,15),limits=c(0,45))+
  scale_y_continuous(breaks=seq(0.0,2.5,0.5),limits=c(0.0,2.5))


df=data.frame(MMEANH=MMEANH,CVST4=CVST4,MMEANTO=MMEANTO)
figadd7B=ggplot(data = df,aes(x=MMEANH,y=CVST4))+geom_point(size=5)+geom_smooth(method="lm",span=1,size=3)+
  labs(title="Corn wheat mixture",colour="",x="Mean height (cm)",y="CVST")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.9,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(20,35,5),limits=c(20,35))+
  scale_y_continuous(breaks=seq(0.3,1.5,0.4),limits=c(0.3,1.5))
fit=lm(CVST4~MMEANH,data = df)
summary(fit)


figadd8B=ggplot(data = df,aes(x=MMEANTO,y=CVST4))+geom_point(size=3)+geom_smooth(method="lm",span=1)+
  labs(title="Corn wheat mixture",colour="",x="Mean total biomass (g)",y="CVST")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.9,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,2,0.5),limits=c(0,2))+
  scale_y_continuous(breaks=seq(0.3,1.5,0.4),limits=c(0.3,1.5))
fit=lm(CVST4~MMEANTO,data = df)
summary(fit)
adf.test(CVST)
deCVST=diff(CVST)
adf.test(deCVST)
dedf=data.frame(decompetition=qq[-1],deCVST=deCVST,declass=class[-1])

figS1A=ggplot(data = dedf,aes(x=decompetition,y=deCVST,group=declass,color=declass))+geom_point(size=3)+geom_smooth(method="lm")+
  labs(title="Corn wheat mixture",colour="",x="Plant interaction",y="Detrended CVST")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.8,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,45,15),limits=c(0,45))+
  scale_y_continuous(breaks=seq(-1,1,by=1),limits=c(-1,1))


#mixture flax corn
together1=together2=0;MLT=matrix(nrow=5,ncol=15);GAO=matrix(nrow=5,ncol=15);MIDUcorn=matrix(nrow=5,ncol=15);MIDUflax=matrix(nrow=5,ncol=15)
together3=together4=together5=together6=together7=together8=together9=NA
for (i in 1:5){
   #生物量修正的carring capacity
  Kcorn=KTcorn[i]/Mtotalcorn
  Kflax=KTflax[i]/Mtotalflax
  q1=(Kcorn-densecorn)*Kflax/(denseflax*Kcorn)
  q2=(Kflax-denseflax)*Kcorn/(densecorn*Kflax)
  together1=together1+q1
  together2=together2+q2
  
  together3=c(together3,df$stem);together4=c(together4,df$total);together5=c(together5,df$stemfraction);together6=c(together6,df$densityflaxcorn);  
  together7=c(together7,df$id1);together8=c(together8,df$id2);together9=c(together9,df$heightflaxcorn)
  
}
mlt=as.data.frame(MLT)
gao=as.data.frame(GAO)
miducorn=as.data.frame(MIDUcorn)
miduflax=as.data.frame(MIDUflax)
df2=data.frame(stem=together3,total=together4,stemfraction=together5,height=together9,density=together6,id1=together7,id2=together8)
stemflaxcorn=df2$stem;totalflaxcorn=df2$total;classflaxcorn=length(df2$total)
df2=df2[complete.cases(df2),]
competition4=exponent4=CVST4=MMEANH=MMEANTO=0;j=1
for (i in 1:length(position)){ 
  dfsub=subset(df2,df2$id1==position[i]);
  total=dfsub$total;stem=dfsub$stem;density=dfsub$density;stemfraction=dfsub$stemfraction;height=dfsub$height
  
  competition4[j]=mean(density)
  result=lmodel2(log(stem)~log(total))
  exponent4[j]=result$regression.results[1,3]
  CVST4[j]=sd(stemfraction,na.rm=T)/mean(stemfraction,na.rm=T) 
  MMEANH[j]=mean(height,na.rm=T)
  MMEANTO[j]=mean(total,na.rm=T)
  j=j+1
} 
ontogeny=stemfraction=exponent=comp=Height=Ddencorn=Ddenflax=0
for (i in 1:length(mlt[1,])){
  ontogeny[((i-1)*5+1):((i-1)*5+5)]=c(30,40,50,60,70)
  stemfraction[((i-1)*5+1):((i-1)*5+5)]=mlt[,i]
  Height[((i-1)*5+1):((i-1)*5+5)]=gao[,i]
  Ddencorn[((i-1)*5+1):((i-1)*5+5)]=miducorn[,i]
  Ddenflax[((i-1)*5+1):((i-1)*5+5)]=miduflax[,i]
  exponent[((i-1)*5+1):((i-1)*5+5)]=rep(exponent4[i],5)
  comp[((i-1)*5+1):((i-1)*5+5)]=rep(competition4[i],5)
}
df=data.frame(ontogeny=ontogeny,stemfraction=stemfraction,Height=Height,exponent=exponent,density=comp/2.25,Ddencorn=Ddencorn,Ddenflax=Ddenflax)
fig4C=ggplot(data = df,aes(x=ontogeny,y=stemfraction,group=density,color=density))+
  scale_colour_gradientn(breaks=c(150,500,1150),limits=c(150,1150),colours = hcl.colors(length(density)))+geom_point(shape=NA)+geom_path()+
  labs(title="Corn flax mixture",x="Plant ontogeny (days)",y="Mean stem mass fraction",color="Density")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.72,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.3, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(30,70,10),limits=c(30,70))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1),10^(-0.0)))

figadd5C=ggplot(data = df,aes(x=Height,y=stemfraction,group=density,color=density))+
  scale_colour_gradientn(breaks=c(150,500,1150),limits=c(150,1150),colours = hcl.colors(length(density)))+geom_point(shape=NA)+geom_path()+
  labs(title="Corn flax mixture",x="Plant height (cm)",y="Mean stem mass fraction",color="Density")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.72,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.3, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,60,20),limits=c(0,60))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1),10^(-0.0)))

class=c(rep("Corn",length(df$ontogeny)),rep("Flax",length(df$ontogeny)))
TIME=rep(df$ontogeny,2)
dn=c(df$Ddencorn,df$Ddenflax)
subforDN=data.frame(class=class,TIME=TIME,dn=dn)
figadd6C=ggplot(data = subforDN,aes(x=TIME,y=dn,group=class,color=class))+geom_point(size=3)+
  labs(title="Corn flax mixture",x="Plant ontogeny (days)",y=expression("Density (no./" ~ m^{2}~")"),color="")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.75,0.7), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(30,70,10),limits=c(30,70))+
  scale_y_continuous(breaks=seq(0,6000,2000),limits=c(0,6000))


q1=together1/5;q2=together2/5;asymmetryC=abs(q1-q2) 
df=data.frame(exponent4=round(exponent4,2),CVST4=round(CVST4,1),asymmetryC=round(log(asymmetryC),0))
flaxcorn=ggplot(df,aes(x=exponent4,y=asymmetryC,fill=CVST4))+scale_fill_gradient(low="blue",high="red")+geom_tile()

class=c(rep("q1",length(q1)),rep("q2",length(q2)))
qq=c(q1,q2);CVST=c(CVST4,CVST4)
df=data.frame(class=class,qq=qq,CVST=CVST)
fig5C=ggplot(data = df,aes(x=qq,y=CVST,group=class,color=class))+geom_point(size=5)+geom_smooth(method="lm",span=1,size=3)+
  labs(title="Corn flax mixture",colour="",x="Plant interaction",y="CVST")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.8,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,90,30),limits=c(0,90))+
  scale_y_continuous(breaks=seq(0.0,2.5,0.5),limits=c(0.0,2.5))


df=data.frame(MMEANH=MMEANH,CVST4=CVST4,MMEANTO=MMEANTO)
figadd7C=ggplot(data = df,aes(x=MMEANH,y=CVST4))+geom_point(size=5)+geom_smooth(method="lm",span=1,size=3)+
  labs(title="Corn flax mixture",colour="",x="Mean height (cm)",y="CVST")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.9,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(20,35,5),limits=c(20,35))+
  scale_y_continuous(breaks=seq(0,2.5,0.5),limits=c(0,2.5))

fit=lm(CVST4~MMEANH,data = df)
summary(fit)

figadd8C=ggplot(data = df,aes(x=MMEANTO,y=CVST4))+geom_point(size=3)+geom_smooth(method="lm",span=1)+
  labs(title="Corn flax mixture",colour="",x="Mean total biomass (g)",y="CVST")+theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.position = c(0.9,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1.2,'line'), # space between legend text
        legend.key = element_blank(),
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,2,0.5),limits=c(0,2))+
  scale_y_continuous(breaks=seq(0,2.5,0.5),limits=c(0,2.5))

fit=lm(CVST4~MMEANTO,data = df);summary(fit)
adf.test(CVST)
deCVST=diff(CVST)
dedf=data.frame(decompetition=qq[-1],deCVST=deCVST,declass=class[-1])

figS1B=ggplot(data = dedf,aes(x=decompetition,y=deCVST,group=declass,color=declass))+geom_point(size=3)+geom_smooth(method="lm")+
  labs(title="Corn flax mixture",colour="",x="Plant interaction",y="Detrended CVST")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.8,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,90,30),limits=c(0,90))+
  scale_y_continuous(breaks=seq(-1.5,1.5,by=1),limits=c(-1.5,1.5))


#basic characteristic of the data set
stem=c(stemflaxwheat,stemcornwheat,stemflaxcorn);total=c(totalflaxwheat,totalcornwheat,totalflaxcorn);class=c(rep("Flax wheat",classflaxwheat),rep("Corn wheat",classcornwheat),rep("Flax corn",classflaxcorn))
df=data.frame(stem=stem,total=total,class=class)
figA=ggplot(data = df,aes(x=total,y=stem,group=class,color=class))+geom_point(size=3)+geom_smooth(method="lm",span=1)+labs(title="2011 mixture",colour="",x="Total biomass (g)",y="Stem biomass (g)")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.8,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.8, b=0.3, l=0.12, "cm") )+
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-3),10^(3)) )+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-4),10^(2)) )

result=lmodel2(log(stem)~log(total))

#analysis of one species systems in 2015 and 2016
#2015 corn or soybean
hddcorn1516=c(corn2015$hdd2,corn2015$hdd3,corn2015$hdd4,corn2015$hdd5)
stemcorn1516=c(corn2015$stem2,corn2015$stem3,corn2015$stem4,corn2015$stem5)
heightcorn1516=c(corn2015$height2,corn2015$height3,corn2015$height4,corn2015$height5)
hddsoybean1516=c(soybean2015$hdd1,soybean2015$hdd2,soybean2015$hdd3,soybean2015$hdd4,soybean2015$hdd5)
stemsoybean1516=c(soybean2015$stem1,soybean2015$stem2,soybean2015$stem3,soybean2015$stem4,soybean2015$stem5)
heightsoybean1516=c(soybean2015$height1,soybean2015$height2,soybean2015$height3,soybean2015$height4,soybean2015$height5)
HDDcorn=log(hddcorn1516);        HDDsoybean=log(hddsoybean1516)
Totalcorn1516=exp(0.74*HDDcorn-0.29);Totalsoybean1516=exp(HDDsoybean-0.7) 
IDcorn1516=c(corn2015$ID2,corn2015$ID3,corn2015$ID4,corn2015$ID5)
IDsoybean1516=c(soybean2015$ID1,soybean2015$ID2,soybean2015$ID3,soybean2015$ID4,soybean2015$ID5)
densitycorn1516=c(corn2015$density2,corn2015$density3,corn2015$density4,corn2015$density5)
densitysoybean1516=c(soybean2015$density1,soybean2015$density2,soybean2015$density3,soybean2015$density4,soybean2015$density5)
densitycorn1516=densitycorn1516/2.25
densitysoybean1516=densitysoybean1516/2.25
repeatcorn1516=c(corn2015$repeat2,corn2015$repeat3,corn2015$repeat4,corn2015$repeat5)
repeatsoybean1516=c(soybean2015$repeat1,soybean2015$repeat2,soybean2015$repeat3,soybean2015$repeat4,soybean2015$repeat5)

height=heightcorn1516;stem=stemcorn1516;total=Totalcorn1516;ID=IDcorn1516;repe=repeatcorn1516;dense=densitycorn1516
stemcorn=stem;totalcorn=total;classcorn=length(total)
df=data.frame(height=height,stem=stem,total=total,ID=ID,repe=repe,dense=dense)
gradient1=unique(df$ID); exponent7=CVST7=competition7=height7=total7=0;j=1
for (i in 1:length(gradient1)){
  dfsub1=subset(df,df$ID==gradient1[i]);dfsub1=dfsub1[complete.cases(dfsub1),];
  for (ii in 1:4){
    dfsub=subset(dfsub1,dfsub1$repe==ii);dfsub=dfsub[complete.cases(dfsub),];
    total=dfsub$total;stem=dfsub$stem;density=dfsub$dense;height=dfsub$height
    if (length(stem)>5){
      competition7[j]=mean(density)
      height7[j]=mean(height)
      total7[j]=mean(total)
      result=lmodel2(log(stem)~log(total))
      exponent7[j]=result$regression.results[1,3]
      stemfraction=stem/total
      CVST7[j]=sd(stemfraction,na.rm=T)/mean(stemfraction,na.rm=T) 
      j=j+1
    }
  }
}
species7="corn"

height=heightsoybean1516;stem=stemsoybean1516;total=Totalsoybean1516;ID=IDsoybean1516;repe=repeatsoybean1516;dense=densitysoybean1516
stemsoybean=stem;totalsoybean=total;classsoybean=length(total)
df=data.frame(height=height,stem=stem,total=total,ID=ID,repe=repe,dense=dense)
gradient1=unique(df$ID); exponent8=CVST8=competition8=height8=total8=0;j=1
for (i in 1:length(gradient1)){
  dfsub1=subset(df,df$ID==gradient1[i]);dfsub1=dfsub1[complete.cases(dfsub1),];
  for (ii in 1:4){
    dfsub=subset(dfsub1,dfsub1$repe==ii);dfsub=dfsub[complete.cases(dfsub),];
    total=dfsub$total;stem=dfsub$stem;density=dfsub$dense;height=dfsub$height
    if (length(stem)>5){
      competition8[j]=mean(density)
      height8[j]=mean(height)
      total8[j]=mean(total)
      result=lmodel2(log(stem)~log(total))
      exponent8[j]=result$regression.results[1,3]
      stemfraction=stem/total
      CVST8[j]=sd(stemfraction,na.rm=T)/mean(stemfraction,na.rm=T) 
      j=j+1
    }
  }
}
species8="soybean"
#basic characteristic of the data set
stem=c(stemcorn,stemsoybean);total=c(totalcorn,totalsoybean);class=c(rep("Corn",classcorn),rep("Soybean",classsoybean))
df=data.frame(stem=stem,total=total,class=class)
figSB=ggplot(data = df,aes(x=total,y=stem,group=class,color=class))+geom_point(size=3)+geom_smooth(method="lm",span=1)+labs(title="2015 single",colour="",x="Total biomass (g)",y="Stem biomass (g)")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.8,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(2)))+
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(3))  )


result=lmodel2(log(stem)~log(total))



#2016 corn or soybean
hddcorn1516=c(corn2016$hdd1,corn2016$hdd2,corn2016$hdd3,corn2016$hdd4,corn2016$hdd5)
stemcorn1516=c(corn2016$stem1,corn2016$stem2,corn2016$stem3,corn2016$stem4,corn2016$stem5)
heightcorn1516=c(corn2016$height1,corn2016$height2,corn2016$height3,corn2016$height4,corn2016$height5)
hddsoybean1516=c(soybean2016$hdd1,soybean2016$hdd2,soybean2016$hdd3,soybean2016$hdd4,soybean2016$hdd5)
stemsoybean1516=c(soybean2016$stem1,soybean2016$stem2,soybean2016$stem3,soybean2016$stem4,soybean2016$stem5)
heightsoybean1516=c(soybean2016$height1,soybean2016$height2,soybean2016$height3,soybean2016$height4,soybean2016$height5)
HDDcorn=log(hddcorn1516);        HDDsoybean=log(hddsoybean1516)
Totalcorn1516=exp(0.74*HDDcorn-0.29);Totalsoybean1516=exp(HDDsoybean-0.7) 
IDcorn1516=c(corn2016$ID1,corn2016$ID2,corn2016$ID3,corn2016$ID4,corn2016$ID5)
IDsoybean1516=c(soybean2016$ID1,soybean2016$ID2,soybean2016$ID3,soybean2016$ID4,soybean2016$ID5)
densitycorn1516=c(corn2016$density1,corn2016$density2,corn2016$density3,corn2016$density4,corn2016$density5)
densitysoybean1516=c(soybean2016$density1,soybean2016$density2,soybean2016$density3,soybean2016$density4,soybean2016$density5)
densitycorn1516=densitycorn1516/2.25
densitysoybean1516=densitysoybean1516/2.25
repeatcorn1516=c(corn2016$repeat1,corn2016$repeat2,corn2016$repeat3,corn2016$repeat4,corn2016$repeat5)
repeatsoybean1516=c(soybean2016$repeat1,soybean2016$repeat2,soybean2016$repeat3,soybean2016$repeat4,soybean2016$repeat5)

height=heightcorn1516;stem=stemcorn1516;total=Totalcorn1516;ID=IDcorn1516;repe=repeatcorn1516;dense=densitycorn1516
stemcorn=stem;totalcorn=total;classcorn=length(total)
df=data.frame(height=height,stem=stem,total=total,ID=ID,repe=repe,dense=dense)
gradient1=unique(df$ID); exponent9=CVST9=competition9=height9=total9=0;j=1
for (i in 1:length(gradient1)){
  dfsub1=subset(df,df$ID==gradient1[i]);dfsub1=dfsub1[complete.cases(dfsub1),];
  for (ii in 1:4){
    dfsub=subset(dfsub1,dfsub1$repe==ii);dfsub=dfsub[complete.cases(dfsub),];
    total=dfsub$total;stem=dfsub$stem;density=dfsub$dense;height=dfsub$height
    if (length(stem)>5){
      competition9[j]=mean(density)
      height9[j]=mean(height)
      total9[j]=mean(total)
      result=lmodel2(log(stem)~log(total))
      exponent9[j]=result$regression.results[1,3]
      stemfraction=stem/total
      CVST9[j]=sd(stemfraction,na.rm=T)/mean(stemfraction,na.rm=T) 
      j=j+1
    }
  }
}
species9="corn"

height=heightsoybean1516;stem=stemsoybean1516;total=Totalsoybean1516;ID=IDsoybean1516;repe=repeatsoybean1516;dense=densitysoybean1516
height[height==0]=NA;stem[stem==0]=NA;total[total==0]=NA;ID[ID==0]=NA;repe[repe==0]=NA;dense[dense==0]=NA;#将0值变为NA
stemsoybean=stem;totalsoybean=total;classsoybean=length(total)
df=data.frame(height=height,stem=stem,total=total,ID=ID,repe=repe,dense=dense)
gradient1=unique(df$ID); exponent10=CVST10=competition10=height10=total10=0;j=1
for (i in 1:length(gradient1)){
  dfsub1=subset(df,df$ID==gradient1[i]);dfsub1=dfsub1[complete.cases(dfsub1),];
  for (ii in 1:4){
    dfsub=subset(dfsub1,dfsub1$repe==ii);dfsub=dfsub[complete.cases(dfsub),];
    total=dfsub$total;stem=dfsub$stem;density=dfsub$dense;height=dfsub$height
    if (length(stem)>5){
      competition10[j]=mean(density)
      height10[j]=mean(height)
      total10[j]=mean(total)
      result=lmodel2(log(stem)~log(total))
      exponent10[j]=result$regression.results[1,3]
      stemfraction=stem/total
      CVST10[j]=sd(stemfraction,na.rm=T)/mean(stemfraction,na.rm=T) 
      j=j+1
    }
  }
}
species10="soybean"
#basic characteristic of the data set
stem=c(stemcorn,stemsoybean);total=c(totalcorn,totalsoybean);class=c(rep("Corn",classcorn),rep("Soybean",classsoybean))
df=data.frame(stem=stem,total=total,class=class)
fig1C=ggplot(data = df,aes(x=total,y=stem,group=class,color=class))+geom_point(size=3)+geom_smooth(method="lm",span=1)+labs(title="2016 single",colour="",x="Total biomass (g)",y="Stem biomass (g)")+
  #theme(legend.key.height=unit(2,"line"),plot.title = element_text(hjust = 0.5))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.8,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-3),10^(3)) )+
  scale_x_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(3))  )


result=lmodel2(log(stem)~log(total))


total=c(total7,total8,total9,total10);height=c(height7,height8,height9,height10);competition=c(competition7,competition8,competition9,competition10);CVST=c(CVST7,CVST8,CVST9,CVST10);exponent=c(exponent7,exponent8,exponent9,exponent10);
class=c(rep("Corn 2015",length(exponent7)),rep("Soybean 2015",length(exponent8)),rep("Corn 2016",length(exponent9)),rep("Soybean 2016",length(exponent10)))
df=data.frame(total=total,height=height,competition=competition,CVST=CVST,exponent=exponent,class=class)
fig3A=ggplot(data = df,aes(x=log10(competition),y=CVST,group=class,color=class))+geom_point(size=5)+geom_smooth(method="lm",size=3)+
  labs(colour="",x=expression("Log density (no./" ~ m^{2}~")"))+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.6,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,3,by=1),limits=c(0.0,3))+
  scale_y_continuous(breaks=seq(-1,2,by=1),limits=c(-1,2))

figadd4A=ggplot(data = df,aes(x=height,y=CVST,group=class,color=class))+geom_point(size=5)+geom_smooth(method="lm",size=3)+
  labs(colour="",x="Plant height (cm)")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.6,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,150,by=50),limits=c(0.0,150))+
  scale_y_continuous(breaks=seq(-1,2,by=1),limits=c(-1,2))

figadd9A=ggplot(data = df,aes(x=total,y=CVST,group=class,color=class))+geom_point(size=1)+geom_smooth(method="lm")+
  labs(colour="",x="Mean total biomass (g)")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.8,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,120,by=40),limits=c(0,120))+
  scale_y_continuous(breaks=seq(-1,2,by=1),limits=c(-1,2))

#########detrend analyses
adf.test(CVST)
deCVST=diff(CVST)
dedf=data.frame(detotal=total[-1],deheight=height[-1],decompetition=competition[-1],deCVST=deCVST,declass=class[-1])

fig3B=ggplot(data = dedf,aes(x=log10(decompetition),y=deCVST,group=declass,color=declass))+geom_point(size=5)+
  geom_smooth(method="lm",size=3)+labs(colour="",x=expression("Log density (no./" ~ m^{2}~")"),y="Detrended CVST")+
  #theme(legend.key.height=unit(2,"line"))
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.4,0.75), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,3,by=1),limits=c(0.0,3))+
  scale_y_continuous(breaks=seq(-1,2,by=1),limits=c(-1,2))

figadd4B=ggplot(data = dedf,aes(x=deheight,y=deCVST,group=declass,color=declass))+geom_point(size=5)+geom_smooth(method="lm",size=3)+
  labs(colour="",x="Plant height (cm)",y="Detrended CVST")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.7,0.8), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 30,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(2.5,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,150,by=50),limits=c(0.0,150))+
  scale_y_continuous(breaks=seq(-1,2,by=1),limits=c(-1,2))

figadd9B=ggplot(data = dedf,aes(x=detotal,y=deCVST,group=declass,color=declass))+geom_point(size=1)+geom_smooth(method="lm")+
  labs(colour="",x="Mean total biomass (g)",y="Detrended CVST")+
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.8,0.9), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size=30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.3, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,120,by=40),limits=c(0.0,120))+
  scale_y_continuous(breaks=seq(-1,2,by=1),limits=c(-1,2))

#output of statistical regression analyses results
fit=lm(CVST~log10(competition),data = df)
summary(fit)
fit=lm(deCVST~log10(decompetition),data = dedf)
summary(fit)

fit=lm(CVST~height,data = df)
summary(fit)
fit=lm(deCVST~deheight,data = dedf)
summary(fit)

fit=lm(CVST~total,data = df);summary(fit)
fit=lm(deCVST~detotal,data = dedf);summary(fit)

Soybean16=subset(df,df$class=="Soybean 2016")
Soybean15=subset(df,df$class=="Soybean 2015")
Corn16=subset(df,df$class=="Corn 2016")
Corn15=subset(df,df$class=="Corn 2015")
fit=lm(CVST~log10(competition),data = Soybean16)
summary(fit)
fit=lm(CVST~log10(competition),data = Soybean15)
summary(fit)
fit=lm(CVST~log10(competition),data = Corn16)
summary(fit)
fit=lm(CVST~log10(competition),data = Corn15)
summary(fit)
#height
fit=lm(CVST~height,data = Soybean16)
summary(fit)
fit=lm(CVST~height,data = Soybean15)
summary(fit)
fit=lm(CVST~height,data = Corn16)
summary(fit)
fit=lm(CVST~height,data = Corn15)
summary(fit)
#total
fit=lm(CVST~total,data = Soybean16)
summary(fit)
fit=lm(CVST~total,data = Soybean15)
summary(fit)
fit=lm(CVST~total,data = Corn16)
summary(fit)
fit=lm(CVST~total,data = Corn15)
summary(fit)
#################################################
Soybean16=subset(dedf,dedf$declass=="Soybean 2016")
Soybean15=subset(dedf,dedf$declass=="Soybean 2015")
Corn16=subset(dedf,dedf$declass=="Corn 2016")
Corn15=subset(dedf,dedf$declass=="Corn 2015")
fit=lm(deCVST~log10(decompetition),data = Soybean16)
summary(fit)
fit=lm(deCVST~log10(decompetition),data = Soybean15)
summary(fit)
fit=lm(deCVST~log10(decompetition),data = Corn16)
summary(fit)
fit=lm(deCVST~log10(decompetition),data = Corn15)
summary(fit)

fit=lm(deCVST~deheight,data = Soybean16)
summary(fit)
fit=lm(deCVST~deheight,data = Soybean15)
summary(fit)
fit=lm(deCVST~deheight,data = Corn16)
summary(fit)
fit=lm(deCVST~deheight,data = Corn15)
summary(fit)

#total
fit=lm(deCVST~detotal,data = Soybean16)
summary(fit)
fit=lm(deCVST~detotal,data = Soybean15)
summary(fit)
fit=lm(deCVST~detotal,data = Corn16)
summary(fit)
fit=lm(deCVST~detotal,data = Corn15)
summary(fit)

library(sqldf)
library(dplyr)

#2015 corn
#ontogeny
ontogeny=meanfivedens=meanMLT=meanfivedensity=cvLT=meanH=meantotal=DN=class=0
heightcorn2=corn2015$height2;heightcorn3=corn2015$height3;heightcorn4=corn2015$height4;heightcorn5=corn2015$height5
HDDcorn2=log(corn2015$hdd2);HDDcorn3=log(corn2015$hdd3);HDDcorn4=log(corn2015$hdd4);HDDcorn5=log(corn2015$hdd5)
Totalcorn2=exp(0.74*HDDcorn2-0.29);Totalcorn3=exp(0.74*HDDcorn3-0.29);Totalcorn4=exp(0.74*HDDcorn4-0.29);Totalcorn5=exp(0.74*HDDcorn5-0.29)
stemfraction2=(corn2015$stem2)/Totalcorn2;stemfraction3=(corn2015$stem3)/Totalcorn3;stemfraction4=(corn2015$stem4)/Totalcorn4;stemfraction5=(corn2015$stem5)/Totalcorn5;
df=data.frame(ID=corn2015$ID2,repea=corn2015$repeat2,density=corn2015$density2,stemfraction=stemfraction2,height=heightcorn2,total=Totalcorn2)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
meanPdMLT2=hel$stemfraction_avg;dense2=(hel$density_avg)/2.25;meanh2=hel$height_avg;meantotal2=hel$total_avg

df=data.frame(ID=corn2015$ID3,repea=corn2015$repeat3,density=corn2015$density3,stemfraction=stemfraction3,height=heightcorn2,total=Totalcorn2)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT3=hel$stemfraction_avg;dense3=(hel$density_avg)/2.25;meanh3=hel$height_avg;meantotal3=hel$total_avg

df=data.frame(ID=corn2015$ID4,repea=corn2015$repeat4,density=corn2015$density4,stemfraction=stemfraction4,height=heightcorn2,total=Totalcorn2)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT4=hel$stemfraction_avg;dense4=(hel$density_avg)/2.25;meanh4=hel$height_avg;meantotal4=hel$total_avg

df=data.frame(ID=corn2015$ID5,repea=corn2015$repeat5,density=corn2015$density5,stemfraction=stemfraction5,height=heightcorn2,total=Totalcorn2)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT5=hel$stemfraction_avg;dense5=(hel$density_avg)/2.25;meanh5=hel$height_avg;meantotal5=hel$total_avg

for (i in 1:length(meanPdMLT2)){
  ontogeny[((i-1)*4+1):((i-1)*4+4)]=c(65,79,112,148)
  DN[((i-1)*4+1):((i-1)*4+4)] =c(dense2[i],dense3[i],dense4[i],dense5[i])
  class[((i-1)*4+1):((i-1)*4+4)]=rep(i,4)
  dense=c(dense2[i],dense3[i],dense4[i],dense5[i])
  meanfivedensity[i]=mean(dense)
  meanfivedens[((i-1)*4+1):((i-1)*4+4)]  =rep(mean(dense),4)
  mlt=c(meanPdMLT2[i],meanPdMLT3[i],meanPdMLT4[i],meanPdMLT5[i])
  meanMLT[((i-1)*4+1):((i-1)*4+4)]  =mlt
  meanH[((i-1)*4+1):((i-1)*4+4)]=c(meanh2[i],meanh3[i],meanh4[i],meanh5[i])
  meantotal[((i-1)*4+1):((i-1)*4+4)]=c(meantotal2[i],meantotal3[i],meantotal4[i],meantotal5[i])
  cvLT[i]=sd(mlt)/mean(mlt)
}
df=data.frame(meanfivedensity=meanfivedensity,  cvLT=cvLT,
              ontogeny=as.numeric(ontogeny),
              meanfivedens=as.numeric(meanfivedens),
              meanMLT=as.numeric(meanMLT),DN=as.numeric(DN),class=as.numeric(class),
              meantotal=as.numeric(meantotal),meanH=as.numeric(meanH))

fig2A=ggplot(data = df,aes(x=ontogeny,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,90,180,265),colours = hcl.colors(length(meanfivedens)))+
  geom_point(shape=NA)+geom_path()+
  labs(title="Corn 2015",x="Plant ontogeny (days)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(60,160,by=25),limits=c(60,160))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(-0.0)))

figadd1A=ggplot(data = df,aes(x=meantotal,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,90,180,265),colours = hcl.colors(length(meanfivedens)))+
  geom_point(shape=NA)+geom_line()+ labs(title="Corn 2015",x="Plant total biomass (g)",y="Mean stem mass fraction",color="Density")+ 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,40,by=10),limits=c(0,40))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(-0.0)))

figadd2A=ggplot(data = df,aes(x=meanH,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,90,180,265),colours = hcl.colors(length(meanfivedens)))+
  geom_point(shape=NA)+geom_line()+ labs(title="Corn 2015",x="Plant height (cm)",y="Mean stem mass fraction",color="Density")+ 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,80,by=20),limits=c(0,80))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(-0.0)))

figadd3A=ggplot(data = df,aes(x=ontogeny,y=DN,group=class,color=class))+scale_colour_gradientn(breaks=c(1,20,40,60),colours = hcl.colors(length(class)))+
  geom_point(shape=NA)+geom_line()+
  labs(title="Corn 2015",x="Plant ontogeny (days)",y=expression("Density (no./" ~ m^{2}~")"),color="Plot ID")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.85), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(60,160,by=25),limits=c(60,160))+
  scale_y_continuous(breaks=seq(0,360,by=90),limits=c(0,360))

#corn2016
ontogeny=meanfivedens=meanMLT=meanfivedensity=cvLT=meanH=meantotal=DN=class=0
heightcorn1=corn2016$height1;heightcorn2=corn2016$height2;heightcorn3=corn2016$height3;heightcorn4=corn2016$height4;heightcorn5=corn2016$height5
HDDcorn1=log(corn2016$hdd1);HDDcorn2=log(corn2016$hdd2);HDDcorn3=log(corn2016$hdd3);HDDcorn4=log(corn2016$hdd4);HDDcorn5=log(corn2016$hdd5)
Totalcorn1=exp(0.74*HDDcorn1-0.29);Totalcorn2=exp(0.74*HDDcorn2-0.29);Totalcorn3=exp(0.74*HDDcorn3-0.29);Totalcorn4=exp(0.74*HDDcorn4-0.29);Totalcorn5=exp(0.74*HDDcorn5-0.29)
stemfraction1=(corn2016$stem1)/Totalcorn1;stemfraction2=(corn2016$stem2)/Totalcorn2;stemfraction3=(corn2016$stem3)/Totalcorn3;stemfraction4=(corn2016$stem4)/Totalcorn4;stemfraction5=(corn2016$stem5)/Totalcorn5;

df=data.frame(ID=corn2016$ID1,repea=corn2016$repeat1,density=corn2016$density1,stemfraction=stemfraction1,height=heightcorn1,total=Totalcorn1)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT1=hel$stemfraction_avg;dense1=(hel$density_avg)/2.25;meanh1=hel$height_avg;meantotal1=hel$total_avg

df=data.frame(ID=corn2016$ID2,repea=corn2016$repeat2,density=corn2016$density2,stemfraction=stemfraction2,height=heightcorn2,total=Totalcorn2)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT2=hel$stemfraction_avg;dense2=(hel$density_avg)/2.25;meanh2=hel$height_avg;meantotal2=hel$total_avg

df=data.frame(ID=corn2016$ID3,repea=corn2016$repeat3,density=corn2016$density3,stemfraction=stemfraction3,height=heightcorn3,total=Totalcorn3)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
meanPdMLT3=hel$stemfraction_avg;dense3=(hel$density_avg)/2.25;meanh3=hel$height_avg;meantotal3=hel$total_avg

df=data.frame(ID=corn2016$ID4,repea=corn2016$repeat4,density=corn2016$density4,stemfraction=stemfraction4,height=heightcorn4,total=Totalcorn4)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT4=hel$stemfraction_avg;dense4=(hel$density_avg)/2.25;meanh4=hel$height_avg;meantotal4=hel$total_avg

df=data.frame(ID=corn2016$ID5,repea=corn2016$repeat5,density=corn2016$density5,stemfraction=stemfraction5,height=heightcorn5,total=Totalcorn5)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT5=hel$stemfraction_avg;dense5=(hel$density_avg)/2.25;meanh5=hel$height_avg;meantotal5=hel$total_avg

for (i in 1:length(meanPdMLT2)){
  ontogeny[((i-1)*5+1):((i-1)*5+5)]=c(20,58,78,119,159)
  DN[((i-1)*5+1):((i-1)*5+5)] =c(dense1[i],dense2[i],dense3[i],dense4[i],dense5[i])
  class[((i-1)*5+1):((i-1)*5+5)]=rep(i,5)
  dense=c(dense1[i],dense2[i],dense3[i],dense4[i],dense5[i])
  meanfivedensity[i]=mean(dense)
  meanfivedens[((i-1)*5+1):((i-1)*5+5)]  =rep(mean(dense),5)
  mlt=c(meanPdMLT1[i],meanPdMLT2[i],meanPdMLT3[i],meanPdMLT4[i],meanPdMLT5[i])
  meanMLT[((i-1)*5+1):((i-1)*5+5)]  =mlt
  meanH[((i-1)*5+1):((i-1)*5+5)]=c(meanh1[i],meanh2[i],meanh3[i],meanh4[i],meanh5[i])
  meantotal[((i-1)*5+1):((i-1)*5+5)]=c(meantotal1[i],meantotal2[i],meantotal3[i],meantotal4[i],meantotal5[i])
  cvLT[i]=sd(mlt)/mean(mlt)
}
df=data.frame(
  meanfivedensity=meanfivedensity,
  cvLT=cvLT,
  ontogeny=as.numeric(ontogeny),DN=as.numeric(DN),class=as.numeric(class),
  meanfivedens=as.numeric(meanfivedens), meantotal=as.numeric(meantotal),meanH=as.numeric(meanH),
  meanMLT=as.numeric(meanMLT))
fig2B=ggplot(data = df,aes(x=ontogeny,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,200,400,607),colours = hcl.colors(length(meanfivedens)))+
  geom_point(shape=NA)+geom_path()+  labs(title="Corn 2016",x="Plant ontogeny (days)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(10,160,by=50),limits=c(10,160))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-3),10^(-0))  )


figadd1B=ggplot(data = df,aes(x=meantotal,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,200,400,607),colours = hcl.colors(length(meanfivedens)))+
  geom_point(shape=NA)+geom_line()+ labs(title="Corn 2016",x="Plant total biomass (g)",y="Mean stem mass fraction",color="Density")+ 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,200,by=50),limits=c(0,200))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-3),10^(-0))  )

figadd2B=ggplot(data = df,aes(x=meanH,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,200,400,607),colours = hcl.colors(length(meanfivedens)))+
  geom_point(shape=NA)+geom_line()+ labs(title="Corn 2016",x="Plant height (cm)",y="Mean stem mass fraction",color="Density")+ 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,240,by=60),limits=c(0,240))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-3),10^(-0)))


figadd3B=ggplot(data = df,aes(x=ontogeny,y=DN,group=class,color=class))+scale_colour_gradientn(breaks=c(1,20,40,60),colours = hcl.colors(length(class)))+geom_point(shape=NA)+geom_line()+
  labs(title="Corn 2016",x="Plant ontogeny (days)",y=expression("Density (no./" ~ m^{2}~")"),color="Plot ID")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.85), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(10,160,by=50),limits=c(10,160))+
  scale_y_continuous(breaks=seq(0,210,by=70),limits=c(0,210))


#soybean2015
ontogeny=meanfivedens=meanMLT=meanfivedensity=cvLT=meanH=meantotal=DN=class=0
heightsoybean1=soybean2015$height1;heightsoybean2=soybean2015$height2;heightsoybean3=soybean2015$height3;heightsoybean4=soybean2015$height4;heightsoybean5=soybean2015$height5
HDDsoybean1=log(soybean2015$hdd1);HDDsoybean2=log(soybean2015$hdd2);HDDsoybean3=log(soybean2015$hdd3);HDDsoybean4=log(soybean2015$hdd4);HDDsoybean5=log(soybean2015$hdd5)
Totalsoybean1=exp(HDDsoybean1-0.7);Totalsoybean2=exp(HDDsoybean2-0.7);Totalsoybean3=exp(HDDsoybean3-0.7);Totalsoybean4=exp(HDDsoybean4-0.7);Totalsoybean5=exp(HDDsoybean5-0.7)
stemfraction1=(soybean2015$stem1)/Totalsoybean1;stemfraction2=(soybean2015$stem2)/Totalsoybean2;stemfraction3=(soybean2015$stem3)/Totalsoybean3;stemfraction4=(soybean2015$stem4)/Totalsoybean4;stemfraction5=(soybean2015$stem5)/Totalsoybean5;

df=data.frame(ID=soybean2015$ID1,repea=soybean2015$repeat1,density=soybean2015$density1,stemfraction=stemfraction1,height=heightsoybean1,total=Totalsoybean1)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")

meanPdMLT1=hel$stemfraction_avg;dense1=(hel$density_avg)/2.25;meanh1=hel$height_avg;meantotal1=hel$total_avg

df=data.frame(ID=soybean2015$ID2,repea=soybean2015$repeat2,density=soybean2015$density2,stemfraction=stemfraction2,height=heightsoybean2,total=Totalsoybean2)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT2=hel$stemfraction_avg;dense2=(hel$density_avg)/2.25;meanh2=hel$height_avg;meantotal2=hel$total_avg

df=data.frame(ID=soybean2015$ID3,repea=soybean2015$repeat3,density=soybean2015$density3,stemfraction=stemfraction3,height=heightsoybean3,total=Totalsoybean3)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT3=hel$stemfraction_avg;dense3=(hel$density_avg)/2.25;meanh3=hel$height_avg;meantotal3=hel$total_avg

df=data.frame(ID=soybean2015$ID4,repea=soybean2015$repeat4,density=soybean2015$density4,stemfraction=stemfraction4,height=heightsoybean4,total=Totalsoybean4)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT4=hel$stemfraction_avg;dense4=(hel$density_avg)/2.25;meanh4=hel$height_avg;meantotal4=hel$total_avg

df=data.frame(ID=soybean2015$ID5,repea=soybean2015$repeat5,density=soybean2015$density5,stemfraction=stemfraction5,height=heightsoybean5,total=Totalsoybean5)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT5=hel$stemfraction_avg;dense5=(hel$density_avg)/2.25;meanh5=hel$height_avg;meantotal5=hel$total_avg


for (i in 1:length(meanPdMLT2)){
  ontogeny[((i-1)*5+1):((i-1)*5+5)]=c(30,62,79,113,148)
  DN[((i-1)*5+1):((i-1)*5+5)] =c(dense1[i],dense2[i],dense3[i],dense4[i],dense5[i])
  class[((i-1)*5+1):((i-1)*5+5)]=rep(i,5)
  dense=c(dense1[i],dense2[i],dense3[i],dense4[i],dense5[i])
  meanfivedensity[i]=mean(dense)
  meanfivedens[((i-1)*5+1):((i-1)*5+5)]  =rep(mean(dense),5)
  mlt=c(meanPdMLT1[i],meanPdMLT2[i],meanPdMLT3[i],meanPdMLT4[i],meanPdMLT5[i])
  meanMLT[((i-1)*5+1):((i-1)*5+5)]  =mlt
  meanH[((i-1)*5+1):((i-1)*5+5)]=c(meanh1[i],meanh2[i],meanh3[i],meanh4[i],meanh5[i])
  meantotal[((i-1)*5+1):((i-1)*5+5)]=c(meantotal1[i],meantotal2[i],meantotal3[i],meantotal4[i],meantotal5[i])
  cvLT[i]=sd(mlt)/mean(mlt)
}
df=data.frame(
  meanfivedensity=meanfivedensity,
  cvLT=cvLT,
  ontogeny=as.numeric(ontogeny),DN=as.numeric(DN),class=as.numeric(class),
  meanfivedens=as.numeric(meanfivedens),meantotal=as.numeric(meantotal),meanH=as.numeric(meanH),
  meanMLT=as.numeric(meanMLT))
fig2C=ggplot(data = df,aes(x=ontogeny,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,56,112,168),colours = hcl.colors(length(meanfivedens)))+
  geom_point(shape=NA)+geom_path()+  labs(title="Soybean 2015",x="Plant ontogeny (days)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5))
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(10,160,by=50),limits=c(10,160)  )+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(0))  )

figadd1C=ggplot(data = df,aes(x=meantotal,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,56,112,168),colours = hcl.colors(length(meanfivedens)))+
  geom_point(shape=NA)+geom_line()+  labs(title="Soybean 2015",x="Plant total biomass (g)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5))
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,80,by=20),limits=c(0,80)  )+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(0))  )

figadd2C=ggplot(data = df,aes(x=meanH,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,56,112,168),colours = hcl.colors(length(meanfivedens)))+geom_point(shape=NA)+geom_line()+
  labs(title="Soybean 2015",x="Plant height (cm)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5))
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,120,by=40),limits=c(0,120)  )+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-2),10^(0)) )


figadd3C=ggplot(data = df,aes(x=ontogeny,y=DN,group=class,color=class))+scale_colour_gradientn(breaks=c(1,20,40,60),colours = hcl.colors(length(class)))+geom_point(shape=NA)+geom_line()+
  labs(title="Soybean 2015",x="Plant ontogeny (days)",y=expression("Density (no./" ~ m^{2}~")"),color="Plot ID")+#theme(plot.title = element_text(hjust = 0.5))
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        #text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.85), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(10,160,by=50),limits=c(10,160)  )+
  scale_y_continuous(breaks=seq(0,240,by=60),limits=c(0,240)  )
#soybean2016
ontogeny=meanfivedens=meanMLT=meanfivedensity=cvLT=meanH=meantotal=DN=class=0
heightsoybean1=soybean2016$height1;heightsoybean2=soybean2016$height2;heightsoybean3=soybean2016$height3;heightsoybean4=soybean2016$height4;heightsoybean5=soybean2016$height5
HDDsoybean1=log(soybean2016$hdd1);HDDsoybean2=log(soybean2016$hdd2);HDDsoybean3=log(soybean2016$hdd3);HDDsoybean4=log(soybean2016$hdd4);HDDsoybean5=log(soybean2016$hdd5)
Totalsoybean1=exp(HDDsoybean1-0.7);Totalsoybean2=exp(HDDsoybean2-0.7);Totalsoybean3=exp(HDDsoybean3-0.7);Totalsoybean4=exp(HDDsoybean4-0.7);Totalsoybean5=exp(HDDsoybean5-0.7)
stemfraction1=(soybean2016$stem1)/Totalsoybean1;stemfraction2=(soybean2016$stem2)/Totalsoybean2;stemfraction3=(soybean2016$stem3)/Totalsoybean3;stemfraction4=(soybean2016$stem4)/Totalsoybean4;stemfraction5=(soybean2016$stem5)/Totalsoybean5;

df=data.frame(ID=soybean2016$ID1,repea=soybean2016$repeat1,density=soybean2016$density1,stemfraction=stemfraction1,height=heightsoybean1,total=Totalsoybean1)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT1=hel$stemfraction_avg;dense1=(hel$density_avg)/2.25;meanh1=hel$height_avg;meantotal1=hel$total_avg

df=data.frame(ID=soybean2016$ID2,repea=soybean2016$repeat2,density=soybean2016$density2,stemfraction=stemfraction2,height=heightsoybean2,total=Totalsoybean2)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")

meanPdMLT2=hel$stemfraction_avg;dense2=(hel$density_avg)/2.25;meanh2=hel$height_avg;meantotal2=hel$total_avg

df=data.frame(ID=soybean2016$ID3,repea=soybean2016$repeat3,density=soybean2016$density3,stemfraction=stemfraction3,height=heightsoybean3,total=Totalsoybean3)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT3=hel$stemfraction_avg;dense3=(hel$density_avg)/2.25;meanh3=hel$height_avg;meantotal3=hel$total_avg

df=data.frame(ID=soybean2016$ID4,repea=soybean2016$repeat4,density=soybean2016$density4,stemfraction=stemfraction4,height=heightsoybean4,total=Totalsoybean4)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT4=hel$stemfraction_avg;dense4=(hel$density_avg)/2.25;meanh4=hel$height_avg;meantotal4=hel$total_avg

df=data.frame(ID=soybean2016$ID5,repea=soybean2016$repeat5,density=soybean2016$density5,stemfraction=stemfraction5,height=heightsoybean5,total=Totalsoybean5)
hel=sqldf("select ID,repea,avg(height) as height_avg,avg(total) as total_avg,avg(stemfraction) as stemfraction_avg,avg(density) as density_avg from df group by ID,repea")
hel=hel[-1,]
meanPdMLT5=hel$stemfraction_avg;dense5=(hel$density_avg)/2.25;meanh5=hel$height_avg;meantotal5=hel$total_avg


for (i in 1:length(meanPdMLT2)){
  ontogeny[((i-1)*5+1):((i-1)*5+5)]=c(20,58,81,118,159)
  DN[((i-1)*5+1):((i-1)*5+5)] =c(dense1[i],dense2[i],dense3[i],dense4[i],dense5[i])
  class[((i-1)*5+1):((i-1)*5+5)]=rep(i,5)
  dense=c(dense1[i],dense2[i],dense3[i],dense4[i],dense5[i])
  meanfivedensity[i]=mean(dense)
  meanfivedens[((i-1)*5+1):((i-1)*5+5)]  =rep(mean(dense),5)
  mlt=c(meanPdMLT1[i],meanPdMLT2[i],meanPdMLT3[i],meanPdMLT4[i],meanPdMLT5[i])
  meanMLT[((i-1)*5+1):((i-1)*5+5)]  =mlt
  meanH[((i-1)*5+1):((i-1)*5+5)]=c(meanh1[i],meanh2[i],meanh3[i],meanh4[i],meanh5[i])
  meantotal[((i-1)*5+1):((i-1)*5+5)]=c(meantotal1[i],meantotal2[i],meantotal3[i],meantotal4[i],meantotal5[i])
  cvLT[i]=sd(mlt)/mean(mlt)
}
df=data.frame(
  meanfivedensity=meanfivedensity,
  cvLT=cvLT,
  ontogeny=as.numeric(ontogeny),DN=as.numeric(DN),class=as.numeric(class),
  meanfivedens=as.numeric(meanfivedens),meantotal=as.numeric(meantotal),meanH=as.numeric(meanH),
  meanMLT=as.numeric(meanMLT))
fig2D=ggplot(data = df,aes(x=ontogeny,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,58,113),colours = hcl.colors(length(meanfivedens)))+geom_point(shape=NA)+geom_path()+
  labs(title="Soybean 2016",x="Plant ontogeny (days)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        ##text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(10,160,by=50),limits=c(10,160))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1.5),10^(0)) )


figadd1D=ggplot(data = df,aes(x=meantotal,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,58,113),colours = hcl.colors(length(meanfivedens)))+geom_point(shape=NA)+geom_line()+
  labs(title="Soybean 2016",x="Plant total biomass (g)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        ##text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,150,by=50),limits=c(0,150))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1.5),10^(0)) )

figadd2D=ggplot(data = df,aes(x=meanH,y=meanMLT,group=meanfivedens,color=meanfivedens))+scale_colour_gradientn(breaks=c(1,58,113),colours = hcl.colors(length(meanfivedens)))+geom_point(shape=NA)+geom_line()+
  labs(title="Soybean 2016",x="Plant height (cm)",y="Mean stem mass fraction",color="Density")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        ##text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.2), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(0,120,by=40),limits=c(0,120))+
  scale_y_continuous(trans = log10_trans(),
                     breaks = trans_breaks("log10", function(x) 10^(x)),
                     labels = trans_format("log10", math_format(10^.x)),
                     limits = c(10^(-1.5),10^(0)) )

figadd3D=ggplot(data = df,aes(x=ontogeny,y=DN,group=class,color=class))+scale_colour_gradientn(breaks=c(1,20,40,60),colours = hcl.colors(length(class)))+geom_point(shape=NA)+geom_line()+
  labs(title="Soybean 2016",x="Plant ontogeny (days)",y=expression("Density (no./" ~ m^{2}~")"),color="Plot ID")+#theme(plot.title = element_text(hjust = 0.5)) 
  theme(axis.text=element_text(size=30,family="serif",colour = "black"),
        axis.text.x = element_text(vjust=0.0),
        axis.title=element_text(size=30,family="serif", angle=0, face="plain"),
        axis.title.x = element_text(vjust=0.0),
        axis.line=element_line(size = 0.1, colour = "black", linetype=1),
        ##text = element_text(family="Times New Roman",color = "black"),
        axis.ticks.length = unit(0.15, "cm"),
        axis.ticks = element_line(size = 0.5, color="black"),
        plot.background = element_rect(fill = "white", color = NA), 
        panel.border = element_rect(size = 2.0,colour = "black",fill = "NA", linetype=1),
        panel.background = element_rect(fill = "white", color = NA),
        line = element_blank(),
        legend.key = element_blank(),
        legend.position = c(0.9,0.85), 
        legend.background = element_rect(fill = "NA", color = NA),
        legend.text =element_text(size = 16,color='black'),
        legend.title = element_text(size=16, face = "bold"),
        legend.direction="vertical", #horizontal; vertical
        legend.key.size = unit(1,'line'), # space between legend text
        plot.title = element_text(size= 30, hjust=0.4, color = "black", face = "bold",
                                  margin = margin(b = -0.0, t = 0.4, l = 0, unit = "cm")),
        plot.margin = margin(t=0.2, r=0.2, b=0.2, l=0.12, "cm") )+
  scale_x_continuous(breaks=seq(10,160,by=50),limits=c(10,160))+
  scale_y_continuous(breaks=seq(0,150,by=50),limits=c(0,150))

#ggsave("fig2onespeciesDyn.pdf",(fig2A+fig2B)/(fig2C+fig2D),width = 40, height = 40, units = "cm", dpi = 300) 
#ggsave("fig1basic.pdf",fig1A+fig1B+fig1C,width = 60, height = 20, units = "cm", dpi = 300) 
#ggsave("fig3.pdf",fig3A+fig3B,width = 40, height = 20, units = "cm", dpi = 300) 
#ggsave("fig4.pdf",fig4A+fig4B+fig4C,width = 60, height = 20, units = "cm", dpi = 300) 
#ggsave("fig5.pdf",fig5A+fig5B+fig5C,width = 60, height = 20, units = "cm", dpi = 300) 
#ggsave("figS1.pdf",figS1A+figS1B,width = 40, height = 20, units = "cm", dpi = 300) 

#ggsave("figadd1.pdf",(figadd1A+figadd1B)/(figadd1C+figadd1D),width = 40, height = 40, units = "cm", dpi = 300)
#ggsave("figadd2.pdf",(figadd2A+figadd2B)/(figadd2C+figadd2D),width = 40, height = 40, units = "cm", dpi = 300)
#ggsave("figadd3.pdf",(figadd3A+figadd3B)/(figadd3C+figadd3D),width = 40, height = 40, units = "cm", dpi = 300)
#ggsave("figadd4.pdf",figadd4A+figadd4B,width = 40, height = 20, units = "cm", dpi = 300)

#ggsave("figadd5.pdf",figadd5A+figadd5B+figadd5C,width = 60, height = 20, units = "cm", dpi = 300) 
#ggsave("figadd6.pdf",figadd6A+figadd6B+figadd6C,width = 60, height = 20, units = "cm", dpi = 300) 
#ggsave("figadd7.pdf",figadd7A+figadd7B+figadd7C,width = 60, height = 20, units = "cm", dpi = 300) 

#ggsave("figadd8.pdf",figadd8A+figadd8B+figadd8C,width = 60, height = 20, units = "cm", dpi = 300) 
#ggsave("figadd9.pdf",figadd9A+figadd9B,width = 40, height = 20, units = "cm", dpi = 300)


#ggsave("fignew3.pdf",(fig3A+fig3B)/(figadd4A+figadd4B),width = 40, height = 40, units = "cm", dpi = 300)
#ggsave("fignew5.pdf",(fig5A+fig5B+fig5C)/(figadd7A+figadd7B+figadd7C),width = 60, height = 40, units = "cm", dpi = 300)


