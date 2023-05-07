#For guidance, Nature's standard figure sizes are 89 mm wide (single column) and 183 mm wide (double column). 
#The full depth of a Nature page is 247 mm. Figures can also be a column-and-a-half where necessary (120-136 mm).

###scater arrow with uncertainty
##########
rm(list = ls())
library(shape)
library(plotrix)
wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangrove/")
rsk <- read.csv(paste0(wd,'result/hurricaneFrequency/byCountryAreaRisk.csv'),stringsAsFactors = FALSE)
rsk <- rsk[which(rsk$Risk!=0),]

frq <- read.csv(paste0(wd,'result/hurricaneFrequency/byCountryAreaFrq.csv'),stringsAsFactors = FALSE)
frq <- frq[which((frq$h5 + frq$h4 + frq$h3)!=0),]
frq[is.na(frq)]<-0

data <- merge(rsk,frq,by=c('ISO3','Name','FID','SUM_AREA_K','Shape'))
#cols <- c(rgb(62,84,150, maxColorValue = 255),
#          rgb(10,144,134,maxColorValue = 255),
#          rgb(0.8,0.1,0.1),
#          NA)

#colinc <- 'darkred'
#coldcr <- 'darkgreen'
colinc <- rgb(173,19,19, maxColorValue = 255)
coldcr <- rgb(52,83,142, maxColorValue = 255)

cols <- rep(colinc,nrow(data))
cols[which(data$RiskChg<0)] <- coldcr
data$cols <- cols

syms <- rep(24,nrow(data))
syms[which(data$RiskChg<0)] <- 25
data$syms <- syms


gap <- 0.3

if (TRUE){
  tiff(file = paste0(wd,'result/figure/','Figure4Risk2.tif'), width =247, height =125, units = 'mm', res=300) 
  par(oma=c(0.5,0.5,0.5,0.5),mar=c(8,1.5,0,0),xpd=NA) 
  layout(matrix(c(1,1), 1, byrow = TRUE))
  
  #original
  if(TRUE){
  data <- data[order(-data$Risk,data$Name),]
  
  plot(seq(1,nrow(data),1),data$Risk,pch='-',col=data$cols,
       ylim=c(0,data$Risk[which(data$ISO3=='AUS')]*1.2), xlim=c(1,75), xaxs = 'i', yaxs = 'i', xlab="", ylab="", yaxt="n",xaxt="n",frame=FALSE)
  arrows(seq(1,nrow(data),1),data$Risk-data$RiskSD, seq(1,nrow(data),1), data$Risk+data$RiskSD,length=0.00,angle=90,code=3,lwd=1,lty=1,
         col=adjustcolor(data$cols,alpha.f = 0.5))
  
  points(seq(1,nrow(data),1)+gap,data$RiskFtr,pch=4,cex=0.5,col=data$cols)
  arrows(seq(1,nrow(data),1)+gap,data$RiskFtrlwSD, seq(1,nrow(data),1)+gap, data$RiskFtrupSD,length=0.00,angle=90,code=3,lwd=1,lty=1,
         col=adjustcolor(data$cols,alpha.f = 0.5))
  
  axis(1, at=seq(1,nrow(data),1), lwd=0, labels=data$Name, las=3,  line=-0.5, cex.axis=0.6)
  
  axis(2, at=seq(0,data$Risk[which(data$ISO3=='AUS')]*1.2,data$Risk[which(data$ISO3=='AUS')]/5), labels=FALSE, tck=-0.02,  pos=0.5)
  axis(2, at=seq(0,data$Risk[which(data$ISO3=='AUS')]*1.2,data$Risk[which(data$ISO3=='AUS')]/5), lwd=0, labels=seq(0,1.2,0.2), las=0, line=-0.4, cex.axis=0.6)
  mtext('TCRI',2,line=1.1,cex=0.6)
  }
  
  #zone
  if(TRUE){
    par(new = TRUE, fig = c(.19, 1, .5,1), mar = c(0,0,0,0))
    dataZone <- data[13:nrow(data),]
    
    
    plot(seq(1,nrow(dataZone),1),dataZone$Risk,pch='-',col=dataZone$cols,
         ylim=c(0,data$Risk[which(data$ISO3=='AUS')]*0.1), xlim=c(1,nrow(dataZone)), xaxs = 'i', yaxs = 'i', xlab="", ylab="", yaxt="n",xaxt="n",frame=FALSE)
    arrows(seq(1,nrow(dataZone),1),dataZone$Risk-dataZone$RiskSD, seq(1,nrow(dataZone),1), dataZone$Risk+dataZone$RiskSD,length=0.00,angle=90,code=3,lwd=1,lty=1,
           col=adjustcolor(dataZone$cols,alpha.f = 0.5))
    
    points(seq(1,nrow(dataZone),1)+gap,dataZone$RiskFtr,pch=4,cex=0.5,col=dataZone$cols)
    arrows(seq(1,nrow(dataZone),1)+gap,dataZone$RiskFtrlwSD, seq(1,nrow(dataZone),1)+gap, dataZone$RiskFtrupSD,length=0.00,angle=90,code=3,lwd=1,lty=1,
           col=adjustcolor(dataZone$cols,alpha.f = 0.5))
    
    axis(1, at=seq(1,nrow(dataZone),1), lwd=0, labels=dataZone$ISO3, las=3,  line=-0.5, cex.axis=0.6)
    
    axis(2, at=seq(0,data$Risk[which(data$ISO3=='AUS')]*0.1,data$Risk[which(data$ISO3=='AUS')]/50), labels=FALSE, tck=-0.02,  pos=0.5)
    axis(2, at=seq(0,data$Risk[which(data$ISO3=='AUS')]*0.1,data$Risk[which(data$ISO3=='AUS')]/50), lwd=0, labels=seq(0,0.1,0.02), las=0, line=-0.4, cex.axis=0.6)
    mtext('TCRI',2,line=1.1,cex=0.6)
  }
  dev.off()
}