rm(list = ls())
library(shape)
library(plotrix)
wd <- paste0("C:/Users/Y/Desktop/CHES/hurricane/")
rsk <- read.csv(paste0(wd,'result/hurricaneFrequency/byCountryAreaRisk.csv'),stringsAsFactors = FALSE)
rsk <- rsk[which(rsk$Risk!=0),]

frq <- read.csv(paste0(wd,'result/hurricaneFrequency/byCountryAreaFrq.csv'),stringsAsFactors = FALSE)
frq <- frq[which((frq$h5 + frq$h4 + frq$h3)!=0),]
frq[is.na(frq)]<-0

data <- merge(rsk,frq,by=c('ISO3','Name','FID','SUM_AREA_K','Shape'))

colinc <- rgb(173,19,19, maxColorValue = 255)
coldcr <- rgb(52,83,142, maxColorValue = 255)

cols <- rep(colinc,nrow(data))
cols[which(data$RiskChg<0)] <- coldcr
data$cols <- cols

syms <- rep(24,nrow(data))
syms[which(data$RiskChg<0)] <- 25
data$syms <- syms


cntyChg <- c('AUS','JPN','MEX','USA','BHS','WSM','NZL','COL','ABW','GRD','CUB')
cnty <- c('MEX','AUS','USA','BHS','CUB','PHL','IND','JPN','JAM','HND','CHN','VEN')

splmean <- (data$h3spacialmean+data$h4spacialmean+data$h5spacialmean)
splmeanFtr <- (data$h3ftrspacialmean+data$h4ftrspacialmean+data$h5ftrspacialmean)

axs2col <- 'darkorange4'

if (TRUE){
tiff(file = paste0(wd,'result/graph/','Figure4Risk2.tif'), width =183, height = 63, units = 'mm', res=400) 
par(mfrow=c(1,1),oma=c(0.5,1.1,0.8,0.5),mar=c(1.1,1,0,0),xpd=NA) 
#symbol
if(TRUE){ 
plot(splmeanFtr,(data$RiskFtr),col=data$cols,pch=1,
     ylim=c(-5,50), xlim=c(0,0.25), xaxs = 'i', yaxs = 'i', cex=0.5,
     xlab="", ylab="", yaxt="n",xaxt="n", frame=FALSE)
arrows(splmeanFtr,data$RiskFtr-data$RiskFtrSD,
       splmeanFtr,(data$RiskFtr+data$RiskFtrSD),
       length=0.00,angle=90,code=3,lwd=1.5,col=adjustcolor(data$cols,alpha.f = 0.3))
arrows(splmean,data$Risk-data$RiskSD,
       splmean,data$Risk+data$RiskSD,
       length=0.00,angle=90,code=3,lwd=1.5,col=adjustcolor(data$cols,alpha.f = 0.3))
points(splmean,(data$Risk),pch=16,cex=0.55,col=data$cols)
dr <- splmeanFtr-splmean
Arrows(splmean[which(dr>0)],data$Risk[which(dr>0)],
       splmeanFtr[which(dr>0)]-0.001,(data$RiskFtr)[which(dr>0)]-0.2,
       arr.type="triangle",arr.length = 0.05, arr.width = 0.05,arr.adj = 1,lwd=0.5,col=data$cols[which(dr>0)])  
Arrows(splmean[which(dr<0)],data$Risk[which(dr<0)],
       splmeanFtr[which(dr<0)]+0.001,(data$RiskFtr)[which(dr<0)]+0.2,
       arr.type="triangle",arr.length = 0.05, arr.width = 0.05,arr.adj = 1,lwd=0.5,col=data$cols[which(dr<0)])  
}
#axis
if(TRUE){
axis(1, at=seq(0,0.25,0.05), labels=FALSE, tck=-0.02,  pos=-5)
axis(1, at=seq(0,0.25,0.05), lwd=0, labels=seq(0,0.25,0.05), las=1,  line=-1, cex.axis=0.5)
mtext('Unit-area annual hurricane frequency',1,line=0.6,cex=0.55)
axis(2, at=seq(0,data$Risk[which(data$ISO3=='AUS')],data$Risk[which(data$ISO3=='AUS')]/2), labels=FALSE, tck=-0.02,  pos=-0.001)
axis(2, at=seq(0,data$Risk[which(data$ISO3=='AUS')],data$Risk[which(data$ISO3=='AUS')]/2), lwd=0, labels=seq(0,1,0.5), las=0, line=-0.6, cex.axis=0.5)
mtext('HRI',2,line=1,cex=0.55)
}
#txt
if(TRUE){
text(-0.015,50,'b',cex=0.6,pos=4,font=2)  
text((splmean[which(data$ISO3 %in% cnty)]+
        splmeanFtr[which(data$ISO3 %in% cnty)])/2+
       c(0,0,0,0,0,0,0,0,-0.002,-0.006,-0.002,-0.007),
     (data$Risk[which(data$ISO3 %in% cnty)]+
       data$RiskFtr[which(data$ISO3 %in% cnty)])/2+
       c(3,3,3,-3,2,3,2,2,3,0,3,1),
     cex=0.5, 
     labels = data$ISO3[which(data$ISO3 %in% cnty)],
     col=data$cols[which(data$ISO3 %in% cnty)])
}
#plot2
if(TRUE){
par(new = TRUE, fig = c(.47, 1, .56,1), mar = c(0,0,0,0))
plot(data$RiskChgPct,data$RiskChg/data$Risk[which(data$ISO3=='AUS')],col=adjustcolor(data$cols,alpha.f = 0.6),pch=data$syms,cex=0.5,
     ylim=c(-0.1,0.2), xlim=c(-0.3,0.5), xaxs = 'i', yaxs = 'i', 
     xlab="", ylab="", yaxt="n",xaxt="n", frame=FALSE)
arrows(0,-0.1, 0, 0.2,length=0.00,angle=90,code=3,lwd=1,lty=3,col='grey80')  
axis(1, at=seq(-0.3,0.5,0.1), labels=FALSE, tck=-0.03,  pos=-0.11)
axis(1, at=seq(-0.3,0.5,0.1), lwd=0, labels=seq(-30,50,10), las=1,  line=-1.0, cex.axis=0.5)
mtext('Relative HRI change (%)',1,line=0.6,cex=0.55)
axis(2, at=c(-0.1,0,0.1,0.2), labels=FALSE, tck=-0.03,  pos=-0.305)
axis(2, at=c(-0.1,0,0.1,0.2), lwd=0, labels=c(-0.1,0,0.1,0.2), las=0, line=-0.8, cex.axis=0.5)
mtext('HRI change',2,line=0.8,cex=0.55)
text(0,0.18,'n = 41',cex=0.55,pos=4,col=colinc)
text(-0.1,0.18,'n = 30',cex=0.55,pos=4,col=coldcr)  
text(data$RiskChgPct[which(data$ISO3 %in% cntyChg)]+
     c(0.03,0.03,-0.03,0.03,-0.03,0.03,0,0.03,-0.02,0.03,-0.02),
     (data$RiskChg[which(data$ISO3 %in% cntyChg)]+
     c(-0.5,0,0.3,0.5,0.2,0.4,-1,0,0.9,0.2,-1.1))/data$Risk[which(data$ISO3=='AUS')],
     cex=0.5, 
     labels = data$ISO3[which(data$ISO3 %in% cntyChg)],
     col=data$cols[which(data$ISO3 %in% cntyChg)])
}
dev.off()
}
