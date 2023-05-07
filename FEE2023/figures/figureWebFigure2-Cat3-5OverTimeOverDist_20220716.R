###h3-5 EVI Area mean SD over time over dist (long graph)
#continuous impact
#############
rm(list = ls())
library(dplyr)

wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangrove/result/")

files <- list.files(pattern='anova',paste0(wd,'test/SD010/'))
all <- data.frame()
for (file in files){
#file <- files[1] 
data <- read.csv(paste0(wd,'test/SD010/',file))
all <- rbind(all,data)
}  

unique(all$Variable)
cats <- c('h5','h4','h3')
ylbs <- c('Cat. 5','Cat. 4','Cat. 3')
#ylbspos <- c(0.7,0.7,0.7,0.7,0.7)
pnl <- letters[3:1]
cols <- c(rgb(0.8,0.1,0.1),
          rgb(10,144,134,maxColorValue = 255),
          rgb(62,84,150, maxColorValue = 255),
          rgb(77,79,83,maxColorValue = 255),
          rgb(116,118,120, maxColorValue = 255))

axscol <- 'black'
arwcolleft <- axscol
arwcolright <- axscol


if (TRUE) {
  tiff(file = paste0(wd,'figure/','FigureA7ANOVAh3-5EVITimeDistCon2.tif'), width = 89, height = 200, units = 'mm', res=300) 
  par(oma=c(3,2,1,0.5),mar=c(0.2,2,0.5,0.5),xpd=NA) 
  layout(matrix(c(1,2,3), 3, 1, byrow = TRUE),heights=c(1,1,1))
  #plot(data_plot$Level,rep(1,nrow(data_plot)),ylim=c(0.3,1.7),col='white',xaxt='n',yaxt='n',xlab='',ylab='',bty='n') 
  
  for (c in c(3,2,1)){
    #c<-1
    ylm <- c(0,105)
    ylb <- seq(0,100,20)
    
    cat <- cats[c]
    data_plot <- all %>%
      filter(Distance == 10 & Category == cat & Variable == 'EVIRAnml')
    
    pchs <- rep("-",20)
    pchs[which(data_plot$ImpactOverTimeDistCon==1)] <- '+'
    plot(data_plot$Level+0.5,rep((10-5),nrow(data_plot)),pch=pchs,ylim=ylm,xlim=c(-4,16),col=cols[c],cex=2,
         xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    
    for (dd in seq(20,ylm[2],10)){
      #dd <- 20
      data_plot <- all %>%
        filter(Distance == dd & Category == cat & Variable == 'EVIRAnml') 
      
      pchs <- rep("-",20)
      pchs[which(data_plot$ImpactOverTimeDistCon==1 )] <- '+'
      points(data_plot$Level+0.5,rep((dd-5),nrow(data_plot)),pch=pchs,col=cols[c],cex=2) 
    }
    
    axis(2,at=ylb,labels=FALSE,tck=-0.03,col=axscol,col.ticks=axscol, col.axis=axscol)
    axis(2,at=ylb, labels=ylb, lwd=0, line=0,col.axis=axscol,cex.axis=0.8)
    mtext('Distance from Track (km)', side=2, line=2, col=axscol, cex=0.6)
    axis(1,at=c(-4,0,4,8,12,16),labels=FALSE,tck=-0.03,col=axscol,col.ticks=axscol, col.axis=axscol,pos=0)
    text(-8,ylm[2]+5,paste0(pnl[c],' ',ylbs[c]),col=cols[c],cex=0.9,pos=4,font=2)
    if(c==1){
      axis(1,at=c(-4,0,4,8,12,16),labels=c('–1',0,1,2,3,4),lwd=0,line=-0.7,col=axscol,col.ticks=axscol, col.axis=axscol,cex.axis=0.85)
      mtext('Year', side=1, line=1.5, col=axscol, cex=0.6)
    }
  }
  dev.off()
}#plot

if (FALSE) {
tiff(file = paste0(wd,'figure/','FigureA7ANOVAh3-5EVIAreaTimeDistCon2.tif'), width = 89, height = 200, units = 'mm', res=300) 
par(oma=c(3,2,1,0.5),par(mar=c(0.2,2,0.5,0.5)),xpd=NA) 
layout(matrix(c(1,2,3), 3, 1, byrow = TRUE),heights=c(1,1,1))
#plot(data_plot$Level,rep(1,nrow(data_plot)),ylim=c(0.3,1.7),col='white',xaxt='n',yaxt='n',xlab='',ylab='',bty='n') 
  
for (c in 1:length(cats)){
#c<-1
ylm <- c(0,105)
ylb <- seq(0,100,20)
    
cat <- cats[c]
data_plot <- all %>%
             filter(Distance == 10 & Category == cat)
    
pchs <- rep("-",20)
pchs[which(data_plot$ImpOverTimeDistCon==1 & data_plot$EVI_Impact==1)] <- '^'
plot(data_plot$Level+0.125,rep((10+2),nrow(data_plot)),pch=pchs,ylim=ylm,col=cols[c],cex=2,
         xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    
for (dd in seq(20,ylm[2],10)){
#dd <- 20
data_plot <- all %>%
             filter(Distance == dd & Category == cat) 
      
pchs <- rep("-",20)
pchs[which(data_plot$ImpOverTimeDistCon==1 & data_plot$EVI_Impact==1)] <- '^'
points(data_plot$Level+0.125,rep((dd+2),nrow(data_plot)),pch=pchs,col=cols[c],cex=2) 
}
    
for (dd in seq(10,ylm[2],10)){
#dd <- 20
data_plot <- all %>%
             filter(Distance == dd & Category == cat)
      
pchs <- rep("-",20)
pchs[which(data_plot$ImpOverTimeDistCon==1 & data_plot$area_Impact==1)] <- 'v'
points(data_plot$Level+0.125,rep((dd-2),nrow(data_plot)),pch=pchs,col=adjustcolor(cols[c],alpha.f = 0.5) ,cex=1.5) 
}
    
axis(2,at=ylb,labels=FALSE,tck=-0.03,col=axscol,col.ticks=axscol, col.axis=axscol)
axis(2,at=ylb, labels=ylb, lwd=0, line=0,col.axis=axscol,cex.axis=0.8)
mtext('Distance from Track (km)', side=2, line=2, col=axscol, cex=0.6)
axis(1,at=c(-1,0,1,2,3,4),labels=FALSE,tck=-0.03,col=axscol,col.ticks=axscol, col.axis=axscol,pos=0)
text(-1,ylm[2]+5,paste0(pnl[c],' ',ylbs[c]),col=cols[c],cex=0.9,pos=4,font=2)
if(c==3){
axis(1,at=c(-1,0,1,2,3,4),labels=seq(0,5,1),lwd=0,line=-0.7,col=axscol,col.ticks=axscol, col.axis=axscol,cex.axis=0.85)
mtext('Year', side=1, line=1.5, col=axscol, cex=0.6)
}
}
dev.off()
}#plot
################



