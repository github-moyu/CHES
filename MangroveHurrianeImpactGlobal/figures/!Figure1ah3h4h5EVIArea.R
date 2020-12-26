###h3-5 EVI and Area mean SD over time 10km (long graph)
#############
rm(list = ls())
library(dplyr)

wd <- paste0("C:/Users/Y/Desktop/CHES/hurricane/result/")

files <- list.files(pattern='anova',paste0(wd,'test/SD010/'))

all <- data.frame()
for (file in files){
  #file <- files[1] 
  data <- read.csv(paste0(wd,'test/SD010/',file))
  all <- rbind(all,data)
}  

unique(all$Variable)
vars <- c('h5','h4','h3')
ylbs <- c('H5','H4','H3')
ylbspos <- rep(0.6,6)
pnl <- letters[1:6]
cols <- c(rgb(0.8,0.1,0.1),
          rgb(10,144,134,maxColorValue = 255),
          rgb(62,84,150, maxColorValue = 255))
#rgb(77,79,83,maxColorValue = 255),
#rgb(116,118,120, maxColorValue = 255))

axscol <- 'black'
arwcolleft <- axscol
arwcolright <- axscol

ylm <- c(0.45,1.2)
ax2 <- c(0.5,0.8,1)
abl <- c(0.5,0.8,1)

if (TRUE) {
  tiff(file = paste0(wd,'graph/','Figure1Ameanh3-5EVIAreaTime10km2.tif'), width = 89, height = 100, units = 'mm', res=300) 
  par(oma=c(1.5,2,0,0.5),par(mar=c(1,1,0,0)),xpd=NA) 
  layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = FALSE),heights=c(1,1,1))
  
  #EVI
  for (v in 1:length(vars)){
    #v <- 1
    data_plot <- all %>%
                  filter(Time=='HurricaneSeasonRlt' & Variable== 'EVIRAnml'  & Distance == 10 & Category == vars[v])
    
    plot(data_plot$Level-0.125,rep(1,nrow(data_plot)),xlim=c(-1,4),ylim=ylm,col='white',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    axis(2,at=ax2,labels=FALSE,tck=-0.04,col=axscol,col.ticks=axscol, col.axis=axscol)
    axis(2,at=ax2, labels=ax2, lwd=0, line=-0.5,col.axis=axscol,cex.axis=0.8)
    mtext('Normalized Values         ', side=2, line=1.5, col=axscol, cex=0.55)
#    text(par("usr")[2]*0.5,ylbspos[v],paste0(pnl[(v-1)*2+1]),col=axscol,cex=0.8,pos=4,font=2)
#    text(par("usr")[2]*0.57,ylbspos[v],paste0(ylbs[v], ', ', 'EVI'),col=axscol,cex=0.8,pos=4)
    #text(par("usr")[2]*0.5,ylbspos[v],paste0(pnl[(v-1)*2+1],' ',ylbs[v], ', ', 'EVI'),col=axscol,cex=0.8,pos=4)
    text(par("usr")[2]*0.5,ylbspos[v],paste0(ylbs[v], ', ', 'EVI'),col=axscol,cex=0.8,pos=4)
    
    arrows(-1,0.5,3.75,0.5,col='grey',lty=3,length=0)
    arrows(-1,0.8,3.75,0.8,col='grey',lty=3,length=0)
    
    if(v==1){
    text(-2.6,1.15,paste0('a'),col=axscol,cex=1,pos=4, font=2)
    }
    
    if(v %in% length(vars)){
      axis(1,at=c(-1,0,1,2,3,4),labels=FALSE,tck=-0.04,col=axscol,col.ticks=axscol, col.axis=axscol)
      axis(1,at=c(-1,0,1,2,3,4),labels=seq(-1,4,1),lwd=0,line=-0.7,col=axscol,col.ticks=axscol, col.axis=axscol,cex.axis=0.8)
      mtext('Year', side=1, line=1.2, col=axscol, cex=0.55)
    }
    
    polygon(c((data_plot$Level[1]-0.1),data_plot$Level[2:(nrow(data_plot)-1)],(data_plot$Level[nrow(data_plot)]+0.1),
              rev(c((data_plot$Level[1]-0.1),data_plot$Level[2:(nrow(data_plot)-1)],(data_plot$Level[nrow(data_plot)]+0.1))))+0.125,
            c(data_plot$Mean+data_plot$SD,rev(data_plot$Mean-data_plot$SD)),
            col=adjustcolor(cols[v], alpha.f = 0.3),border=NA)
    with(data_plot[which(data_plot$ImpactOverTimeCon==1),],points(Level+0.125,Mean,pch=21,type='p',lwd=1,cex=1.3,col=cols[v],bg=adjustcolor(cols[v],alpha.f = 0.3)))
    with(data_plot[which(data_plot$ImpactOverTimeCon==0),],points(Level+0.125,Mean,pch='-',type='p',lwd=1,cex=1.2,col=cols[v]))
  }
  #Area
  for (v in 1:length(vars)){
    #v <- 1
    data_plot <- all %>%
      filter(Time=='HurricaneSeasonRlt' & Variable== 'totalAreaRAnml'  & Distance == 10 & Category == vars[v])
    
    plot(data_plot$Level-0.125,rep(1,nrow(data_plot)),xlim=c(-1,4),ylim=ylm,col='white',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    axis(2,at=ax2,labels=FALSE,tck=-0.04,col=axscol,col.ticks=axscol, col.axis=axscol)
#    text(par("usr")[2]*0.5,ylbspos[v],paste0(pnl[(v-1)*2+2]),col=axscol,cex=0.8,pos=4,font=2)
#    text(par("usr")[2]*0.57,ylbspos[v],paste0(ylbs[v],', ','Area'),col=axscol,cex=0.8,pos=4)
    #text(par("usr")[2]*0.5,ylbspos[v],paste0(pnl[(v-1)*2+2],' ',ylbs[v],', ','Area'),col=axscol,cex=0.8,pos=4)
    text(par("usr")[2]*0.5,ylbspos[v],paste0(ylbs[v],', ','Area'),col=axscol,cex=0.8,pos=4)
    
    arrows(-1,0.5,3.75,0.5,col='grey',lty=3,length=0)
    arrows(-1,0.8,3.75,0.8,col='grey',lty=3,length=0)
    
    if(v %in% length(vars)){
      axis(1,at=c(-1,0,1,2,3,4),labels=FALSE,tck=-0.04,col=axscol,col.ticks=axscol, col.axis=axscol)
      axis(1,at=c(-1,0,1,2,3,4),labels=seq(-1,4,1),lwd=0,line=-0.7,col=axscol,col.ticks=axscol, col.axis=axscol,cex.axis=0.8)
      mtext('Year', side=1, line=1.2, col=axscol, cex=0.55)
    }
    
    polygon(c((data_plot$Level[1]-0.1),data_plot$Level[2:(nrow(data_plot)-1)],(data_plot$Level[nrow(data_plot)]+0.1),
              rev(c((data_plot$Level[1]-0.1),data_plot$Level[2:(nrow(data_plot)-1)],(data_plot$Level[nrow(data_plot)]+0.1))))+0.125,
            c(data_plot$Mean+data_plot$SD,rev(data_plot$Mean-data_plot$SD)),
            col=adjustcolor(cols[v], alpha.f = 0.3),border=NA)
    with(data_plot[which(data_plot$ImpactOverTimeCon==1),],points(Level+0.125,Mean,pch=21,type='p',lwd=1,cex=1.3,col=cols[v],bg=adjustcolor(cols[v],alpha.f = 0.3)))
    with(data_plot[which(data_plot$ImpactOverTimeCon==0),],points(Level+0.125,Mean,pch='-',type='p',lwd=1,cex=1.2,col=cols[v]))
  }
  dev.off()
}#plot
################