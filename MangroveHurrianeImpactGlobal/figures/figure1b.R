rm(list = ls())
library(dplyr)

wd <- paste0("C:/Users/Y/Desktop/CHES/hurricane/result/")

files <- list.files(pattern='anova',paste0(wd,'test/NSSD010/'))

all <- data.frame()
for (file in files){
  #file <- files[1] 
  data <- read.csv(paste0(wd,'test/NSSD010/',file))
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
          
axscol <- 'black'
arwcolleft <- axscol
arwcolright <- axscol

ylm <- c(0.45,1.2)
ax2 <- c(0.5,0.8,1)
abl <- c(0.5,0.8,1)

if (TRUE) {
  tiff(file = paste0(wd,'graph/','Figure1Bmeanh3-5EVIAreaTimeOverDist2.tif'), width = 89, height = 100, units = 'mm', res=300) 
  par(oma=c(1.5,2,0,0.5),par(mar=c(1,1,0,0)),xpd=NA) 
  layout(matrix(c(1,2,3,4,5,6), 3, 2, byrow = FALSE),heights=c(1,1,1))
  
  #EVI
  for (v in 1:length(vars)){
    #v <- 3
    data_plot <- all %>%
                  filter(Time=='HurricaneSeasonRlt' & Variable== 'EVIRAnml'  & Level == 1 & Category == vars[v] & Distance<110)
    
    plot(data_plot$Distance-5,data_plot$Mean,col='white',xlim = c(0,100),ylim=ylm,xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    axis(2,at=ax2,labels=FALSE,tck=-0.04,col=axscol,col.ticks=axscol, col.axis=axscol)
    axis(2,at=ax2, labels=ax2, lwd=0, line=-0.5,col.axis=axscol,cex.axis=0.8)
    mtext('Normalized Values       ', side=2, line=1.5, col=axscol, cex=0.55)
    text(par("usr")[2]*0.5,ylbspos[v],paste0(ylbs[v], ', ', 'EVI'),col=axscol,cex=0.8,pos=4)
    
    arrows(0,0.5,100,0.5,col='grey',lty=3,length=0)
    arrows(0,0.8,100,0.8,col='grey',lty=3,length=0)
    
    if(v==1){
    text(-32,1.15,paste0('b'),col=axscol,cex=1,pos=4, font=2)
    }
    
    if(v %in% length(vars)){
    axis(1,at=seq(0,100,20),labels=FALSE,tck=-0.04,col=axscol,col.ticks=axscol, col.axis=axscol)
    axis(1,at=seq(0,100,20),labels=seq(0,100,20),lwd=0,line=-0.7,col=axscol,col.ticks=axscol, col.axis=axscol,cex.axis=0.8)
    mtext('Distance from track', side=1, line=1.2, col=axscol, cex=0.55)
    }
    
    polygon(c(c(0,seq(15,85,10),100), rev(c(0,seq(15,85,10),100))),
            c(data_plot$Mean+data_plot$SD,rev(data_plot$Mean-data_plot$SD)),
            col=adjustcolor(cols[v], alpha.f = 0.3),border=NA)
    with(data_plot[which(data_plot$ImpactOverTimeCon==1),],points(Distance-5,Mean,pch=21,type='p',lwd=1,cex=1.3,col=cols[v],bg=adjustcolor(cols[v],alpha.f = 0.3)))
    with(data_plot[which(data_plot$ImpactOverTimeCon==0),],points(Distance-5,Mean,pch='-',type='p',lwd=1,cex=1.2,col=cols[v]))
  }
  #Area
  for (v in 1:length(vars)){
    #v <- 1
    data_plot <- all %>%
      filter(Time=='HurricaneSeasonRlt' & Variable== 'totalAreaRAnml'  & Level == 1 & Category == vars[v]& Distance<110)
    
    plot(data_plot$Distance-5,data_plot$Mean,xlim=c(0,100),ylim=ylm,col='white',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
    axis(2,at=ax2,labels=FALSE,tck=-0.04,col=axscol,col.ticks=axscol, col.axis=axscol)
    text(par("usr")[2]*0.5,ylbspos[v],paste0(ylbs[v],', ','Area'),col=axscol,cex=0.8,pos=4)
    
    arrows(0,0.5,100,0.5,col='grey',lty=3,length=0)
    arrows(0,0.8,100,0.8,col='grey',lty=3,length=0)
    
    if(v %in% length(vars)){
      axis(1,at=seq(0,100,20),labels=FALSE,tck=-0.04,col=axscol,col.ticks=axscol, col.axis=axscol)
      axis(1,at=seq(0,100,20),labels=seq(0,100,20),lwd=0,line=-0.7,col=axscol,col.ticks=axscol, col.axis=axscol,cex.axis=0.8)
      mtext('Distance from track', side=1, line=1.2, col=axscol, cex=0.55)
    }
    
    polygon(c(c(0,seq(15,85,10),100), rev(c(0,seq(15,85,10),100))),
            c(data_plot$Mean+data_plot$SD,rev(data_plot$Mean-data_plot$SD)),
            col=adjustcolor(cols[v], alpha.f = 0.3),border=NA)
    with(data_plot[which(data_plot$ImpactOverTimeCon==1),],points(Distance-5,Mean,pch=21,type='p',lwd=1,cex=1.3,col=cols[v],bg=adjustcolor(cols[v],alpha.f = 0.3)))
    with(data_plot[which(data_plot$ImpactOverTimeCon==0),],points(Distance-5,Mean,pch='-',type='p',lwd=1,cex=1.2,col=cols[v]))
  }
  dev.off()
}#plot
