###
#############
rm(list = ls())

wd <- paste0("C:/Users/Y/Desktop/CHES/hurricane/")
HII <- read.csv(paste0(wd,'result/test/HII.csv'),stringsAsFactors = FALSE)

data <- read.csv(paste0(wd,'result/hurricaneFrequency/byCountryAreaRisk.csv'),stringsAsFactors = FALSE)

imp_nw <- colSums(data[,c('h3Risk','h4Risk','h5Risk')])
imp_ftr <- colSums(data[,c('h3RiskFtr','h4RiskFtr','h5RiskFtr')])
imp_chg <- sum(imp_ftr)-sum(imp_nw)

cb <- as.data.frame(t(rbind(unlist(c(imp_nw,imp_chg)),unlist(c(imp_ftr,0)))))
colnames(cb) <- c('Nw','Ftr')
cb$cat <- c('h3','h4','h5','chg')
cb$Nw_pct <- cb$Nw/sum(cb$Nw[1:3])
cb$Ftr_pct <- cb$Ftr/sum(cb$Ftr)
cb$pct <- cb$Ftr/cb$Nw-1
cb$pct[4] <- cb$Nw_pct[4]
cb$hii <- c(rev(HII$ImpactIndex)/sum(HII$ImpactIndex),0.155)

cols <- c(rgb(62,84,150, maxColorValue = 255),
          rgb(10,144,134,maxColorValue = 255),
          rgb(0.8,0.1,0.1),
          NA)

labcex <- 0.55
axscex <- 0.55
notecex <- 0.55

if(TRUE){
tiff(file = paste0(wd,'result/graph/','Figure2hiiImpactChg2.tif'), width = 89, height = 70, units = 'mm', res=300) 
par(mfrow=c(1,2),oma=c(0.5,0.5,1,0),mar=c(0.5,0.5,0,2.2),xpd=NA) 
layout(matrix(c(1,2), 1, 2, byrow = TRUE),widths=c(1,2))
  
par(lwd=1)
bp <-barplot(as.matrix(cb[,c('hii')]),col=adjustcolor(cols,alpha.f = 0.2),space=0.6,border=cols,xlab="", ylab="", yaxt="n",xaxt="n")
#text(1.1,0.15,paste0(round(cb[which(cb$cat=='h3'),'hii']/cb[which(cb$cat=='h3'),'hii'],digits = 1)),cex=notecex,col=cols[1],pos=1)
#text(1.1,0.32,paste0(round(cb[which(cb$cat=='h4'),'hii']/cb[which(cb$cat=='h3'),'hii'],digits = 1)),cex=notecex,col=cols[2],pos=1)
#text(1.1,0.73,paste0(round(cb[which(cb$cat=='h5'),'hii']/cb[which(cb$cat=='h3'),'hii'],digits = 1)),cex=notecex,col=cols[3],pos=1)
text(1.1,0.15,'1.0',cex=0.5,col=cols[1],pos=1)
text(1.1,0.32,'1.3',cex=0.5,col=cols[2],pos=1)
text(1.1,0.73,'4.0',cex=0.5,col=cols[3],pos=1)
axis(1, at=bp,lwd=0, labels=c("HII"), las=1,cex.axis=labcex,line=-1.2)  
text(0.4,1.09,paste0('a'),col='black',cex=0.7,pos=4, font=2)  

bp <- barplot(as.matrix(cb[,c('Nw','Ftr')]),col=adjustcolor(cols,alpha.f = 0.2),space=0.9,border=cols,xlab="", ylab="", yaxt="n",xaxt="n")
text(0,300,paste0('b'),col='black',cex=0.7,pos=4, font=2)  

axis(2, at=c(0,sum(cb$Nw[1:3]),sum(cb$Nw[1:3])/2),labels= NA,tck=-0.03)
axis(2, at=c(0,sum(cb$Nw[1:3]),sum(cb$Nw[1:3])/2),lwd=0, labels=c(0,100,50), las=2,cex.axis=0.5,line=-0.7,
       las=0)
mtext("Percentage (%)",side=2, line = 1, cex=0.5)
  
axis(4, at=seq(0,sum(cb$Ftr),sum(cb$Ftr)/2),labels= NA,col="darkred",tck=-0.04)
axis(4, at=seq(0,sum(cb$Ftr),sum(cb$Ftr)/2),lwd=0, labels=seq(0,100,50), las=1, cex.axis=0.5,line=-0.9,las=0,col="darkred",
       col.axis="darkred")
mtext("Percentage (%)", side=4, line = 0.7,cex=0.5,las=3,col="darkred")
  
#axis(1, at=seq(1,2,1),labels= seq(1,2,1))
axis(1, at=bp,lwd=0, labels=c("Present HRI","End-of-century HRI"), las=1,cex.axis=0.5,line=-1.2)
  
text(bp[1],0.2*sum(cb$Ftr),paste0(round(cb$Nw_pct[1],digits = 2)*100,'%'),cex=0.5,pos=1)
text(bp[1],0.55*sum(cb$Ftr),paste0(round(cb$Nw_pct[2],digits = 2)*100,'%'),cex=0.5,pos=1)
text(bp[1],0.8*sum(cb$Ftr),paste0(round(cb$Nw_pct[3],digits = 2)*100,'%'),cex=0.5,pos=1)
  
text(bp[2],0.2*sum(cb$Ftr),paste0('',round(cb$Ftr_pct[1],digits = 2)*100,'%'),cex=0.5,pos=1,col="darkred")
text(bp[2],0.55*sum(cb$Ftr),paste0('',round(cb$Ftr_pct[2],digits = 2)*100,'%'),cex=0.5,pos=1,col="darkred")
text(bp[2],0.89*sum(cb$Ftr),paste0('',round(cb$Ftr_pct[3],digits = 2)*100,'%'),cex=0.5,pos=1,col="darkred")
  
text(mean(bp),0.2*sum(cb$Ftr),paste0('H3: ',round(cb$pct[1],digits = 2)*100,'%'),cex=0.5,pos=1,col=cols[1])
text(mean(bp),0.55*sum(cb$Ftr),paste0('H4: +',round(cb$pct[2],digits = 2)*100,'%'),cex=0.5,pos=1,col=cols[2])
text(mean(bp),0.82*sum(cb$Ftr),paste0('H5: +',round(cb$pct[3],digits = 2)*100,'%'),cex=0.5,pos=1,col=cols[3])
text(mean(bp),1*sum(cb$Ftr),paste0('Tot.: +',round(cb$pct[4],digits = 2)*100,'%'),cex=0.5,pos=1,col="black")
  
dev.off()
}


if (FALSE){ #scatter plot
  tiff(file = paste0(wd,'result/graph/','impacth3h4h5.tif'), width = 89, height = 60, units = 'mm', res=300) 
  par(mfrow=c(1,1),oma=c(1,2,0.5,2),mar=c(1,1,0,1)) 
  
  plot(fre_x,imp_y,
       xlim=c(-0.2,1.2),ylim=c(-0.2,1.2),
       xaxt='n',yaxt='n',xlab='',ylab='',bty="n",
       cex=sqrt(impfull_size)*10,pch=21, 
       col=cols,bg=adjustcolor(cols,alpha.f = 0.2))
  text(fre_x,imp_y,labels=c('h5','h4','h3'),cex=0.7,font=2,col=cols)
  
  axis(1,at=c(0,0.5,1),labels=FALSE,tck=-0.03,col='black',col.ticks='black', col.axis='black')
  axis(1,at=c(0,0.5,1),labels = c(0,0.5,1),line=-0.8,lwd=0,cex.axis=0.55)
  mtext('Impact', side=1, line=0.8, col='black', cex=0.55)
  
  axis(2,at=c(0,0.5,1),labels=FALSE,tck=-0.03,col='black',col.ticks='black', col.axis='black')
  axis(2,at=c(0,0.5,1),labels = c(0,0.5,1),line=-0.6,lwd=0,cex.axis=0.55)
  mtext('Frequency', side=2, line=1.1, col='black', cex=0.55)
  
  dev.off()}

plot(1,1,cex=sqrt(10))
points(1,1,cex=sqrt(20))
##############
