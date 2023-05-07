###
#############
rm(list = ls())

wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangrove/")
HII <- read.csv(paste0(wd,'result/test/TCII.csv'),stringsAsFactors = FALSE)
data_frq <-  read.csv(paste0(wd,'result/hurricaneFrequency/byCountryAreaFrq.csv'),stringsAsFactors = FALSE)
data_rsk <- read.csv(paste0(wd,'result/hurricaneFrequency/byCountryAreaRisk.csv'),stringsAsFactors = FALSE)

cb <- data.frame(cat = c('h5','h4','h3','diff'))
cb$TCII <- c(abs(HII$ImpactIndex)/min(abs(HII$ImpactIndex)),0)
cb$frq <- c(colSums(data_frq[,c('h5','h4','h3')]),0)
cb$rsk <- c(colSums(data_rsk[,c('h5Risk','h4Risk','h3Risk')]),0)
cb$frq_pct <- cb$frq/sum(cb$frq,na.rm = TRUE)
cb$rsk_pct <- cb$rsk/sum(cb$rsk,na.rm = TRUE)
cb$frqFtr <- c(colSums(data_frq[,c('h5ftr','h4ftr','h3ftr')]),0)
cb$rskFtr <- c(colSums(data_rsk[,c('h5RiskFtr','h4RiskFtr','h3RiskFtr')]),0)
cb$frqFtr_pct <- cb$frqFtr/sum(cb$frqFtr,na.rm = TRUE)
cb$rskFtr_pct <- cb$rskFtr/sum(cb$rskFtr,na.rm = TRUE)

sum(cb$frq)
sum(cb$frqFtr)

sum(cb$rsk)
sum(cb$rskFtr)

cols <- c(NA,
          rgb(62,84,150, maxColorValue = 255),
          rgb(10,144,134,maxColorValue = 255),
          rgb(0.8,0.1,0.1))

panelcex <- 1
axslabcex <- 0.7
notecex <- 0.9

if(TRUE){
  tiff(file = paste0(wd,'result/figure/','Figure2TCIIFrqRiskFtr2.tif'), width = 120, height = 70, units = 'mm', res=300) 
  par(mfrow=c(1,2),oma=c(1,0.5,1,0),mar=c(0.5,0.5,2,2.2),xpd=TRUE) 
  layout(matrix(c(1,2,3,4,5), 1, 5, byrow = TRUE))
  
  par(lwd=1)
  #TCII
  if(TRUE){
  bp <-barplot(as.matrix(rev(cb[,c('TCII')])),col=adjustcolor(cols,alpha.f = 0.2),space=0.6,border=cols,
  #                          ylim=c(0,32),
               xlab="", ylab="", yaxt="n",xaxt="n")
  text(1.1,30,round(cb[which(cb$cat=='h5'),'TCII'],digits = 1), cex=notecex,col=cols[4],pos=1)
  text(1.1,9, round(cb[which(cb$cat=='h4'),'TCII'],digits = 1), cex=notecex,col=cols[3],pos=1)
  text(1.1,2.5,round(cb[which(cb$cat=='h3'),'TCII'],digits = 1), cex=notecex,col=cols[2],pos=1)
  mtext("TCDI",side=1, line = 0, cex=axslabcex)
  text(0.39,45,paste0('a'),col='black',cex=panelcex,pos=4, font=2)  
  }
  
  #current
  if(TRUE){
  bp <- barplot(as.matrix(rev(cb[,c('frq_pct')])),col=adjustcolor(cols,alpha.f = 0.2),space=0.9,border=cols,
                ylim=c(0,1),xlab="", ylab="", yaxt="n",xaxt="n")
  text(bp[1],0.95,paste0(round(cb[which(cb$cat=='h5'),'frq_pct']*100,digits = 0),'%'), cex=notecex,col=cols[4],pos=1)
  text(bp[1],0.64, paste0(round(cb[which(cb$cat=='h4'),'frq_pct']*100,digits = 0),'%'), cex=notecex,col=cols[3],pos=1)
  text(bp[1],0.22,paste0(round(cb[which(cb$cat=='h3'),'frq_pct']*100,digits = 0),'%'), cex=notecex,col=cols[2],pos=1)
  mtext("Frequency",side=1, line = 0, cex=axslabcex)
  text(0.69,1.06,paste0('b'),col='black',cex=panelcex,pos=4, font=2)  
  
  
  bp <- barplot(as.matrix(rev(cb[,c('rsk_pct')])),col=adjustcolor(cols,alpha.f = 0.2),space=0.9,border=cols,
                xlab="", ylab="", yaxt="n",xaxt="n")  
  mtext("Risk", side=1, line = 0,cex=axslabcex,las=1)
  text(bp[1],0.85,paste0(round(cb[which(cb$cat=='h5'),'rsk_pct']*100,digits = 0),'%'), cex=notecex,col=cols[4],pos=1)
  text(bp[1],0.35, paste0(round(cb[which(cb$cat=='h4'),'rsk_pct']*100,digits = 0),'%'), cex=notecex,col=cols[3],pos=1)
  text(bp[1],0.06,paste0(round(cb[which(cb$cat=='h3'),'rsk_pct']*100,digits = 0),'%'), cex=notecex,col=cols[2],pos=1)
  }
   
  #future
  if(TRUE){
  bp <- barplot(as.matrix(rev(cb[,c('frqFtr_pct')])),col=adjustcolor(cols,alpha.f = 0.2),space=0.9,border=cols,
                ylim=c(0,sum(cb$frq)/sum(cb$frqFtr)),xlab="", ylab="", yaxt="n",xaxt="n")  
  text(bp[1],0.95,paste0(round(cb[which(cb$cat=='h5'),'frqFtr_pct']*100,digits = 0),'%'), cex=notecex,col=cols[4],pos=1)
  text(bp[1],0.6, paste0(round(cb[which(cb$cat=='h4'),'frqFtr_pct']*100,digits = 0),'%'), cex=notecex,col=cols[3],pos=1)
  text(bp[1],0.2,paste0(round(cb[which(cb$cat=='h3'),'frqFtr_pct']*100,digits = 0),'%'), cex=notecex,col=cols[2],pos=1)
  text(bp[1],1.1,paste0('–',round(-(1-sum(cb$frq)/sum(cb$frqFtr))*100,digits = 0),'%'), cex=notecex,col='black',pos=1)
  mtext("Frequency (+2C)", side=1, line = 0,cex=axslabcex,las=1)
  text(0.69,1.08,paste0('c'),col='black',cex=panelcex,pos=4, font=2)  
  
  bp <- barplot(as.matrix(rev(cb[,c('rskFtr_pct')])),col=adjustcolor(cols,alpha.f = 0.2),space=0.9,border=cols,
                ylim=c(0,sum(cb$rsk)/sum(cb$rskFtr)),xlab="", ylab="", yaxt="n",xaxt="n")  
  text(bp[1],0.84,paste0(round(cb[which(cb$cat=='h5'),'rskFtr_pct']*100,digits = 0),'%'), cex=notecex,col=cols[4],pos=1)
  text(bp[1],0.3, paste0(round(cb[which(cb$cat=='h4'),'rskFtr_pct']*100,digits = 0),'%'), cex=notecex,col=cols[3],pos=1)
  text(bp[1],0.06,paste0('2%'), cex=notecex,col=cols[2],pos=1)
  #text(bp[1],0.06,paste0(round(cb[which(cb$cat=='h3'),'rskFtr_pct']*100,digits = 0),'%'), cex=notecex,col=cols[2],pos=1)
  #text(bp[1],0.06,paste0(round((1-sum(cb$frq)/sum(cb$frqFtr))*100,digits = 0),'%'), cex=notecex,col=cols[2],pos=1)
  text(bp[1],1.09,paste0('+',round((1-sum(cb$rsk)/sum(cb$rskFtr))*100,digits = 0),'%'), cex=notecex,col='black',pos=1)
  mtext("Risk (+2C)", side=1, line = 0,cex=axslabcex,las=1)
  }
  dev.off()
}

if(FALSE){
tiff(file = paste0(wd,'result/figure/','Figure3TCIIFrqRisk2.tif'), width = 89, height = 70, units = 'mm', res=300) 
par(mfrow=c(1,2),oma=c(1,0.5,1,0),mar=c(0.5,0.5,0,2.2),xpd=TRUE) 
layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
  
par(lwd=1)
bp <-barplot(as.matrix(rev(cb[,c('TCII')])),col=adjustcolor(cols,alpha.f = 0.2),space=0.6,border=cols,
#             ylim=c(0,1),
             xlab="", ylab="", yaxt="n",xaxt="n")
text(1.1,22,round(cb[which(cb$cat=='h5'),'TCII'],digits = 1), cex=notecex,col=cols[3],pos=1)
text(1.1,6, round(cb[which(cb$cat=='h4'),'TCII'],digits = 1), cex=notecex,col=cols[2],pos=1)
text(1.1,1.8,round(cb[which(cb$cat=='h3'),'TCII'],digits = 1), cex=notecex,col=cols[1],pos=1)
mtext("TCII",side=1, line = 0, cex=axslabcex)
#text(0.39,33,paste0('a'),col='black',cex=panelcex,pos=4, font=2)  

bp <- barplot(as.matrix(rev(cb[,c('frq')])),col=adjustcolor(cols,alpha.f = 0.2),space=0.9,border=cols,
              xlab="", ylab="", yaxt="n",xaxt="n")
text(bp[1],102,paste0(round(cb[which(cb$cat=='h5'),'frq_pct']*100,digits = 0),'%'), cex=notecex,col=cols[3],pos=1)
text(bp[1],65, paste0(round(cb[which(cb$cat=='h4'),'frq_pct']*100,digits = 0),'%'), cex=notecex,col=cols[2],pos=1)
text(bp[1],24,paste0(round(cb[which(cb$cat=='h3'),'frq_pct']*100,digits = 0),'%'), cex=notecex,col=cols[1],pos=1)
mtext("Frequence",side=1, line = 0, cex=axslabcex)
#text(0.69,105,paste0('b'),col='black',cex=panelcex,pos=4, font=2)  

bp <- barplot(as.matrix(rev(cb[,c('rsk')])),col=adjustcolor(cols,alpha.f = 0.2),space=0.9,border=cols,
              xlab="", ylab="", yaxt="n",xaxt="n")  
mtext("Risk", side=1, line = 0,cex=axslabcex,las=1)
text(bp[1],100,paste0(round(cb[which(cb$cat=='h5'),'rsk_pct']*100,digits = 0),'%'), cex=notecex,col=cols[3],pos=1)
text(bp[1],38, paste0(round(cb[which(cb$cat=='h4'),'rsk_pct']*100,digits = 0),'%'), cex=notecex,col=cols[2],pos=1)
text(bp[1],7,paste0(round(cb[which(cb$cat=='h3'),'rsk_pct']*100,digits = 0),'%'), cex=notecex,col=cols[1],pos=1)
#text(0.68,128,paste0('c'),col='black',cex=panelcex,pos=4, font=2)  

dev.off()
}
##############
