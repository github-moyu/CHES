rm(list = ls())
library(dplyr)

wd <- paste0("C:/Users/Y/Desktop/CHES/hurricane/result/")

file <- list.files(pattern='extent.csv',paste0(wd,'test/'))
data0 <- read.csv(paste0(wd,'test/',file),stringsAsFactors = FALSE)

files <- list.files(pattern='anova',paste0(wd,'test/SD010/'))
all <- data.frame()
for (file in files){
  data <- read.csv(paste0(wd,'test/SD010/',file),stringsAsFactors = FALSE)
  all <- rbind(all,data)
} 

filesCb <- list.files(pattern='Cb.csv',paste0(wd,'test/'))
allCb <- data.frame()
for (file in filesCb[2:4]){
data <- read.csv(paste0(wd,'test/',file),stringsAsFactors = FALSE)
allCb <- rbind(allCb,data)
} 

#get data from this study
if(TRUE){
#EVI
all_agg_EVI <- data.frame(matrix(nrow=3,ncol=3))
colnames(all_agg_EVI) <- c('Cat','Mean','SD')
all_agg_EVI$Cat <- c('h5','h4','h3')
for (cat in c('h5','h4','h3')){
#cat <- 'h5'
flt <- all %>%
      filter(Time=='HurricaneSeasonRlt' & ImpactOverTimeCon==1 & Category==cat & Variable=='EVIRAnml')
maxInt <- abs(as.numeric(flt[which(flt$Median_dfc==min(flt$Median_dfc, na.rm = TRUE)),'Mean_dfc']))
sd <- flt[which(flt$Median_dfc==min(flt$Median_dfc, na.rm = TRUE)),'SD']
all_agg_EVI[which(all_agg_EVI$Cat==cat),'Mean'] <- as.numeric(maxInt)
all_agg_EVI[which(all_agg_EVI$Cat==cat),'SD'] <- as.numeric(sd)
}#loop for cat
all_agg_EVI <- all_agg_EVI[order(all_agg_EVI$Cat,decreasing = TRUE),]
  
#area
all_agg_area <- data.frame(matrix(nrow=3,ncol=3))
colnames(all_agg_area) <- c('Cat','Mean','SD')
all_agg_area$Cat <- c('h5','h4','h3')
for (cat in c('h5','h4','h3')){
#cat <- 'h5'
flt <- all %>%
      filter(Time=='HurricaneSeasonRlt' & ImpactOverTimeCon==1 & Category==cat & Variable=='totalAreaRAnml')
maxInt <- abs(as.numeric(flt[which(flt$Median_dfc==min(flt$Median_dfc, na.rm = TRUE)),'Mean_dfc']))
sd <- flt[which(flt$Median_dfc==min(flt$Median_dfc, na.rm = TRUE)),'SD']
all_agg_area[which(all_agg_area$Cat==cat),'Mean'] <- as.numeric(maxInt)
all_agg_area[which(all_agg_area$Cat==cat),'SD'] <- as.numeric(sd)
}#loop for cat
all_agg_area <- all_agg_area[order(all_agg_area$Cat,decreasing = TRUE),]  

#extent
all_agg_ext <- data.frame(matrix(nrow=3,ncol=3))
colnames(all_agg_ext) <- c('Cat','Mean','SD')
all_agg_ext$Cat <- c('h5','h4','h3')
for (cat in c('h5','h4','h3')){
  flt <- allCb %>%
    filter(Time=='HurricaneSeasonRlt' & ImpOverTimeDistCon==1 & Category==cat)
  wid <- flt[which(flt$Distance==max(flt$Distance, na.rm = TRUE)),]
  wid <- as.numeric(wid[1,'Distance'])
  all_agg_ext[which(all_agg_ext$Cat==cat),'Mean'] <- as.numeric(wid)
all_agg_ext[which(all_agg_ext$Cat==cat),'SD'] <- 5
}#loop for cat
all_agg_ext <- all_agg_ext[order(all_agg_ext$Cat, decreasing = TRUE),]


#duration
all_agg_dur <- data.frame(matrix(nrow=3,ncol=3))
colnames(all_agg_dur) <- c('Cat','Mean','SD')
all_agg_dur$Cat <- c('h5','h4','h3')
for (cat in c('h5','h4','h3')){
#cat <- 'h5'
flt <- allCb %>%
       filter(Time=='HurricaneSeasonRlt' & ImpOverTimeDistCon==1 & Category==cat) %>%
       group_by(Distance) %>%
       tally()
len <- as.numeric(max(flt$n))
all_agg_dur[which(all_agg_dur$Cat==cat),'Mean'] <- as.numeric(len)*3-1.5
all_agg_dur[which(all_agg_dur$Cat==cat),'SD'] <- 1.5
}#loop for cat
all_agg_dur <- all_agg_dur[order(all_agg_dur$Cat, decreasing = TRUE),]
}
 

#get data from liturature 
if(TRUE){
flt0_EVI <- data0 %>%
    filter(Var=='Biomass')
flt0_area <- data0 %>%
    filter(Var=='Area')
flt0_ext <- data0 %>%
    filter(Var=='Extent')
flt0_dur <- data0 %>%
    filter(Var=='Duration')
}


cats <- c(5,4,3)
cols <- c(rgb(62,84,150, maxColorValue = 255),
          rgb(10,144,134,maxColorValue = 255),
          rgb(0.8,0.1,0.1))

#with literature
if (TRUE) {
tiff(file = paste0(wd,'graph/','Figure2WithLit2.tif'), width = 89, height = 100, units = 'mm', res=300) 
par(oma=c(1,2,0.5,0.25),par(mar=c(1.5,1,1,0)),xpd=NA) 
layout(matrix(c(1,2,3,4), 4, 1, byrow = TRUE),widths=c(1,1,1,1), heights=c(1))

#EVI
if(TRUE){  
plot(all_agg_EVI$Mean,c(3,2,1),col=rev(cols),pch=16,cex=1.3,
       ylim=c(0.5,3.5),xlim=c(0,1),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
arrows((all_agg_EVI$Mean-all_agg_EVI$SD),c(3,2,1),(all_agg_EVI$Mean+all_agg_EVI$SD),c(3,2,1),
         length=0.05,angle=90,code=3,lwd=1,col=rev(cols))  

for(r in 1:nrow(flt0_EVI)){
if(flt0_EVI$Min[r]==flt0_EVI$Max[r]){
points(flt0_EVI$Min[r]/100,flt0_EVI$Cat[r]-2,pch=16,lwd=2,cex=1.9,col=adjustcolor(cols[flt0_EVI$Cat[r]-2],alpha.f = 0.2))}
if(flt0_EVI$Min[r]!=flt0_EVI$Max[r]){
arrows(flt0_EVI$Min[r]/100,flt0_EVI$Cat[r]-2,flt0_EVI$Max[r]/100,flt0_EVI$Cat[r]-2,
       length=0.00,angle=90,code=3,lwd=10,col=adjustcolor(cols[flt0_EVI$Cat[r]-2],alpha.f = 0.2)) }
}

axis(2,at=seq(1,3,1),labels=FALSE,tck=-0.08,col='black',col.ticks='black', col.axis='black')
axis(2,at=seq(1,3,1), labels=c('H3','H4','H5'), lwd=0, line=-0.5,col.axis='black',cex.axis=0.8)
mtext('Canopy', side=2, line=1.5, col='black', cex=0.55)
axis(1,at=seq(0,1,0.2),labels=FALSE,tck=-0.07,col='black',col.ticks='black', col.axis='black')
axis(1,at=seq(0,1,0.2), labels=paste0('-',seq(0,100,20)), lwd=0, line=-0.7,col.axis='black',cex.axis=0.8)
mtext('Relative Change (%)', side=1, line=1.2, col='black', cex=0.55)
text(-0.18,4,paste0('a'),col='black',cex=1,pos=4, font=2)
}

#area
if(TRUE){  
plot(all_agg_area$Mean,c(3,2,1),col=rev(cols),pch=16, cex=1.3,
       ylim=c(0.5,3.5),xlim=c(0,1),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
arrows((all_agg_area$Mean-all_agg_area$SD),c(3,2,1),(all_agg_area$Mean+all_agg_area$SD),c(3,2,1),
         length=0.05,angle=90,code=3,lwd=1,col=rev(cols))  
  
for(r in 1:nrow(flt0_area)){ 
if(flt0_area$Min[r]==flt0_area$Max[r]){
points(flt0_area$Min[r]/100,flt0_area$Cat[r]-2,pch=16,lwd=2,cex=1.9,col=adjustcolor(cols[flt0_area$Cat[r]-2],alpha.f = 0.2))}
if(flt0_area$Min[r]!=flt0_area$Max[r]){
arrows(flt0_area$Min[r]/100,flt0_area$Cat[r]-2,flt0_area$Max[r]/100,flt0_area$Cat[r]-2,
length=0.00,angle=90,code=3,lwd=10,col=adjustcolor(cols[flt0_area$Cat[r]-2],alpha.f = 0.2)) }
}
  
axis(2,at=seq(1,3,1),labels=FALSE,tck=-0.06,col='black',col.ticks='black', col.axis='black')
axis(2,at=seq(1,3,1), labels=c('H3','H4','H5'), lwd=0, line=-0.5,col.axis='black',cex.axis=0.8)
mtext('Area', side=2, line=1.5, col='black', cex=0.55)
axis(1,at=seq(0,1,0.2),labels=FALSE,tck=-0.07,col='black',col.ticks='black', col.axis='black')
axis(1,at=seq(0,1,0.2), labels=paste0('-',seq(0,100,20)), lwd=0, line=-0.7,col.axis='black',cex.axis=0.8)
mtext('Raletive Change (%)', side=1, line=1.2, col='black', cex=0.55)
text(-0.18,4,paste0('b'),col='black',cex=1,pos=4, font=2)
}

#duration
if(TRUE){  
  plot(all_agg_dur$Mean,c(3,2,1),col=rev(cols),pch=16, cex=1.3,
       ylim=c(0.5,3.5),xlim=c(0,120),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  arrows((all_agg_dur$Mean-all_agg_dur$SD),c(3,2,1),(all_agg_dur$Mean+all_agg_dur$SD),c(3,2,1),
         length=0.05,angle=90,code=3,lwd=1,col=rev(cols))  
  
  for(r in 1:nrow(flt0_dur)){ 
    if(flt0_dur$Min[r]==flt0_dur$Max[r]){
      points(flt0_dur$Min[r],flt0_dur$Cat[r]-2,pch=16,lwd=2,cex=1.9,col=adjustcolor(cols[flt0_dur$Cat[r]-2],alpha.f = 0.2))}
    if(flt0_dur$Min[r]!=flt0_dur$Max[r]){
      arrows(flt0_dur$Min[r],flt0_dur$Cat[r]-2,flt0_dur$Max[r],flt0_dur$Cat[r]-2,
             length=0.00,angle=90,code=3,lwd=10,col=adjustcolor(cols[flt0_dur$Cat[r]-2],alpha.f = 0.2)) }
  }
  
  axis(2,at=seq(1,3,1),labels=FALSE,tck=-0.06,col='black',col.ticks='black', col.axis='black')
  axis(2,at=seq(1,3,1), labels=c('H3','H4','H5'), lwd=0, line=-0.5,col.axis='black',cex.axis=0.8)
  mtext('Duration', side=2, line=1.5, col='black', cex=0.55)
  axis(1,at=seq(0,120,12),labels=FALSE,tck=-0.07,col='black',col.ticks='black', col.axis='black')
  axis(1,at=seq(0,120,12), labels=seq(0,10,1), lwd=0, line=-0.7,col.axis='black',cex.axis=0.8)
  mtext('Duration (yr)', side=1, line=1.2, col='black', cex=0.55)  
  text(-21.6,4,paste0('c'),col='black',cex=1,pos=4, font=2) 
  }

#extent
if(TRUE){  
plot(all_agg_ext$Mean,c(3,2,1),col=rev(cols),pch=16, cex=1.3,
     ylim=c(0.5,3.5),xlim=c(0,100),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
arrows((all_agg_ext$Mean-all_agg_ext$SD),c(3,2,1),(all_agg_ext$Mean+all_agg_ext$SD),c(3,2,1),
         length=0.05,angle=90,code=3,lwd=1,col=rev(cols))  
  
for(r in 1:nrow(flt0_ext)){
if(flt0_ext$Min[r]==flt0_ext$Max[r]){
      points(flt0_ext$Min[r],flt0_ext$Cat[r]-2,pch=16,lwd=2,cex=1.9,col=adjustcolor(cols[flt0_ext$Cat[r]-2],alpha.f = 0.2))}
if(flt0_ext$Min[r]!=flt0_ext$Max[r]){
      arrows(flt0_ext$Min[r],flt0_ext$Cat[r]-2,flt0_ext$Max[r],flt0_ext$Cat[r]-2,
             length=0.00,angle=90,code=3,lwd=10,col=adjustcolor(cols[flt0_ext$Cat[r]-2],alpha.f = 0.2)) }
}
  
axis(2,at=seq(1,3,1),labels=FALSE,tck=-0.06,col='black',col.ticks='black', col.axis='black')
axis(2,at=seq(1,3,1), labels=c('H3','H4','H5'), lwd=0, line=-0.5,col.axis='black',cex.axis=0.8)
mtext('Width', side=2, line=1.5, col='black', cex=0.55)
axis(1,at=seq(0,100,20),labels=FALSE,tck=-0.07,col='black',col.ticks='black', col.axis='black')
axis(1,at=seq(0,100,20), labels=seq(0,100,20), lwd=0, line=-0.7,col.axis='black',cex.axis=0.8)
mtext('Distance from Track (km)', side=1, line=1.2, col='black', cex=0.55)  
text(-18,4,paste0('d'),col='black',cex=1,pos=4, font=2)
}

dev.off()
}#plot
