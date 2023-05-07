#For guidance, Nature's standard figure sizes are 89 mm wide (single column) and 183 mm wide (double column). 
#The full depth of a Nature page is 247 mm. Figures can also be a column-and-a-half where necessary (120-136 mm).

###
#############
rm(list = ls())
library(dplyr)

wd <- "C:/Users/Y/Desktop/CHES/TCMangrove/"

track.file<-"all_formatted.csv"
track.data<-read.csv(paste0(wd,'data/ibtracs/',track.file),stringsAsFactors = FALSE)

dataAll <- data.frame()

for (id.file in c('h5','h4','h3','h2','h1')){
  #id.file<-'h5'
  id.data<-read.csv(paste0(wd,'data/ibtracs/',id.file,'ID.csv'),stringsAsFactors = FALSE)
  id.data2001 <- id.data[which(id.data$Year>=2001),]
  id.data2001$cat <-substr(id.file,2,2)
  id.data2001$landingWind <- NA 
  
  for (row in 1:nrow(id.data2001)){
    #row <- 5
    id <- id.data[row,'ID']
    nm <- id.data[row,'Name']  
    yr <- id.data[row,'Year']  
    
    track <- track.data[which(track.data$SID ==id),]  
    
    id.data2001[row,'landingWind'] <- track[which(track$LANDFALL ==0),][1,'WMO_WIND'] 
    id.data2001[row,'landingLat'] <- track[which(track$LANDFALL ==0),][1,'LAT'] 
  }#hurricane
  
  dataAll <- rbind(dataAll,id.data2001)  
  
} #category

dataAll <- dataAll[which(!is.na(dataAll$landingWind)),]
#dataAll <- dataAll[which(dataAll$landingLat<=30 & dataAll$landingLat>=-30),]

if(FALSE){
agg <- dataAll %>%
       group_by(cat) %>%
       summarise(mean = mean(landingWind,na.rm = TRUE),
                 median = median(landingWind,na.rm = TRUE),
                SD = sd(landingWind,na.rm = TRUE),
                q25 = quantile(landingWind,0.25,na.rm = TRUE),
                q75 = quantile(landingWind,0.75,na.rm = TRUE),.groups = 'drop')

agg2 <- dataAll %>%
  filter (landingWind > 50) %>%
  group_by(cat) %>%
  summarise(mean = mean(landingWind,na.rm = TRUE),
            median = median(landingWind,na.rm = TRUE),
            SD = sd(landingWind,na.rm = TRUE),
            q25 = quantile(landingWind,0.25,na.rm = TRUE),
            q75 = quantile(landingWind,0.75,na.rm = TRUE),.groups = 'drop')


cats <- seq(1,5,1)
xlbs <- c('Cat 1','Cat 2','Cat 3','Cat 4','Cat 5')
#ylbspos <- rep(0.6,4)
#pnl <- letters[1:4]
cols <- c(rgb(239,189,71, maxColorValue = 255),
          rgb(233,131,0, maxColorValue = 255),
          rgb(62,84,150, maxColorValue = 255),
          rgb(10,144,134,maxColorValue = 255),
          rgb(0.8,0.1,0.1))

dataAll$col <- NA
for (c in 1:length(cols)){
  dataAll[which(dataAll$cat==c),'col'] <- cols[c]
}
}

#with literature
if (TRUE) {
tiff(file = paste0(wd,'result/figure/','FigureXlandingWindByCat2.tif'), width = 89, height = 65, units = 'mm', res=300) 
par(oma=c(1,2,0,1),par(mar=c(0,1,0.5,0.5)),xpd=NA) 


 boxplot(landingWind~cat,data=dataAll,cex=1,frame=F,
       ylim=c(0,150),xlim=c(1,5),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  #plot(dataAll$cat,dataAll$landingWind,col=adjustcolor(dataAll$col,alpha.f = 0.05),pch=16,cex=1.5,
  #     ylim=c(0,150),xlim=c(1,5),xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  
  #points(seq(1,5,1),agg$median,col=cols, pch='-', cex=2.5)
  #arrows(seq(1,5,1),(agg$q25),seq(1,5,1),(agg$q75),
  #       length=0.05,angle=90,code=3,lwd=1,col=cols)  
  
  #arrows(1,75,5,75,length=0,lty=2)
  
  axis(2,at=seq(0,150,25),labels=FALSE,tck=-0.03,col='black',col.ticks='black', col.axis='black',pos=0.5)
  axis(2,at=seq(0,150,25), labels=seq(0,150,25), lwd=0, line=0.3,col.axis='black',cex.axis=0.8)
  mtext('Wind speed (km/h)', side=2, line=2, col='black', cex=0.8)
  axis(1,at=seq(1,5,1),labels=FALSE,tck=-0.03,col='black',col.ticks='black', col.axis='black', pos=0.57)
  axis(1,at=seq(1,5,1), labels=paste0('Cat. ',seq(1,5,1)), lwd=0, line=-1.3,col.axis='black',cex.axis=0.8)
#  mtext('Relative Change (%)', side=1, line=1.3, col='black', cex=0.55)
#  text(-0.18,3.5,paste0('a'),col='black',cex=1,pos=4, font=2)

dev.off()
}#plot
################
