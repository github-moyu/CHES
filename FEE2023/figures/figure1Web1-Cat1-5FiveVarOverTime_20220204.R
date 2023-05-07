#For guidance, Nature's standard figure sizes are 89 mm wide (single column) and 183 mm wide (double column). 
#The full depth of a Nature page is 247 mm. Figures can also be a column-and-a-half where necessary (120-136 mm).

###5 variables mean SD over time 10km (long graph)
#############
rm(list = ls())
library(dplyr)

wd <- paste0("C:/Users/Y/Desktop/CHES/TCMangrove/result/")

sd <- '10'
files <- list.files(pattern='anova',paste0(wd,'test/SD0',sd,'/'))

all <- data.frame()
for (file in files){
  #file <- files[1] 
  data <- read.csv(paste0(wd,'test/SD0',sd,'/',file))
  all <- rbind(all,data)
}  

unique(all$Variable)
cats <- c('h5','h4','h3','h2','h1')
vars <- c('EVIRAnml', 'totalAreaRAnml','meanPatchRAnml','maxPatchRAnml',  'numberPatchesRAnml')
ylbs <-c ('EVI', 'Total Area', 'Avg. Patch Area', 'Lrgst. Patch Area',  'No. of Patches')
ylbspos <- c(0.55,0.55,0.05,0.05,2.95)
pnl <- letters[1:25]
cols <- c(rgb(0.8,0.1,0.1),
          rgb(10,144,134,maxColorValue = 255),
          rgb(62,84,150, maxColorValue = 255),
          rgb(233,131,0, maxColorValue = 255),
          rgb(239,189,71, maxColorValue = 255))

cex_tck <- 0.9
cex_lbl <- 0.65
cex_pnl <- 1.1
cex_pnl2 <- 1


#figure 1
if (TRUE) {
  tiff(file = paste0(wd,'figure/','Figure1ah3-5ThreeVarOverTime2.tif'), width = 89, height = 80, units = 'mm', res=300) 
  par(oma=c(2,1.5,1,0.5),mar=c(0.5,1.5,1,0.5),xpd=TRUE) 
  layout(matrix(seq(1,9,1),3, 3, byrow = FALSE),heights=c(1,1,1))
  for (c in 1:3){
    #cat<-'h5'  
    cat <- cats[c]  
    for (v in 1:3){
      #v <- 2
      #set range
      if(TRUE){
      if (v %in% 1){
      ylm <- c(0.7,1.05)
      ytck <- c(0.7,0.9,1)
      ytcklbl <- c(0.7,0.9,1)}
      if (v %in% 2){
      ylm <- c(0.4,1.05)
      ytck <- c(0.5,0.8,1)
      ytcklbl <- c(0,0.5,0.8,1)}
      if (v %in% 3){
      ylm <- c(0.4,1.05)
      ytck <- c(0.5,0.8,1)
      ytcklbl <-c(0.5,0.8,1)}
      }
      
      ###plot
      if(TRUE){
      #get data
      if(TRUE){
      data_plot <- all %>%
        filter(Time=='HurricaneSeasonRlt' & Variable== vars[v] & Distance == 10 & Category == cat)
      if(cat == 'h3'){
        data_plot <- all %>%
          filter(Time=='HurricaneSeasonRlt' & Variable== vars[v] & Distance == 30 & Category == cat)}
      }
      #set frame
      if(TRUE){
      plot(data_plot$Level,rep(1,nrow(data_plot)),xlim=c(-4,8),ylim=ylm,col='white',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
      axis(2,at=ytck,labels=FALSE,tck=-0.04,)
      arrows(rep(-4,length(ytcklbl)),ytcklbl,rep(8,length(ytcklbl)),ytcklbl,col='grey',lty=3,length=0)
      
      
      if(cat=='h5'){
      axis(2,at=ytcklbl,labels=ytcklbl,line=-0.3,lwd=0,cex.axis=cex_tck) 
      mtext(ylbs[v], side=2, line=1.7,  cex=cex_lbl)}
      
      if(v %in% 3){
      axis(1,at=c(-4,0,4,8),labels=FALSE,tck=-0.04,pos=0.4)
      axis(1,at=c(-4,0,4,8),labels=c('–1',0,1,2),lwd=0,line=-1,cex.axis=cex_tck)
      mtext('Year', side=1, line=0.8, cex=cex_lbl)
      }
      
      if (v==1 & c ==1){text(-6,1.15,paste0('a'),cex=cex_pnl,pos=1,font = 2)}
      if(ylbs[v]=='EVI'){text(2,1.15,paste0('Cat. ',6-c),cex=cex_pnl2,pos=1,font = 2)}
      }
      #plot
      if(TRUE){
        with(data_plot[which(data_plot$ImpactOverTimeDistCon==1),],arrows((Level),(Mean+SD),(Level),(Mean-SD),
                                                                          length=0.00,angle=90,code=3,lwd=2,
                                                                          col=adjustcolor(cols[c],alpha.f = 0.3)))
        with(data_plot[which(data_plot$ImpactOverTimeDistCon==1),],points(Level,Mean,pch=20,type='p',lwd=2,cex=1,col=cols[c],
                                                                          bg=adjustcolor(cols[c],alpha.f = 0.3)))
        
        with(data_plot[which(data_plot$ImpactOverTimeDistCon==0),],arrows((Level),(Mean+SD),(Level),(Mean-SD),
                                                                          length=0.00,angle=90,code=3,lwd=2,
                                                                          col=adjustcolor(cols[c],alpha.f = 0.3)))
        with(data_plot[which(data_plot$ImpactOverTimeDistCon==0),],points(Level,Mean,pch='-',type='p',lwd=1,cex=1.2,
                                                                          col=adjustcolor(cols[c],alpha.f=1)))
      }
      }
    }
  }  
  dev.off()
}#plot


#figure S1
if (TRUE) {
tiff(file = paste0(wd,'figure/','FigureS1h1-5FiveVarOverTime2.tif'), width = 183, height = 120, units = 'mm', res=300) 
  par(oma=c(2,1.5,1,0.5),mar=c(0.5,1.5,1.5,0.5),xpd=TRUE)
layout(matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25), 
                5, 5, byrow = FALSE),heights=c(1,1,1,1,1))
for (c in 1:length(cats)){
#cat<-'h5'  
cat <- cats[c]  
for (v in 1:length(vars)){
#v <- 2
  if (v %in% 1){
    ylm <- c(0.7,1.)
    ytck <- c(0.7,0.9,1)
    ytcklbl <- c(0.7,0.9,1)}
  if (v %in% 2:3){
    ylm <- c(0.5,1.05)
    ytck <- c(0.5,0.8,1)
    ytcklbl <- c(0.5,0.8,1)}
  if (v %in% 4){
    ylm <- c(0.5,1.05)
    ytck <- c(0.5,0.8,1)
    ytcklbl <- c(0.5,0.8,1)}
  if(v %in% length(vars)){
    ylm <- c(0.7,1.4)
    ytck <- c(1,1.5)
    ytcklbl <- c(1,1.5)}
    
###over time
data_plot <- all %>%
             filter(Time=='HurricaneSeasonRlt' & Variable== vars[v] & Distance == 10 & Category == cat)

if(cat == 'h3'){
data_plot <- all %>%
  filter(Time=='HurricaneSeasonRlt' & Variable== vars[v] & Distance == 30 & Category == cat)}
    
plot(data_plot$Level,rep(1,nrow(data_plot)),xlim=c(-4,8),ylim=ylm,col='white',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
axis(2,at=ytck,labels=FALSE,tck=-0.04)
if(cat=='h5'){
axis(2,at=ytck, labels=ytcklbl, lwd=0, line=-0.3,cex.axis=cex_tck)
mtext(ylbs[v], side=2, line=1.7, cex=cex_lbl)}

arrows(rep(-4,length(ytcklbl)),ytcklbl,rep(8,length(ytcklbl)),ytcklbl,col='grey',lty=3,length=0)


if(v %in% length(vars)){
axis(1,at=c(-4,0,4,8),labels=FALSE,tck=-0.04,pos=0.7)
axis(1,at=c(-4,0,4,8),labels=c('–1',0,1,2),lwd=0,line=-1,cex.axis=cex_tck)
mtext('Year', side=1, line=0.8, cex=cex_lbl)
}
    
if (v==1 & c ==1){text(-6,1.15,paste0('a'),cex=cex_pnl,pos=1,font = 2)}
if(ylbs[v]=='EVI'){text(1.5,1.15,paste0('Cat. ',6-c),cex=cex_pnl2,pos=1,font = 2)}

#plot
if(TRUE){
with(data_plot[which(data_plot$ImpactOverTimeDistCon==1),],arrows((Level),(Mean+SD),(Level),(Mean-SD),
                                                              length=0.00,angle=90,code=3,lwd=2,
                                                              col=adjustcolor(cols[c],alpha.f = 0.3)))
with(data_plot[which(data_plot$ImpactOverTimeDistCon==1),],points(Level,Mean,pch=20,type='p',lwd=2,cex=1,col=cols[c],
                                                            bg=adjustcolor(cols[c],alpha.f = 0.3)))

with(data_plot[which(data_plot$ImpactOverTimeDistCon==0),],arrows((Level),(Mean+SD),(Level),(Mean-SD),
                                                              length=0.00,angle=90,code=3,lwd=2,
                                                              col=adjustcolor(cols[c],alpha.f = 0.3)))
with(data_plot[which(data_plot$ImpactOverTimeDistCon==0),],points(Level,Mean,pch='-',type='p',lwd=1,cex=1.2,
                                                              col=adjustcolor(cols[c],alpha.f=1)))
}
}
}  
dev.off()
}#plot

#output table s1
if(TRUE){
  output_all <- data.frame()  
  for (v in 1:length(vars)){  
  #v<-1  
    output_cat_all <- data.frame(Variable=rep(vars[v],10),Level=seq(-4,15,1))  
    for (c in 1:length(cats)){
      #c <-1  
      cat <- cats[c]  
      
      data_plot <- all %>%
        filter(Time=='HurricaneSeasonRlt' & Variable== vars[v] & Distance == 10 & Category == cat)
      
      if(cat == 'h3'){
        data_plot <- all %>%
          filter(Time=='HurricaneSeasonRlt' & Variable== vars[v] & Distance == 30 & Category == cat)}
      
      output <- data_plot[,c("Variable","Level", "Group",  "Mean","SD","n")]
      output[,c("Mean","SD")] <- round(output[,c("Mean","SD")],digits = 2)
      colnames(output)[3:6] <-paste0(rep(cat,4),'-',c("Group",  "Mean","SD","n")) 
      output_cat_all<- merge(output_cat_all,output,by=c('Level',"Variable"),all=TRUE)
      output_cat_all<-output_cat_all[order(output_cat_all$Level),]
    }#loop cat
    
    output_all <- rbind(output_all,output_cat_all)
  }#loop var
  
  output_all$Variable <- substr(output_all$Variable,1,nchar(output_all$Variable)-5)
  
  write.csv(output_all,paste0(wd,'table/TableS1Time.csv'),row.names = FALSE)
  
}  
################