###filter by normal previous year
#############
if(TRUE){
rm(list = ls())
library(dplyr)
  
wd <- paste0("C:/Users/Y/Desktop/CHES/hurricane/result/")

treshold_nml <- 0.15
treshold_area <- 16

for (treshold_nml in seq(0.10,0.14,0.01)){

for (cat in c('h5','h4','h3','h2','h1')){
#cat <- 'h5'
info <- read.csv(paste0('C:/Users/Y/Desktop/CHES/hurricane/data/ibtracs/byCategory/',cat,'ID.csv'),stringsAsFactors = FALSE)
info$Basin[is.na(info$Basin)] <- 'Nat'

files<-list.files(pattern='.csv',paste0(wd,'formatted/',cat,'/'))
hnames <- substr(files,1,nchar(files)-4)

data_f <- read.csv(paste0(wd,'formatted/',cat,'/',hnames[1],'.csv'),stringsAsFactors = FALSE)
dataNew <-data.frame(matrix(ncol = ncol(data_f)+20,nrow=0))
colnames(dataNew) <- c('Hurricane','HurricaneYear','HurricaneMonth','Cat','Basin','HurricaneYearRlt','HurricaneHalfYearRlt','HurricaneSeasonRlt',
                       'HurricaneMonthRlt','HurricaneDateRlt',
                       colnames(data_f),
                       c('EVIRAnml','EVIRAnml2','totalAreaRAnml','totalAreaRAnml2','numberPatchesRAnml','numberPatchesRAnml2',
                         'maxPatchRAnml','maxPatchRAnml2', 'meanPatchRAnml','meanPatchRAnml2'))

for (hn in hnames) {
data0 <- read.csv(paste0(wd,'formatted/',cat,'/',hn,'.csv'),stringsAsFactors = FALSE)
hname <- substr(hn,1,nchar(hn)-4)
hyr <- as.numeric(gsub(".*?([0-9]+).*", "\\1", hn))
hmo <- info[which(info$Name==hname & info$Year==hyr),'Month']
bsn <- info[which(info$Name==hname & info$Year==hyr),'Basin']
byr <- hyr - 1

if (bsn %in% c("WP", "EP", "Nat", "NI")){
data <- data0[which(data0$Year %in% c(byr:(hyr+3))),]

distances <- unique(data$distanceKM)
for (i in 1:length(distances)){
  data_d<- data[which(data$distanceKM==distances[i]),]
  
  #test if is normal in the year before hurricane
  data_b <- data_d[which(data_d$Year==byr),]
  if(length(unique(data_b$Month))==12){ 
  mean_b_TA <- mean(data_b[,'totalAreaRA'],na.rm = TRUE)
  if (mean_b_TA >= treshold_area){
    sd_b_TA <- sd(data_b[,'totalAreaRA'],na.rm = TRUE)
    psd_b_TA <- sd_b_TA/mean_b_TA
    
    mean_b_EVI <- mean(data_b[,'EVIRA'],na.rm = TRUE)
    sd_b_EVI <- sd(data_b[,'EVIRA'],na.rm = TRUE)
    psd_b_EVI <- sd_b_EVI/mean_b_EVI
    
    if (psd_b_EVI <= treshold_nml & psd_b_TA <= treshold_nml){
      data_d$Hurricane <- hname
      data_d$Cat <- cat
      data_d$Basin <- bsn
      data_d$HurricaneYear <- hyr 
      data_d$HurricaneMonth <- hmo 
      data_d$HurricaneYearRlt <- data_d$Year-data_d$HurricaneYear
      data_d[which(data_d$Month %in% seq(1,6,1)),'HurricaneHalfYearRlt'] <- 0
      data_d[which(data_d$Month %in% seq(7,12,1)),'HurricaneHalfYearRlt'] <- 1
      data_d$HurricaneHalfYearRlt <- data_d$Year-data_d$HurricaneYear+data_d$HurricaneHalfYearRlt/2
      data_d[which(data_d$Month %in% c(1,2,3)),'HurricaneSeasonRlt'] <- 0
      data_d[which(data_d$Month %in% c(4,5,6)),'HurricaneSeasonRlt'] <- 1
      data_d[which(data_d$Month %in% c(7,8,9)),'HurricaneSeasonRlt'] <- 2
      data_d[which(data_d$Month %in% c(10,11,12)),'HurricaneSeasonRlt'] <- 3
      data_d$HurricaneSeasonRlt <- data_d$Year-data_d$HurricaneYear+data_d$HurricaneSeasonRlt/4
      data_d$HurricaneMonthRlt <- data_d$Year-data_d$HurricaneYear+round((data_d$Month-1)/12,digits = 2)
      data_d$HurricaneDateRlt <- data_d$Date-data_d$HurricaneYear
      
      data_01 <- data_d %>% 
        group_by(HurricaneSeasonRlt)%>%
        summarize(mn=mean(EVI))
      pTime <- summary(lm(data_01$mn~data_01$HurricaneSeasonRlt))$coefficients[2,4]
      if (pTime > 0.05){
        for (var in c('EVIRA','totalAreaRA','numberPatchesRA','maxPatchRA','meanPatchRA')){
          #var <- 'EVIRA'  
          mean_before <- mean(data_b[,var],na.rm = TRUE)
          data_d[,paste0(var,'nml')] <- data_d[,var]/mean_before
          data_d[,paste0(var,'nml2')] <- data_d[,paste0(var,'nml')]
          data_d[which(data_d[,paste0(var,'nml')]>=(1-treshold_nml) & data_d[,paste0(var,'nml')]<=(1+treshold_nml)),paste0(var,'nml2')] <- 1
          plot(data_d$Date,data_d[,paste0(var,'nml')],ylim=c(0,1))
          points(data_d$Date,data_d[,paste0(var,'nml2')],col='red')
        }#variable loop  
        
        dataNew<-rbind(dataNew,data_d)
      }#if no trend over time
    }#if normal
  }#if over area treshold
  }#if full year before
}#distance loop   
}#Northern Hemisphere

if (bsn %in% c('SP','SI','SA')){
if (hmo %in% seq(1:6)){
data0$Month <- data0$Month + 7   
data0$Year[which(data0$Month>12)] <- data0$Year[which(data0$Month>12)] +1 
data0$Month[which(data0$Month>12)] <- data0$Month[which(data0$Month>12)] -12 
data0$Date <- data0$Date + 7/12 

data <- data0[which(data0$Year %in% c(byr:(hyr+3))),]
  
distances <- unique(data$distanceKM)
for (i in 1:length(distances)){
#i <- 1
data_d<- data[which(data$distanceKM==distances[i]),]
  
#test if is normal in the year before hurricane
data_b <- data_d[which(data_d$Year==byr),]

if(length(unique(data_b$Month))==12){ 

mean_b_TA <- mean(data_b[,'totalAreaRA'],na.rm = TRUE)
if (mean_b_TA >= treshold_area){
sd_b_TA <- sd(data_b[,'totalAreaRA'],na.rm = TRUE)
psd_b_TA <- sd_b_TA/mean_b_TA
    
mean_b_EVI <- mean(data_b[,'EVIRA'],na.rm = TRUE)
sd_b_EVI <- sd(data_b[,'EVIRA'],na.rm = TRUE)
psd_b_EVI <- sd_b_EVI/mean_b_EVI
#plot(data_b$EVIRA)
    
if (psd_b_EVI <= treshold_nml & psd_b_TA <= treshold_nml){
data_d$Hurricane <- hname
data_d$Cat <- cat
data_d$Basin <- bsn
data_d$HurricaneYear <- hyr 
data_d$HurricaneMonth <- hmo 
data_d$HurricaneYearRlt <- data_d$Year-data_d$HurricaneYear
data_d[which(data_d$Month %in% seq(1,6,1)),'HurricaneHalfYearRlt'] <- 0
data_d[which(data_d$Month %in% seq(7,12,1)),'HurricaneHalfYearRlt'] <- 1
data_d$HurricaneHalfYearRlt <- data_d$Year-data_d$HurricaneYear+data_d$HurricaneHalfYearRlt/2
data_d[which(data_d$Month %in% c(1,2,3)),'HurricaneSeasonRlt'] <- 0
data_d[which(data_d$Month %in% c(4,5,6)),'HurricaneSeasonRlt'] <- 1
data_d[which(data_d$Month %in% c(7,8,9)),'HurricaneSeasonRlt'] <- 2
data_d[which(data_d$Month %in% c(10,11,12)),'HurricaneSeasonRlt'] <- 3
data_d$HurricaneSeasonRlt <- data_d$Year-data_d$HurricaneYear+data_d$HurricaneSeasonRlt/4
data_d$HurricaneMonthRlt <- data_d$Year-data_d$HurricaneYear+round((data_d$Month-1)/12,digits = 2)
data_d$HurricaneDateRlt <- data_d$Date-data_d$HurricaneYear
      
data_01 <- data_d %>% 
           group_by(HurricaneSeasonRlt)%>%
           summarize(mn=mean(EVI))
pTime <- summary(lm(data_01$mn~data_01$HurricaneSeasonRlt))$coefficients[2,4]
if (pTime > 0.05){
for (var in c('EVIRA','totalAreaRA','numberPatchesRA','maxPatchRA','meanPatchRA')){
mean_before <- mean(data_b[,var],na.rm = TRUE)
data_d[,paste0(var,'nml')] <- data_d[,var]/mean_before
data_d[,paste0(var,'nml2')] <- data_d[,paste0(var,'nml')]
data_d[which(data_d[,paste0(var,'nml')]>=(1-treshold_nml) & data_d[,paste0(var,'nml')]<=(1+treshold_nml)),paste0(var,'nml2')] <- 1
plot(data_d$Date,data_d[,paste0(var,'nml')],ylim=c(0,1))
points(data_d$Date,data_d[,paste0(var,'nml2')],col='red')
}#variable loop  
        
dataNew<-rbind(dataNew,data_d)
}#if no trend over time
}#if normal
}#if over area treshold
}#if full year before
}#distance loop   
}#first half of year
  
if (hmo %in% seq(6:12)){
data_d$Month <- data_d$Month - 5   
data_d$Year[which(data_d$Month < 1)] <- data_d$Year[which(data_d$Month < 1)] - 1 
data_d$Month[which(data_d$Month < 1 )] <- data_d$Month[which(data_d$Month < 1  )] + 12 
data0$Date <- data0$Date - 5/12 

data <- data0[which(data0$Year %in% c(byr:(hyr+3))),]

distances <- unique(data$distanceKM)
for (i in 1:length(distances)){
  #i <- 1
  data_d<- data[which(data$distanceKM==distances[i]),]
  
  #test if is normal in the year before hurricane
  data_b <- data_d[which(data_d$Year==byr),]
  if(length(unique(data_b$Month))==12){ 
  mean_b_TA <- mean(data_b[,'totalAreaRA'],na.rm = TRUE)
  if (mean_b_TA >= treshold_area){
    sd_b_TA <- sd(data_b[,'totalAreaRA'],na.rm = TRUE)
    psd_b_TA <- sd_b_TA/mean_b_TA
    
    mean_b_EVI <- mean(data_b[,'EVIRA'],na.rm = TRUE)
    sd_b_EVI <- sd(data_b[,'EVIRA'],na.rm = TRUE)
    psd_b_EVI <- sd_b_EVI/mean_b_EVI
    #plot(data_b$EVIRA)
    
    if (psd_b_EVI <= treshold_nml & psd_b_TA <= treshold_nml){
      data_d$Hurricane <- hname
      data_d$Cat <- cat
      data_d$Basin <- bsn
      data_d$HurricaneYear <- hyr 
      data_d$HurricaneMonth <- hmo 
      data_d$HurricaneYearRlt <- data_d$Year-data_d$HurricaneYear
      data_d[which(data_d$Month %in% seq(1,6,1)),'HurricaneHalfYearRlt'] <- 0
      data_d[which(data_d$Month %in% seq(7,12,1)),'HurricaneHalfYearRlt'] <- 1
      data_d$HurricaneHalfYearRlt <- data_d$Year-data_d$HurricaneYear+data_d$HurricaneHalfYearRlt/2
      data_d[which(data_d$Month %in% c(1,2,3)),'HurricaneSeasonRlt'] <- 0
      data_d[which(data_d$Month %in% c(4,5,6)),'HurricaneSeasonRlt'] <- 1
      data_d[which(data_d$Month %in% c(7,8,9)),'HurricaneSeasonRlt'] <- 2
      data_d[which(data_d$Month %in% c(10,11,12)),'HurricaneSeasonRlt'] <- 3
      data_d$HurricaneSeasonRlt <- data_d$Year-data_d$HurricaneYear+data_d$HurricaneSeasonRlt/4
      data_d$HurricaneMonthRlt <- data_d$Year-data_d$HurricaneYear+round((data_d$Month-1)/12,digits = 2)
      data_d$HurricaneDateRlt <- data_d$Date-data_d$HurricaneYear
      
      data_01 <- data_d %>% 
        group_by(HurricaneSeasonRlt)%>%
        summarize(mn=mean(EVI))
      pTime <- summary(lm(data_01$mn~data_01$HurricaneSeasonRlt))$coefficients[2,4]
      if (pTime > 0.05){
        for (var in c('EVIRA','totalAreaRA','numberPatchesRA','maxPatchRA','meanPatchRA')){
          #var <- 'EVIRA'  
          mean_before <- mean(data_b[,var],na.rm = TRUE)
          data_d[,paste0(var,'nml')] <- data_d[,var]/mean_before
          data_d[,paste0(var,'nml2')] <- data_d[,paste0(var,'nml')]
          data_d[which(data_d[,paste0(var,'nml')]>=(1-treshold_nml) & data_d[,paste0(var,'nml')]<=(1+treshold_nml)),paste0(var,'nml2')] <- 1
          plot(data_d$Date,data_d[,paste0(var,'nml')],ylim=c(0,1))
          points(data_d$Date,data_d[,paste0(var,'nml2')],col='red')
        }#variable loop  
        
        dataNew<-rbind(dataNew,data_d)
      }#if no trend over time
    }#if normal
  }#if over area treshold
  }#if full year before
}#distance loop   
}#second half of year  
}#Southern Hemisphere
}#hurricane loop

dataNew <- dataNew[,c('Hurricane','HurricaneYear','HurricaneMonth','Cat','Basin','HurricaneYearRlt','HurricaneHalfYearRlt',
                      'HurricaneSeasonRlt', 'HurricaneMonthRlt','HurricaneDateRlt', 'Vegetation','distanceKM',
                      colnames(dataNew)[!(colnames(dataNew) %in% c('Hurricane','HurricaneYear','HurricaneMonth','Cat',
                                                                   'HurricaneYearRlt','Basin',
                                                                   'HurricaneHalfYearRlt','HurricaneSeasonRlt',
                                                                   'HurricaneMonthRlt','HurricaneDateRlt', 'Vegetation',
                                                                   'distanceKM'))])]

write.csv(dataNew,paste0(wd,'formatted/SD',sub('\\.','',as.character(formatC(treshold_nml, format='f', digits=2 ))),'/',
                         cat,'All.csv'),row.names = FALSE)  
}#loop for cat
}#loop for treshold nml  
}#block
#############


###ANOVA by catergory 
############
rm(list = ls())
library(dplyr)
library(agricolae)

wd <- paste0("C:/Users/Y/Desktop/CHES/hurricane/result/")

sd <- '0.05' 

for ( sd in seq(0.05,0.15,0.01)){

folder <- sub('\\.','',as.character(formatC(sd, format='f', digits=2 )))

for (cat in c('h5','h4','h3','h2','h1')){
file<-intersect(list.files(pattern='All.csv',paste0(wd,'formatted/SD',folder,'/')),
                list.files(pattern=cat,paste0(wd,'formatted/SD',folder,'/')))
data <- read.csv(paste0(wd,'formatted/SD',folder,'/',file),stringsAsFactors = FALSE)

data$distanceKM <- as.factor(data$distanceKM)
data$HurricaneYearRlt <- as.factor(data$HurricaneYearRlt)
data$HurricaneHalfYearRlt <- as.factor(data$HurricaneHalfYearRlt)
data$HurricaneSeasonRlt <- as.factor(data$HurricaneSeasonRlt)
data$HurricaneMonthRlt <- as.factor(data$HurricaneMonthRlt)

grp <- data.frame(matrix(nrow=0, ncol=18))
colnames(grp) <- c('Category','Distance','Variable','Time','Level','Group','Impact','Impact_dfc','ImpactOverTimeCon',
                   #'ImpactOverTime2','ImpactOverTime3','ImpactOverDist1','ImpactOverDist2','ImpactOverDist3',
                   'Mean','SD','SE','n','Median','q25','q75','Mean_dfc','Median_dfc')

distances <- unique(data$distanceKM)

for (var in c('EVIRAnml','totalAreaRAnml','numberPatchesRAnml','maxPatchRAnml','meanPatchRAnml')){
for (time in c('HurricaneSeasonRlt')){
alldist <- data.frame()
 
for (dist in distances){
data_test<- data[which(data$distanceKM==dist),c(var,time)]  
colnames(data_test) <- c('v','t')  
fit <- aov(v ~ t, data=data_test)
table <- HSD.test(fit, 't', group=TRUE)
output <- as.data.frame(table['groups'])
colnames(output) <- c('Variable','Group')
output$Level <- as.numeric(row.names(output))  
output$Time <- time
output$Variable <- var 
output$Distance <- dist
output$Category <- cat
output<- output[order(output$Level),]
      
#get mean and median and their deficit
data_agg <- data_test %>%
            group_by(t) %>%
            summarise(Mean = mean(v,na.rm = TRUE),
                      Median = median(v,na.rm = TRUE),
                      SD = sd(v,na.rm = TRUE),
                      q25 = quantile(v,0.25,na.rm = TRUE),
                      q75 = quantile(v,0.75,na.rm = TRUE))
data_agg$Mean_dfc <- data_agg$Mean-1
data_agg$Median_dfc <- data_agg$Median-1
     
data_count <- data_test %>%
              group_by(t) %>%
              tally()
data_agg <- merge(data_agg,data_count,by='t')
data_agg$SE <- data_agg$SD/sqrt(data_agg$n)
    
output <- merge(output,data_agg,by.x='Level',by.y='t')

#catogery impact
output$Impact <- 0  #split the time to 'normal' or 'stressed'
if (time == 'HurricaneYearRlt'){beforeRow <- 1}
if (time == 'HurricaneHalfYearRlt'){beforeRow <- 2 }
if (time == 'HurricaneSeasonRlt'){beforeRow <- 4 }
if (time == 'HurricaneMonthRlt'){beforeRow <- 12 }
letter0 <- unique(unlist(strsplit(toString(output$Group[1:beforeRow]),split='')))  #letters in the groups of the year before
letter0 <- letter0[letter0 %in% letters]
for (r in c((beforeRow+1):nrow(output))){
lt <- unlist(strsplit(toString(output$Group[r]),split=''))  
overlap <- intersect(lt,letter0)
if(length(overlap) == 0){output[r,'Impact'] <- 1} 
}#rest of the row in output

output$Impact_dfc <- 0  #correct direction of change
for (rr in c((beforeRow+1):nrow(output))){
if (output[rr,'Impact']==1 & output[rr,'Mean_dfc']<0 &
    output[rr,'Variable'] %in% c('EVIRAnml','totalAreaRAnml','maxPatchRAnml','meanPatchRAnml')){
output[rr,'Impact_dfc'] <- 1}
if (output[rr,'Impact']==1 & output[rr,'Mean_dfc']>0 &
    output[rr,'Variable'] %in% c('numberPatchesRAnml')){
output[rr,'Impact_dfc'] <- 1}  
}#rest of the row in output

output$ImpactOverTimeCon<-0 #Impact has to be continuous over time
if (sum(output$Impact_dfc[(beforeRow*1.5):(beforeRow*4)])>0){       #only the first few continous significant impact starting from the one and half hurricane year 
for (rr in (beforeRow*1.5):nrow(output)){
if (output[rr,'Impact_dfc']==1){output[rr,'ImpactOverTimeCon'] <- 1}  
if (output[rr,'Impact_dfc']==1 & output[rr+1,'Impact_dfc']==0){break}    
}#row in the output
}#if impact start in the hurricane year
  

alldist <- rbind(alldist,output)
}#loop for dist
  
alldist$Distance <- as.numeric(alldist$Distance) 
alldist <- alldist[order(alldist$Distance, alldist$Level),]   
block <- length(unique(alldist$Level))

grp <- rbind(grp,alldist)
    
}#loop for variable   
}#loop for time  

grp <- grp[,c('Category','Distance','Variable','Time','Level','Group','Impact','Impact_dfc','ImpactOverTimeCon',
              #'ImpactOverTimeOneGapFld','ImpactOverTimeOneGap',
              #'ImpactOverTimeOneGapFldOverDistOneGap','ImpactOverTimeOneGapFldOverDistOneGap',
              'Mean','SD','SE','n','Median','q25','q75','Mean_dfc','Median_dfc')]
write.csv(grp,paste0(wd,'test/SD',folder,'/',cat,'anova.csv'),row.names = FALSE)
}#for hurricane category
}#for sd
############

