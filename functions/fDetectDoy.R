print("Function: Zonal statistics of reference units for raster data")
#-------------------------------------------------------------------------------
fDetectTempSum <- function(
  TS.SHP,
  PP.DIR,
  OUT.DIR,
  YEARS,
  PLANT,
  PHASES){
  
  
#-----------------------------------------------------------------------------------------------------
print("Import test site")
#-----------------------------------------------------------------------------------------------------
ts <- st_read(paste(TS.SHP))
df.t.sum <- data.frame("V1"=NULL,"V2"=NULL,"V3"=NULL, "V4"=NULL)
pb <- txtProgressBar(min=min(YEARS), max=max(YEARS), style=3)
for(i in 1:length(YEARS)){
for(PHASE in PHASES){
tryCatch({
pp <- st_read(paste(PP.DIR,"DOY_",PLANT,"-",PHASE,"_",YEARS[i],".shp",sep=""))
#reproject ts according to pp
pp <- st_transform(pp, st_crs(ts))
ts.pp <- st_intersection(ts,pp)
ts.pp$YEAR <- YEARS[i]
ts.pp$PHASE <- PHASE
df <- data.frame("V1"= ts.pp$YEAR, "V2"=ts.pp$t_sums, "V3"=ts.pp$PHASE, "V4"=ts.pp$Jultag)
df.t.sum <- rbind(df.t.sum,df)
},error=function(e){})  
}
setTxtProgressBar(pb, i)
}
colnames(df.t.sum) <- c("YEAR","T.SUM","PHASE", "DOY") 

#Export intersection result
setwd(OUT.DIR)
write.csv2(df.t.sum, paste(substr(TS.SHP,1,nchar(TS.SHP)-4),"_DOY-HS_",PLANT,c(".csv"),sep=""))

df.t.sum.median <- data.frame(aggregate(df.t.sum[c(2,4)], 
                               by=list(df.t.sum$PHASE),
                               FUN=median, 
                               na.rm=TRUE))
colnames(df.t.sum.median) <- c("PHASE","T.SUM","DOY")
df.t.sum.median$T.SUM <- round(df.t.sum.median$T.SUM,0)
df.t.sum.median$DOY <- round(df.t.sum.median$DOY,0)
write.csv2(df.t.sum.median, paste(substr(TS.SHP,1,nchar(TS.SHP)-4),"_DOY-HS_agg_",PLANT,c(".csv"),sep=""))

#plot
#colors
if(PLANT==201){
  CT.P <- PHASES
  col.p <- c("#e5f4e3", #1
             "#feecb9", #25
             "#FDAE61")#26
}
if(PLANT==202){
CT.P <- PHASES
col.p <- c("#3288BD", #10
           "#e5f4e3", #12
           "#ABDDA4", #15
           "#66C2A5", #18
           "#feecb9", #19
           "#FDAE61", #21
           "#F46D43") #24
}
if(PLANT==204){
  CT.P <- PHASES
  col.p <- c("#3288BD", #10
             "#e5f4e3", #12
             "#ABDDA4", #15
             "#66C2A5", #18
             "#FDAE61", #21
             "#F46D43") #24
}
if(PLANT==205){
  CT.P <- PHASES
  col.p <- c("#3288BD", #10
             "#e5f4e3", #12
             "#c4e7bf", #14
             "#99c693", #67
             "#93d4c0", #17
             "#FFFE89", #5
             "#FDAE61", #22
             "#F46D43") #AH
}
    
if(PLANT==208){
  CT.P <- PHASES
  col.p <- c("#3288BD", #10
              "#e5f4e3", #12
              "#ABDDA4", #15
              "#99c693", #66
              "#feecb9", #19
              "#FDAE61", #21
              "#F46D43") #24
  }  

if(PLANT==215){
  CT.P <- PHASES
  col.p <- c("#3288BD", #10
             "#e5f4e3", #12
             "#99c693", #67
             "#66C2A5", #5
             "#519b84", #65
             "#feecb9", #19
             "#fee08b", #20
             "#FDAE61", #21
             "#F46D43") #24
}


if(PLANT==252 | PLANT==253){
  CT.P <- PHASES
  col.p <- c("#3288BD", #10
             "#e5f4e3", #12
             "#99c693", #13
             "#F46D43") #24
}
#automatic range detection
df.range <- data.frame("range"=NULL)
df.median <<- data.frame("median"=NULL)
for(PHASE in PHASES){
  x <- density(df.t.sum[which(df.t.sum$PHASE==PHASE),]$T.SUM,na.rm=TRUE)
  df.range  <- rbind(df.range,max(x[[2]]))
  m <- median(df.t.sum[which(df.t.sum$PHASE==PHASE),]$T.SUM,na.rm=TRUE)
  df.median <- rbind(df.median,round(m,0))
}

df.range.DOY <- data.frame("range"=NULL)
df.median.DOY <<- data.frame("median"=NULL)
for(PHASE in PHASES){
  x <- density(df.t.sum[which(df.t.sum$PHASE==PHASE),]$DOY,na.rm=TRUE)
  df.range.DOY  <- rbind(df.range.DOY,max(x[[2]]))
  m <- median(df.t.sum[which(df.t.sum$PHASE==PHASE),]$DOY,na.rm=TRUE)
  df.median.DOY <- rbind(df.median.DOY,round(m,0))
}

#density plot
setwd(file.path(OUT.DIR))
pdf(paste(substr(TS.SHP,1,nchar(TS.SHP)-4),"_Temperatursummen_",PLANT,c(".pdf"),sep=""), 
    height=4,width=8)
plot(density(df.t.sum[which(df.t.sum$PHASE==min(PHASES)),]$T.SUM,na.rm=TRUE),
 xlim=range(density(df.t.sum$T.SUM)[[1]]),
 ylim=c(0,max(df.range)),
 main=paste("Temperatursummen:",PLANT),
 xlab= paste("Durchschnittliche Temperatursummen für",min(YEARS),"bis",max(YEARS)),
 ylab="Dichtefunktionen")

for(i in 1:length(PHASES)){
  polygon(density(df.t.sum[which(df.t.sum$PHASE==PHASES[i]),]$T.SUM,na.rm=TRUE),col=col.p[i])
  abline(v=median(df.t.sum[which(df.t.sum$PHASE==PHASES[i]),]$T.SUM,na.rm = TRUE),col="grey",lty=5)
}
text(x = df.median[[1]], 
     y=max(df.range), 
     label = df.median[[1]], 
     pos = 2, 
     cex = 0.8, 
     col = "red",
     srt = 90)

legend("left", title="PHASE",
       legend=c(paste(CT.P)),
       fill=col.p,bty="n")
dev.off()

substr(TS.SHP,1,nchar(TS.SHP)-4)
pdf(paste(substr(TS.SHP,1,nchar(TS.SHP)-4),"_Kalendertage_",PLANT,c(".pdf"),sep=""), 
    height=4,width=8)
plot(density(df.t.sum[which(df.t.sum$PHASE==min(PHASES)),]$DOY,na.rm=TRUE),
     xlim=c(0,365),
     ylim=c(0,max(df.range.DOY)),
     main=paste("Kalendertage:",PLANT),
     xlab= paste("Phasenspezifische Kalendertage für",min(YEARS),"bis",max(YEARS)),
     ylab="Dichtefunktionen",
     xaxt="n")
#axis
x1 <- seq(1,365,10)
x2 <- seq(1,365,2)
axis(1, at=x2, col.tick="grey", las=1,labels=FALSE,cex=1.2)
axis(1, at=x1, col.axis="black", las=2,cex=1.2)

for(i in 1:length(PHASES)){
  polygon(density(df.t.sum[which(df.t.sum$PHASE==PHASES[i]),]$DOY,na.rm=TRUE),col=col.p[i])
  abline(v=median(df.t.sum[which(df.t.sum$PHASE==PHASES[i]),]$DOY,na.rm = TRUE),col="grey",lty=5)
}
text(x = df.median.DOY[[1]], 
     y=max(df.range.DOY), 
     label = df.median.DOY[[1]], 
     pos = 2, 
     cex = 0.8, 
     col = "red",
     srt = 90)

legend("left", title="PHASE",
       legend=c(paste(CT.P)),
       fill=col.p,bty="n")
dev.off()
}
