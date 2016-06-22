#set working directory
setwd("~/Dropbox/enram/kullaberg")

# we work in UTC time zone, make this the default in handling dates
Sys.setenv(TZ='UTC')

#Read user functions
source("functions.plotvol2bird.R")

#header of the data file
header.names=c("Date","Time","Heig","U","V","W","Speed","Direc","StdDev","Gap","dBZ","eta","DensBird","dBZAll","n","ndBZ","nAll","nAlldBZ")
#read the data, make sure to exclude all the lines starting with #
data=read.table(file="kullaberg_nocomments.txt", header = F, col.names=header.names)
# convert Time into a POSIXct date-time
data$Time <- as.POSIXct(paste(data$Date, sprintf('%04d', data$Time), sep = ""), format = "%Y%m%d%H%M", tz='UTC')

# set a time range and plot
tmin=as.POSIXct("2015-09-01 12:00", format = "%Y-%m-%d %H:%M",tz='UTC')
tmax=as.POSIXct("2015-11-08 12:00", format = "%Y-%m-%d %H:%M",tz='UTC')
startdates=seq(tmin,tmax,"weeks")
for(i in 1:(length(startdates)-1)){
  plotvol2bird(data=data,pdfname=paste("Angelholm_by_week_birds_",as.Date(startdates[i]),".pdf",sep=""),radar.lat=56.3675,radar.lon=12.8517,tmin=startdates[i],tmax=startdates[i+1],h.int.range=c(0.2,5))
}
for(i in 1:(length(startdates)-1)){
  plotvol2bird(data=data,pdfname=paste("Angelholm_by_week_all_",as.Date(startdates[i]),".pdf",sep=""),radar.lat=56.3675,radar.lon=12.8517,tmin=startdates[i],tmax=startdates[i+1],h.int.range=c(0.2,5),all.dBZ = T,thres.stdev=0,yrange=c(0,100000))
}
startdates=seq(tmin,tmax,"days")
for(i in 1:(length(startdates)-1)){
  plotvol2bird(data=data,pdfname=paste("Angelholm_by_day_birds_",as.Date(startdates[i]),".pdf",sep=""),radar.lat=56.3675,radar.lon=12.8517,tmin=startdates[i],tmax=startdates[i+1],h.int.range=c(0.2,5))
}
startdates=seq(tmin,tmax,"days")
for(i in 1:(length(startdates)-1)){
  plotvol2bird(data=data,pdfname=paste("Angelholm_by_day_all_",as.Date(startdates[i]),".pdf",sep=""),radar.lat=56.3675,radar.lon=12.8517,tmin=startdates[i],tmax=startdates[i+1],h.int.range=c(0.2,5),all.dBZ = T,thres.stdev=0,yrange=c(0,10000))
}

