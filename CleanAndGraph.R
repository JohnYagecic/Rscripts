# Reads a specific text file, processes, and generates a 2 panel plot of temperature
# time series data.  Developed at the request of NJ.  Of particular note is the lower
# panel which includes the mean (as point) with daily range (as vertical line).
#  See Plot1.png for example.
# Coded by John Yagecic in June 2014
#  JYagecic@gmail.com
setwd("~/AssessmentsGeneral/forRoop")  #This is my working directory for this project.  Reset for your own.
diurnal<-read.table("diurnaldata_john.txt", comment.char="#", header=FALSE, sep="", quote="", na.strings="", skip=2, fill=TRUE)
diurnal<-diurnal[-1*c(1:2),]  # skip in the previous line seems to skip commented lines.  This deletes the database lines
names(diurnal)<-c("Agency", "Station", "Date", "Time", "Zone", "Temp.C", "Flag")
diurnal$Temp.C<-as.numeric(levels(diurnal$Temp.C))[diurnal$Temp.C] #convert temperature to numeric
diurnal$Date<-as.Date(diurnal$Date, format="%m/%d/%Y") #convert date to date
diurnal$Date_Time<-as.POSIXct(paste(diurnal$Date, as.character(diurnal$Time))) #combine date and time

diurnal$Month<-as.Date(diurnal$Date, format="%b") #create month date column for plotting axes

# The lines below create a new column where data flagged with "l" is provided as points, all others as NA
# This facilitates plotting flagged data separately
diurnal$FlagDat <- rep(NA, nrow(diurnal))
for (i in 1:nrow(diurnal)){
  if (grepl("l", as.character(diurnal$Flag[i]))){
    diurnal$FlagDat[i]<-diurnal$Temp.C[i]
  }
}

#  If the gridExtra package is not installed on your computer, you need to install it first
require(gridExtra)

s<-ggplot(diurnal, aes(x=Date_Time, y=Temp.C))+geom_line()
s1 <- s + geom_point(data=diurnal, aes(x=Date_Time, y=FlagDat), colour="red", alpha=0.1) + xlab("Date") +ylab("Temperature (degrees C)")
s2 <- s1 + ylim(0,30) + ggtitle(paste0("USGS Station ", diurnal$Station[1]))
s3 <- s2 +annotate("point", x=diurnal$Date_Time[44], y=25, size=2, colour="red")
s4 <- s3 + annotate("text", x=diurnal$Date_Time[4000], y=25, label="Flagged value")
s4

# belows computes new DF of daily min, mean, max
dailyvals<-data.frame(unique(diurnal$Date), tapply(diurnal$Temp.C, diurnal$Date, min), tapply(diurnal$Temp.C, diurnal$Date, mean), tapply(diurnal$Temp.C, diurnal$Date, max))
names(dailyvals)<-c("Date", "Min.C", "Mean.C", "Max.C")

# plot of above as vertical range
g <- ggplot(dailyvals, aes(x=Date, y=Mean.C))+geom_point(size=1, colour="blue") +ylim(0,30)

g1 <- g + ylab("Daily Range Temperature (degrees C)")
g2 <- g1 + geom_segment(data=dailyvals, aes(xend=Date, y=Min.C, yend=Max.C))

grid.arrange(s4, g2, nrow=2)  # Gives the 2 plots on 1 page top to bottom