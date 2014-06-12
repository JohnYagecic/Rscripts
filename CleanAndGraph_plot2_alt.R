# Reads a specific text file, processes, and generates a facet plot of temperature
# time series data faceted by individual months.  Developed at the request of NJ.
#  See Plot2_alt.png for example.
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
diurnal$Month.char <- as.character(diurnal$Date, format="%b")
diurnal$Month.char <- factor(diurnal$Month.char, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#  If the gridExtra package is not installed on your computer, you need to install it first
require(gridExtra)
require(scales)
require(ggplot2)

bp <- ggplot(diurnal, aes(x=Date_Time, y=Temp.C))+geom_point(alpha=0.1)+ylim(-10,30)
bp1 <- bp + theme(axis.text.x = element_blank())+xlab("Months")+ylab("Temperature (degrees C)")
bp2 <- bp1 + facet_wrap( ~ Month.char, nrow=4, scales="free_x")
bp2 + ggtitle(paste0("USGS Station ", diurnal$Station[1]))