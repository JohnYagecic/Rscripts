TextNWISreal<-function(date1, date2, station, paramcd){
# A function to retrieve NWIS near real time data
# Written by John Yagecic in 2014
#  JYagecic@gmail.com
#
# enter date as yyyymmdd
# station is a 8 digit number such as 01463500

sdate<-strptime(as.character(date1), "%Y%m%d")
finaldate<-strptime(as.character(date2), "%Y%m%d")
station<-as.character(station)
paramcd<-as.character(paramcd)


char1<-"http://waterdata.usgs.gov/nwis/uv?cb_"
# then param code
char2<-"=on&format=rdb&site_no="
# then site no
char3<-"&period=&begin_date="
# then begin date
char4<-"&end_date="
# then end date

BeginDate<-as.character(sdate, format="%Y-%m-%d")
EndDate<-as.character(finaldate, format="%Y-%m-%d")

fileURL<-paste0(char1, paramcd, char2, station, char3, BeginDate, char4, EndDate)
print(station)
print(paramcd)
print(fileURL)

download.file(fileURL, destfile="tempNWIS.txt")
NWIS1<<-read.table("tempNWIS.txt", comment.char="#", header=FALSE, sep="", quote="", na.strings="", skip=2, fill=TRUE)
NWIS1<<-NWIS1[-1*c(1:2),]	

}
