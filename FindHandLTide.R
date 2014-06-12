# This function is not finished.  I am storing it for sake keeping until I need to
# complete it.  When complete, it will crawl through a NOAA PORTS water level data
# observation file and flag the high and low tide values.
# Coded by John Yagecic 2014
#  JYagecic@gmail.com

HLTide<-function(df){
TideHL<-rep(NA,nrow(df))
for (i in 1:nrow(df)){
if ((i>2)&(i<(nrow(df)-2))){
	TwoSide<-c(df$Water.Level[i-2], df$Water.Level[i-1], df$Water.Level[i+1], df$Water.Level[i+2])
	if (sum(df$Water.Level[i] >= TwoSide)==4){TideHL[i]<-"H"}
	else if (sum(df$Water.Level[i] <= TwoSide)==4){TideHL[i]<-"L"}
}}
TideHL
}