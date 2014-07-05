CitName<-c("A", "B", "C", "D", "E", "F","G", "H", "I", "J")
Lat<-runif(10,0,1000)
Long<-runif(10,0,1000)
Cities<-data.frame(CitName,Lat,Long)
plot(Cities$Long, Cities$Lat)
routelist<-c(1,2,3,4,5,6,7,8,9,10)
AllRoutes<-data.frame(rep(NA, 10000), rep(NA, 10000))
BestSeq<-NA
Shortest<-NA
names(AllRoutes)<-c("Route.Sequence", "Total.Distance")
for (i in 1:10000){ #Start Monte Carlo Loop
  dist<-0
  routeletters<-NA
  route<-sample(routelist, 10, replace=FALSE)
  for (j in 1:10){ # Loop through route to compute distance
    if (j==1){
      dist=sqrt(Cities$Lat[route[j]]^2 + Cities$Long[route[j]]^2)
      routeletters<-Cities$CitName[route[1]]
    }
    if (j>1){
      dist=dist+sqrt((Cities$Lat[route[j]]-Cities$Lat[route[j-1]])^2 + 
                       (Cities$Long[route[j]]-Cities$Long[route[j-1]])^2)
      routeletters<-paste0(routeletters, Cities$CitName[route[j]])
    }
  }
  dist=dist+sqrt(Cities$Lat[route[10]]^2 + Cities$Long[route[10]]^2)
  AllRoutes$Route.Sequence[i]<-routeletters
  AllRoutes$Total.Distance[i]<-dist
  if (i==1){
    Shortest<-dist
  }
  if (dist < Shortest){
    Shortest<-dist
    BestSeq<-c(route[1], route[2], route[3], route[4], route[5],
               route[6], route[7], route[8], route[9], route[10])
  }
}

bestroute<-AllRoutes[order(AllRoutes$Total.Distance),]
head(bestroute)

myBest<-bestroute$Route.Sequence[1]

CityMap<-Cities[BestSeq,]
CityMap
plot(CityMap$Long, CityMap$Lat, type='o')
