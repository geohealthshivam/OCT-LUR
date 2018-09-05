##### Packages required ##############
require(sp)
require(raster)
require(rgeos)
require(gstat)
require(rgdal)
require(geoR)
require(maptools)

#Set working directory with all data 
setwd("Land use regression/Working directory")

##Reading shape files 
monitor_2<-monitor
monitor_2<-readShapePoints('~/Shape files 32632/SHAPE FILE/Prediction points 1000 /New monitor /new_monitors1')
major.roads<-readShapeLines("~/Shape files 32632/SHAPE FILE/Major roads.shp")
minor.roads<-readShapeSpatial("~/Shape files 32632/SHAPE FILE/Minor Roads.shp")
#Confirm classes and projection for each object
class(major.roads)
class(minor.roads)
proj4string(monitor)
proj4string(landuse)
proj4string(roads)
proj4string(muenster)
proj4string(major.roads)
proj4string(minor.roads)
#If required, change the projection to the desired one
projection(muenster) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
projection(monitor) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
projection(landuse) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
projection(roads) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
projection(buildings) <- CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
projection(major.roads)<-CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
projection(minor.roads)<-CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
plot(major.roads,axes=T,col='black')
plot(minor.roads,col='grey',add=T)

#plot(monitor,col='blue',cex=2,pch=13,add=T)
#Create a ploygon fo the study area by merging the sub polygon inside the main polygon
shape_mun<-gUnionCascaded(muenster)

plot(shape_mun)
#Create buffer of different sizes for extractign explainatory variabels of LUR
#25 meters
plot(monitor,col='magenta',asp=2,add=TRUE)
m.buf_25<-gBuffer(monitor,width = 25,byid=TRUE)
plot(m.buf_25,add=T,lty=2, lwd=2,border="brown")

#50 meters
m.buf_50<-gBuffer(monitor,width = 50,byid=TRUE)
plot(m.buf_50,add=T,lty=2, lwd=2,border="pink")

#100 meters
m.buf_100<-gBuffer(monitor,width = 100,byid = TRUE)
plot(m.buf_100,add=T,lty=2, lwd=2,border="orange")

#300 meters
m.buf_300<-gBuffer(monitor,width = 300,byid=TRUE)
plot(m.buf_300,add=T,lty=2, lwd=2,border="green")

#500 meters
m.buf_500<-gBuffer(monitor,width = 500,byid = TRUE)
plot(m.buf_500,add=T,lty=2, lwd=2,border="blue")

#1000 meters
m.buf_1000<-gBuffer(monitor,width = 1000,byid=TRUE)
plot(m.buf_1000,add=T,lty=2, lwd=2,border="yellow")

#5000 meters
m.buf_5000<-gBuffer(monitor,width = 5000,byid=TRUE)
plot(m.buf_5000,add=T,lty=2, lwd=2,border="black")



#Extracting number of buildings in each buffer extrated above
buildcount_5000<-over(m.buf_5000,buildings,fn=length)
monitor_es$buildcount_5000<- buildcount_5000$name

buildcount_1000<-over(m.buf_1000,buildings,fn=length)
monitor_es$buildcount_1000<- buildcount_1000$name


buildcount_500<-over(m.buf_500,buildings,fn=length)
monitor_es$buildcount_500<- buildcount_500$name


buildcount_300<-over(m.buf_300,buildings,fn=length)
monitor_es$buildcount_300<- buildcount_300$name

buildcount_100<-over(m.buf_100,buildings,fn=length)
monitor_es$buildcount_100<- buildcount_100$name

monitor_es@data[is.na(monitor_es@data)]<-0

head(monitor_es)

#removing the inner buffer value from the bigger buffer 
monitor_es$buildcount_300<-monitor_es$buildcount_300-monitor_es$buildcount_100
monitor_es$buildcount_500<-monitor_es$buildcount_500-monitor_es$buildcount_300
monitor_es$buildcount_1000<-monitor_es$buildcount_1000-monitor_es$buildcount_500
monitor_es$buildcount_5000<-monitor_es$buildcount_5000-monitor_es$buildcount_1000

#Extracting number of roads in buffer 
#function to extract the roads in buffer
road_4m_buffer<-function(line,buffer){
  result<-data.frame(road.count=double(),road.length=double())
  for(i in 1:length(buffer)){
    intersec<-raster::intersect(line,buffer[i,])
    if(is.null(intersec)){
      result[i,1]<-0
      result[i,2]<-0
      i=i+1
    }else{
      result[i,1]<-length(intersec)
      result[i,2]<-rgeos::gLength(intersec)
      i=i+1
    }}
  return(result)
}
##Road count for buffers
road.count.1000<-road_4m_buffer(roads,m.buf_1000)
monitor_es$rdcount_1000<-road.count.1000$road.count

road.count.500<-road_4m_buffer(roads,m.buf_500)
monitor_es$rdcount_500<-road.count.500$road.count

road.count.300<-road_4m_buffer(roads,m.buf_300)
monitor_es$rdcount_300<-road.count.300$road.count

road.count.100<-road_4m_buffer(roads,m.buf_100)
monitor_es$rdcount_100<-road.count.100$road.count

road.count.50<-road_4m_buffer(roads,m.buf_50)
monitor_es$rdcount_50<-road.count.50$road.count

road.count.25<-road_4m_buffer(roads,m.buf_25)
monitor_es$rdcount_25<-road.count.25$road.count

#substracting the inner buffer values from the outer buffer
monitor_es$rdcount_50<-monitor_es$rdcount_50-monitor_es$rdcount_25
monitor_es$rdcount_100<-monitor_es$rdcount_100-monitor_es$rdcount_50
monitor_es$rdcount_300<-monitor_es$rdcount_300-monitor_es$rdcount_100
monitor_es$rdcount_500<-monitor_es$rdcount_500-monitor_es$rdcount_300
monitor_es$rdcount_1000<-monitor_es$rdcount_1000-monitor_es$rdcount_500
head(monitor_es)

#Major roads in buffer 

mjroad.count.1000<-road_4m_buffer(major.roads,m.buf_1000)
monitor_es$mjrdcount_1000<-mjroad.count.1000$road.count

mjroad.count.500<-road_4m_buffer(major.roads,m.buf_500)
monitor_es$mjrdcount_500<-mjroad.count.500$road.count

mjroad.count.300<-road_4m_buffer(major.roads,m.buf_300)
monitor_es$mjrdcount_300<-mjroad.count.300$road.count

mjroad.count.100<-road_4m_buffer(major.roads,m.buf_100)
monitor_es$mjrdcount_100<-mjroad.count.100$road.count

mjroad.count.50<-road_4m_buffer(major.roads,m.buf_50)
monitor_es$mjrdcount_50<-mjroad.count.50$road.count

mjroad.count.25<-road_4m_buffer(major.roads,m.buf_25)
monitor_es$mjrdcount_25<-mjroad.count.25$road.count

monitor_es$mjrdcount_50<-monitor_es$mjrdcount_50-monitor_es$mjrdcount_25
monitor_es$mjrdcount_100<-monitor_es$mjrdcount_100-monitor_es$mjrdcount_50
monitor_es$mjrdcount_300<-monitor_es$mjrdcount_300-monitor_es$mjrdcount_100
monitor_es$mjrdcount_500<-monitor_es$mjrdcount_500-monitor_es$mjrdcount_300
monitor_es$mjrdcount_1000<-monitor_es$mjrdcount_1000-monitor_es$mjrdcount_500

#minor roads in buffer 

min.road.count.1000<-road_4m_buffer(minor.roads,m.buf_1000)
monitor_es$minrdcount_1000<-min.road.count.1000$road.count

min.road.count.500<-road_4m_buffer(minor.roads,m.buf_500)
monitor_es$minrdcount_500<-min.road.count.500$road.count

min.road.count.300<-road_4m_buffer(minor.roads,m.buf_300)
monitor_es$minrdcount_300<-min.road.count.300$road.count

min.road.count.100<-road_4m_buffer(minor.roads,m.buf_100)
monitor_es$minrdcount_100<-min.road.count.100$road.count

min.road.count.50<-road_4m_buffer(minor.roads,m.buf_50)
monitor_es$minrdcount_50<-min.road.count.50$road.count

min.road.count.25<-road_4m_buffer(minor.roads,m.buf_25)
monitor_es$minrdcount_25<-min.road.count.25$road.count

monitor_es$minrdcount_50<-monitor_es$minrdcount_50-monitor_es$minrdcount_25
monitor_es$minrdcount_100<-monitor_es$minrdcount_300-monitor_es$minrdcount_50
monitor_es$minrdcount_300<-monitor_es$minrdcount_300-monitor_es$minrdcount_100
monitor_es$minrdcount_500<-monitor_es$minrdcount_500-monitor_es$minrdcount_250
monitor_es$minrdcount_1000<-monitor_es$minrdcount_1000-monitor_es$minrdcount_500
head(monitor_es)


####--------------------------------- Road length -------------------------------##
#road length from the buffer areas
monitor_es$rdlength_1000<-road.count.1000$road.length
monitor_es$rdlength_500<-road.count.500$road.length
monitor_es$rdlength_300<-road.count.300$road.length
monitor_es$rdlength_100<-road.count.100$road.length
monitor_es$rdlength_50<-road.count.50$road.length
monitor_es$rdlength_25<-road.count.25$road.length
#substracting the inner buffer from out buffer 
monitor_es$rdlength_50<-monitor_es$rdlength_50-monitor_es$rdlength_25
monitor_es$rdlength_100<-monitor_es$rdlength_100-monitor_es$rdlength_50
monitor_es$rdlength_300<-monitor_es$rdlength_300-monitor_es$rdlength_100
monitor_es$rdlength_500<-monitor_es$rdlength_500-monitor_es$rdlength_300
monitor_es$rdlength_1000<-monitor_es$rdlength_1000-monitor_es$rdlength_500

#moajor road length
monitor_es$mjrdlength_1000<-mjroad.count.1000$road.length
monitor_es$mjrdlength_500<-mjroad.count.500$road.length
monitor_es$mjrdlength_300<-mjroad.count.300$road.length
monitor_es$mjrdlength_100<-mjroad.count.100$road.length
monitor_es$mjrdlength_50<-mjroad.count.50$road.length
monitor_es$mjrdlength_25<-mjroad.count.25$road.length
head(monitor_es)
#substracting inner buffer values from outer buffer 
monitor_es$mjrdlength_50<-monitor_es$mjrdlength_50-monitor_es$mjrdlength_25
monitor_es$mjrdlength_100<-monitor_es$mjrdlength_100-monitor_es$mjrdlength_50
monitor_es$mjrdlength_300<-monitor_es$mjrdlength_300-monitor_es$mjrdlength_100
monitor_es$mjrdlength_500<-monitor_es$mjrdlength_500-monitor_es$mjrdlength_300
monitor_es$mjrdlength_1000<-monitor_es$mjrdlength_1000-monitor_es$mjrdlength_500




#Calculating distance to road 
distance2road<-function(rd,pt){
  distan<-data.frame(distance=double())
  for(i in 1:length(pt)){
    distan[i,]<-gDistance(rd,pt[i,])
    i=i+1
  }
  return(distan)
}

#for all roads
dist_rd<-distance2road(roads,monitor_es)
monitor_es$dist.rd<- dist_rd$distance


#for major roads
dist_mjrd<-distance2road(major.roads,monitor_es)
monitor_es$dist.mjrd<- dist_mjrd$distance

# for minor roads
dist_minrd<-distance2road(minor.roads,monitor_es)
monitor_es$dist.minrd<- dist_minrd$distance
head(monitor_es)


#calculating traffic congestion using the values for the study area
congestion<-data.frame(congest=as.double())
traffic_conjestion<-function(rd,val){
  for (i in 1:length(rd)){
    alrd<-rgeos::gLength(rd)
    congestion[i,]<-rgeos::gLength(rd[i,])/alrd*val
  }}

#for major roads 
traffic_conjestion(major.roads,112500)

major.roads$congestion<-congestion$congest
#for minor roads 
traffic_conjestion(minor.roads,37500)
minor.roads$congestion<-
  head(minor.roads$congestion)

head(congestion)
congestion<-data.frame(congestion=double())

for (i in 1:length(minor.roads)){
  congestion[i,]<-rgeos::gLength(minor.roads[i,])/rgeos::gLength(minor.roads)*37500
}


112500
#function to extract teh congestion values to the nearest road


#calculate the congestiona and put it into monitor_es layer
congest_dat<-congestion_nearest_road(major.roads,monitor_es)
monitor_es$congestion_mjrd<-congest_dat$congestion
head(monitor_es)


congestion_nearest_road<-function(rd,pt){
  cong<-data.frame(congestion=double())
  for(i in 1:length(pt)){
    dist2road<-gDistance(rd,pt[i,])
    for ( j in 1 : length(rd)) 
    { 
      if (gWithinDistance(rd[j,],pt[i,], dist2road)) 
        rownumber<-j
    }
    cong[i,1]<-rd[rownumber,]$congestion
    i=i+1
  }
  return(cong)
}



#minor roads roads 
congest_dat<-congestion_nearest_road(minor.roads,monitor_es)
monitor_es$congestion_minrd<-congest_dat$congestion
head(monitor_es@data)


mjrds<-rgeos::intersect(major.roads,m.buf_100)
sum(mjrds$congestion_mjrd*gLength(mjrds))



#trafic intensity distance 
monitor_es$TRAFINVDIST_min<-monitor_es$congestion_minrd*(1/monitor_es$dist.minrd)
monitor_es$TRAFINVDIST_min2<-monitor_es$congestion_minrd*(1/monitor_es$dist.minrd)^2

monitor_es$INTMAJORINVDIST<-monitor_es$congestion_mjrd*(1/monitor_es$dist.mjrd)
monitor_es$INTMAJORINVDIST2<-monitor_es$congestion_mjrd*(1/monitor_es$dist.mjrd)^2



