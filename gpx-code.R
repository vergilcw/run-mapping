library(plyr)
library(stringr)
library(lubridate)
library(plotKML)
library(ggmap)

setwd("~/R/projects/gpx-mapping")
files <- list.files('rawdata',full.names=T) 
#Activities <- read.csv(files[grep('Activities',files)])


parseGPX <- function(f){
  raw <- readGPX(f)$tracks[[1]]
  desc <- names(raw)[1]
   date <- as.Date(unlist(strsplit(desc, ' '))[2], 
                   format='%m/%d/%y')
   type <- unlist(strsplit(desc, ' '))[1]
  names(raw) <- seq(length(names(raw)))
  clean <- cbind(ldply(raw), 
                 type=type, 
                 desc=desc,
                 date=date,
                 stringsAsFactors=F)
  clean <- within(clean,{
    datetime <- as.POSIXct(time,
                           #2013-11-12T19:43:18+00:00
                           format='%Y-%m-%dT%H:%M:%S+00:00',
                           tz='GMT')
    segment <- .id
    rm(list=c('.id','desc'))
  })
  return(clean)
}

datalist <- lapply(files[grep('gpx',files)], parseGPX)
data <- do.call('rbind',datalist)


#open streetmap
osm <- get_openstreetmap(bbox = c(left = -82.6299,
                                  bottom = 35.5523, 
                                  right = -82.5039, 
                                  top = 35.6542),
                         scale=25000,
                         urlonly=F)

pdf('runkeeper_osm.pdf', width=2003, height=1993)
ggmap(osm, extent = "device") + # takes out axes, etc.
  geom_point(aes(x = lon,
                 y = lat),
             data = data,
             alpha = 0.3,
             color = 'darkred',
             size = 5,
             pch = 20)
dev.off()

#get google map
mapData <- get_googlemap(center = c(lon = -82.56011, 
                                    lat = 35.599783),
                         zoom = 13,
                         size = c(640,640), #640x640 is max
                         maptype = 'hybrid')
png('runkeeper_googlemap.png', width=640, height=640)
ggmap(mapData,
      extent = "device") + # takes out axes, etc.
  geom_point(aes(x = lon,
                 y = lat),
             data = data,
             alpha = 0.1,
             color = 'cyan',
             size = 1.25,
             pch = 20)
dev.off()


# 
# ## convert times to seconds since the start of the run
# trkData <- ddply(trkData, .(Activity), mutate, Time=str_match(DateTime, "T(.*)Z")[,2], TimeSec=period_to_seconds(hms(Time)), TimeSinceStart=TimeSec-TimeSec[1])
# trkData$Time <- NULL; trkData$TimeSec <- NULL;
# 
# ## add the next frame for difference calculation
# trkData <- ddply(trkData, .(Activity), transform, NextLatiude=c(Latitude[-1], NA), NextLongitude=c(Longitude[-1], NA), NextTimeSinceStart=c(TimeSinceStart[-1], NA))
# 
# ## The following program computes the distance on the surface of the earth between two points point1 and point2. Both the points are of the form (Longitude, Latitude)
# ## http://www.biostat.umn.edu/~sudiptob/Software/distonearth.R
# geodetic.distance <- function(point1, point2) {
#   R <- 6371000
#   p1rad <- point1 * pi/180
#   p2rad <- point2 * pi/180
#   d <- sin(p1rad[2])*sin(p2rad[2])+cos(p1rad[2])*cos(p2rad[2])*cos(abs(p1rad[1]-p2rad[1]))
#   d <- acos(d)
#   R*d
# }
# 
# ## calculate the distances
# trkData <- adply(trkData, 1, with, c(distance=geodetic.distance(c(Longitude, Latitude), c(NextLongitude, NextLatiude))))
# 
# ## calculate speed
# trkData <- trkData[complete.cases(trkData),]
# trkData <- mutate(trkData, speed=distance/(NextTimeSinceStart-TimeSinceStart), smooth_speed=fitted(smooth.spline(TimeSinceStart, speed)))
# 
# ggplot(trkData, aes(TimeSinceStart/60, smooth_speed*3.6)) + geom_smooth() + ggtitle("Joint training speed profile")   + ylab("km/h")  +xlab("min")
# 
# ggplot(trkData, aes(TimeSinceStart/60, 60/(smooth_speed*3.6))) + geom_smooth() + ggtitle("Joint training speed profile")   + ylab("pace")  +xlab("min")