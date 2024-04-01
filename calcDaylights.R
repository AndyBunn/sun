### GFY
rm(list=ls())
source("eqtime.R")
source("declination.R")

library(ggplot2)

# from insol
JD <-
  function(x, inverse=FALSE) {
    # 	options(digits=12)
    if (inverse){
      return(as.POSIXct((x-2440587.5)*86400,origin=ISOdate(1970,01,01,0,0,0),format="%Y-%m-%d %H:%M:%S" ))
    }else{
      return(as.numeric(x)/86400 + 2440587.5)
    }
  }

radians <-
  function (degree) {
    radian = degree * (pi/180.0)
    return(radian)
  }
degrees <-
  function (radian) {
    degree = radian * (180.0/pi)
    return(degree)
  }



# rewrite insol::daylength to remove rounding
daylength <- function(lat, long, jd, tmz)
{
  if (nargs() < 4) {
    cat("USAGE: daylength(latitude, longitude, jd, timezone) \n values in degrees, julian days, hours \n")
    return()
  }
  EqTime = eqtime(jd)
  delta = declination(jd)
  latRads <- lat * (pi/180.0)
  tanlatdel = -tan(radians(lat)) * tan(radians(delta))
  tanlatdel[tanlatdel > 1] = 1
  omega = acos(tanlatdel)
  daylen = (2 * omega)/(2 * pi/24)
  stndmeridian = tmz * 15
  deltaLatTime = long - stndmeridian
  deltaLatTime = deltaLatTime * 24/360
  sunrise = 12 * (1 - omega/pi) - deltaLatTime - EqTime/60
  sunset = 12 * (1 + omega/pi) - deltaLatTime - EqTime/60
  sunrise[omega == 0] = NA
  sunset[omega == 0] = NA
  return(cbind(sunrise, sunset, daylen))
}

# today
todayDate <- Sys.Date()
dateVec <- seq(todayDate-182,todayDate+182,by="1 day")
todayDatePOS <- as.POSIXct(todayDate)
dateVecPOS <- as.POSIXct(dateVec)
dateVecJD <- JD(dateVecPOS)

lat2get <- 48.75
long2get <- 122.48

dat <- daylength(lat = lat2get,long = long2get,jd = dateVecJD,tmz = -1)
dat <- as.data.frame(dat)
names(dat) <- c("sunrise","sunset","day.length")
# use Date rather than POSIXct for plotting
dat$Date <- dateVec
dat$JD <- dateVecJD

dat$day.delta <- c(NA,diff(dat$day.length)/diff(dat$JD) * 60)
todayDF <- data.frame(todayDate = todayDate,
                      todayDayLength = dat$day.length[dat$Date == todayDate],
                      todayDayDelta = dat$day.delta[dat$Date == todayDate])
todayDF

arrowDF <- data.frame(todayDate = c(todayDF$todayDate-20,todayDF$todayDate+20),
                      todayDayLength = c(todayDF$todayDayLength - (todayDF$todayDayDelta/60),
                                         todayDF$todayDayLength + todayDF$todayDayDelta/60))



# labels
todayLab <- format(todayDate,format="%Y-%m-%d")
todayLengthLab <- round(todayDF$todayDayLength,1)
todayDeltaLab <- round(todayDF$todayDayDelta,0)

pDayLength <- ggplot() +
  geom_hline(yintercept = 12,linetype = "dotted") +
  geom_line(data=dat,aes(x=Date,y=day.length)) +
  geom_point(data=todayDF,aes(x=todayDate,y=todayDayLength),
             color="blue",size=3,alpha=0.5) +
  geom_segment(data=arrowDF,aes(x=todayDate[1],
                                y=todayDayLength[1],
                                xend = todayDate[2],
                                yend = todayDayLength[2]),
               color = "blue",
               arrow = arrow(length = unit(0.2, "cm"),angle = 30)) +
  labs(y="Day Length (Hours)",x=element_blank(),
       title = paste0(todayLab, " Latitude ", lat2get),
  subtitle = paste0("Day Length: ",todayLengthLab," Hours, Delta: ", todayDeltaLab, " Minutes")) +
scale_x_date(date_breaks = "months",date_labels = "%b",expand = c(0,0)) +
  theme_minimal()

pDayLength

pDayLengthDelta <- ggplot() +
  geom_hline(yintercept = 0,linetype = "dotted") +
  geom_line(data=dat,aes(x=Date,y=day.delta)) +
  geom_vline(xintercept = todayDate,color="blue",linetype = "dashed") +
  labs(y="Difference (Min.)",x=element_blank(),
       title = "",subtitle="") +
  scale_x_date(date_breaks = "months",date_labels = "%b",expand = c(0,0)) +
  theme_minimal()

pDayLengthDelta


png(filename = paste0("dayLength",todayDate,".png"),
    width = 10,height = 5,units = "in",
    res = 256, pointsize = 9)
gridExtra::grid.arrange(pDayLength,pDayLengthDelta,ncol=2)
dev.off()

#48.75N Con: we only have 8.2hrs of daylight today. 48.75N Pro: we are only losing 20s/d of daylight today. SuperPro: I can see you Solstice. Welcome.
