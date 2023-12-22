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

dat <- daylength(lat = 48.75,long = 122.48,jd = dateVecJD,tmz = -1)
dat <- as.data.frame(dat)
names(dat) <- c("sunrise","sunset","day.length")
# use Date rather than POSIXct for plotting
dat$Date <- dateVec
dat$JD <- dateVecJD
mean(dat$day.length)


dat$day.delta <- c(NA,diff(dat$day.length)/diff(dat$JD) * 60)

today <- format(Sys.Date(),format="%Y-%m-%d")
todayDL <- round(dat$day.length[dat$Date == Sys.Date()],1)
todayDD <- round(dat$day.delta[dat$Date == Sys.Date()],1)

dat$Date[dat$Date == Sys.Date()]

pDayLength <- ggplot(data=dat,aes(x=Date,y=day.length)) +
  geom_hline(yintercept = 12,linetype = "dotted") +
  geom_line() +
  geom_vline(xintercept = Sys.Date(),color="blue",linetype = "dashed") +
  labs(y="Day Length (Hours)",title = paste(today,"48.75 N",sep=", ")) +
  scale_x_date(date_breaks = "months",date_labels = "%b",expand = c(0,0)) +
  annotate(geom = "text", max(dat$day.length), x=Sys.Date()-5,
           label=paste(todayDL, "hr"),hjust=1) +
  theme_minimal()

pDayLength

pDayLengthDelta <- ggplot(data=dat,aes(x=Date,y=day.delta)) +
  geom_hline(yintercept = 0,linetype = "dotted") +
  geom_line() +
  geom_vline(xintercept = Sys.Date(),color="blue",linetype = "dashed") +
  annotate(geom = "text", max(dat$day.delta,na.rm = TRUE), x=Sys.Date()-5,
                    label=paste(todayDD,"min"),hjust=1) +
  labs(y="Difference (Min.)",title = "") +
  scale_x_date(date_breaks = "months",date_labels = "%b",expand = c(0,0)) +
  theme_minimal()

pDayLengthDelta


png(filename = paste0("dayLength",todayDate,".png"),
    width = 10,height = 5,units = "in",
    res = 256, pointsize = 9)
  gridExtra::grid.arrange(pDayLength,pDayLengthDelta,ncol=2)
dev.off()

#48.75N Con: we only have 8.2hrs of daylight today. 48.75N Pro: we are only losing 20s/d of daylight today. SuperPro: I can see you Solstice. Welcome.
