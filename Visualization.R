# Project 1

# Alex Chao
### 

load(url("http://www.stat.berkeley.edu/users/nolan/data/weather2011.rda"))

makePlotRegion = function(xlim, ylim, bgcolor, ylabels,
               margins, cityName, xtop = TRUE) {
  par(bg = bgcolor, mar = margins)
  plot(NULL, xlim = xlim, ylim =ylim, yaxt = "n", xaxt = "n")
  axis(side = 2, at = ylabels, tick = TRUE, lty = "solid", lwd = 1, col = "grey", col.ticks = "white", las = 1)
  axis(side = 4, at = ylabels, tick = TRUE, lty = "solid", lwd = 1, col = "grey", col.ticks = "white", las = 1)
  title(cityName)
  side = 1 + 2*xtop
  axis(side, at = cumDays[-13]+ 15, tick = FALSE, labels = monthNames, cex.axis = 0.8)
}

# This function is to produce a blank plot that has 
# the proper axes labels, background color, etc.
# It is to be used for both the top and bottom plot.

# The parameters are
# xlim is a two element numeric vector used for the two
#   end points of the x axis
# ylim is the same as xlim, but for the y axis
# ylabels is a numeric vector of labels for "tick marks"
#   on the y axis
# We don't need to x labels because they are Month names
# margins specifies the size of the plot margins (see mar parameter in par)
# cityName is a character string to use in the title
# xtop indicates whether the month names are to appear
# at the top of the plot or the bottom of the plot
# 
# See the assignment for a pdf image of the plot that is
# produced as a result of calling this function.

drawTempRegion = function(day, high, low, col){
  rect(xleft = c(0:364), xright = c(1:365), ybottom = low, ytop = high, col = col)
}

# This plot will produce 365 rectangles, one for each day
# It will be used for the record temps, normal temps, and 
# observed temps

# day - a numeric vector of 365 dates
# high - a numeric vector of 365 high temperatures
# low - a numeric vector of 365 low temperatures
# col - color to fill the rectangles

addGrid = function(location, col, ltype, vertical = TRUE) {
  if (vertical){
    abline(v = location, col = col, lty = ltype)}
  else if (!vertical){
    abline(h = location, col = col, lty = ltype)}
}

# This function adds a set of parallel grid lines
# It will be used to place vertical and horizontal lines
# on both temp and precip plots

# location is a numeric vector of locations for the lines
# col - the color to make the lines
# ltype - the type of line to make
# vertical - indicates whether the lines are vertical or horizontal

monthPrecip = function(day, dailyprecip, normal){
  points(x = day, y = dailyprecip, col = "dodgerblue", type = "l", lwd = 4)
  polygon(x = c(day, max(day), day[1]), y = c(dailyprecip, 0, 0), col = "cornsilk2", border = NA)
  points(x =c(day[1], max(day)), y = rep(normal,2), type = "l", lwd = 2)
}

# This function adds one month's precipitation to the 
#   precipitation plot.
# It will be called 12 times, once for each month
# It creates the cumulative precipitation curve,
# fills the area below with color, add the total
# precipitation for the month, and adds a reference
# line and text for the normal value for the month

# day a numeric vector of dates for the month
# dailyprecip a numeric vector of precipitation recorded
# for the month (any NAs can be set to 0)
# normal a single value, which is the normal total precip
#  for the month

finalPlot = function(temp, precip){
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or laxWeather
  # precip is the data frame sfoMonthlyPrecip or laxMonthlyPrecip

  
  # Here are some vectors that you might find handy
  
  monthNames = c("January", "February", "March", "April",
               "May", "June", "July", "August", "September",
               "October", "November", "December")
  daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 
                  31, 30, 31, 30, 31)
  cumDays = cumsum(c(1, daysInMonth))
  
  normPrecip = as.numeric(as.character(precip$normal))
  ### Fill in the various stages with your code
 
  
  ### Add any additional variables that you will need here
  
  
  ### Set up the graphics device to plot to pdf and layout
  ### the two plots on one canvas
  ### pdf("", width = , height = )
  ### layout(  )
  
  pdf"AlexChaoProj1", width = 8, height = 11)
  layout(matrix(c(2,1), nrow = 2, ncol = 1, byrow = TRUE), height = c(3,1))

  ### Call makePlotRegion to create the plotting region
  ### for the temperature plot
  
  makePlotRegion(c(1,365), c(-20, 110), "cornsilk2", seq(-30,120, by = 10), c(3,3,5,3), "San Francisco Weather", TRUE)
  
  
  ### Call drawTempRegion 3 times to add the rectangles for
  ### the record, normal, and observed temps
  attach(sfoWeather)
  drawTempRegion(c(1:365), RecordHigh, RecordLow, col = "lemonchiffon3")
  drawTempRegion(c(1:365), NormalHigh, NormalLow, col = "grey65")
  drawTempRegion(c(1:365), High, Low, col = "darkred")
  
  
  ### Call addGrid to add the grid lines to the plot
  addGrid(cumDays, col = "black", lty = 3, TRUE)
  addGrid(seq(-20, 120, by = 10), col = "white", lty = 1, FALSE)
  
  
  ### Add the markers for the record breaking days
  
  ### Add the titles 
  text(x=25, y = 108, labels = "Temperature", cex = 1.1, col = "black", font=2)
  text(x=45, y = 103, labels = "Bars represent range between the daily high and low.", cex = .6, col = "black")
  text(x=45, y = 100, labels = "Average daily low temperature for the year was 49.7,", cex = .6, col = "black")
  text(x=32, y = 97, labels = "and the average daily high was 64.3.", cex = .6, col = "black")
  
  rect(xleft=(364/2)-2, xright=(364/2)+2, ytop= 30+5  , ybottom=  15, col="lemonchiffon3",border=NA)
  rect(xleft=(364/2)-2, xright=(364/2)+2 ,ytop= 25+5, ybottom= 20  , col="gray65",border=NA)
  rect(xleft=(364/2)-2, xright=(364/2),ytop=28+5, ybottom=23, col="darkred",border=NA)

  text(x=(364/2)-11, y = 35, labels = "RECORD HIGH", cex = .4, col = "black")
  text(x=(364/2)-11, y = 20, labels = "RECORD LOW", cex = .4, col = "black")
  text(x=(364/2)-11, y = 26, labels = "NORMAL RANGE", cex = .4, col = "black")
  text(x=(364/2)+9, y = 33, labels = "ACTUAL HIGH", cex = .4, col = "black")
  text(x=(364/2)+9, y = 25, labels = "ACTUAL LOW", cex = .4, col = "black")

  ### Call makePlotRegion to create the plotting region
  ### for the precipitation plot
  makePlotRegion(xlim = c(1, 365), ylim = c(-2, 8), bgcolor = "cornsilk2",
                 ylabels = seq(0, 6, by = 1), margins = c(2,2,1,2), cityName = "", xtop = FALSE)
  
  ### Call monthPrecip 12 times to create each months 
  ### cumulative precipitation plot. To do this use 
  ### sapply(1:12, function(m) {
  ###             code
  ###             monthPrecip(XXXX)
  ###             }) 
  ### the anonymous function calls monthPrecip with the 
  ### appropriate arguments
  sapply(1:12, function(m){
    monthPrecip(day = cumDays[m]+temp$Day[temp$Month==m], dailyprecip = cumsum(temp$Precip[temp$Month==m]),
                normal = as.numeric(as.character(precip$normal[m])))
  })
  
  ### Call addGrid to add the grid lines to the plot
  segments(x0 = c(cumsum(c(1+31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))), y0 = 0, y1 = 6,
         col = "grey", lty="dotted")
  addGrid(location = seq(0, 6, by = 1), col = "white", ltype = "solid", vertical = FALSE)
  
  ### Add the titles
  text(x=15, y = 6.5, labels="Precipitation", cex= 1.1, font=2, pos=3)
  text(x=180, y = 6.5, labels="Cumulative monthly precipitation in inches compared with normal monthly precipitation. Total precipitation in SF was 16.59.", cex= .8, pos=3)

  
  ### Close the pdf device dev.off()
  dev.off()
}

### Call: finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)
finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)

