###Name: Arif Ali
###SID: 21922601

load(url("http://www.stat.berkeley.edu/users/nolan/data/weather2011.rda"))

makePlotRegion = function(xlim, ylim, bgcolor, ylabels, margins, cityName, xtop = TRUE) {
  par(bg = bgcolor)
  plot(x = NULL, y = NULL, xlim = xlim, ylim = ylim, mar = margins, main = " ", xlab = " ", ylab = ylabels, xaxt = "n", xaxs = "i", las=2, cex = 0.5, axes = FALSE)
  if (xtop) {
    axis(side = 3, tick = FALSE, labels = monthNames, at = cumDays-15, cex.axis = 0.5) 
  } else {
    axis(side = 1, tick = FALSE, labels = monthNames, at = cumDays-15, cex.axis = 0.5)
  }
  axis(4, tick = FALSE, las = 2, cex.axis = .75)
  axis(2, tick = FALSE, las = 2, cex.axis = .75)
  abline(v = min(xlim), col= "#9C7405", lwd = 2)
  abline(v = max(xlim), col= "#9C7405", lwd = 2)
  
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
  
  
}

drawTempRegion = function(day, high, low, col){
  rect(day-0.5, low, day+0.5, high, col = col, border = NA)
}
# This plot will produce 365 rectangles, one for each day
# It will be used for the record temps, normal temps, and 
# observed temps

# day - a numeric vector of 365 dates
# high - a numeric vector of 365 high temperatures
# low - a numeric vector of 365 low temperatures
# col - color to fill the rectangles



addGrid = function(location, col, ltype, vertical = TRUE) {
  if (vertical) {
    abline(v = location, col = col, lty = ltype)
  } else {
    abline(h= location, col = col, lty = ltype)
  }
}

# This function adds a set of parallel grid lines
# It will be used to place vertical and horizontal lines
# on both temp and precip plots

# location is a numeric vector of locations for the lines
# col - the color to make the lines
# ltype - the type of line to make
# vertical - indicates whether the lines are vertical or horizontal



monthPrecip = function(day, dailyprecip, normal){
  is.na(dailyprecip) = 0
  polygon(c(day, tail(day, n = 1), head(day, n = 1)), c(cumsum(dailyprecip), 0, 0), col = "#E1391E", border = NA)
  segments(x0 = min(day), y0 = normal,x1 = max(day), col = "#00AA9A", lwd=2)
  lines(day, cumsum(dailyprecip), col = "#194759", lwd = 3)
  text(max(day)-2, sum(dailyprecip)+0.1, labels = sum(dailyprecip), cex = 1)
  text(min(day)+2, normal+0.1, labels =  normal, cex = 1)
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

monthNames = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
daysInMonth = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
cumDays = cumsum(daysInMonth)

finalPlot = function(temp, precip){
  # The purpose of this function is to create the whole plot
  # Include here all of the set up that you need for
  # calling each of the above functions.
  # temp is the data frame sfoWeather or laxWeather
  # precip is the data frame sfoMonthlyPrecip or laxMonthlyPrecip

normPrecip = as.numeric(as.character(precip$normal))
### Fill in the various stages with your code


### Add any additional variables that you will need here
repdays = c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
day = rep(repdays, times = daysInMonth) + temp$Day
normhigh = temp$NormalHigh
normlow = temp$NormalLow
rechigh = temp$RecordHigh
reclow = temp$RecordLow
obshigh = temp$High
obslow = temp$Low
temp$dayofyear = day
newhigh = temp[which(laxWeather$RecordHigh == laxWeather$High), "High"]
newlow = temp[which(laxWeather$RecordLow == laxWeather$Low), "Low"]
breakinghigh = temp[which(laxWeather$RecordHigh == laxWeather$High), "dayofyear"]
breakinglow = temp[which(laxWeather$RecordLow == laxWeather$Low) == newlow, "dayofyear"]
### Set up the graphics device to plot to pdf and layout
### the two plots on one canvas
### pdf("", width = , height = )
### layout(  )
pdf("Project1stat133ArifAli.pdf", width = 30, height = 20)  
layout(matrix(c(1,2)), width = 1, height = c(2,1))
### Call makePlotRegion to create the plotting region
### for the temperature plot

makePlotRegion(c(0,366), c(min(reclow)-10, max(rechigh)+10), "#DECB92", " ", c(1,2, 3, 2)+3, " ")
### Call drawTempRegion 3 times to add the rectangles for
### the record, normal, and observed temps
drawTempRegion(day, rechigh, reclow, "#F2E5B9")
drawTempRegion(day, normhigh, normlow, "#5A0300")
drawTempRegion(day, obshigh, obslow, "#9C7405")
### Call addGrid to add the grid lines to the plot
addGrid(cumDays+0.5, "#737271", 3)
addGrid(cumsum(c(20, rep(10, times=10))), "#DECB92", 1, vertical = FALSE)
### Add the markers for the record breaking days
segments(x0 = breakinghigh, y0 = newhigh, x1 = breakinghigh, y1 = newhigh+10)
text(x = c(breakinghigh, breakinghigh+12), y = (newhigh)+15, labels = c("Record High", newhigh), cex = 1.5)

segments(x0 = c(278, 280), y0 = c(52, 53), x1 = c(278, 280), y1 = c(52, 53)-10)
text(x = c(278, 278+12), y = 52-13, labels = c("Record Low", 52), , cex = 1.5)
text(x = c(280, 280+12), y = 53-11, labels = c("Record Low", 53), , cex = 1.5)
### Add the titles 
title(main = "The City of Los Angeles Weather 2011", adj = 0)
text(x=c(25,25), y=110, labels = "Temperature", cex = 2)
text(x=c(35, 35), y=105, labels = "Bars represent range between the daily high and low")

rect(180, 15, 186, 10+35, col = "#F2E5B9", border = NA)
text(x=175, y=45, labels = "Record High")
text(x=175, y=15, labels = "Record Low")
rect(181, 10+15, 185, 10+25, col = "#5A0300", border = NA)

rect(182, 10+20, 184, 10+30, col = "#9C7405", border = NA)
segments(x0=c(181,181), x1=c(174,174), y0=c(25,35))
segments(x0=174, y0=25, y1=35)
text(x=c(166, 166), y=30, labels = "Normal Range")
### Call makePlotRegion to create the plotting region
### for the precipitation plot
makePlotRegion(c(1,366), c(0, 5), "#DECB92", " ", c(3,2,1,2)+3, "Precipitation", xtop=FALSE)
### Call monthPrecip 12 times to create each months 
### cumulative precipitation plot. To do this use 
for (i in (1:12)) 
{daysofmonth = temp[temp$Month == i, "dayofyear"]
 NormPrecipofmonth = normPrecip[i]
 dayprecipsofmonth = temp[temp$Month == i, "Precip"]
 monthPrecip(daysofmonth, dayprecipsofmonth, NormPrecipofmonth)
} 
### the anonymous function calls monthPrecip with the 
### appropriate arguments

### Call addGrid to add the grid lines to the plot
addGrid(cumDays+0.5, "#737271", 3)
addGrid(cumsum(c(1, 2, 3, 4, 5)), "#DECB92", 1, vertical = FALSE)
### Add the titles
text(x=15, y=3.1, labels = "NORMAL")
text(x=15, y=.75, labels = "ACTUAL")
title(main ="Precipitation", adj = 0, cex = 1.5)
title (main = "Cumulative monthly precipitation in inches compared with normal precipitation", adj =0.5)
### Close the pdf device dev.off()
dev.off()
}
### Call: finalPlot(temp = sfoWeather, precip = sfoMonthlyPrecip)
finalPlot(temp = laxWeather, precip = laxMonthlyPrecip)