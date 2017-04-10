# Boeing: Plotting Multiple Variables against the Timestamps

setwd("/Users/Hao/Projects/RPI-DARPA-DS/")

# Install Packages
install.packages("reshape2")
install.packages("xts")
install.packages("dygraphs")
require(ggplot2)
require(reshape2)
require(xts)
require(dygraphs)

# Data Import and Preprocessing
# 
# mydata <- read.csv("Fake Data Boeing Defect Distances.csv", header = TRUE, stringsAsFactors = FALSE)
mydata <- read.csv("Defects.csv", header = TRUE, stringsAsFactors = FALSE)

# Using dygraph package to make it interactive time series plot
# 
# Scale the variables of interest to [0, 1] and add it mydata as a new column: [varname].Scaled
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
mydata$EnergyFbk..J.Scaled <- range01(mydata$EnergyFbk..J.)
mydata$PowerFbk..W.Scaled <- range01(mydata$PowerFbk..W.)
mydata$Exposure.Time.Scaled <- range01(mydata$Exposure.Time)
mydata$UTS.Scaled <- range01(mydata$UTS)
# Use mydata$XFER.SysTime and convert the dataset into a xts (Extensible Time Series) object
#   Also, pick the variables you want here, so that the result object won't be too large
mydata.xts <- xts(mydata[, c("EnergyFbk..J.Scaled", "PowerFbk..W.Scaled", "Exposure.Time.Scaled")], as.POSIXlt(mydata$XFER.SysTime, origin = Sys.time()))
# Run dygraph with range selector
dygraph(mydata.xts, main = "Converted System Time vs. Multiple Variables w/ Interactive Range Selector") %>% dyRangeSelector()
# Now go play with it

#
# Belows are abandoned scripts, all to be ignored, just put here as reference
# 

ggplot(data = mydata, aes(x = XFER.SysTime)) + 
  geom_line(aes(y = EnergyFbk..J.)) +
  geom_line(aes(y = PowerFbk..W.)) +
  geom_line(aes(y = Exposure.Time))
  
# Issues:
#   = Missing lables
#   - Gaps in timestamps
#   - Timestamps are meaningless
#   - Scaling of data
#   - Resolution
#   

# Here is an improved approach 
#   (http://r-statistics.co/ggplot2-Tutorial-With-R.html#6.2%20Plot%20multiple%20timeseries%20on%20same%20ggplot)
# 
# Unscaled; no UTS
df <- melt(mydata[, c("XFER.SysTime", "EnergyFbk..J.", "PowerFbk..W.", "Exposure.Time")], id = "XFER.SysTime")
ggplot(df) + geom_line(aes(x = XFER.SysTime, y = value, color = variable)) + labs(title="XFER.SysTime vs Multiple Variables: Unscaled; no UTS")
#
# Unscaled; with UTS
df <- melt(mydata[, c("XFER.SysTime", "EnergyFbk..J.", "PowerFbk..W.", "Exposure.Time", "UTS")], id = "XFER.SysTime")
ggplot(df) + geom_line(aes(x = XFER.SysTime, y = value, color = variable)) + labs(title="XFER.SysTime vs Multiple Variables; Unscaled; with UTS")
#
# Scaled; no UTS
df <- melt(mydata[, c("XFER.SysTime", "EnergyFbk..J.Scaled", "PowerFbk..W.Scaled", "Exposure.Time.Scaled")], id = "XFER.SysTime")
ggplot(df) + geom_line(aes(x = XFER.SysTime, y = value, color = variable)) + labs(title="XFER.SysTime vs Multiple Variables (Scaled to (0, 1))")
# 
# Scaled; with UTS
df <- melt(mydata[, c("XFER.SysTime", "EnergyFbk..J.Scaled", "PowerFbk..W.Scaled", "Exposure.Time.Scaled", "UTS.Scaled")], id = "XFER.SysTime")
ggplot(df) + geom_line(aes(x = XFER.SysTime, y = value, color = variable)) + labs(title="XFER.SysTime vs Multiple Variables (Scaled to (0, 1))")