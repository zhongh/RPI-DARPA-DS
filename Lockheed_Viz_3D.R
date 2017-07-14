#
# 3D visualizations
#
library(rgl)
#
plot3d(cbind(mydata$Adhesive.Out.Time, mydata$Prep..to.Bond.Time, mydata$Contamination.Amount), col = as.numeric(mydata$Gic))


install.packages("plot3D")
install.packages("plot3Drgl")
library(scatterplot3d)
library(plot3D)
library(plot3Drgl)
#
scatterplot3d(x = mydata$Adhesive.Out.Time, y= mydata$Prep..to.Bond.Time, z = mydata$Contamination.Amount, color = as.numeric(mydata$Surface.Preparation))
legend("topright", levels(mydata$Surface.Preparation), pch = 16, col = as.numeric(mydata$Surface.Preparation))
#
scatter3Drgl(x = mydata$Adhesive.Out.Time, y= mydata$Prep..to.Bond.Time, z = mydata$Contamination.Amount, col = as.numeric(mydata$Surface.Preparation))
scatter3D(x = mydata$Adhesive.Out.Time, y= mydata$Prep..to.Bond.Time, z = mydata$Contamination.Amount, col = as.numeric(mydata$Surface.Preparation))


plot3d(cbind(mydata$Adhesive.Out.Time, mydata$Prep..to.Bond.Time, mydata$Contamination.Amount), col = as.numeric(mydata$Gic))
plot3d(cbind(mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT"), ]$Adhesive.Out.Time, 
             mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT"), ]$Prep..to.Bond.Time, 
             mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT"), ]$Contamination.Amount), 
       col = as.numeric(mydata[which(mydata$Adhesive.Out.Time.Level=="LowAOT"), ]$y), 
       size = 20,
       xlab = "Adhesive Out Time", ylab = "Prep to Bond Time", zlab = "Contamination Amount",
       main = "y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Contamination.Amount (at Low AOT")
plot3d(cbind(mydata[which(mydata$Adhesive.Out.Time.Level=="HighAOT"), ]$Adhesive.Out.Time, 
             mydata[which(mydata$Adhesive.Out.Time.Level=="HighAOT"), ]$Prep..to.Bond.Time, 
             mydata[which(mydata$Adhesive.Out.Time.Level=="HighAOT"), ]$Contamination.Amount), 
       col = as.numeric(mydata[which(mydata$Adhesive.Out.Time.Level=="HighAOT"), ]$y), 
       size = 20,
       xlab = "Adhesive Out Time", ylab = "Prep to Bond Time", zlab = "Contamination Amount",
       main = "y ~ Adhesive.Out.Time + Prep..to.Bond.Time + Contamination.Amount (at High AOT")
#

## Scatterplot and clusters
scatterplot3d(mydata[, c("Adhesive.Out.Time", "Prep..to.Bond.Time", "Contamination.Amount")], color = as.numeric(mydata$Contaminate.Type), pch = as.numeric(mydata$Surface.Preparation))
scatterplot3d(mydata[, c("Gic", "Prep..to.Bond.Time", "Contamination.Amount")], color = as.numeric(mydata$Contaminate.Type), pch = as.numeric(mydata$Surface.Preparation))
scatter3Drgl(x = mydata$Gic, y= mydata$Prep..to.Bond.Time, z = mydata$Contamination.Amount, color = as.numeric(mydata$Contaminate.Type), pch = as.numeric(mydata$Surface.Preparation))

## (+) Interactive 3D plot
install.packages("rgl")
library(rgl)
plot3d(cbind(Adhesive.Out.Time, Prep..to.Bond.Time, Contamination.Amount), col = as.numeric(y), size = 10)
plot3d(cbind(X.Cohesive.1, X.Interfacial.1, X.Laminate.1), col = Gic)