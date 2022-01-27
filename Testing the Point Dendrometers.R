
# Testing the Point Dendrometers

# David Moore
# January 21st, 2022


# Explanation

# This R script should be used in conjunction with the 'Testing the Point
# Dendrometers' CRBasic script.

# It's also intended to be used with the plexiglass device for keeping all the
# point dendrometer plungers depressed the same amount and for holding all the
# point dendrometer plungers at a constant length.

# You may need to expand the 'Plots' part of the RStudio window for the figure.


# Constants

Working_Directory <- "C:\\Campbellsci\\LoggerNet"


# Import and Format the Data

setwd(Working_Directory)
Point_Dendrometer_Data <- read.table("CR1000_4_Point_Dendrometers.dat", skip = 1, header = T, sep = ",")
Point_Dendrometer_Data <- Point_Dendrometer_Data[-c(1:2), ]
Point_Dendrometer_Data$TIMESTAMP <- as.POSIXct(Point_Dendrometer_Data$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
if (!all(sapply(Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))], is.numeric))) {
  Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))] <- lapply(Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))], function (x) {
    as.numeric(as.character(x))
  })
}


# Create a Figure

par(mar = c(10, 4, 4, 2), xpd = T)
matplot(Point_Dendrometer_Data$TIMESTAMP, Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))], xaxt = 'n', yaxt = 'n', main = "Testing the Point Dendrometers", pch = 19, col = rainbow(length(colnames(Point_Dendrometer_Data[grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))]))), ylab = "Sensor Output (mV)", xlab = "Time")
Axis_Ranges <- par('usr')
axis.POSIXct(1, pretty(Point_Dendrometer_Data$TIMESTAMP)[pretty(Point_Dendrometer_Data$TIMESTAMP) > Axis_Ranges[1] & pretty(Point_Dendrometer_Data$TIMESTAMP) < Axis_Ranges[2]], format = "%b. %d, %H:%M")
axis(2, pretty(as.matrix(Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))]))[pretty(as.matrix(Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))])) > Axis_Ranges[3] & pretty(as.matrix(Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))])) < Axis_Ranges[4]])
legend("bottom", legend = gsub("[.]", "", gsub("Point_Dendrometer_Reading.", "", colnames(Point_Dendrometer_Data[grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))]))), title = "Point Dendrometer Number", pch = 19, col = rainbow(length(colnames(Point_Dendrometer_Data[grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))]))), horiz = T, inset = c(0, -0.25))


# Calculate Measurement Variability

Standard_Deviations <- sapply(Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))], function (x) {
  sd(x, na.rm = T)
})
names(Standard_Deviations) <- gsub("[.]", "", gsub("Reading[.]", "", gsub("_", " ", colnames(Point_Dendrometer_Data[, grep("Point_Dendrometer_Reading", colnames(Point_Dendrometer_Data))]))))
Standard_Deviations
