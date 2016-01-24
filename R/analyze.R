### Analyze Water Quality Data

# Central question: What are the correlations between changes in water quality measued by Eventlab sensors (vitnor)
# and other real-time measurements, statuses and alarm values?
# See "./doc/Vitens Data Challenge 07122015.pdf" file for details about data



## PREPARE

# Libraries
library(dplyr)
library(reshape2)
library(lubridate)
library(ggplot2)
library(caret)
library(GGally)     # for ggpairs
library(Rmisc)
library(gplots)     # for heatmap.2

# Print session info
sessionInfo()

# Variables
dir.data <- "~/git/smart_water_grid/data"
dir.results <- "~/git/smart_water_grid/data"
#url.data <- "https://db.tt/wf2nir4S"

# Workdir
setwd(dir.data)



## FUNCTIONS

createvariablefromfilename <- function(filename) {
    # Chop off beginning and end of filename and convert to valid variable name
    variablename <- make.names(gsub("(^Dump_|.csv$)", "", filename))
}

readfile <- function(file) {
    variablename <- createvariablefromfilename(file)
    
    data <- read.csv(file, comment.char = "#", col.names = c("Time", "Value"), colClasses = c("character", "numeric"),
                     na.strings = c("", " ", "CalcFailed", "Calc Failed", "Bad", "BadInput", "Bad Input", "PtCreated", "Pt Created", "CommFail", "ScanOff", "Configure", "I/OTimeout"))
    
    # Check for empty file
    if(nrow(data) != 0) {
        # Convert to POSIXct/POSIXt time format
        data$Time <- ymd_hms(data$Time)
        
        # Add variablename as column
        data$Variable <- variablename
    } else {
        # Add Variable column
        data <- data.frame(Time=character(), Value=numeric(), Variable=character())
    }

    # Reorder columns
    return(data[, c(1, 3, 2)])
}

# Read files and optionally aggregate
readfiles <- function(fileslist, aggregate.period="hour", aggregate.method="none") {
    if (! aggregate.method == "none") {
        listofdataframes <- lapply(fileslist, function(file) aggregatebytime(readfile(file), period=aggregate.period, method=aggregate.method))
    } else {
        listofdataframes <- lapply(fileslist, function(file) readfile(file))
    }
    
    # Merge vertically
    merged <- Reduce(function(x, y) rbind(x, y), listofdataframes)
    
    return(merged)
}

aggregatebytime <- function(dataframe, period="hour", method="mean") {
    if (nrow(dataframe) == 0) {
        return(dataframe)
    }
    if (! period %in% c("hour", "day", "week", "month")) {
        warning(paste("Invalid period option ", "'", period, "'", sep=""))
        return(na.omit(dataframe))
    }
    if (! method %in% c("mean", "max", "var")) {
        warning(paste("Invalid method option ", "'", method, "'", sep=""))
        return(na.omit(dataframe))
    }    
    dataframe$Time <- as.POSIXct(cut(dataframe$Time, breaks=period))
    na.omit(aggregate(Value ~ Time + Variable, dataframe, FUN=method))
}


## List files for each data category (eventlab, temperature, flow, etc)

files.all <- list.files(path=dir.data, pattern="csv$")
files.eventlab <- list.files(path=dir.data, pattern="vitnor.*csv$")
files.temperature <- list.files(path=dir.data, pattern="TM.*csv$")
files.flow <- list.files(path=dir.data, pattern="(FT|VO).*csv$")
files.pressure <- list.files(path=dir.data, pattern="(PT|DO)[0123456789][0123456789]-.*csv$")
files.conductivity <- list.files(path=dir.data, pattern="(FR-PNB_TR00QI03PV|FR-POH_-TR00QI03PV).*csv$")
files.acidity <- list.files(path=dir.data, pattern="(FR-PNB_TR00QI01PV|FR-POH_-TR00QI02PV|FR-PSP-TR00QI01|FR-PTW_TR01QI01PV).*csv$")
files.turbidity <- list.files(path=dir.data, pattern="(FR-PTW_TR01QI02PV|FR-PSP-TR00QI02|FR-PNB_TR00QI02PV|FR-POH_-TR00QI01PV).*csv$")
files.other <- Reduce(setdiff, list(files.all, files.eventlab, files.temperature, files.flow, files.pressure, files.conductivity, files.acidity, files.turbidity))



## Create dataset for each category

# Note that we took hourly aggregates (less NAs, less peak events, smaller data sets, easier finding signals with small time lag)

#df.eventlab <- readfiles(files.eventlab, aggregate.period="hour", aggregate.method="max")
df.eventlab <- readfiles(files.eventlab[1:30], aggregate.period="hour", aggregate.method="max")
df.temperature <- readfiles(files.temperature, aggregate.period="hour", aggregate.method="mean")
df.flow <- readfiles(files.flow, aggregate.period="hour", aggregate.method="mean")
df.pressure <- readfiles(files.pressure, aggregate.period="hour", aggregate.method="mean")
df.conductivity <- readfiles(files.conductivity, aggregate.period="hour", aggregate.method="mean")
df.acidity <- readfiles(files.acidity, aggregate.period="hour", aggregate.method="mean")
df.turbidity <- readfiles(files.turbidity, aggregate.period="hour", aggregate.method="mean")
# Ignoring 'other' vars for now
#df.other <- readfiles(files.other, aggregate.period="hour", aggregate.method="mean")

# TODO: troubleshoot df.flow not found



## Eventlab vars: convert from numeric to factor (normal/alarm)



## Multivariate plots (for multiple categories)

threshold.orange=1
threshold.red=1.5
plot.eventlab <- ggplot(df.eventlab, aes(x=Time, y=Value, color=Variable)) + geom_point() + ggtitle("Eventlab measurements (hourly max)")+ geom_hline(yintercept=threshold.orange, color="orange") + geom_hline(yintercept=threshold.red, color="red")

plot.temperature <- ggplot(df.temperature, aes(x=Time, y=Value, color=Variable)) + geom_point() + ggtitle("Temperature (hourly means)")
plot.flow <- ggplot(df.flow, aes(x=Time, y=Value, color=Variable)) + geom_point() + ggtitle("Flow (hourly means)")
plot.pressure <- ggplot(df.pressure, aes(x=Time, y=Value, color=Variable)) + geom_point() + ggtitle("Pressure (hourly means)")
plot.conductivity <- ggplot(df.conductivity, aes(x=Time, y=Value, color=Variable)) + geom_point() + ggtitle("Conductivity (hourly means)")
plot.acidity <- ggplot(df.acidity, aes(x=Time, y=Value, color=Variable)) + geom_point() + ggtitle("Acidity (hourly means)")
plot.turbidity <- ggplot(df.turbidity, aes(x=Time, y=Value, color=Variable)) + geom_point() + ggtitle("Turbidity (hourly means)")

plot.eventlab + scale_y_log10()
plot.temperature + scale_y_log10()    # Can be negative (linear scale?)
plot.flow + scale_y_log10()           # Can be negative (linear scale?)
plot.pressure + scale_y_log10()
plot.conductivity + scale_y_log10()
plot.acidity + scale_y_log10()
plot.turbidity + scale_y_log10()



## Heatmaps (at least for eventlab, possibly for other categories)



## Dendrogram (related vars within categories)



## Select 1 response var

## Select 1 interesting dataset with predictors (for example turbidity)

## Select interesting time range (maybe not necessary)

## Merge predictor set and response var

## Remove rows with NA (consider these as clean datasets that we can work with)


## Split into train and test sets

## Apply ML algorithm (RF, GBM, Tree)

## Predict and validate

## Check list of important predictors