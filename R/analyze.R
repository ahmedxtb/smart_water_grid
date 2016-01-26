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
dir.cache <- "~/git/smart_water_grid/cache"
dir.results <- "~/git/smart_water_grid/results"
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
        data$Time <- ymd_hms(data$Time, tz = "CET")
        
        # Add variablename as column
        data$Variable <- variablename
    } else {
        # Add Variable column
        data <- data.frame(Time=character(), Value=numeric(), Variable=character())
    }

    # Reorder columns
    return(data[, c(1, 3, 2)])
}

readfilesraw <- function(fileslist) {
    listofdataframes <- lapply(fileslist, function(file) readfile(file))
    merged <- Reduce(function(x, y) rbind(x, y), listofdataframes)
    
    return(merged)
}

# Read files and aggregate by time
readfilesandaggregate <- function(fileslist, aggregate.period="hours") {
    listofdataframes <- lapply(fileslist, function(file) aggregatebytime(readfile(file), period=aggregate.period))
    
    # Merge vertically
    merged <- Reduce(function(x, y) rbind(x, y), listofdataframes)
    
    return(merged)
}

aggregatebytime <- function(dataframe, period="hours") {
    allowedperiods <- c("none", "secs", "mins", "hours", "days", "weeks", "months", "quarters", "years")

    # Calculate new aggregate vars 'mean', 'var', 'min', 'max'
    statistics <- function(values) { c(mean=mean(values), var=var(values), min=min(values), max=max(values)) }

    # Checks
    if (nrow(na.omit(dataframe)) == 0) {
        return(na.omit(dataframe))
    }
     if (! period %in% allowedperiods) {
         warning(paste("Invalid period option ", "'", period, "'", sep=""))         
     } else {
         if (! period == "none") {
             # Break into time periods
             dataframe$Time <- as.POSIXct(cut(dataframe$Time, breaks=period))
         }
     }
    
    # Aggregate and convert to proper data frame
    newdataframe <- as.data.frame(as.list(aggregate(Value ~ Time + Variable, dataframe, FUN=statistics)))
    
    # Give proper variable name
    names(newdataframe) <- gsub("Value.", "", names(newdataframe))
    
    return(newdataframe)
}

selecttimerange <- function(dataframe, begintime = -Inf, endtime = Inf) {
    subset(dataframe, Time >= begintime & Time <= endtime)
}

wideformat <- function(df.timevariablevalue) {
    # Melt 'Statistics' column
    df.responsepredictors.molten <- melt(df.timevariablevalue, id.vars=c("Time", "Variable"), variable.name="Statistic", value.name="Value")
    
    # Cast
    dcast(df.responsepredictors.molten, Time ~ Variable + Statistic, value.var="Value")
}

drawheatmap <- function(dataframe) {
    dataframe.wide <- wideformat(dataframe)
    matrixwithouttimecolumn <- as.matrix(subset(dataframe.wide, select=-Time))
    rownames(matrixwithouttimecolumn) <- as.character(dataframe.wide$Time)
    heatmap.2(matrixwithouttimecolumn, Rowv=NA, Colv=NA, na.color='Grey', scale="column", trace="none", margins=c(5,5))
}

drawheatmapwithdendrogram <- function(dataframe) {
    dataframe.wide <- wideformat(dataframe)
    matrixwithouttimecolumn <- as.matrix(subset(dataframe.wide, select=-Time))
    rownames(matrixwithouttimecolumn) <- as.character(dataframe.wide$Time)
    heatmap.2(matrixwithouttimecolumn, Rowv=NA, Colv=TRUE, na.color='Grey', scale="column", trace="none", margins=c(5,5))
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



## Create dataset for each category and take hourly aggregates

# Why aggregates? Less NAs, less peak events, smaller data sets, easier finding signals with small time lag

df.eventlab.hourly <- readfilesandaggregate(files.eventlab, aggregate.period="hours")
df.temperature.hourly <- readfilesandaggregate(files.temperature, aggregate.period="hours")
df.flow.hourly <- readfilesandaggregate(files.flow, aggregate.period="hours")
df.pressure.hourly <- readfilesandaggregate(files.pressure, aggregate.period="hours")
df.conductivity.hourly <- readfilesandaggregate(files.conductivity, aggregate.period="hours")
df.acidity.hourly <- readfilesandaggregate(files.acidity, aggregate.period="hours")
df.turbidity.hourly <- readfilesandaggregate(files.turbidity, aggregate.period="hours")
# Ignoring 'other' variables for now

# # Write/read aggregated datasets to/from dir.cache
setwd(dir.cache)
# save(df.eventlab.hourly, file="eventlab.hourly.Rdata")
# save(df.temperature.hourly, file="temperature.hourly.Rdata")
# save(df.flow.hourly, file="flow.hourly.Rdata")
# save(df.pressure.hourly, file="pressure.hourly.Rdata")
# save(df.conductivity.hourly, file="conductivity.hourly.Rdata")
# save(df.acidity.hourly, file="acidity.hourly.Rdata")
# save(df.turbidity.hourly, file="turbidity.hourly.Rdata")
load(file="eventlab.hourly.Rdata")
load(file="temperature.hourly.Rdata")
load(file="flow.hourly.Rdata")
load(file="pressure.hourly.Rdata")
load(file="conductivity.hourly.Rdata")
load(file="acidity.hourly.Rdata")
load(file="turbidity.hourly.Rdata")
setwd(dir.data)



## Plot hourly maximums and variances for Eventlab data

plot.eventlab.hourly.max <- ggplot(df.eventlab.hourly, aes(x=Time, y=max, color=Variable)) + geom_point() + ggtitle("Eventlab measurements (hourly max)") + geom_hline(yintercept=1, color="orange") + geom_hline(yintercept=1.5, color="red")
plot.eventlab.hourly.var <- ggplot(df.eventlab.hourly, aes(x=Time, y=var, color=Variable)) + geom_point() + ggtitle("Eventlab measurements (hourly variances)")
plot.eventlab.hourly.max + scale_y_log10()
plot.eventlab.hourly.var



## Plot hourly means and variances for other variables

# Hourly means
plot.temperature.hourly.mean <- ggplot(df.temperature.hourly, aes(x=Time, y=mean, color=Variable)) + geom_point() + ggtitle("Temperature (hourly means)")
plot.flow.hourly.mean <- ggplot(df.flow.hourly, aes(x=Time, y=mean, color=Variable)) + geom_point() + ggtitle("Flow (hourly means)")
plot.pressure.hourly.mean <- ggplot(df.pressure.hourly, aes(x=Time, y=mean, color=Variable)) + geom_point() + ggtitle("Pressure (hourly means)")
plot.conductivity.hourly.mean <- ggplot(df.conductivity.hourly, aes(x=Time, y=mean, color=Variable)) + geom_point() + ggtitle("Conductivity (hourly means)")
plot.acidity.hourly.mean <- ggplot(df.acidity.hourly, aes(x=Time, y=mean, color=Variable)) + geom_point() + ggtitle("Acidity (hourly means)")
plot.turbidity.hourly.mean <- ggplot(df.turbidity.hourly, aes(x=Time, y=mean, color=Variable)) + geom_point() + ggtitle("Turbidity (hourly means)")

plot.temperature.hourly.mean
plot.flow.hourly.mean
plot.pressure.hourly.mean + scale_y_log10()
plot.conductivity.hourly.mean + scale_y_log10()
plot.acidity.hourly.mean + scale_y_log10()
plot.turbidity.hourly.mean + scale_y_log10()

# Hourly vars (to investigate deltas/variability)
plot.temperature.hourly.var <- ggplot(df.temperature.hourly, aes(x=Time, y=var, color=Variable)) + geom_point() + ggtitle("Temperature (hourly vars)")
plot.flow.hourly.var <- ggplot(df.flow.hourly, aes(x=Time, y=var, color=Variable)) + geom_point() + ggtitle("Flow (hourly vars)")
plot.pressure.hourly.var <- ggplot(df.pressure.hourly, aes(x=Time, y=var, color=Variable)) + geom_point() + ggtitle("Pressure (hourly vars)")
plot.conductivity.hourly.var <- ggplot(df.conductivity.hourly, aes(x=Time, y=var, color=Variable)) + geom_point() + ggtitle("Conductivity (hourly vars)")
plot.acidity.hourly.var <- ggplot(df.acidity.hourly, aes(x=Time, y=var, color=Variable)) + geom_point() + ggtitle("Acidity (hourly vars)")
plot.turbidity.hourly.var <- ggplot(df.turbidity.hourly, aes(x=Time, y=var, color=Variable)) + geom_point() + ggtitle("Turbidity (hourly vars)")

plot.temperature.hourly.var + scale_y_log10()
plot.flow.hourly.var + scale_y_log10()
plot.pressure.hourly.var + scale_y_log10()
plot.conductivity.hourly.var + scale_y_log10()
plot.acidity.hourly.var + scale_y_log10()
plot.turbidity.hourly.var + scale_y_log10()



## Explore single Eventlab variable

pattern.response="FR-MOBMS-vitnor1-meetwaarde"
time.begin.1=ymd("2015-06-27", tz = "CET")
time.end.1=ymd("2015-07-04", tz = "CET")
time.begin.2=ymd_hms("2015-06-29T00:00:00", tz = "CET")
time.end.2=ymd_hms("2015-06-29T12:00:00", tz = "CET")
time.begin.3=ymd_hms("2015-06-29T02:00:00", tz = "CET")
time.end.3=ymd_hms("2015-06-29T03:00:00", tz = "CET")
threshold.orange=1
threshold.red=1.5
files.response <- list.files(path=dir.data, pattern=pattern.response)
variables.response <- createvariablefromfilename(files.response)

setwd(dir.data)
df.eventlab.response <- readfilesraw(files.response)
plot.response <- ggplot(df.eventlab.response, aes(x=Time, y=Value)) + geom_point() + geom_hline(yintercept=threshold.orange, color="orange") + geom_hline(yintercept=threshold.red, color="red")
plot.response + ggtitle("Eventlab - days timescale") + scale_x_datetime(limits = as.POSIXct(c(time.begin.1, time.end.1)))
plot.response + ggtitle("Eventlab - hours timescale") + scale_x_datetime(limits = as.POSIXct(c(time.begin.2, time.end.2)))
plot.response + ggtitle("Eventlab - minutes timescale") + scale_x_datetime(limits = as.POSIXct(c(time.begin.3, time.end.3)))



## Investigate interesting time range with Eventlab peaks

time.begin=ymd("2015-06-28", tz = "CET")
time.end=ymd("2015-07-04", tz = "CET")
plot.response + ggtitle("Eventlab - days timescale") + scale_x_datetime(limits = as.POSIXct(c(time.begin, time.end)))
plot.eventlab.hourly.max + scale_x_datetime(limits = as.POSIXct(c(time.begin, time.end))) + scale_y_log10()
plot.turbidity.hourly.mean + scale_x_datetime(limits = as.POSIXct(c(time.begin, time.end))) + scale_y_log10() + geom_line()
#plot.turbidity.hourly.var + scale_x_datetime(limits = as.POSIXct(c(time.begin, time.end))) + scale_y_log10() + geom_line()
#plot.acidity.hourly.mean + scale_x_datetime(limits = as.POSIXct(c(time.begin, time.end))) + scale_y_log10() + geom_line()
plot.conductivity.hourly.var + scale_x_datetime(limits = as.POSIXct(c(time.begin, time.end))) + scale_y_log10() + geom_line()
plot.pressure.hourly.var + scale_x_datetime(limits = as.POSIXct(c(time.begin, time.end))) + scale_y_log10()
#plot.temperature.hourly.var + scale_x_datetime(limits = as.POSIXct(c(time.begin, time.end))) + scale_y_log10()



## Correlate Eventlab hourly max (Heatmap with certain time period)

selectedvariables <- levels(df.eventlab.hourly$Variable)
df.eventlab.selection <- subset(df.eventlab.hourly,
                                Variable %in% selectedvariables &
                                Time >= ymd_hms("2015-06-01T00:00:00", tz = "CET") & Time <= ymd_hms("2015-07-04T12:00:00", tz = "CET"),
                                select=c(Time, Variable, max)
)

# Calculate alarm if max > 1
df.eventlab.selection$alarm <- 1 * (df.eventlab.selection$max > 1.5)
df.eventlab.selection <- subset(df.eventlab.selection, select=-max)

#drawheatmap(df.eventlab.selection)
drawheatmapwithdendrogram(df.eventlab.selection)
# Might give us ideas about which Eventlab sensor alarms are related




## Correlate interesting variables (FR.MOBMS.vitnor1.meetwaarde_var with flow vars)

# Select single variable from specific Eventlab dataset as response
df.response <- subset(df.eventlab.hourly, Variable=="FR.MOBMS.vitnor1.meetwaarde", select=c(Time, Variable, var))

# Select dataset containing predictors
df.predictors <- subset(df.flow.hourly, select=c(Time, Variable, var))

# Merge vertically
df.merged <- rbind(df.response, df.predictors)

# Correlation plot 

# Dendrogram



## Machine Learning

# * Split dataset into training and test sets
# * Apply ML algorithm (RF, GBM, Tree model?)
# * Predict and validate
# * We are intested in list of most important predictors
