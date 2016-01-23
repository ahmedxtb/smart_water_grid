### Analyze Water Quality Measurements

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
library(GGally)
library(Rmisc)

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
    
    data <- read.csv(file, comment.char = "#", col.names = c("Time", variablename), colClasses = c("character", "numeric"),
                     na.strings = c("", " ", "CalcFailed", "Calc Failed", "Bad", "BadInput", "Bad Input", "PtCreated", "Pt Created", "CommFail", "ScanOff", "Configure", "I/OTimeout"))
    
    # Convert to POSIXct/POSIXt time format
    data$Time <- ymd_hms(data$Time)
    
    return(data)
}

readfiles <- function(fileslist) {
    listofdataframes <- lapply(fileslist, function(file) readfile(file))
    merged <- Reduce(function(x, y) merge(x, y, all=TRUE), listofdataframes)

    return(merged)
}
# test
#head(readfiles(list.files()[1:3]))

selecttimerange <- function(dataframe, begintime = -Inf, endtime = Inf) {
    subset(dataframe, Time >= begintime & Time <= endtime)
}
# test
#selecttimerange(head(readfiles(list.files()[1:3])), ymd("2014-01-01"), ymd("2015-03-01"))

addsensoralarms <- function(dataframe) {
    # Select variable names containing 'vitnor'
    matchingnames <- grep("vitnor", names(dataframe), value=TRUE)
    
    # Select entries of these variables above thresholds (redalarms threshold at 1.5, orangealarms at 1)
    redalarms <- dataframe[, matchingnames] > 1.5
    orangealarms <- dataframe[, matchingnames] > 1 & !(dataframe[, matchingnames] > 1.5)
    
    # Create new variable names for each of the matching names by appending '.alarm'
    newnames <- paste(matchingnames, ".alarm", sep="")
    
    # Append new variables to dataframe. Set 'Green' as default, 'Orange' for orangealarms, 'Red' for redalarms
    dataframe[, newnames] <- 'Green'
    dataframe[, newnames][orangealarms] <- 'Orange'
    dataframe[, newnames][redalarms] <- 'Red'
    
    # Convert new variables to factors
    dataframe[, newnames] <- lapply(dataframe[, newnames], function (column) factor(column, levels=c('Green', 'Orange', 'Red')) )
    
    return(dataframe)
}
# test
#dataframe <- data.frame(matrix(seq(0.2, 2, by=0.2), nrow=2, ncol=5))
#names(dataframe)=c("Time", "test.vitnor1.lh", "test.vitnor2.lh", "test.vitnor3.sj", "test.var2.sj")
#addsensoralarms(dataframe)


## EXPLORE DATA AND DATA QUALITY

# There are 885 files in dir.data, all of which are csv files
# Example: Dump_PE-FR-Deelbalansgebied-Westeinde-levering.csv
# We are interested in vitnor data as response variables. Specifically, we focus on values >1 for these variables
# File name seems to contain variable info that is being measured
# There are 186 files whose filename contains vitnor

files.select <- list.files(path=dir.data, pattern="vitnor.*csv$")[1:3]
data.select <- readfiles(files.select)
summary(data.select)

# Measurements seem to be done in time range Januari 2014 to December 2015 (only checked sample of files)
# We observe that some variables in this subset contain many missing values (after being merged with other variables)
# A few exploratory plots make clear that for some variables data is only available in specific time ranges

ggplot(data.select, aes_string(x="Time", y=names(data.select)[2])) + geom_point()

# Checking number of files of certain measurement type (vitnor, temperature, flow, etc)
# Note that we updated the pattern to filter for pressure measurements wrt the (likely incomplete/incorrect) documentation
files.all <- list.files(path=dir.data, pattern="csv$")
files.vitnor <- list.files(path=dir.data, pattern="vitnor.*csv$")
files.temperature <- list.files(path=dir.data, pattern="TM.*csv$")
files.flow <- list.files(path=dir.data, pattern="(FT|VO).*csv$")
files.pressure <- list.files(path=dir.data, pattern="(PT|DO)[0123456789][0123456789]-.*csv$")
files.conductivity <- list.files(path=dir.data, pattern="(FR-PNB_TR00QI03PV|FR-POH_-TR00QI03PV).*csv$")
files.acidity <- list.files(path=dir.data, pattern="(FR-PNB_TR00QI01PV|FR-POH_-TR00QI02PV|FR-PSP-TR00QI01|FR-PTW_TR01QI01PV).*csv$")
files.turbidity <- list.files(path=dir.data, pattern="(FR-PTW_TR01QI02PV|FR-PSP-TR00QI02|FR-PNB_TR00QI02PV|FR-POH_-TR00QI01PV).*csv$")
files.other <- Reduce(setdiff, list(files.all, files.vitnor, files.temperature, files.flow, files.pressure, files.conductivity, files.acidity, files.turbidity))

length(files.all)
length(files.vitnor)
length(files.temperature)
length(files.flow)
length(files.pressure)
length(files.conductivity)
length(files.acidity)
length(files.turbidity)
length(files.other)

# Check: should be TRUE
#length(files.all) == length(files.vitnor) + length(files.temperature) + length(files.flow) + length(files.pressure) + length(files.conductivity) + length(files.acidity) + length(files.turbidity) + length(files.other)


# We first want to find a high quality subset that is suitable for analysis (of correlations, etc)
# Questions:
# 1. Which of the vitnor variable(s) should we choose as response variables?
#    Note that we are specifically interested in data points with values >1 for these variables
# 2. Which of the non-vitnor variables should we choose as interesting predictor variables?
# 3. Which time range should we choose in which both predictors and response variables are very well available?


# 1 & 3) Investigating suitable response variables and time range

# Based on large blocks of consecutive data seen in exploratory plots and counts of variables exceeding the threshold (>1)
# we decided to choose the following vitnor variable as response variable, in the following time range
pattern.response="FR-MOBMS-vitnor1-meetwaarde"
#time.begin=ymd("2015-06-25")
#time.end=ymd("2015-07-05")
time.begin=ymd_hms("2015-06-29T00:00:00")
time.end=ymd_hms("2015-06-29T12:00:00")
files.response <- list.files(path=dir.data, pattern=pattern.response)
vars.response <- createvariablefromfilename(files.response)
data.response <- selecttimerange(readfiles(files.response), time.begin, time.end)
summary(data.response)
ggplot(data.response, aes_string(x="Time", y=names(data.response)[2])) + geom_point() + ggtitle("Eventlab measurements (unaggregated)")

pattern.response="FR-MOBMS-vitnor1-meetwaarde"
time.begin.1=ymd("2015-06-25")
time.end.1=ymd("2015-07-05")
time.begin.2=ymd_hms("2015-06-29T00:00:00")
time.end.2=ymd_hms("2015-06-29T12:00:00")
time.begin.3=ymd_hms("2015-06-29T02:00:00")
time.end.3=ymd_hms("2015-06-29T03:00:00")
threshold.orange=1
threshold.red=1.5
files.response <- list.files(path=dir.data, pattern=pattern.response)
vars.response <- createvariablefromfilename(files.response)
data.response <- readfiles(files.response)
data.response.1 <- selecttimerange(data.response, time.begin.1, time.end.1)
data.response.2 <- selecttimerange(data.response, time.begin.2, time.end.2)
data.response.3 <- selecttimerange(data.response, time.begin.3, time.end.3)
plot.response.1 <- ggplot(data.response.1, aes_string(x="Time", y=names(data.response)[2])) + geom_point() + ggtitle("Eventlab - day timescale") + geom_hline(yintercept=threshold.orange, color="orange") + geom_hline(yintercept=threshold.red, color="red")
plot.response.2 <- ggplot(data.response.2, aes_string(x="Time", y=names(data.response)[2])) + geom_point() + ggtitle("Eventlab - hour timescale") + geom_hline(yintercept=threshold.orange, color="orange") + geom_hline(yintercept=threshold.red, color="red")
plot.response.3 <- ggplot(data.response.3, aes_string(x="Time", y=names(data.response)[2])) + geom_point() + ggtitle("Eventlab - minute timescale") + geom_hline(yintercept=threshold.orange, color="orange") + geom_hline(yintercept=threshold.red, color="red")
plot.response.1
plot.response.2
plot.response.3
#multiplot(plot.response.1, plot.response.2, cols=2)


# # Find timestamps for vitnor events above threshold
# # Select variable names containing 'vitnor'
# vitnorvariables <- grep("vitnor", names(data.response.select), value=TRUE)
# # Select entries of these variables above threshold
# vitnoralarms <- data.response.select[, vitnorvariables] > 1
# # Find alarm timestamps
# vitnoralarmrows <- apply(vitnoralarms, 1, function(row) any(row, na.rm=TRUE))
# vitnoralarmtimes <- data.response.select[vitnoralarmrows,]$Time
# hist(vitnoralarmtimes, breaks="day")
# # Count alarms per variable
# apply(vitnoralarms, 2, function(column) sum(column, na.rm=TRUE))
# 
# # Count number of non-NA values per variable
# apply(data.response.select, 2, function(column) sum(!is.na(column)))


# 2) Select predictors

#files.predictor.select <- union(files.turbidity, files.acidity)
files.predictor <- files.temperature[1:10]
data.predictor <- selecttimerange(readfiles(files.predictor), ymd("2015-06-25"), ymd("2015-07-05"))
summary(data.predictor)
ggplot(data.predictor, aes_string(x="Time", y=names(data.predictor)[8])) + geom_point()

ggpairs(data.predictor[2:4])
#data.predictor.select
