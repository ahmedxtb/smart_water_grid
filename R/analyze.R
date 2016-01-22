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

# Print session info
sessionInfo()

# Variables
dir.data <- "~/git/smart_water_grid/data"
dir.results <- "~/git/smart_water_grid/data"
#url.data <- "https://db.tt/wf2nir4S"

# Workdir
setwd(dir.data)



## FUNCTIONS

readfiles <- function(fileslist) {
    listofdataframes <- lapply(fileslist, function(file) read.csv(file, 
                                                                  comment.char = "#", 
                                                                  col.names = c("Time", file), 
                                                                  colClasses = c("character", "numeric"),
                                                                  na.strings = c("", " ", "CalcFailed", "Calc Failed", "BadInput", "Bad Input", "PtCreated", "Pt Created")))

    merged <- Reduce(function(x, y) merge(x, y, all=TRUE), listofdataframes)
    
    # Convert to POSIXct/POSIXt time format
    merged$Time <- ymd_hms(merged$Time)
    
    return(merged)
}
# test
#head(readfiles(list.files()[1:3]))


selecttimerange <- function(dataframe, begintime = -Inf, endtime = Inf) {
    subset(dataframe, Time >= begintime & Time <= endtime)
}
# test
#selecttimerange(head(readfiles(list.files()[1:3])), ymd("2014-01-01"), ymd("2015-03-01"))



## EXPLORE DATA AND DATA QUALITY

# There are 885 files in dir.data, all of which are csv files
# Example: Dump_PE-FR-Deelbalansgebied-Westeinde-levering.csv
# We are interested in vitnor data as response variables. Specifically, we focus on values >1 for these variables
# File name seems to contain variable info that is being measured
# There are 186 files whose filename contains vitnor

files.select <- list.files(path=dir.data, pattern="vitnor.*csv$")[1:9]
data.select <- readfiles(files.select)
summary(data.select)

# Measurements seem to be done in time range Januari 2014 to December 2015 (only checked sample of files)
# We observe that some variables in this subset contain many missing values (after being merged with other variables)
# A few exploratory plots make clear that for some variables data is only available in specific time ranges

ggplot(data.select, aes_string(x="Time", y=names(data.select)[2])) + geom_point()
ggplot(data.select, aes_string(x="Time", y=names(data.select)[10])) + geom_point()


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


# 1. Investigating suitable vitnor variables

# Since availability of vitnor1, vitnor2 and vitnor3 variables seems to be related (when one is available
# the others are often too), we only look at the vitnor1 variables for now (62 files).
files.vitnor.select <- list.files(path=dir.data, pattern="vitnor1.*csv$")[1:30]
data.vitnor.select <- readfiles(files.vitnor.select)
#hist(data.vitnor.select[!is.na(data.vitnor.select[,2]), "Time"], breaks = "weeks")
summary(data.vitnor.select)

#head(data.vitnor.select[data.vitnor.select>1])


# Based on large blocks of consecutive data seen in previous exploratory plots we decided to choose
# the following group of 3 vitnor variables as response variables, in a time range of 1 month
files.response.select <- list.files(path=dir.data, pattern="MOLAB.*vitnor.*csv$")
data.response.select <- selecttimerange(readfiles(files.response.select), ymd("2015-04-01"), ymd("2015-04-30"))
summary(data.response.select)
