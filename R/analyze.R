### Analyze Water Quality Measurements


## Central question:
# What are the correlations between changes in water quality measued by Eventlab sensors (vitnor)
# and other real-time measurements, statuses and alarm values?

## See "./doc/Vitens Data Challenge 07122015.pdf" file for details about data


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



## EXPLORE DATA AND DATA QUALITY

files.all <- list.files(path=dir.data, pattern=".csv$")
files.all

# 885 files in dir.data, all of them are csv files
# Example: Dump_PE-FR-Deelbalansgebied-Westeinde-levering.csv
# We are interested in vitnor data as response variables
# File name seems to contain variable info that is being measured
# 186 files whose filename contains vitnor


# Example file
file.select <- files.all[2]

# Read data (ignore comment lines)
df.select <- read.csv(file.select, comment.char = "#", na.strings = c("", "Calc Failed"))
head(df.select)

# Convert Timestamp
df.select$Timestamp <- ymd_hms(df.select$Timestamp)
# Rename Value to filename
varname <- make.names(file.select)
names(df.select) <- c("Time", varname)

# Check for NA values
sum(is.na(df.select))
summary(df.select)

# Exploratory plot
ggplot(df.select, aes_string(x="Time", y=varname)) + geom_point()


