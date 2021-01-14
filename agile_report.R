library(ggplot2)
library(data.table)
library(rmarkdown)
#library(lubridate)

# Get command line arguments
args = commandArgs(trailingOnly=TRUE)

# Read the provided csv file
dt <- fread(args[1])
#dt[, date:=as.Date(date)]

# Make the report
render('Agile_Metrics.Rmd', output_file='Agile_Metrics.pdf')
