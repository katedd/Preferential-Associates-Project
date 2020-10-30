# prerequisites 
# library(tidyverse)
library(tidyverse)



# import data and coerce a the data frame to a tibble 
# with as_tibble("file name"), replace "file name" with
# the name of the file in your library
library(tidyverse)
library(readxl)
X20200923_SeptemberPlotData_v1 <- read_excel("raw data/20200923_SeptemberPlotData_v1.xlsx")
as_tibble("X20200923_SeptemberPlotData_v1")



# transposes the data, turns rows into columns
X20200923_SeptemberPlotData_v1<-t(X20200923_SeptemberPlotData_v1) 

#removes data points with missing information
X20200923_SeptemberPlotData_v1<-X20200923_SeptemberPlotData_v1 %>%filter(!is.na("num_Typha"), !is.na("TH"), !is.na("CD/furthest ramet"), !is.na("BC/ # ramet"), !is.na("num_stem"), !is.na("num_leaves"), !is.na("num_SH"))



# the column names are strings and so are 
# the corresponding data points



# import biomass data and coerce data frame as a tibble
library(readxl)
X20200927_BiomassPlotData <- read_excel("raw data/20200927_BiomassPlotData.xlsx")
as_tibble(X20200927_BiomassPlotData)



# rearrange columns so that block, plot, and plant 
# are the first three columns 
col_order<- c("Block", "Plot", "Plant", "Species", "Mass")
X20200927_BiomassPlotData <- X20200927_BiomassPlotData[,col_order]

#removes data points with missing information
X20200927_BiomassPlotData<- X20200927_BiomassPlotData%>%filter(!is.na(Mass))
view(X20200927_BiomassPlotData)


SeptemberBiomassMerge<-merge(X20200923_SeptemberPlotData_v1,X20200927_BiomassPlotData)
view(SeptemberBiomassMerge)















