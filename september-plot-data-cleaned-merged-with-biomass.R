# prerequisites 
# library(tidyverse)
library(tidyverse)



# import data and coerce a the data frame to a tibble 
# with as_tibble("file name"), replace "file name" with
# the name of the file in your library


X20200923_SeptemberPlotData_v1 <- read_csv("raw data/20200923_SeptemberPlotData_v1.xlsx")
as_tibble("X20200923_SeptemberPlotData_v1")



# transposes the data, turns rows into columns
X20200923_SeptemberPlotData_v1<-t(X20200923_SeptemberPlotData_v1) 

#removes data points with missing information
X20200923_SeptemberPlotData_v1<-X20200923_SeptemberPlotData_v1 %>%filter(!is.na("num_Typha"), !is.na("TH"), !is.na("CD/furthest ramet"), !is.na("BC/ # ramet"), !is.na("num_stem"), !is.na("num_leaves"), !is.na("num_SH"))



# the column names are strings and so are 
# the corresponding data points have to convert to integers so they can be compared with the Biomass data 



# import biomass data and coerce data frame as a tibble

X20200927_BiomassPlotData <- read_csv("raw data/20200927_BiomassPlotData.xlsx")
as_tibble(X20200927_BiomassPlotData)



# rearrange columns so that block, plot, and plant 
# are the first three columns 

X20200927_BiomassPlotData <- select(X20200927_BiomassPlotData, Block, Plot, Plant, Species, Mass)

#removes data points with missing information
X20200927_BiomassPlotData<- X20200927_BiomassPlotData%>%filter(!is.na(Mass))
view(X20200927_BiomassPlotData)


SeptemberBiomassMerge<-left_join(X20200923_SeptemberPlotData_v1,X20200927_BiomassPlotData, by = Block, Plot, Plant, Species)
view(SeptemberBiomassMerge)















