# prerequisites 
# library(tidyverse)
library(tidyverse)
library(readxl)
library(readr)

#preparing the data for merging =================================================================================================================
# import data september plot data
sept <- read_csv("raw data/20200923_SeptemberPlotData_v1.csv")


# transposes the data, turns rows into columns, fixes the nasty way it imports the Block column (added a filler row)
sept<-as.data.frame(sept)
sept <- rownames_to_column(sept, var = "Block")

sept <- t(sept)

# removes the filler row
names(sept)<-sept[1,]
sept<-sept[-1,]


#makes column headers the first row, and then removes first row
colnames(sept)<-as.character(unlist(sept[1,]))
sept<-sept[-1,]


#replaces name of column "Plant_num" to "Plant" so it matches the BM data table
#makes it easier to merge
colnames(sept)[colnames(sept) == "Plant_num"] <-"Plant"

# the column names are strings and so are 
# the corresponding data points have to convert to integers so they can be compared with the Biomass data 2<-as.numeric(sept[ ,2])
s14<-as.numeric(sept[, 1:4])
sept[ ,1:4]<-s14

# import biomass data 
BM<- read_excel("raw data/20200927_BiomassPlotData (2).xlsx")

#replace SALA with SALA2 in BM and BOFL with BOFL3
len<-NROW(BM)

for (i in 1:len){
if(BM[i, 4]=="SALA"){
 BM[i,4]<-"SALA2"}

if (BM[i,4]=="BOFL"){
  BM[i,4]<-"BOFL3"
  
  }
  
}


# rearrange columns so that block, plot, and plant are the first three columns 
# drops empty data entries
BM %>%
  
 
  rename(Species="Species") %>% 
  select(Block, Plot, Plant, Species, Biomass) %>%
  drop_na(Biomass)
  


as_tibble(BM)

#MERGING THE SEPTEMBER DATA AND THE BIOMASS DATA =================================================================================================
# combines sept and BM data table by the columns by using the shared columns Block, Plot, Plant as the key 
# drops the repeated columns and some random empty columns
# renames Mass.y to Mass
merge <-merge(sept, BM, by=c("Block", "Plot", "Plant")) 
merge<-merge[-c(13:19)] 

colnames(merge)[colnames(merge) == "Mass.y"] <-"Mass"

#changes characters to other numeric datatypes in merge
#block, plot, plant, num_stem, num_sh, num_leaves are integers
#Plant_ species is char
#TH, CD, BC, Mass, bag_wt, true_mass are doubles

d13<-as.double(merge[ ,13])
merge[, 13]<-d13

i<-as.integer(unlist(merge[ ,1:4]))
merge[ ,1:4]<-i


d79<-as.double(unlist(merge[ ,7:9]))
merge[ ,7:9]<-d79

i1012<-as.integer(unlist(merge[ ,10:12]))
merge[ ,10:12]<-i1012


#adding bag wt and true mass to the merged data =====================================================================================================
#adds a new column to merged data, allocated for bag_wt 
merge<-mutate(merge, bag_wt=60.2)


#assigns the bag weight to the correct plants
n<-NROW(merge)
for(i in 1:n){
if ((merge[i,1]==1 || merge[i,1] ==2 ||merge[i,1]== 3 || merge[i,1]==4 || merge[i,1]==5)){
  merge[i,14]<-5.43}
  
if ((merge[i,1]==6 && merge[i,2]<=13)){
  merge[i,14]<-6.82
}  

if ((merge[i,1]==6 && merge[i,2]>=14 && merge[i,2]<=24)){
  merge[i,14]<-6.32
}
if (merge[i,1]==6 && merge[i,2]==13 && (merge[i,3]==9 || merge[i,3]==10)){
  merge[i,14]<-6.34
}
}


# creates a new column and calculates the true_mass
merge<-mutate(merge, true_mass=Biomass-bag_wt)

#removes columns 4 and 5 from merge, renames CD and BC columns
merge<-merge[ ,-(4:5)]
colnames(merge)[colnames(merge) == "CD/furthest ramet"] <-"CD"
colnames(merge)[colnames(merge) == "BC/ # ramet"] <-"BC"



