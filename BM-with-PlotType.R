#prerequisites
library(tidyverse)
library(readxl)
#requires you to run the september-plot-data-cleaned-merged script


#this script sorts the biomass file by plot type
#reads the biomass file
BM<- read_excel("raw data/20200927_BiomassPlotData (2).xlsx")


#adds another column for plot_type
BM<-mutate(BM, plot_type="test")

#getting number of rows in BM 
n<-NROW(BM)
m<-NCOL(BM)

BM[is.na(BM)]=0


#categorizes control
for(i in 2:n){
  if (BM[i, 3]=="Cattails" &&  BM[i,2]!=BM[i-1,2]) {
    BM[i,6]<-"CONTROL"}}


#categorizes mono 
for (j in 1:n){
  if (BM[j,4]==1 || BM[j,4]==2 ||BM[j,4]==3 ||BM[j,4]==4 ||BM[j,4]==5 ||BM[j,4]==6 ||BM[j,4]==7 ||BM[j,4]==8 ||BM[j,4]==9 ||BM[j,4]==10 
      ){
   BM[j,6]<-"MONO"
  }
  }
  
for (k in 2:n){
  if (BM[k,6]=="MONO" && BM[k-1,6]=="CONTROL"){
    BM[k-1, 6]<-"MONO"
  }
  
}

BM[1,6]<-"MONO"

#categorizes mix_pa
for (p in 2:n){
  if ((BM[p-1,3]=="PEVI" && BM[p,3]=="ACAM") || (BM[p,3]=="PEVI" && BM[p-1,3]=="ACAM" )|| (BM[p-1,3]=="SALA" && BM[p,3]=="SPEU") || (BM[p,3]=="SALA" && BM[p-1,3]=="SPEU" )|| (BM[p-1,3]=="SCCY" && BM[p,3]=="JUEF") || (BM[p,3]=="SCCY" && BM[p-1,3]=="JUEF" )){
    BM[p,6]<-"MIX_PA"
    BM[p-1,6]<-"MIX_PA"
  }
  
}

for (p in 2:n){
  if (BM[p,6]=="MIX_PA" && BM[p-1,6]=="CONTROL"){
    BM[p-1,6]<-"MIX_PA"
  }
}



#categorizes mix_npa

for (p in 2:n){
  if ((BM[p-1,3]=="SALA" && BM[p,3]=="ACAM") || (BM[p,3]=="SALA" && BM[p-1,3]=="ACAM" )|| (BM[p-1,3]=="JUEF" && BM[p,3]=="SPEU") || (BM[p,3]=="JUEF" && BM[p-1,3]=="SPEU" )|| (BM[p-1,3]=="SCCY" && BM[p,3]=="PEVI") || (BM[p,3]=="SCCY" && BM[p-1,3]=="PEVI" )){
    BM[p,6]<-"MIX_NPA"
    BM[p-1,6]<-"MIX_NPA"
  }
  
}

for (p in 2:n){
  if (BM[p,6]=="MIX_NPA" && BM[p-1,6]=="CONTROL"){
    BM[p-1,6]<-"MIX_NPA"
  }
}




