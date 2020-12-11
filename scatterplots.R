
#requires you to run the september-plot-data-cleaned-merge script and BM-with-PlotType script for the BM data table and merge data table
#prerequisites
library(tidyverse)
library(ggplot2)
library(cowplot)


#filtering out data that only relate to corresponding plant species and preparing it for gg plots=======================================================================================================
sala<-filter(merge, Plant_spec=="SALA2"| Plant_spec=="SALA")
speu<-filter(merge, Plant_spec=="SPEU")
pevi<-filter(merge,Plant_spec=="PEVI")
himo<-filter(merge, Plant_spec=="HIMO")
bofl<-filter(merge, Plant_spec=="BOFL3" | Plant_spec=="BOFL")

#making individual ggplots for biomass vs target species (TH, CD/Furthest Ramet, BC/Number of Ramets, num_leaves, num_stems) ==========================================================================================================================

#ggplot for sala (g)
s1<-ggplot(sala, aes(x=TH, y=true_mass))+geom_point(shape=1) + labs(title="SALA2: Biomass (g) vs Total Height (cm)", x="Total Height (cm)", y="Biomass (g)")+ geom_point(color='blue')
s2<-ggplot(sala, aes(x=CD, y=true_mass))+geom_point(shape=1) + labs(title="SALA2: Biomass (g) vs Furthest Ramet (cm)", x="Furthest Ramet (cm)", y="Biomass (g)")+ geom_point(color='blue')
s3<-ggplot(sala, aes(x=BC, y=true_mass))+geom_point(shape=1) + labs(title="SALA2: Biomass (g) vs Number of Ramets", x="Number of Ramets", y="Biomass (g)")+ geom_point(color='blue')
s4<-ggplot(sala, aes(x=num_stem, y=true_mass))+geom_point(shape=1) + labs(title="SALA2: Biomass (g) vs Number of stems", x="Number of Stems", y="Biomass (g)")+ geom_point(color='blue')
s5<-ggplot(sala, aes(x=num_leaves, y=true_mass))+geom_point(shape=1) + labs(title="SALA2: Biomass (g) vs Number of Leaves", x="Number of Leaves", y="Biomass (g)")+ geom_point(color='blue')

#ggplot for speu

sp1<-ggplot(speu, aes(x=TH, y=true_mass))+geom_point(shape=1) + labs(title="SPEU:Biomass (g) vs Total Height (cm)", x="Total Height (cm)", y="Biomass (g)")+ geom_point(color='green')
sp2<-ggplot(speu, aes(x=CD, y=true_mass))+geom_point(shape=1) + labs(title="SPEU: Biomass (g) vs Furthest Ramet (cm)", x="Furthest Ramet (cm)", y="Biomass (g)")+ geom_point(color='green')
sp3<-ggplot(speu, aes(x=BC, y=true_mass))+geom_point(shape=1) + labs(title="SPEU: Biomass (g) vs Number of Ramets", x="Number of Ramets", y="Biomass (g)")+ geom_point(color='green')
sp4<-ggplot(speu, aes(x=num_stem, y=true_mass))+geom_point(shape=1) + labs(title="SPEU: Biomass (g) vs Number of Stems", x="Number of Stems", y="Biomass (g)")+ geom_point(color='green')
sp5<-ggplot(speu, aes(x=num_leaves, y=true_mass))+geom_point(shape=1) + labs(title="SPEU: Biomass (g) vs Number of Leaves", x="Number of Leaves", y="Biomass (g)")+ geom_point(color='green')

#ggplot for pevi

p1<-ggplot(pevi, aes(x=TH, y=true_mass))+geom_point(shape=1) + labs(title="PEVI: Biomass (g) vs Total Height (cm)", x="Total Height (cm)", y="Biomass (g)")+ geom_point(color='red')
p2<-ggplot(pevi, aes(x=CD, y=true_mass))+geom_point(shape=1) + labs(title="PEVI: Biomass (g) vs Canopy Diameter (cm)", x="Canopy Diameter (cm)", y="Biomass (g)")+ geom_point(color='red')
p3<-ggplot(pevi, aes(x=BC, y=true_mass))+geom_point(shape=1) + labs(title="PEVI: Biomass (g) vs Basal Circumference (cm)", x="Basal Circumference (cm)", y="Biomass (g)")+ geom_point(color='red')
p4<-ggplot(pevi, aes(x=num_stem, y=true_mass))+geom_point(shape=1) + labs(title="PEVI: Biomass (g) vs Number of Stems", x="Number of Stems", y="Biomass (g)")+ geom_point(color='red')
p5<-ggplot(pevi, aes(x=num_leaves, y=true_mass))+geom_point(shape=1) + labs(title="PEVI: Biomass (g) vs Number of Leaves", x="Number of Leaves", y="Biomass (g)")+ geom_point(color='red')

#ggplot himo

h1<-ggplot(himo, aes(x=TH, y=true_mass))+geom_point(shape=1) + labs(title="HIMO: Biomass (g) vs Total Height (cm)", x="Total Height (cm)", y="Biomass (g)")+ geom_point(color='purple')
h2<-ggplot(himo, aes(x=CD, y=true_mass))+geom_point(shape=1) + labs(title="HIMO: Biomass (g) vs Canopy Diameter (cm)", x="Canopy Diameter (cm)", y="Biomass (g)")+ geom_point(color='purple')
h3<-ggplot(himo, aes(x=BC, y=true_mass))+geom_point(shape=1) + labs(title="HIMO: Biomass (g) vs Basal Circumference (cm)", x="Basal Circumference (cm)", y="Biomass (g)")+ geom_point(color='purple')
h4<-ggplot(himo, aes(x=num_stem, y=true_mass))+geom_point(shape=1) + labs(title="HIMO: Biomass (g) vs Number of stems", x="Number of Stems", y="Biomass (g)")+ geom_point(color='purple')
h5<-ggplot(himo, aes(x=num_leaves, y=true_mass))+geom_point(shape=1) + labs(title="HIMO: Biomass (g) vs Number of Leaves", x="Number of Leaves", y="Biomass (g)")+ geom_point(color='purple')

#ggplot for BOFL 

b1<-ggplot(bofl, aes(x=TH, y=true_mass))+geom_point(shape=1) + labs(title="BOFL3: Biomass (g) vs Total Height (cm)", x="Total Height (cm)", y="Biomass (g)")+ geom_point(color='pink')
b2<-ggplot(bofl, aes(x=CD, y=true_mass))+geom_point(shape=1) + labs(title="BOFL3: Biomass (g) vs Furthest Ramet (cm)", x="Furthest Ramet (cm)", y="Biomass (g)")+ geom_point(color='pink')
b3<-ggplot(bofl, aes(x=BC, y=true_mass))+geom_point(shape=1) + labs(title="BOFL3: Biomass (g) vs Number of Ramets", x="Number of Ramets", y="Biomass (g)")+ geom_point(color='pink')
b4<-ggplot(bofl, aes(x=num_stem, y=true_mass))+geom_point(shape=1) + labs(title="BOFL3: Biomass (g) vs Number of Stems", x="Number of Stems", y="Biomass (g)")+ geom_point(color='pink')
b5<-ggplot(bofl, aes(x=num_leaves, y=true_mass))+geom_point(shape=1) + labs(title="BOFL3: Biomass (g) vs Number of Leaves", x="Number of Leaves", y="Biomass (g)")+ geom_point(color='pink')


#combining the bivariate plots onto one page corresponding to characteristic=========================================================================================================================

#ggplot for TH, all target species 
plot_grid(s1, sp1, p1, h1, b1)

#ggplot for CD, all species 
plot_grid(p2,h2)

#ggplot for Furthest Ramet, all target species 
plot_grid(s2, sp2, b2)

#ggplot for BC, all species
plot_grid(p3,h3)

#ggplot for Number of Ramets, all target species
plot_grid(s3, sp3, b3)


#ggplot for num_stem, all target species 
plot_grid(s4,sp4,p4,h4,b4)


#ggplot for num_leaves, all target species 
plot_grid(s5,sp5,p5,h5,b5)



#biomass vs number of cattails// preparing the data for gg plots==========================================================================================================================================
sept<-as.data.frame(sept)

#removes extraneous columns in sept
sept<-select(sept, -13:-18)

#selects all data the that correlates to plant 11(numtypha)
cat<-filter(sept, Plant=="11")

#removes extraneous columns in cat
cat<-select(cat, -5:-12)

#changes datatype of cat from char to numeric
cat[,1:4]<-as.numeric(unlist(cat[,1:4]))

#merge cattails and biomass data
#add a species column in cat for the key 
cat<-mutate(cat, Species="Cattails")
cat<-cat[, c(1,2,5,3,4)]

#merge cat and BM
catbm <-merge(cat, BM, by=c("Block", "Plot", "Species")) 

#changes biomass column of catbm to numeric
catbm[,7]<-as.numeric(catbm[,7])

#subtract the bag weight from the biomass to get the true mass
catbm<-mutate(catbm, true_mass=Biomass-41.73)


#biomass vs number of cattails // making the gg plots==========================================================================================================================================================
#filtering out data by plot type 
cmixpa<-filter(catbm, plot_type=="MIX_PA")
cmixnpa<-filter(catbm, plot_type=="MIX_NPA")
cmono<-filter(catbm, plot_type=="MONO")
ccont<-filter(catbm, plot_type=="CONTROL")

#creating ggplots for biomass vs number of cattails for each type of plot
cp1<-ggplot(cmixpa, aes(x=num_Typha, y=Biomass))+geom_point(shape=1) + labs(title="Biomass vs Number of Cattails in MIX_PA", x="Number of Cattails", y="Biomass")+ geom_point(color='orange')
cp2<-ggplot(cmixnpa, aes(x=num_Typha, y=Biomass))+geom_point(shape=1) + labs(title="Biomass vs Number of Cattails in MIX_NPA", x="Number of Cattails", y="Biomass")+ geom_point(color='blue')
cp3<-ggplot(cmono, aes(x=num_Typha, y=Biomass))+geom_point(shape=1) + labs(title="Biomass vs Number of Cattails in Mono", x="Number of Cattails", y="Biomass")+ geom_point(color='purple')
cp4<-ggplot(ccont, aes(x=num_Typha, y=Biomass))+geom_point(shape=1) + labs(title="Biomass vs Number of Cattails in Control", x="Number of Cattails", y="Biomass")+ geom_point(color='green')

#combining all individual ggplots onto one page 
plot_grid(cp1, cp2, cp3, cp4)

