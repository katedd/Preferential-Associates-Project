#prerequisites
library(tidyverse)
library(pastecs)
#requires you to run the september-plot-data-cleaned-merged, scatterplots, and BM-with-PlotType scripts for the BM data table





#prepares BM data table for summaries=======================================================================================
# add true mass column to BM
BM<-mutate(BM, bag_wt=0)


#adds bag_wt according to plot, block, and plot type
length<-NROW(BM)

for (i in 1:length){
  if (BM[i,6]=="MIX_PA" || BM[i,6]=="MIX_NPA" || BM[i,6]=="CONTROL"){
    BM[i,7]<-41.73}

if(BM[i,1]<=5 && BM[i,6]=="MONO"){
  BM[i,7]<-5.43
}
if (BM[i,1]==6 && BM[i,2]<=13 && BM[i,6]=="MONO"){
  BM[i,7]<-6.82
}
if (BM[i,1]==6 && BM[i,2]>=14 && BM[i,2]<= 24 && BM[1,6]=="MONO"){
  BM[i,7]<-6.34
}
if (BM[i,1]==6 && BM[i,2]==13 && BM[i,4]>=9 && BM[i,4]<=10){
  BM[i,7]<-6.34
}

}

# compute the true mass
BM[,5]<-as.numeric(unlist(BM[,5]))
BM<-mutate(BM, true_mass=Biomass-bag_wt)



#Biomass of all 10 target species by each plot type over all six blocks (Summary Table 1)===========================================================================

#grouped by plot type
#filters out data entries with same plot types in the BM data table
#makes a summary table for each plot type's biomass

mono<-filter(BM, plot_type=="MONO" & Species!="Cattails")
mono<-stat.desc(mono[,8])

mix_pa<-filter(BM, plot_type=="MIX_PA"  & Species!="Cattails")
mix_pa<-stat.desc(mix_pa[,8])

mix_npa<-filter(BM,plot_type=="MIX_NPA"  & Species!="Cattails")
mix_npa<-stat.desc(mix_npa[,8])


#summary of biomass for each target species and typha by each plot type over all 6 blocks(Summary Table 2)==========================================================


#ACAM, by plot type over all blocks
macam<-filter(BM, Species=="ACAM" & plot_type=="MONO")
macam<-stat.desc(macam[,8])
paacam<-filter(BM, Species=="ACAM" & plot_type=="MIX_PA")
paacam<-stat.desc(paacam[,8])
npaacam<-filter(BM, Species=="ACAM" & plot_type=="MIX_NPA")
npaacam<-stat.desc(npaacam[,8])

#PEVI, by plot type over all blocks
mpevi<-filter(BM, Species=="PEVI" & plot_type=="MONO")
mpevi<-stat.desc(mpevi[,8])
papevi<-filter(BM, Species=="PEVI" & plot_type=="MIX_PA")
papevi<-stat.desc(papevi[,8])
npapevi<-filter(BM, Species=="PEVI" & plot_type=="MIX_NPA")
npapevi<-stat.desc(npapevi[,8])

#SALA, by plot type over all block=s
msala<-filter(BM, Species=="SALA" | Species=="SALA2" & plot_type=="MONO")
msala<-stat.desc(msala[,8])
pasala<-filter(BM, Species=="SALA" | Species=="SALA2" & plot_type=="MIX_PA")
pasala<-stat.desc(pasala[,8])
npasala<-filter(BM, Species=="SALA" | Species=="SALA2" & plot_type=="MIx_NPA")
npasala<-stat.desc(npasala[,8])

#SPEU, by plot type over all blocks
mspeu<-filter(BM, Species=="SPEU" & plot_type=="MONO")
mspeu<-stat.desc(mspeu[,8])
paspeu<-filter(BM, Species=="SPEU" & plot_type=="MIX_PA")
paspeu<-stat.desc(paspeu[,8])
npaspeu<-filter(BM, Species=="SPEU" & plot_type=="MIX_NPA")
npaspeu<-stat.desc(npaspeu[,8])

#SCCY by plot type over all blocks
msccy<-filter(BM, Species=="SCCY" & plot_type=="MONO")
msccy<-stat.desc(msccy[,8])
pasccy<-filter(BM, Species=="SCCY" & plot_type=="MIX_PA")
pasccy<-stat.desc(pasccy[,8])
npasccy<-filter(BM, Species=="SCCY" & plot_type=="MIX_NPA")
npasccy<-stat.desc(npasccy[,8])

#JUEF by plot type over all blocks
mjuef<-filter(BM, Species=="JUEF" & plot_type=="MONO")
mjuef<-stat.desc(mjuef[,8])
pajuef<-filter(BM, Species=="JUEF" & plot_type=="MIX_PA")
pajuef<-stat.desc(pajuef[,8])
npajuef<-filter(BM, Species=="JUEF" & plot_type=="MIX_NPA")
npajuef<-stat.desc(npajuef[,8])

#BOFL by plot type over all blocks
mbofl<-filter(BM, Species=="BOFL" & plot_type=="MONO")
mbofl<-stat.desc(mbofl[,8])
pabofl<-filter(BM, Species=="BOFL" & plot_type=="MIX_PA")
pabofl<-stat.desc(pabofl[,8])
npabofl<-filter(BM, Species=="BOFL" & plot_type=="MIX_NPA")
npabofl<-stat.desc(npabofl[,8])

#SCTA by plot type over all blocks
mscta<-filter(BM, Species=="SCTA" & plot_type=="MONO")
mscta<-stat.desc(mscta[,8])
pascta<-filter(BM, Species=="SCTA" & plot_type=="MIX_PA")
pascta<-stat.desc(pascta[,8])
npascta<-filter(BM, Species=="SCTA" & plot_type=="MIX_NPA")
npascta<-stat.desc(npascta[,8])

#SYPU by plot type over all blocks
msypu<-filter(BM, Species=="SYPU" & plot_type=="MONO")
msypu<-stat.desc(msypu[,8])
pasypu<-filter(BM, Species=="SYPU" & plot_type=="MIX_PA")
pasypu<-stat.desc(pasypu[,8])
npasypu<-filter(BM, Species=="SYPU" & plot_type=="MIX_NPA")
npasypu<-stat.desc(npasypu[,8])

#Typha by plot type over all blocks
mty<-filter(BM, Species=="Cattails" & plot_type=="MONO")
mty<-stat.desc(mty[,8])
paty<-filter(BM, Species=="Cattails" & plot_type=="MIX_PA")
paty<-stat.desc(paty[,8])
npaty<-filter(BM, Species=="Cattails" & plot_type=="MIX_NPA")
npaty<-stat.desc(npaty[,8])
cty<-filter(BM, Species=="Cattails" & plot_type=="CONTROL")
cty<-stat.desc(cty[,8])


#summaries of biomass for all 10 target species by block for each plot type (Summary Table 3)================================================================================================

#blocks 1-6, all target species, and MONO 
onem<-filter(BM, Block==1 & Species!="Cattails" & plot_type=="MONO")
onem<-stat.desc(onem[,8])

twom<-filter(BM, Block==2 & Species!="Cattails" & plot_type=="MONO")
twom<-stat.desc(twom[,8])

threem<-filter(BM, Block==3 & Species!="Cattails" & plot_type=="MONO" )
threem<-stat.desc(threem[,8])

fourm<=filter(BM, Block==4 & Species!="Cattails" & plot_type=="MONO")
fourm<-stat.desc(fourm[,8])

fivem<=filter(BM,Block==5 & Species!="Cattails" & plot_type=="MONO")
fivem<-stat.desc(fivem[i,8]) 
                
sixm<-filter(BM, Block==6 & Species!="Cattails" & plot_type="MONO")
sixm<-stat.desc(sixm[,8])


#blocks 1-6, all target species, and MIX_PA
onepa<-filter(BM, Block==1 & Species!="Cattails" & plot_type=="MIX_PA")
onepa<-stat.desc(onepa[,8])

twopa<-filter(BM, Block==2 & Species!="Cattails" & plot_type=="MIX_PA")
twopa<-stat.desc(twopa[,8])

threepa<-filter(BM, Block==3 & Species!="Cattails" & plot_type=="MIX_PA" )
threepa<-stat.desc(threepa[,8])

fourpa<=filter(BM, Block==4 & Species!="Cattails" & plot_type=="MIX_PA")
fourpa<-stat.desc(fourpa[,8])

fivepa<=filter(BM,Block==5 & Species!="Cattails" & plot_type=="MIX_PA")
fivepa<-stat.desc(fivepa[i,8]) 

sixpa<-filter(BM, Block==6 & Species!="Cattails" & plot_type=="MIX_PA")
sixpa<-stat.desc(sixpa[,8])



#blocks 1-6, all target species, and MIX_NPA
onenpa<-filter(BM, Block==1 & Species!="Cattails" & plot_type=="MIX_NPA")
onenpa<-stat.desc(onenpa[,8])

twonpa<-filter(BM, Block==2 & Species!="Cattails" & plot_type=="MIX_NPA")
twonpa<-stat.desc(twonpa[,8])

threenpa<-filter(BM, Block==3 & Species!="Cattails" & plot_type=="MIX_NPA" )
threenpa<-stat.desc(threenpa[,8])

fournpa<=filter(BM, Block==4 & Species!="Cattails" & plot_type=="MIX_NPA")
fournpa<-stat.desc(fournpa[,8])

fivenpa<=filter(BM,Block==5 & Species!="Cattails" & plot_type=="MIX_NPA")
fivenpa<-stat.desc(fivenpa[i,8]) 

sixnpa<-filter(BM, Block==6 & Species!="Cattails" & plot_type=="MIX_NPA")
sixnpa<-stat.desc(sixnpa[,8])


#blocks 1-6, all target species, and all plot types
one<-filter(BM, Block==1 & Species!="Cattails")
one<-stat.desc(one[,8])

two<-filter(BM, Block==2 & Species!="Cattails" )
two<-stat.desc(two[,8])

three<-filter(BM, Block==3 & Species!="Cattails" )
three<-stat.desc(three[,8])

four<=filter(BM, Block==4 & Species!="Cattails" )
four<-stat.desc(fournpa[,8])

five<=filter(BM,Block==5 & Species!="Cattails" )
five<-stat.desc(fivenpa[i,8]) 

six<-filter(BM, Block==6 & Species!="Cattails" )
six<-stat.desc(six[,8])



#Summaries of biomass of all Typha for each block for each plot type (Summary Table 4)===========================================================================================================


#blocks 1-6, typha, mono
onecm<-filter(BM, Block==1 & Species=="Cattails" & plot_type=="MONO")
onecm<-stat.desc(onecm[,8])

twocm<-filter(BM, Block==2 & Species=="Cattails" & plot_type=="MONO")
twocm<-stat.desc(twocm[,8])

threecm<-filter(BM, Block==3 & Species=="Cattails" & plot_type=="MONO" )
threecm<-stat.desc(threecm[,8])

fourcm<=filter(BM, Block==4 & Species=="Cattails" & plot_type=="MONO")
fourcm<-stat.desc(fourcm[,8])

fivecm<=filter(BM,Block==5 & Species=="Cattails" & plot_type=="MONO")
fivecm<-stat.desc(fivem[i,8]) 

sixcm<-filter(BM, Block==6 & Species=="Cattails" & plot_type=="MONO")
sixcm<-stat.desc(sixcm[,8])



#block 1-6, typha, mIX_PA
onecpa<-filter(BM, Block==1 & Species=="Cattails" & plot_type=="MIX_PA")
onecpa<-stat.desc(onepa[,8])

twocpa<-filter(BM, Block==2 & Species=="Cattails" & plot_type=="MIX_PA")
twocpa<-stat.desc(twocpa[,8])

threecpa<-filter(BM, Block==3 & Species=="Cattails" & plot_type=="MIX_PA" )
threecpa<-stat.desc(threecpa[,8])

fourcpa<=filter(BM, Block==4 & Species=="Cattails" & plot_type=="MIX_PA")
fourcpa<-stat.desc(fourcpa[,8])

fivecpa<=filter(BM,Block==5 & Species=="Cattails" & plot_type=="MIX_PA")
fivecpa<-stat.desc(fivecpa[i,8]) 

sixcpa<-filter(BM, Block==6 & Species=="Cattails" & plot_type=="MIX_PA")
sixcpa<-stat.desc(sixcpa[,8])


#blocks 1-6, typha, MIX_NPA
onecnpa<-filter(BM, Block==1 & Species=="Cattails" & plot_type=="MIX_NPA")
onecnpa<-stat.desc(onecnpa[,8])

twocnpa<-filter(BM, Block==2 & Species=="Cattails" & plot_type=="MIX_NPA")
twocnpa<-stat.desc(twocnpa[,8])

threecnpa<-filter(BM, Block==3 & Species=="Cattails" & plot_type=="MIX_NPA" )
threecnpa<-stat.desc(threecnpa[,8])

fourcnpa<=filter(BM, Block==4 & Species=="Cattails" & plot_type=="MIX_NPA")
fourcnpa<-stat.desc(fournpa[,8])

fivecnpa<=filter(BM,Block==5 & Species=="Cattails" & plot_type=="MIX_NPA")
fivecnpa<-stat.desc(fivecnpa[i,8]) 

sixcnpa<-filter(BM, Block==6 & Species=="Cattails" & plot_type=="MIX_NPA")
sixcnpa<-stat.desc(sixcnpa[,8])


#blocks 1-6, typha, control
onecon<-filter(BM, Block==1 & Species=="Cattails" & plot_type=="CONTROL")
onecon<-stat.desc(onecon[,8])

twocon<-filter(BM, Block==2 & Species=="Cattails" & plot_type=="CONTROL")
twocon<-stat.desc(twocon[,8])

threecon<-filter(BM, Block==3 & Species=="Cattails" & plot_type=="CONTROL" )
threecon<-stat.desc(threecon[,8])

fourcon<=filter(BM, Block==4 & Species=="Cattails" & plot_type=="CONTROL")
fourcon<-stat.desc(fourcon[,8])

fivecon<=filter(BM,Block==5 & Species=="Cattails" & plot_type=="CONTROL")
fivecon<-stat.desc(fivecon[i,8]) 

sixcon<-filter(BM, Block==6 & Species=="Cattails" & plot_type=="CONTROL")
sixcon<-stat.desc(sixcon[,8])