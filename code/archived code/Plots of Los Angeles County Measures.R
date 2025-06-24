library(caTools)
library(ggplot2)
library(magrittr)
library(dplyr)

#set working directory to where csv files are
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statements of Votes—Final CSV")

#read in data from Los Angeles County CSV document
csvinput=read.csv("Los Angeles County Organized.csv")

#set input to DATA for repeatability
DATA=csvinput

#blow up data
#View(DATA)

#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Santa Monica, Measure CS, Hotel Tax"

measure="CS"

yescolumn="SANTA.MONICA.CY.GEN.CS.YES"
nocolumn="SANTA.MONICA.CY.GEN.CS.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Santa Monica, Measure DT, Property Sales Tax"

measure="DT"

yescolumn="SANTA.MONICA.CY.GEN.DT.YES"
nocolumn="SANTA.MONICA.CY.GEN.DT.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Santa Monica, Measure EM, State of Emergency Rent Adjustment"

measure="EM"

yescolumn="SANTA.MONICA.CY.GEN.EM.YES" 
nocolumn="SANTA.MONICA.CY.GEN.EM.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Santa Monica, Measure GS, Property Transfer Tax"

measure="GS"

yescolumn="SANTA.MONICA.CY.GEN.GS.YES"
nocolumn="SANTA.MONICA.CY.GEN.GS.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Pasadena, Measure H, Charter Amendment For Rent Control"

measure="H"

yescolumn="PASADENA.CITY.GEN.MUNI.H.YES"
nocolumn="PASADENA.CITY.GEN.MUNI.H.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Los Angeles, Proposition LH, Low-Income Rental Housing"

measure="LH"

yescolumn="LOS.ANGELES.GEN.LH.YES"
nocolumn="LOS.ANGELES.GEN.LH.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Santa Monica, Measure RC, Rental Control Law Amendment"

measure="RC"

yescolumn="SANTA.MONICA.CY.GEN.RC.YES"
nocolumn="SANTA.MONICA.CY.GEN.RC.NO"   
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Los Angeles, Proposition ULA, Tax on $5 Million House Sales"

measure="ULA"

yescolumn="LOS.ANGELES.GEN.ULA.YES"
nocolumn="LOS.ANGELES.GEN.ULA.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#


#get yvar (ratio of yes votes on a given measure)
DATA$yvar=(DATA[[yescolumn]]/(DATA[[yescolumn]]+DATA[[nocolumn]]))

#get xvar (average of ratio of democratic votes across contests). Numbers entered for index since all dataframes are identical
DATA$xvar=rowMeans(data.frame((DATA[2]/(DATA[2]+DATA[3])),(DATA[4]/(DATA[4]+DATA[5])),(DATA[6]/(DATA[6]+DATA[7])),(DATA[8]/(DATA[8]+DATA[9])),(DATA[10]/(DATA[10]+DATA[11])),(DATA[12]/(DATA[12]+DATA[13])),(DATA[14]/(DATA[14]+DATA[15]))))

#make regressor
lm.out=lm(yvar~xvar, data=DATA)




#set working directory to where figs go
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/figs/Los Angeles County-Linear Regression on Measures")

#automize file name
png(paste("Los Angeles County Measure ",measure," Plot.png",sep=""))





#plot scatter plot and LSR line and mark up graph
plot(DATA$yvar~DATA$xvar, pch=19, xlim=c(0,1), ylim=c(0,1),
     main = measurefull,
     sub="Los Angeles County Precinct by Precinct Scatterplot and Best Fit Line", 
     xlab = "Support for Democrats (avg. of ratio of Dem. votes in state contests)",
     ylab = paste("Support for Measure",measure,"(ratio of 'Yes' votes)")
)
abline(coefficients(lm.out))
pearsons_r=cor(DATA$xvar,DATA$yvar,use="pairwise.complete.obs",method='pearson')
legend("topleft", bty="n", legend=paste("R:",format(pearsons_r, digits=4)))

#export as image
dev.off()





#write out statistical analysis of relationship as txt file in new folder
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/figs/Los Angeles County-Linear Regression on Measures/Plot Statistics")
sink(paste("Los Angeles County Measure ",measure," Plot Statistics.txt",sep=""))
print("Summary of Linear Regressor:")
print(summary(lm.out))
print("Scatterplot Coordinates:")
print((lm.out[13]))
sink()

