library(caTools)
library(ggplot2)
library(magrittr)
library(dplyr)

#set working directory to where csv files are
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statements of Votes—Final CSV")

#read in data from Alameda County CSV document
csvinput=read.csv("Alameda County Organized.csv")

#set input to DATA for repeatability
DATA=csvinput

#blow up data
View(DATA)


#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Berkeley, Measure L, Housing and Infrastructure Bond"

measure="L"

yescolumn="Bond.Measure.L...City.of.Berkeley.BONDS.YES"
nocolumn="Bond.Measure.L...City.of.Berkeley.BONDS.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Berkeley, Measure M, Vacant Residential Property Tax"

measure="M"

yescolumn="Measure.M...City.of.Berkeley.YES"
nocolumn="Measure.M...City.of.Berkeley.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Berkeley, Measure N, Low-Rent Housing"

measure="N"

yescolumn="Measure.N...City.of.Berkeley.YES"
nocolumn="Measure.N...City.of.Berkeley.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Oakland, Measure Q, Low-Rent Residential Units"

measure="Q"

yescolumn="Measure.Q...City.of.Oakland.YES"
nocolumn="Measure.Q...City.of.Oakland.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Oakland, Measure U, Public Facilities Bond"

measure="U"

yescolumn="Bond.Measure.U...City.of.Oakland.BONDS.YES"
nocolumn="Bond.Measure.U...City.of.Oakland.BONDS.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Oakland, Measure V, Just Cause for Eviction Ordinance"

measure="V"

yescolumn="Measure.V...City.of.Oakland.YES"
nocolumn="Measure.V...City.of.Oakland.NO"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#

#get yvar (ratio of yes votes on a given measure)
DATA$yvar=(DATA[[yescolumn]]/(DATA[[yescolumn]]+DATA[[nocolumn]]))

#get xvar (average of ratio of democratic votes across contests). Numbers entered for index since all dataframes are identical
DATA$xvar=rowMeans(data.frame((DATA[2]/(DATA[2]+DATA[3])),(DATA[4]/(DATA[4]+DATA[5])),(DATA[6]/(DATA[6]+DATA[7])),(DATA[8]/(DATA[8]+DATA[9])),(DATA[10]/(DATA[10]+DATA[11])),(DATA[12]/(DATA[12]+DATA[13])),(DATA[14]/(DATA[14]+DATA[15]))))

#make regressor
lm.out=lm(yvar~xvar, data=DATA)




#set working directory to where figs go
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/figs/Alameda County-Linear Regression on Measures")

#automize file name
png(paste("Alameda County Measure ",measure," Plot.png",sep=""))





#plot scatter plot and LSR line and mark up graph
plot(DATA$yvar~DATA$xvar, pch=19, xlim=c(0,1), ylim=c(0,1),
     main = measurefull,
     sub="Alameda County Precinct by Precinct Scatterplot and Best Fit Line", 
     xlab = "Support for Democrats (avg. of ratio of Dem. votes in state contests)",
     ylab = paste("Support for Measure",measure,"(ratio of 'Yes' votes)")
)
abline(coefficients(lm.out))
pearsons_r=cor(DATA$xvar,DATA$yvar,use="pairwise.complete.obs",method='pearson')
legend("topleft", bty="n", legend=paste("R:",format(pearsons_r, digits=4)))

#export as image
dev.off()





#write out statistical analysis of relationship as txt file in new folder
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/figs/Alameda County-Linear Regression on Measures/Plot Statistics")
sink(paste("Alameda County Measure ",measure," Plot Statistics.txt",sep=""))
print("Summary of Linear Regressor:")
print(summary(lm.out))
print("Scatterplot Coordinates:")
print((lm.out[13]))
sink()

