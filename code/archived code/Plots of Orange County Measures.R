library(caTools)
library(ggplot2)
library(magrittr)
library(dplyr)

#set working directory to where csv files are
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statements of Votes—Final CSV")

#read in data from Orange County CSV document
csvinput=read.csv("Orange County Organized.csv")

#set input to DATA for repeatability
DATA=csvinput

#blow up data
#View(DATA)


#---------------------------------------------------CHANGE THESE TWO VARIABLES EVERY COUNTY----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Costa Mesa, Measure K, Residential Neighborhood Revitalization"

measure="K"

yescolumn="K.City.of.Costa.Mesa.Yes" 
nocolumn="K.City.of.Costa.Mesa.No"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Laguna Beach, Measure Q, Overlay Zoning District"

measure="Q"

yescolumn="Q.City.of.Laguna.Beach.Yes"
nocolumn="Q.City.of.Laguna.Beach.No"
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#
#choose measure to analyze, save name, shorthand, and column names

measurefull="Yorba Linda, Measure Z, Rezoning Requirements"

measure="Z"

yescolumn= "Z.City.of.Yorba.Linda.Yes"       
nocolumn= "Z.City.of.Yorba.Linda.No"      
#---------------------------------------------------EACH SECTION IS DIFFERENT MEASURE----------------------------------------------------------------#

#get yvar (ratio of yes votes on a given measure)
DATA$yvar=(DATA[[yescolumn]]/(DATA[[yescolumn]]+DATA[[nocolumn]]))

#get xvar (average of ratio of democratic votes across contests). Numbers entered for index since all dataframes are identical
DATA$xvar=rowMeans(data.frame((DATA[2]/(DATA[2]+DATA[3])),(DATA[4]/(DATA[4]+DATA[5])),(DATA[6]/(DATA[6]+DATA[7])),(DATA[8]/(DATA[8]+DATA[9])),(DATA[10]/(DATA[10]+DATA[11])),(DATA[12]/(DATA[12]+DATA[13])),(DATA[14]/(DATA[14]+DATA[15]))))

#make regressor
lm.out=lm(yvar~xvar, data=DATA)




#set working directory to where figs go
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/figs/Orange County-Linear Regression on Measures")

#automize file name
png(paste("Orange County Measure ",measure," Plot.png",sep=""))





#plot scatter plot and LSR line and mark up graph
plot(DATA$yvar~DATA$xvar, pch=19, xlim=c(0,1), ylim=c(0,1),
     main = measurefull,
     sub="Orange County Precinct by Precinct Scatterplot and Best Fit Line", 
     xlab = "Support for Democrats (avg. of ratio of Dem. votes in state contests)",
     ylab = paste("Support for Measure",measure,"(ratio of 'Yes' votes)")
)
abline(coefficients(lm.out))
pearsons_r=cor(DATA$xvar,DATA$yvar,use="pairwise.complete.obs",method='pearson')
legend("topleft", bty="n", legend=paste("R:",format(pearsons_r, digits=4)))

#export as image
dev.off()





#write out statistical analysis of relationship as txt file in new folder
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/figs/Orange County-Linear Regression on Measures/Plot Statistics")
sink(paste("Orange County Measure ",measure," Plot Statistics.txt",sep=""))
print("Summary of Linear Regressor:")
print(summary(lm.out))
print("Scatterplot Coordinates:")
print((lm.out[13]))
sink()

