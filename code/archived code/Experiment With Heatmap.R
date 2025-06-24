library(caTools)
library(ggplot2)
library(magrittr)
library(dplyr)

#set working directory to where csv files are
setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statements of Votes—Final CSV")

#read in data from San Diego County CSV document
csvinput=read.csv("Los Angeles County Organized.csv")

#set input to DATA for repeatability
DATA=csvinput

#blow up data
#View(DATA)
#---------------------------------------------------CHANGE THESE TWO VARIABLES EVERY COUNTY----------------------------------------------------------------#
#choose measure to analyze
measure="LH"

measurefull="Los Angeles Proposition ULA \n ''Mansion Tax''"

#names of measure vote columns
yescolumn="LOS.ANGELES.GEN.ULA.YES" 
nocolumn="LOS.ANGELES.GEN.ULA.NO" 

#---------------------------------------------------CHANGE THESE TWO VARIABLES EVERY COUNTY----------------------------------------------------------------#
#get yvar (ratio of yes votes on a given measure)
DATA$yvar=(DATA[[yescolumn]]/(DATA[[yescolumn]]+DATA[[nocolumn]]))

#get xvar (average of ratio of democratic votes across contests). Numbers entered for index since all dataframes are identical
DATA$xvar=rowMeans(data.frame((DATA[2]/(DATA[2]+DATA[3])),(DATA[4]/(DATA[4]+DATA[5])),(DATA[6]/(DATA[6]+DATA[7])),(DATA[8]/(DATA[8]+DATA[9])),(DATA[10]/(DATA[10]+DATA[11])),(DATA[12]/(DATA[12]+DATA[13])),(DATA[14]/(DATA[14]+DATA[15]))))

#make regressor
lm.out=lm(yvar~xvar, data=DATA)
#and weighted regressor
lm2.out=lm(yvar~xvar, data=DATA, weights = DATA[[yescolumn]]+DATA[[nocolumn]])




#set working directory to where figs go
#setwd("/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/figs/Los Angeles County-Linear Regression on Measures")

#automize file name
#png(paste("Los Angeles County Measure ",measure," Plot.png",sep=""))


#try some heatmap stuff
library(ggplot2)
library(dplyr)
DATA$zvar = DATA[[yescolumn]]+DATA[[nocolumn]]
ggplot(DATA, aes(x=xvar, y=yvar, size=zvar)) +  geom_point(alpha=0.5, color="dodgerblue1") +
  scale_size(range = c(0.5, 3.5), name="Precinct Size") +
  geom_abline(slope=lm2.out[[1]][2], intercept = lm2.out[[1]][1], color = "blue1") +
  xlab("Support for Democrats \n (Average Ratio in State Contests)") +
  ylab(paste("Support for Measure",measure,"(Ratio of 'Yes' Votes)")) +
  ggtitle(measurefull, subtitle = "Precinct by Precinct Scatterplot")


