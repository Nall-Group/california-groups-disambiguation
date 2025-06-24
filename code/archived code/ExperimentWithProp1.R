#import libraries
library(readxl)
library(tidyverse)
library(stringr)
library(caTools)
library(ggplot2)
library(magrittr)
library(dplyr)

ExperimentPath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/ExperimentWithPropositions.xlsx"

#combines multipage excel file into one list of dataframes
combinesheets=function(file){
  sheets = readxl::excel_sheets(file)
  tibble = lapply(sheets, function(x) readxl::read_excel(file, sheet = x))
  data_frame = lapply(tibble, as.data.frame)
  names(data_frame) = sheets
  return(data_frame)
}

#turn every na to 0 and every **** to 0
natozero=function(dflist){
  for(i in seq(length(dflist))){
    dflist[[i]][is.na(dflist[[i]])] = 0   
    dflist[[i]][dflist[[i]]=="****"] = 0                        
  }
  return(dflist)
}


#transform dataframe list into a single CSV file
transform_to_csv=function(dflist,merge_column){
  csv=Reduce(function(dtf1, dtf2) full_join(dtf1, dtf2, by = merge_column, all.x = TRUE),dflist)
}

#export csv to final output
exportcsv=function(csv,name){
  file_name=paste('/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statements of Votes—Final CSV/',name,sep='')
  write.csv(csv,file=file_name, row.names=FALSE,na='')
  print(paste('exported as',file_name))
}



combinedExperiment=combinesheets(ExperimentPath)


Experimentdflist=combinedExperiment

#for each dataframe, filter only rows with total values and columns of precinct and vote counts
for(i in seq(length(Experimentdflist))){
  Experimentdflist[[i]]=filter(Experimentdflist[[i]],`TYPE`=="TOTAL")
  # Experimentdflist[[i]]=select(Experimentdflist[[i]],2,9,10)
}


#transform to CSV
Experimentcsv=transform_to_csv(Experimentdflist,'PRECINCT')

#select only contest candidates we want and order them Democratic-Republican
Experimentcsv=select(Experimentcsv,contains("precinct"),
                     contains("gavin newsom"),contains("brian dahle"),
                     contains("kounalakis"),contains("jacobs"),
                     contains("shirley n weber"),contains("rob bernosky"),
                     contains("malia m cohen"),contains("lanhee j chen"),
                     contains("fiona ma"),contains("jack m guerrero"),
                     contains("rob bonta"),contains("nathan hochman"),
                     contains("alex padilla"),contains("mark p meuser"),
                     contains("ula yes"),contains("ula no"),
                     contains("lh yes"),contains("lh no"),
                     contains("proposition 1 yes"), contains("proposition 1 no"),
                     contains("proposition 27 yes"), contains("proposition 27 no"),
                     contains("proposition 29 yes"), contains("proposition 29 no"),
                     contains("proposition 31 yes"), contains("proposition 31 no"),
                     contains("proposition 26 yes"), contains("proposition 26 no"),
                     contains("proposition 28 yes"), contains("proposition 28 no"),
                     )



DATA=Experimentcsv

measurefull="California Proposition 31\n Uphold Ban on Flavored Tobacco Products"
measure="Prop 31"

yescolumn="PROPOSITION 31 YES" 
nocolumn="PROPOSITION 31 NO" 



#get yvar (ratio of yes votes on a given measure)
DATA$yvar=(DATA[[yescolumn]]/(DATA[[yescolumn]]+DATA[[nocolumn]]))

#get xvar (average of ratio of democratic votes across contests). Numbers entered for index since all dataframes are identical
DATA$xvar=rowMeans(data.frame((DATA[2]/(DATA[2]+DATA[3])),(DATA[4]/(DATA[4]+DATA[5])),(DATA[6]/(DATA[6]+DATA[7])),(DATA[8]/(DATA[8]+DATA[9])),(DATA[10]/(DATA[10]+DATA[11])),(DATA[12]/(DATA[12]+DATA[13])),(DATA[14]/(DATA[14]+DATA[15]))))


#make regressor
lm.out=lm(yvar~xvar, data=DATA, weights = DATA[[yescolumn]]+DATA[[nocolumn]])



#try some heatmap stuff
library(ggplot2)
library(dplyr)
DATA$zvar = DATA[[yescolumn]]+DATA[[nocolumn]]
ggplot(DATA, aes(x=xvar, y=yvar, size=zvar)) +  geom_point(alpha=0.7, color='darkgreen') +
  scale_size(range = c(0.05, 4)) +
  geom_abline(slope=lm.out[[1]][2], intercept = lm.out[[1]][1], color = "springgreen3") +
  ggtitle(measurefull,subtitle="Precinct Scatterplot in LA County") +
  xlab("Democraticness of Precinct") +
  ylab(paste("Support for ",measure))
  





