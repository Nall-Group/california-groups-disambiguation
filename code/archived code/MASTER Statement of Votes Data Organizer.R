#import libraries
library(readxl)
library(tidyverse)
library(stringr)



#initialize file paths
AlamedaFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Alameda County SVC Reformatted.xlsx"
ContraCostaFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Contra Costa County SVC Reformatted.xlsx"
FresnoFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Fresno County SVC Reformatted.xlsx"
LosAngelesFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Los Angeles County SVC Reformatted.xlsx"
OrangeFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Orange County SVC Reformatted.xlsx"
SacramentoFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Sacramento County SVC Reformatted.xlsx"
SanBenitoFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/San Benito County SVC Reformatted.xlsx"
SanBernardinoFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/San Bernardino County SVC Reformatted.xlsx"
SanDiegoFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/San Diego County SVC Reformatted.xlsx"
SanFranciscoFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/San Francisco County SVC Reformatted.xlsx"
SanMateoFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/San Mateo County SVC Reformatted.xlsx"
SantaCruzFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Santa Cruz County SVC Reformatted.xlsx"
SolanoFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Solano County SVC Reformatted.xlsx"
StanislausFilePath="/Users/davidlitman/Library/CloudStorage/Dropbox/niskanen/output/Statement of Votes—Intermediate Reformatting/Stanislaus County SVC Reformatted.xlsx"


###############################################################################################################################

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


###############################################################################################################################



#combine sheets into list of dataframes
combinedAlameda=combinesheets(AlamedaFilePath)
combinedContraCosta=combinesheets(ContraCostaFilePath)
combinedFresno=combinesheets(FresnoFilePath)
combinedLosAngeles=combinesheets(LosAngelesFilePath)
combinedOrange=combinesheets(OrangeFilePath)
combinedSacramento=combinesheets(SacramentoFilePath)
combinedSanBenito=combinesheets(SanBenitoFilePath)
combinedSanBernardino=combinesheets(SanBernardinoFilePath)
combinedSanDiego=combinesheets(SanDiegoFilePath)
combinedSanFrancisco=combinesheets(SanFranciscoFilePath)
combinedSanMateo=combinesheets(SanMateoFilePath)
combinedSantaCruz=combinesheets(SantaCruzFilePath)
combinedSolano=combinesheets(SolanoFilePath)
combinedStanislaus=combinesheets(StanislausFilePath)




###############################################################################################################################Alamedadflist=combinedAlameda
Alamedadflist=combinedAlameda

#make the first column called precinct and second column called vote type and filter by values that say total
for(i in seq(length(Alamedadflist))){
  Alamedadflist[[i]]=rename(Alamedadflist[[i]],"Precinct"=...1)
  Alamedadflist[[i]]=rename(Alamedadflist[[i]],"Vote Type"=...2)
  Alamedadflist[[i]]=filter(Alamedadflist[[i]],`Vote Type`=="Total")
}

#transform to CSV
Alamedacsv=(transform_to_csv(Alamedadflist,'Precinct'))

#get rid of na values
Alamedacsv=natozero(Alamedacsv)

#select only contest candidates we want and order them Democratic-Republican
Alamedacsv=select(Alamedacsv,"Precinct",
                  "Governor GAVIN NEWSOM","Governor BRIAN DAHLE",
                  "Lieutenant Governor ELENI KOUNALAKIS","Lieutenant Governor ANGELA E. UNDERWOOD JACOBS",
                  "Secretary of State SHIRLEY N. WEBER","Secretary of State ROB BERNOSKY",
                  "Controller MALIA M. COHEN","Controller LANHEE J. CHEN",
                  "Treasurer FIONA MA","Treasurer JACK M. GUERRERO",
                  "Attorney General ROB BONTA","Attorney General NATHAN HOCHMAN",
                  "U.S. Senator, Full Term ALEX PADILLA","U.S. Senator, Full Term MARK P. MEUSER",
                  "Bond Measure L - City of Berkeley BONDS YES","Bond Measure L - City of Berkeley BONDS NO",
                  "Measure M - City of Berkeley YES","Measure M - City of Berkeley NO",
                  "Measure N - City of Berkeley YES","Measure N - City of Berkeley NO",
                  "Measure Q - City of Oakland YES","Measure Q - City of Oakland NO",
                  "Bond Measure U - City of Oakland BONDS YES","Bond Measure U - City of Oakland BONDS NO",
                  "Measure V - City of Oakland YES","Measure V - City of Oakland NO"
                  )

#export CSV to Statements of Votes-Final CSV folder in output
exportcsv(Alamedacsv,'Alameda County Organized.csv')

View(Alamedacsv)
###############################################################################################################################
ContraCostadflist=combinedContraCosta

#slice rows to get precincts wihch occur every 5
for(i in seq(length(ContraCostadflist))){
  ContraCostadflist[[i]]=slice(ContraCostadflist[[i]],seq(7,6330,5))
}

#get rid of NAs and ****
ContraCostadflist=natozero(ContraCostadflist)

#transform to CSV
ContraCostacsv=transform_to_csv(ContraCostadflist,'Precinct')

#select only contest candidates we want and order them Democratic-Republican
ContraCostacsv=select(ContraCostacsv, contains("precinct"),
                      contains("gavin newsom"),contains("brian dahle"),
                      contains("eleni kounalakis"),contains("angela e. underwood jacobs"),
                      contains("shirley n. weber"),contains("rob bernosky"),
                      contains("malia m. cohen"),contains("lanhee j. chen"),
                      contains("fiona ma"),contains("jack m. guerrero"),
                      contains("rob bonta"),contains("nathan hochman"),
                      contains("alex padilla"),contains("mark p. meuser"),
                      contains("measure p - city of richmond yes"),contains("measure p - city of richmond no"),
                      )

#get rid of bottom two rows
ContraCostacsv=slice(ContraCostacsv,1:(n()-2))

#export CSV
exportcsv(ContraCostacsv,'Contra Costa County Organized.csv')

###############################################################################################################################
Fresnodflist=combinedFresno

#select only precinct values by filtering if something is numberic
Fresnodflist[[1]]=filter(Fresnodflist[[1]], !is.na(as.numeric(`Precinct`)))
Fresnodflist[[2]]=filter(Fresnodflist[[2]], !is.na(as.numeric(`Precinct`)))

#trasnform into csv
Fresnocsv=transform_to_csv(Fresnodflist,"Precinct")

#reorder columns
Fresnocsv=select(Fresnocsv,contains("precinct"),
                     contains("gavin newsom"),contains("brian dahle"),
                     contains("kounalakis"),contains("jacobs"),
                     contains("weber"),contains("bernosky"),
                     contains("cohen"),contains("chen"),
                     contains("fiona ma"),contains("guerrero"),
                     contains("rob bonta"),contains("nathan hochman"),
                     contains("padilla"),contains("meuser"),
                     contains("M. City of Fresno - Veterans Support Measure YES"),contains("M. City of Fresno - Veterans Support Measure NO")
                 )

#export csv
exportcsv(Fresnocsv,"Fresno County Organized.csv")
###############################################################################################################################
LosAngelesdflist=combinedLosAngeles

#for each dataframe, filter only rows with total values and columns of precinct and vote counts
for(i in seq(length(LosAngelesdflist))){
  LosAngelesdflist[[i]]=filter(LosAngelesdflist[[i]],`TYPE`=="TOTAL")
 # LosAngelesdflist[[i]]=select(LosAngelesdflist[[i]],2,9,10)
}


#transform to CSV
LosAngelescsv=transform_to_csv(LosAngelesdflist,'PRECINCT')

#select only contest candidates we want and order them Democratic-Republican
LosAngelescsv=select(LosAngelescsv,contains("precinct"),
                     contains("gavin newsom"),contains("brian dahle"),
                     contains("kounalakis"),contains("jacobs"),
                     contains("shirley n weber"),contains("rob bernosky"),
                     contains("malia m cohen"),contains("lanhee j chen"),
                     contains("fiona ma"),contains("jack m guerrero"),
                     contains("rob bonta"),contains("nathan hochman"),
                     contains("alex padilla"),contains("mark p meuser"),
                     contains("cs yes"),contains("cs no"),
                     contains("lh yes"),contains("lh no"),
                     contains("em yes"),contains("em no"),
                     contains("rc yes"),contains("rc no"),
                     contains("ula yes"),contains("ula no"),
                     contains("h yes"),contains("h no"),
                     contains("gs yes"),contains("gs no"),
                     contains("dt yes"),contains("dt no"),
                     )

#export CSV
exportcsv(LosAngelescsv,'Los Angeles County Organized.csv')

###############################################################################################################################
Orangecsv=combinedOrange[[1]]

#combine contest to name of candidate
Orangecsv[[10]]=paste(Orangecsv[[7]],Orangecsv[[10]])

#choose relevant columns
Orangecsv=select(Orangecsv,`Precinct ID`,`Choice Name`,`Total Votes`)

#flip data vertical to horizontal
Orangecsv=spread(Orangecsv,"Choice Name","Total Votes")

#select only contest candidates we want and order them Democratic-Republican
Orangecsv=select(Orangecsv,contains("precinct"),
                     contains("gavin newsom"),contains("brian dahle"),
                     contains("kounalakis"),contains("jacobs"),
                     contains("weber"),contains("rob bernosky"),
                     contains("malia m. cohen"),contains("lanhee j. chen"),
                     contains("fiona ma"),contains("jack m. guerrero"),
                     contains("rob bonta"),contains("nathan hochman"),
                     contains("full term alex padilla"),contains("full term mark p. meuser"),
                     contains("k-city of costa mesa yes"),contains("k-city of costa mesa no"),
                     contains("q-city of laguna beach yes"),contains("q-city of laguna beach no"),
                     contains("z-city of yorba linda yes"),contains("z-city of yorba linda no"),
                 )

#export CSV
exportcsv(Orangecsv,'Orange County Organized.csv')

View(Orangecsv)
###############################################################################################################################
Sacramentodflist=combinedSacramento

#transform to csv
Sacramentocsv=transform_to_csv(Sacramentodflist,'Precinct')

#select relevant columns
Sacramentocsv=select(Sacramentocsv,contains("precinct"),
                        contains("gavin newsom"),contains("brian dahle"),
                        contains("kounalakis"),contains("jacobs"),
                        contains("weber"),contains("rob bernosky"),
                        contains("malia m. cohen"),contains("lanhee j. chen"),
                        contains("fiona ma"),contains("jack m. guerrero"),
                        contains("rob bonta"),contains("nathan hochman"),
                        contains("full term alex padilla"),contains("full term mark p. meuser"),
                        contains("measure b yes"),contains("measure b no"),
                        contains("measure d yes"),contains("measure d no"),
                        contains("measure e yes"),contains("measure e no"),
                        contains("measure o yes"),contains("measure o no"),
                        
                        )

#Export CSV
exportcsv(Sacramentocsv,'Sacramento County Organized.csv')

View(Sacramentocsv)
###############################################################################################################################
SanBenitodflist=combinedSanBenito

#fix up dataframes since we only want rows 1 to 50 and columns 1 7 9 and precincts which are identified by lengths of 6
for(i in seq(length(SanBenitodflist))){
  SanBenitodflist[[i]]=slice(SanBenitodflist[[i]],seq(1,50))
  SanBenitodflist[[i]]=select(SanBenitodflist[[i]],c(1,7,9))
  SanBenitodflist[[i]]=filter(SanBenitodflist[[i]],str_length(Precinct)==6)
}

#transform to csv
SanBenitocsv=transform_to_csv(SanBenitodflist,"Precinct")

#get rid of na values
SanBenitocsv=natozero(SanBenitocsv)

#reorder columns
SanBenitocsv=select(SanBenitocsv,contains("precinct"),
                 contains("gavin newsom"),contains("brian dahle"),
                 contains("kounalakis"),contains("jacobs"),
                 contains("weber"),contains("bernosky"),
                 contains("cohen"),contains("chen"),
                 contains("fiona ma"),contains("guerrero"),
                 contains("rob bonta"),contains("nathan hochman"),
                 contains("padilla"),contains("meuser"),
                 contains("measure q yes"), contains("measure q no")
                 )

#export csv
exportcsv(SanBenitocsv,'San Benito County Organized.csv')

View(SanBenitocsv)
###############################################################################################################################
SanBernardinodflist=combinedSanBernardino

#fix up dataframes since we only want rows 1 2 4 and precincts which are identified by lengths of 23
for(i in seq(length(SanBernardinodflist))){
  SanBernardinodflist[[i]]=select(SanBernardinodflist[[i]],c(1,2,4))
  SanBernardinodflist[[i]]=filter(SanBernardinodflist[[i]],str_length(Precinct)==23)
}

#trasnform into csv
SanBernardinocsv=transform_to_csv(SanBernardinodflist,"Precinct")

#get rid of NAs and ****
SanBernardinocsv=natozero(SanBernardinocsv)

#select relevant columns
SanBernardinocsv=select(SanBernardinocsv,contains("precinct"),
                 contains("gavin newsom"),contains("brian dahle"),
                 contains("kounalakis"),contains("jacobs"),
                 contains("weber"),contains("rob bernosky"),
                 contains("malia m. cohen"),contains("lanhee j. chen"),
                 contains("fiona ma"),contains("jack m. guerrero"),
                 contains("rob bonta"),contains("nathan hochman"),
                 contains("full term alex padilla"),contains("full term mark p. meuser"),
                 contains("measure f yes"),contains("measure f no"),
                 )

#export csv
exportcsv(SanBernardinocsv,"San Bernardino County Organized.csv")

View(SanBernardinocsv)
###############################################################################################################################
SanDiegocsv=combinedSanDiego[[1]]

#make column names
for(i in seq(ncol(SanDiegocsv))){
  colnames(SanDiegocsv)[i]=SanDiegocsv[2,i]
}

#Adding contest to name
SanDiegocsv[[3]]=paste(SanDiegocsv[[2]],SanDiegocsv[[3]])

#select relevant columns
SanDiegocsv=select(SanDiegocsv,`Precinct`,`Candidate Name`,`Votes`)

#transform from vertical to horizontal
SanDiegocsv=spread(SanDiegocsv,"Candidate Name","Votes")

#slice off last two rows which are irrelevant
SanDiegocsv=slice(SanDiegocsv,1:(n()-2))

#select relevant columns
SanDiegocsv=select(SanDiegocsv,"Precinct",
                   "GOVERNOR GAVIN NEWSOM","GOVERNOR BRIAN DAHLE",
                   "LIEUTENANT GOVERNOR ELENI KOUNALAKIS","LIEUTENANT GOVERNOR ANGELA E. UNDERWOOD JACOBS",
                   "SECRETARY OF STATE SHIRLEY N. WEBER","SECRETARY OF STATE ROB BERNOSKY",
                   "CONTROLLER MALIA M. COHEN","CONTROLLER LANHEE J. CHEN",
                   "TREASURER FIONA MA","TREASURER JACK M. GUERRERO",
                   "ATTORNEY GENERAL ROB BONTA","ATTORNEY GENERAL NATHAN HOCHMAN",
                   "UNITED STATES SENATOR (Full Term) ALEX PADILLA","UNITED STATES SENATOR (Full Term) MARK P. MEUSER",
                   "CITY SAN DIEGO - MEASURE C – 30-FOOT HEIGHT LIMIT YES","CITY SAN DIEGO - MEASURE C – 30-FOOT HEIGHT LIMIT NO"
                   )

#export CSV
exportcsv(SanDiegocsv,'San Diego County Organized.csv')

View(SanDiegocsv)
###############################################################################################################################
SanFranciscodflist=combinedSanFrancisco

#filter rows by precinct which are filtered by if they start "PCT"
for(i in seq(length(SanFranciscodflist))){
  SanFranciscodflist[[i]]=filter(SanFranciscodflist[[i]],grepl("PCT",Precinct))
}

#trasnform into csv
SanFranciscocsv=transform_to_csv(SanFranciscodflist,"Precinct")

SanFranciscocsv=select(SanFranciscocsv,contains("precinct"),
                        contains("gavin newsom"),contains("brian dahle"),
                        contains("kounalakis"),contains("jacobs"),
                        contains("weber"),contains("rob bernosky"),
                        contains("malia m. cohen"),contains("lanhee j. chen"),
                        contains("fiona ma"),contains("jack m. guerrero"),
                        contains("rob bonta"),contains("nathan hochman"),
                        contains("full term alex padilla"),contains("full term mark p. meuser"),
                       contains("measure c yes"),contains("measure c no"),
                       contains("measure d yes"),contains("measure d no"),
                       contains("measure e yes"),contains("measure e no"),
                       contains("measure m yes"),contains("measure m no"),
                       )

#export csv
exportcsv(SanFranciscocsv,"San Francisco County Organized.csv")


View(SanFranciscocsv)
###############################################################################################################################
SanMateocsv=combinedSanMateo[[1]]

#make column names
for(i in seq(ncol(SanMateocsv))){
  colnames(SanMateocsv)[i]=SanMateocsv[2,i]
}

#select relevant rows aka remove last two rows
SanMateocsv=slice(SanMateocsv,c(-1,-2))

#Adding contest to name
SanMateocsv[[3]]=paste(SanMateocsv[[2]],SanMateocsv[[3]])

#Select relevant columns
SanMateocsv=select(SanMateocsv,`Precinct`,`Candidate Name`, `Votes`)

#switch data from vertical to horizontal
SanMateocsv=spread(SanMateocsv,"Candidate Name","Votes")

#select only contest candidates we want and order them Democratic-Republican
SanMateocsv=select(SanMateocsv,contains("precinct"),
                 contains("gavin newsom"),contains("brian dahle"),
                 contains("kounalakis"),contains("jacobs"),
                 contains("weber"),contains("rob bernosky"),
                 contains("malia m. cohen"),contains("lanhee j. chen"),
                 contains("fiona ma"),contains("jack m. guerrero"),
                 contains("rob bonta"),contains("nathan hochman"),
                 contains("(full term) alex padilla"),contains("(full term) mark p. meuser"),
                 contains("measure v (Majority Approval Required) yes"),contains("measure v (Majority Approval Required) no"),
                 contains("measure aa (Majority Approval Required) yes"),contains("measure aa (Majority Approval Required) no"),
                 )

exportcsv(SanMateocsv,"San Mateo County Organized.csv")

View(SanMateocsv)
###############################################################################################################################
SantaCruzdflist=combinedSantaCruz

#remove all purely NA columns and name first cell "Precinct" and filter for only rows that the pre
for(i in seq(length(SantaCruzdflist))){
  SantaCruzdflist[[i]] = SantaCruzdflist[[i]][,colSums(is.na(SantaCruzdflist[[i]]))<nrow(SantaCruzdflist[[i]])]
  SantaCruzdflist[[i]] = rename(SantaCruzdflist[[i]],"Precinct"=...1,"Vote Type"=...10)
}

#select only rows that are "total" vote type
for(i in seq(length(SantaCruzdflist))){
  SantaCruzdflist[[i]] = filter(SantaCruzdflist[[i]],`Vote Type`=="Total")
}

#transform to csv
SantaCruzcsv=transform_to_csv(SantaCruzdflist,"Precinct")

#get rid of NAs and ****
SantaCruzcsv=natozero(SantaCruzcsv)

#slice off last row which is the cumulative votes
SantaCruzcsv=slice(SantaCruzcsv,1:(n()-1))

#select relevant columns
SantaCruzcsv=select(SantaCruzcsv,contains("precinct"),
                        contains("gavin newsom"),contains("brian dahle"),
                        contains("kounalakis"),contains("jacobs"),
                        contains("weber"),contains("rob bernosky"),
                        contains("malia m. cohen"),contains("lanhee j. chen"),
                        contains("fiona ma"),contains("jack m. guerrero"),
                        contains("rob bonta"),contains("nathan hochman"),
                        contains("alex padilla"),contains("mark p. meuser"),
                        contains("measure n yes"),contains("measure n no"),
                        contains("measure o yes"),contains("measure o no"),
                        contains("measure p yes"),contains("measure p no"),
                        contains("measure q yes"),contains("measure q no"),
                        contains("measure s yes"),contains("measure s no"),
                        )

exportcsv(SantaCruzcsv,"Santa Cruz County Organized.csv")

View(SantaCruzcsv)
###############################################################################################################################
Solanocsv=combinedSolano[[1]]

#select only contest candidates we want and order them Democratic-Republican
Solanocsv=select(Solanocsv,contains("precinct"),
                   contains("gavin newsom"),contains("brian dahle"),
                   contains("kounalakis"),contains("jacobs"),
                   contains("weber"),contains("rob bernosky"),
                   contains("malia m. cohen"),contains("lanhee j. chen"),
                   contains("fiona ma"),contains("jack m. guerrero"),
                   contains("rob bonta"),contains("nathan hochman"),
                   contains("full term alex padilla"),contains("full term mark p. meuser"),
                   contains("measure k yes"),contains("measure k no"),
                 )

#slice off last row which is the cumulative votes
Solanocsv=slice(Solanocsv,1:(n()-1))

#export the csv
exportcsv(Solanocsv,"Solano County organized.csv")

View(Solanocsv)
###############################################################################################################################
Stanislauscsv=combinedStanislaus[[1]]

#make column names
for(i in seq(ncol(Stanislauscsv))){
  colnames(Stanislauscsv)[i]=Stanislauscsv[1,i]
}

#Adding contest to name
Stanislauscsv[[10]]=paste(Stanislauscsv[[7]],Stanislauscsv[[10]])

#select relevant rows aka remove first rows
Stanislauscsv=slice(Stanislauscsv,-1)

#Select relevant columns
Stanislauscsv=select(Stanislauscsv,`Precinct ID`,`Choice Name`, `Total Votes`)

#select only rows with candidates we want
Stanislauscsv=filter(Stanislauscsv,
                     `Choice Name`=="Governor GAVIN NEWSOM" | `Choice Name`=="Governor BRIAN DAHLE"|
                 `Choice Name`=="Lieutenant Governor ELENI KOUNALAKIS"|`Choice Name`=="Lieutenant Governor ANGELA E. UNDERWOOD JACOBS"|
                 `Choice Name`=="Secretary of State SHIRLEY N. WEBER"|`Choice Name`=="Secretary of State ROB BERNOSKY"|
                 `Choice Name`=="Controller MALIA M. COHEN"|`Choice Name`=="Controller LANHEE J. CHEN"|
                 `Choice Name`=="Treasurer FIONA MA"|`Choice Name`=="Treasurer JACK M. GUERRERO"|
                 `Choice Name`=="Attorney General ROB BONTA"|`Choice Name`=="Attorney General NATHAN HOCHMAN"|
                 `Choice Name`=="United States Senator - Full Term ALEX PADILLA"|`Choice Name`=="United States Senator - Full Term MARK P. MEUSER"|
                 `Choice Name`=="City of Modesto Measure H YES"|`Choice Name`=="City of Modesto Measure H NO"
                 )

#switch data from vertical to horizontal
Stanislauscsv=spread(Stanislauscsv,"Choice Name","Total Votes")

#select only contest candidates we want and order them Democratic-Republican
Stanislauscsv=select(Stanislauscsv,contains("precinct"),
                     contains("gavin newsom"),contains("brian dahle"),
                     contains("kounalakis"),contains("jacobs"),
                     contains("weber"),contains("rob bernosky"),
                     contains("malia m. cohen"),contains("lanhee j. chen"),
                     contains("fiona ma"),contains("jack m. guerrero"),
                     contains("rob bonta"),contains("nathan hochman"),
                     contains("full term alex padilla"),contains("full term mark p. meuser"),
                     contains("measure h yes"),contains("measure h no"),
                     )

#export csv
exportcsv(Stanislauscsv,"Stanislaus County Organized.csv")

View(Stanislauscsv)






