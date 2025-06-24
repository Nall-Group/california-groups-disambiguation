library(glue)
setwd("C:/Nall Research Group Dropbox/Clayton Nall/niskanen")

# create a vector of the 15 words
keywords <- c("affordable", "housing", "homelessness", "housing crisis", "rent crisis", "rental assistance", "preservation", "accessory dwelling units", "tenant protections", "zoning", "density",  
              "low-income", "land use", "housing element", "land-use planning", "LIHTC", "homeownership", "mobilehome")

# load the table as a data frame
bills <- read.csv("data/State Legislation/All Bills 2012 to Present.csv", header=T, as.is=T)
bills$housingrelated<-0
bills$housingkeyword<-NA
# loop through each bill and check if any of the keywords are in the subject
for (i in 1:nrow(bills)) {
  subject <- tolower(bills[i, "Subject"])
  for (keyword in keywords) {
    if (grepl(keyword, subject)) {
      print(paste("Bill", bills[i, "Measure"], "contains keyword:", keyword))
      bills$housingkeyword[i]<-keyword
      bills$housingrelated[i]<-1
      print(i)
    }
  }
}
sum(bills$housingrelated)
bills<-bills[bills$housingrelated==1,]
View(bills)
write.csv(bills, file="data/State Legislation/housingbills2012to2023.csv", row.names=F)

## Add bills from 2023-25

# load the table as a data frame
bills <- read.csv("data/State Legislation/allbills2023to2025.csv", header=T, as.is=T)
bills$housingrelated<-0
bills$housingkeyword<-NA
# loop through each bill and check if any of the keywords are in the subject
for (i in 1:nrow(bills)) {
  subject <- tolower(bills[i, "Subject"])
  for (keyword in keywords) {
    if (grepl(keyword, subject)) {
      print(paste("Bill", bills[i, "Measure"], "contains keyword:", keyword))
      bills$housingkeyword[i]<-keyword
      bills$housingrelated[i]<-1
      print(i)
    }
  }
}
sum(bills$housingrelated)
bills<-bills[bills$housingrelated==1,]
View(bills)
bills$url<-paste0("https://leginfo.legislature.ca.gov/faces/billAnalysisClient.xhtml?bill_id=", 
                 trimws(unlist(lapply(strsplit(bills$Session,"-"),(function(x)x[1])))),  trimws(unlist(lapply(strsplit(bills$Session,"-"),(function(x)x[2])))), "0", 
                 lapply(strsplit(bills$Measure,"-"),(function(x)x[1])), lapply(strsplit(bills$Measure,"-"),function(x)x[2]))
write.csv(bills, file="data/State Legislation/housingbills2023to2025.csv", row.names=F)
