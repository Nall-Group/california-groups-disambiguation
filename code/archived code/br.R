install.packages("dataverse")
install.packages("arrow")
library(dataverse)

Sys.setenv("DATAVERSE_SERVER" = "https://dataverse.harvard.edu",
           "DATAVERSE_KEY" = "697fc13f-198b-4049-ab25-103570399aa4")

meetings_2016 <- get_dataframe_by_name(filename = "meetings.2016.parquet",
                                       dataset = "10.7910/DVN/NJTBEM",
                                       .f = arrow::read_parquet)
meetings_2023 <- get_dataframe_by_name(filename = "meetings.2023.parquet",
                                       dataset = "10.7910/DVN/NJTBEM",
                                       .f = arrow::read_parquet)
meetings_2023$br<-0
meetings_2023$br[grep("builder's remedy", meetings_2023$caption_text_clean)]<-1
meetings_2023$br[grep("Builder's Remedy", meetings_2023$caption_text_clean)]<-1
meetings_2023$br[grep("builders remedy", meetings_2023$caption_text_clean)]<-1
meetings_2023$br[grep("Builders Remedy", meetings_2023$caption_text_clean)]<-1
View(meetings_2023[meetings_2023$br==1,])
