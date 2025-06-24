library(tm)
library(dplyr)
library(utf8)      # fast UTF-8 validity checks
library(stringi)   # powerful, vectorised text tools
library(igraph)    # for 
## Read in data
setwd("C:/Nall Research Group Dropbox/Clayton Nall/niskanen/")
bills1<-read.csv("data/State Legislation/housingbillsscraped20092024.csv", header=T, as.is=T)
bills2<-read.csv("data/State Legislation/Housing Bills 2009 to 2024 - cleaned 20250530.csv", header=T, as.is=T)
orgs<-read.csv("data/State Legislation/billsandorgs20092024.csv", header=T, as.is=T, fileEncoding = "latin1")
## Identify unacceptable characters: seems like "latin1" coding was used.

bills1$sessionbill<-paste(bills1$session, bills1$Measure, sep="-")
bills2$sessionbill<-paste(bills2$session, bills2$Measure, sep="-")

## bills1 is original data before student cleaning
bills1<-bills1[,names(bills1)%in%c("coder1","coder2","Session", "Measure", "AssignedTo", "BillAnalysisURL", "Subject", "topic.alt","topic", "Author", "Status", "housingkeyword", "sessionbill")]

## bills2 Contains student judgment calls about bills being housing bills or not.  Also excludes bills that lack a bill analysis with endorsements.
bills2<-bills2[,names(bills2)%in%c("sessionbill", "Housing.Bill",  "Bill.Analysis.with.Endorsements.Available", "BillType_1", "BillType_2", "Overlay_1", "Overlay_2", "BillType_match", 
                                   "Overlay_match","Bill.Type", "Overlay", "Final.Bill.Type", "Final.Overlay", "sessionbill", "X" )]
bills<-merge(bills1, bills2, by.x="sessionbill", by.y="sessionbill")
orgs$orgclean<-trimws(toupper(orgs$orgtrimmed))
uniqueorgs<-unique(toupper(orgs$orgtrimmed))


bills.housing<-bills[bills$Housing.Bill==1 & bills$Bill.Analysis.with.Endorsements.Available==1,]
write.csv(bills.housing, "data/State Legislation/housingbillsonly20092024.csv", row.names=F)

## Creates a two-column table, the second column of which will be manually edited in Excel
## to clean the data.
manualeditingtable<-data.frame(originalname=uniqueorgs, editedname=uniqueorgs)
nameedits<-read.csv("data/State Legislation/Housing Bills 2009 to 2024 - crosswalk.csv", header=T, as.is=T)
nameedits$X<-NULL
nameedits<-unique(nameedits)

orgs<-merge(orgs, nameedits, by.x="orgclean", by.y="originalname")

orgstable<-table(orgs$editedname)
orgs<-orgs[orgs$bill.no%in%bills$Measure[bills$Housing.Bill==1],]
orgs$session.measure<-paste(trimws(orgs$session), trimws(orgs$bill.no), sep="")
## Final cleanup
orgs$editedname<-trimws(orgs$editedname)
table(orgs$editedname)

## Truncate to include only organizations that commented on at least 5 bills
orgstable<-table(orgs$editedname)
orgs<-orgs[orgs$editedname%in%names(orgstable)[orgstable>=5],]
bills$session.measure<-paste(trimws(bills$Session), trimws(bills$Measure), sep=".")
orgs$session.measure<-paste(orgs$session, orgs$bill.no, sep=".")

## Merged bills and orgs
bills.orgs<-merge(bills, orgs, by.x="session.measure", by.y="session.measure")
bills.orgs<-unique(bills.orgs)
write.csv(bills.orgs, "data/State Legislation/billsandorgs.filtered.2025.csv", row.names=F)

checkingfordupes<-sort(table(paste(bills.orgs$session.measure, bills.orgs$editedname)))

## Merge in the organization category data
category<-read.csv("data/State Legislation/organization.category.5bills.csv")
bills.orgs<-merge(bills.orgs, category, by.x="editedname", by.y="Organization", all.x=T)
write.csv(bills.orgs, "data/State Legislation/billsandorgs.filtered.2025.withcategory.csv", row.names=F)


## Begin by calculating frequency of bill comments by session, by different groups.

## Supporters, limited to bills endorsed by California YIMBY.
yimbybills<-bills.orgs$session.measure[bills.orgs$editedname=="CALIFORNIA YIMBY"]
## Any endorsement/support
subgroups<-c("all", "pre-YIMBY", "post-YIMBY", "YIMBYendorsedbills")
tmp<-bills.orgs[bills.orgs$support.lvl==3 & bills.orgs$session.measure%in%yimbybills,]
# Read in the data
data <- data.frame(bill=tmp$session.measure, org=tmp$editedname)
data<-na.omit(data)
g <- graph_from_data_frame(data)
V(g)$type <- bipartite_mapping(g)$type
g.projected<-bipartite_projection(graph = g)
proj1adjacent<-get.adjacency(g.projected$proj1, sparse=FALSE, attr="weight")
proj2adjacent<-get.adjacency(g.projected$proj2, sparse=FALSE, attr="weight")
write.csv(proj1adjacent, "data/State Legislation/adjacency.bills.csv", row.names=T)
write.csv(proj2adjacent, "data/State Legislation/adjacency.orgs.csv", row.names=T)
plot(g.projected$proj2, sparse=TRUE)
# Create the bipartite network
bipartite.mapping(g) 
deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)$vector
plot(g)

cent_df <- data.frame(deg, bet, clos, eig)
write.csv(cent_df,"data/State Legislation/stats.endorsements.csv")

# Explicit sponsorship/source only: all bills

tmp<-bills.orgs[bills.orgs$sponsor==1,]
# Read in the data
data <- data.frame(bill=tmp$session.measure, org=tmp$editedname)
data<-na.omit(data)
g <- graph.data.frame(data)
V(g)$type <- bipartite_mapping(g)$type
g.projected<-bipartite_projection(graph = g)
proj1adjacent<-get.adjacency(g.projected$proj1, sparse=FALSE, attr="weight")
proj2adjacent<-get.adjacency(g.projected$proj2, sparse=FALSE, attr="weight")
write.csv(proj1adjacent, "data/State Legislation/adjacency.bills.sponsorsonly.csv", row.names=T)
write.csv(proj2adjacent, "data/State Legislation/adjacency.orgs.sponsorsonly.csv", row.names=T)
plot(g.projected$proj2, sparse=FALSE, labels="")
# Create the bipartite network
bipartite.mapping(g) 

deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)$vector

plot(g)

cent_df <- data.frame(deg, bet, clos, eig)
write.csv(cent_df,"data/State Legislation/stats.sponsorsonly.csv")


# Explicit sponsorship/source only
## Any endorsement/support
tmp<-bills.orgs[bills.orgs$support.lvl!=3,]
# Read in the data
data <- data.frame(bill=tmp$session.measure, org=tmp$editedname)
data<-na.omit(data)
g <- graph.data.frame(data)
V(g)$type <- bipartite_mapping(g)$type
g.projected<-bipartite_projection(graph = g)
proj1adjacent<-get.adjacency(g.projected$proj1, sparse=FALSE, attr="weight")
proj2adjacent<-get.adjacency(g.projected$proj2, sparse=FALSE, attr="weight")
write.csv(proj1adjacent, "data/State Legislation/adjacency.bills.opponentsonly.csv", row.names=T)
write.csv(proj2adjacent, "data/State Legislation/adjacency.orgs.opponentsonly.csv", row.names=T)
plot(g.projected$proj2, sparse=FALSE, labels="")
# Create the bipartite network
bipartite.mapping(g) 

deg <- degree(g)
bet <- betweenness(g)
clos <- closeness(g)
eig <- eigen_centrality(g)$vector

plot(g)

cent_df <- data.frame(deg, bet, clos, eig)
write.csv(cent_df,"data/State Legislation/stats.opponentsonly.csv")

