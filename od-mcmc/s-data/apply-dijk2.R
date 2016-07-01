#############################################
##
## Apply Dijkstra for MCMC on Sioux Falls Networl
##
#############################################

setwd("C:/git-urata/od-mcmc/s-data")

# for Dijkstra to create the table of linktime
nodes <- read.csv("SiouxNodes.csv", header=F)
colnames(nodes) <- c("no","x","y")

links <- read.csv("SiouxLinks3.csv", header=T)

# ILLstulation
linktimez <- links[,c(2,3,6)]
maxnode <- length(table(c(as.numeric(names(table(as.numeric(linktimez[,1])))),
as.numeric(names(table(as.numeric(linktimez[,2])))))))	# count of nodes
colnames(linktimez) <- c("node1","node2","time")

source("NotDijkstra-speedup.R")	#Look Hatena 05/30

s <- Sys.time()
DIJKSTRA(linktimez,1,9)
Sys.time() - s

################################
## calculate traveltime OD 
mintime <- min(linktimez[,3])*0.5
odtime <- matrix(0, nrow=maxnode*maxnode, ncol=3)
ro <- 1
for(i in 1:maxnode){
	for(j in 1:maxnode){
		if(i != j){
			odtime[ro,1:2] <- c(i,j)
			ro <- ro+1
		} else {
			odtime[ro,1:2] <- c(i,j)
			odtime[ro,3] <- mintime
			ro <- ro+1
		}
	}
}

for(i in 1:nrow(odtime)){
	if(odtime[i,1]!=odtime[i,2]){
		odtime[i,3] <- DIJKSTRA(linktimez, odtime[i,1], odtime[i,2])$time
	}
}
#write.csv(odtime,"veh-time3.csv", row.names=F)

################################
## make OD table "to row"
odt  <- read.csv("sioux-od-table2.csv", header=F)
odr <- c()

for(i in 1:nrow(odt)){
	odr <- c(odr, as.numeric(odt[i,]))
}
#write.csv(odr,"odtable-row2.csv", row.names=F)

odr.ini <- round(odr + odr*runif(length(odr), min=-0.2, max=0.2) + (odr==0)*runif(length(odr), min=0, max=5))
#write.csv(odr.ini,"odtable-row-ini2.csv", row.names=F)

