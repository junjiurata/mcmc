## Dijkstra
## ?o?Rfn??fE?I?e?ö?d?d?!?ƒÊ?A?h?|?A?‘?e

DIJKSTRA <- function(linktime, o, d){
	ods <- list()	## ?o?a?o?H(fix=1), grfõ?v?Z?I?Y(fix=2), grfõ?v?Zfõ(fix=0)
	ods.a <- list()
	mat <- matrix(rep(0, length=maxnode^2), nrow=maxnode)
	ods[[1]] <- list(fix=0, now=d, mat=mat, time=0, d=d)
	repeat{
		nofix <- 0
		for(i in 1:length(ods)){
			#print(i)
			leng <- length(ods)
			if(ods[[i]]$fix == 0){
				temp <- linktime[linktime[,"node2"]==ods[[i]]$now,]
				if(nrow(temp)!=0){
				for(j in 1:nrow(temp)){
					temp_mat <- ods[[i]]$mat
					temp_mat[temp[j,"node1"],temp[j,"node2"]] <- 1	##h-fn?U?A???C?e?A?‘??
					if(temp[j,"node1"]==o){
						ods.a[[length(ods.a)+1]] <- list(fix=1, now=temp[j,"node1"], mat=temp_mat, time=(temp[j,"time"]+ods[[i]]$time), d=d)
					} else {
						ods.a[[length(ods.a)+1]] <- list(fix=0, now=temp[j,"node1"], mat=temp_mat, time=(temp[j,"time"]+ods[[i]]$time), d=d)
						nofix <- nofix + 1
					}
				}
				}
			}
		}
		ods <- ods.a
		ods.a <- list()
		if(nofix==0) break ##eS?Ah-fn?E???C?e?A?‘???c?I?1
	}
	minpath <- list()
	times <- c()
	for(h in 1:length(ods)){
		if(ods[[h]]$fix==1){
			minpath[[1+length(minpath)]] <- ods[[h]]
			times <- c(times, ods[[h]]$time)
		}
	}
	if(length(minpath)!=0){
		mins <- (1:length(times))[min(times)==times]
		if(length(mins)==1){
			minpath[[mins]]
		} else {
			minpath[[ceiling(runif(1, min=0, max=length(mins)))]] ##???O?agP?÷?e?ö?I?ñ?g?_???E
		}
	} else {
		list(fix=1, now=o, mat=mat, time=9999, d=d)
	}
}

# test <- DIJKSTRA(linktime,1,3)
# test2 <- DIJKSTRA(linktime,1,4)
# 
#iter.dijk <- function(n){
#	for(i in 1:n){
#		DIJKSTRA(linktime, 1, 9)
#	}
#}
#iter.dijk.t <- function(n){
#	for(i in 1:n){
#		DIJKSTRA.t(linktime, 1, 9)
#	}
#}

#s <- Sys.time()
#iter.dijk(10000)#DIJKSTRA(linktime, 1, 9)
#Sys.time()-s

#s <- Sys.time()
#iter.dijk.t(10000)#DIJKSTRA.t(linktime, 1, 9)
#Sys.time()-s

