## Dijkstra
## ?o?Rfn??fE?I?e?ö?d?d?!?ƒÊ?A?h?|?A?‘?e

DIJKSTRA <- function(linktime, o, d){
	ods <- list()	## ?o?a?o?H(fix=1), grfõ?v?Z?I?Y(fix=2), grfõ?v?Zfõ(fix=0)
	mat <- matrix(rep(0, length=maxnode^2), nrow=maxnode)
	ods[[1]] <- list(fix=0, now=d, mat=mat, time=0, d=d)
	repeat{
		nofix <- 0
		for(i in 1:length(ods)){
			leng <- length(ods)
			if(ods[[i]]$fix == 0){
				temp <- linktime[linktime[,"node2"]==ods[[i]]$now,]
				if(nrow(temp)!=0){
				for(j in 1:nrow(temp)){
					temp_mat <- ods[[i]]$mat
					temp_mat[temp[j,"node1"],temp[j,"node2"]] <- 1	##h-fn?U?A???C?e?A?‘??
					if(temp[j,"node1"]==o){
						ods[[leng+j]] <- list(fix=1, now=temp[j,"node1"], mat=temp_mat, time=(temp[j,"time"]+ods[[i]]$time), d=d)
					} else {
						ods[[leng+j]] <- list(fix=0, now=temp[j,"node1"], mat=temp_mat, time=(temp[j,"time"]+ods[[i]]$time), d=d)
						nofix <- nofix + 1
					}
				}}
			ods[[i]]$fix <- 2		##?o?Rfn/h-fn?U?A???C?e?A?c?E?‘?a?Ig_
			}
		}
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

