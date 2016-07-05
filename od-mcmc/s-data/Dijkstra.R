## Dijkstra
## 経由地共通の場合を重複して数えている

DIJKSTRA <- function(linktime, o, d){
	ods <- list()	## 候補経路(fix=1), 途中計算済み(fix=2), 途中計算中(fix=0)
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
					temp_mat[temp[j,"node1"],temp[j,"node2"]] <- 1	##発地までたどりついた
					if(temp[j,"node1"]==o){
						ods[[leng+j]] <- list(fix=1, now=temp[j,"node1"], mat=temp_mat, time=(temp[j,"time"]+ods[[i]]$time), d=d)
					} else {
						ods[[leng+j]] <- list(fix=0, now=temp[j,"node1"], mat=temp_mat, time=(temp[j,"time"]+ods[[i]]$time), d=d)
						nofix <- nofix + 1
					}
				}}
			ods[[i]]$fix <- 2		##経由地/発地までたどりつかないが終点
			}
		}
		if(nofix==0) break ##全て発地にたどりついたら終了
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
			minpath[[ceiling(runif(1, min=0, max=length(mins)))]] ##時間が同じ場合はランダムに
		}
	} else {
		list(fix=1, now=o, mat=mat, time=9999, d=d)
	}
}

