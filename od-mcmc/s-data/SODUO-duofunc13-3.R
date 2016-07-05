#################################################
## duoを解くための関数　###############################
# ※　関数は大文字で書く

## 時間別OD表
dQ <- function(time, odmat, o, d){	##時間帯ごとの出発交通量を算出
	mat1 <- odmat[odmat[,"o"]==o,]
	mat2 <- mat1[mat1[,"d"]==d,]
	mat3 <- mat2[mat2[,"start"]<time,]
	mat4 <- c(mat3[mat3[,"goal"]>=time,"od"]*dt, 0)
	mat4[1]
}
# dQ(1.5, od, 1, 3)

#########################
#### STEP1 初期値の設定
# 目的地別を合計
SUM_D <- function(DZ){	##DZ: 目的地別の交通流リスト
	sum_d <- DZ[[1]]
	if(cnt_d > 1){
		for(g in 1:cnt_link){
		for(h in 2:cnt_d){
			sum_d[[g]]$al[,2] <- sum_d[[g]]$al[,2] + DZ[[h]][[g]]$al[,2]
			sum_d[[g]]$dm[,2] <- sum_d[[g]]$dm[,2] + DZ[[h]][[g]]$dm[,2]
	}	}}
	sum_d
}

# 交差点の合流値の参照値(容量の比で与える)
LINKFIND <- function(linkchar, onode, dnode){
	vec <- linkchar[linkchar[,"node1"]==onode,]
	vec <- vec[vec[,"node2"]==dnode,]
	vec
}
LINKFIND_NO <- function(linkchar, onode, dnode){
	vec <- linkchar[linkchar[,"node1"]==onode,]
	no <- (1:nrow(linkchar))[linkchar[,"node1"]==onode]
	no <- no[vec[,"node2"]==dnode]
	vec <- vec[vec[,"node2"]==dnode,]
	cbind(vec, no)
}
# LINKFIND_NO(linkchar, 1, 2)

###############################
#### STEP2 流入流出可能量の判定
BETA <- function(A, D, link){
	beta <- 0
	if(as.numeric(nrow(D)-link["lw2"]/dt) > 0){
	if(A[nrow(A),2]>(D[as.numeric(nrow(D)-link["lw2"]/dt),2]+link["k"]*link["length"])){
		beta <- 1
	}}
	beta
}
#BETA(da[[1]]$al, da[[1]]$dm, linkchar[1,])
M_BETA <- function(DA, LINKCHAR){	##全リンクに対して計算
	beta_t <- c()
	for(i in 1:length(DA)){
		beta_t <- c(beta_t, BETA(DA[[i]]$al, DA[[i]]$dm, LINKCHAR[i,]))
	}
	beta_t
}
#M_BETA(da, linkchar)

YJK <- function(A, D, link, mu){
	beta <- BETA(A, D, link)
	mu_bef <- 0
	if(as.numeric(nrow(mu)-link["lw2"]/dt) >= 0){
		mu_bef <- mu[as.numeric(nrow(mu)-link["lw2"]/dt)+1,2]
	}
	as.numeric((1-beta)*link["f"] + beta*mu_bef)	##A,Dは現時刻までデータがあるが，mu,lamはないので+1
}
#YJK(da[[1]]$al, da[[1]]$dm, linkchar[1,], ml[[1]]$dm)
#YJK(A,D,link,mu)
M_YJK <- function(DA, ML, LINKCHAR){
	yjk_t <- c()
	for(i in 1:length(DA)){
		yjk_t <- c(yjk_t, YJK(DA[[i]]$al, DA[[i]]$dm, LINKCHAR[i,], ML[[i]]$dm))
	}
	yjk_t
}
#m_yjk <- M_YJK(da, ml, linkchar)

## 渋滞判定
CONGEST <- function(A, D, link, lam){
	cong <- 0
	if(nrow(A)-as.numeric(link["lw1"]/dt) > 0){
	if(A[nrow(A)-as.numeric(link["lw1"]/dt),2]>(D[nrow(D),2])){
		cong <- 1
	} else if(lam[nrow(lam)-as.numeric(link["lw1"]/dt)+1, 2] > link["f"]){
		cong <- 1
	}}
	cong
}
# CONGEST(da[[1]]$al, da[[1]]$dm, linkchar[1,], ml[[1]]$al)
M_CONGEST <- function(DA, LINKCHAR, ML){	##全リンクに対して計算
	beta_t <- c()
	for(i in 1:length(DA)){
		beta_t <- c(beta_t, CONGEST(DA[[i]]$al, DA[[i]]$dm, LINKCHAR[i,], ML[[i]]$al))
	}
	beta_t
}
#M_CONGEST(da, linkchar)

XIJ <- function(A, D, link, lam){
	xij <- 0
	if(nrow(lam)-as.numeric(link["lw1"]/dt) >= 0){
		xij <- lam[nrow(lam)-as.numeric(link["lw1"]/dt)+1, 2]
		if(CONGEST(A, D, link, lam)==1){
			xij <- as.numeric(link["f"])
	}	}
	xij
}
# XIJ(da[[1]]$al, da[[1]]$dm, linkchar[1,], ml[[1]]$al)
# XIJ(A, D, link, lam)
M_XIJ <- function(DA, ML, LINKCHAR){
	xij_t <- c()
	for(i in 1:length(DA)){
		xij_t <- c(xij_t, XIJ(DA[[i]]$al, DA[[i]]$dm, LINKCHAR[i,], ML[[i]]$al))
	}
	xij_t
}
#m_xij <- M_XIJ(da, ml, linkchar)

###############################
#### STEP3 リンク旅行時間・速度評価
## 渋滞位置判定
SHOCKWAVE_POSITION <- function(A, D, link, lam){
	xstar <- link["length"]
	if(CONGEST(A, D, link, lam)==1){	##自由流ではない
#	if(BETA(A, D, link)!=1){		##先詰まりはしていない
		amin <- 0
		if(nrow(A)-as.numeric(link["length"]/link["w1"]/dt)>0){		## 自由走行でのリンク通過にかかる時間を差分
			amin <- A[nrow(A)-as.numeric(link["length"]/link["w1"]/dt),2]	## x*=lengthのとき(混雑なし)の通常所要時間の差分のA
		}
		dif <- abs(amin - D[nrow(D),2])	## x=lのときを初期値
		st <- nrow(D) - as.numeric(link["length"]/link["w2"]/dt)		## x=0(全渋滞)のときのDの遡り時間(もっとも遡り)
		if(st < 1) st <- 1
		for(i in (nrow(D)-1):st){
			x_temp <- link["length"]-(nrow(D)- i)*dt*link["w2"]		## iのときのxstarの位置
			anow <- 0
			freetime <- nrow(A)-as.numeric(x_temp/link["w1"]/dt)
			if(freetime>0){
				surp <- freetime - floor(freetime)
				if(i != st) anow <- A[freetime,2] + surp*(A[freetime+1,2]-A[freetime,2])	##x_tempは0.2単位で変わる，A[t]は小数点だと変わらない
				else anow <- A[freetime,2]
			}
			dif_temp <- abs(anow - D[i,2] - link["k"]*(link["length"]-x_temp))
			if(dif > dif_temp){		##一番0に近い値が渋滞位置
				xstar	<- x_temp
				dif <- dif_temp
			}
		}
#	} else { xstar <- 0 }		## 先詰まり
	}
	as.numeric(xstar)
}
# SHOCKWAVE_POSITION(da[[1]]$al, da[[1]]$dm, linkchar[1,], ml[[1]]$al)
# SHOCKWAVE_POSITION(DA_s[[1]]$al, DA_s[[1]]$dm, linkchar[1,], ML_s[[1]]$al)
# SHOCKWAVE_POSITION(A, D, link, lam)
M_SHOCKWAVE_POSITION <- function(DA, ML, LINKCHAR){
	posi <- c()
	for(i in 1:nrow(LINKCHAR)){
		posi <- c(posi, SHOCKWAVE_POSITION(DA[[i]]$al, DA[[i]]$dm, LINKCHAR[i,], ML[[i]]$al))
	}
	posi
}

## リンク旅行時間
LINK_TRAVEL_TIME <- function(A, D, link, lam, mu){
	if(CONGEST(A, D, link, lam)==0){
		time <- link["length"]/link["w1"]
	} else {
		xstar <- SHOCKWAVE_POSITION(A, D, link, lam)
		st <- nrow(mu)-as.numeric((link["length"]-xstar)/link["w2"]/dt)+1
		if(st < 1) st <- 1
		need_length <- as.numeric(link["length"]-xstar)
		sumtime <- 0	## 渋滞時の合計時間
		t <- nrow(mu)-1	##　mu[t]の時間　nrow(mu)～stまで	#修正
		move_l <- 0		## x=lからの移動距離
		repeat{
			DT <- dt
			v_temp <- (mu[t,2]/dt)/as.numeric(link["k"]-((mu[t,2]/dt)/link["w2"]))	#muを単位時間当たりから1時間あたりになおす
			if(v_temp==0){	## 始動時の条件
				v_temp <- (mu[t+1,2]/dt)/as.numeric(link["k"]-((mu[t+1,2]/dt)/link["w2"]))	#muを単位時間当たりから1時間あたりになおす
			#	cat("始動条件\n")
			}
			move_l <- move_l + v_temp*dt
			if(move_l > need_length){	##時間dtの距離を動かなくても充足する場合
				DT <- (need_length - (move_l - v_temp*dt))/v_temp
			}
			sumtime <- sumtime + DT		## 修正
			if(move_l > need_length){
			#	cat("長さで判定\n")
				break	## x*~l間の移動距離を越えたら
			}
			if(t < st){
			#	cat("時間で判定\n")
				break				## st(x=x*のときの時間)を下回ったら
			}
			t <- t-1
			#cat(t)
		}
		time <- as.numeric(xstar/link["w1"]) + sumtime	##自由流と混雑流の通過時間の和
	}
	as.numeric(time)
}

# LINK_TRAVEL_TIME(da[[1]]$al, da[[1]]$dm, linkchar[1,], ml[[1]]$al, ml[[1]]$dm)
# LINK_TRAVEL_TIME(A, D, link, lam, mu)
M_LINK_TRAVEL_TIME <- function(DA, ML, LINKCHAR){
	time_t <- c()
	for(i in 1:length(DA)){
		time_t <- c(time_t, LINK_TRAVEL_TIME(DA[[i]]$al, DA[[i]]$dm, LINKCHAR[i,], ML[[i]]$al, ML[[i]]$dm))
	}
	cbind(LINKCHAR[,1:2], time = time_t)
}
# linktime <- M_LINK_TRAVEL_TIME(da, ml, linkchar)
# M_LINK_TRAVEL_TIME(DA_s, ML_s, linkchar)
# M_LINK_TRAVEL_TIME()

#########################
#### STEP4 最短経路探索
# source("C:\\DUO\\Dijkstra.R")
# DIJKSTRA(linktime, 1, 2)
# DIJKSTRA(linktime, 1, 3)

#########################
#### STEP5 分岐率，合流比
## 分岐率用のt^hatのでの流入率の算出
# A[,2] <- 10*0:5
# D[,2] <- (c(0,0,0,0,0,0,0,10,20,30,40))
# lam[,2] <- 10*0:4
#  mu[,2] <- (c(0,0,0,0,0,0,10,20,30,40))
# da12  <- list(da, da)
# ml12 <- list(ml, ml)
# DA <- da12
# ML <- ml12


LAM_D_IJ <- function(A, D, lam, linkno){
	t_hat <- A[D[nrow(D),2]==A[,2],1]	## 停止はしないので，同じ値になることはないはず．だが，最初は全部0なのでダメ.離散的なので，必ずしも0にならない．
	if(sum(D[nrow(D),2]==A[,2])==0){	## たしか最初のときへの対策
	if(sum(D[nrow(D),2]<=A[,2])!=0){	## D[nrow(D),2]が流入Aを超えている場合は除外　$$そんなことは起こらないはずだが，，
		t_hat <- min(A[D[nrow(D),2]<A[,2],1])	##一番最初の時間をt_hatとする
		#Afirst <- A[A[,1]==t_hat,]
		#if(Afirst[2]!=0){		##渋滞時(流入量がある程度あるとき)はt_hat(一番最初の時間)だとlamが0になってしまいまずい
		#	if(sum(A[,2]==Afirst[2])>1){	## 流入0からの立ち上がりとか
		#		if(lam[lam[,1]==t_hat,2]==0){
		#			t_hat <- t_hat - dt #max(A[A[,2]==Afirst[2],1])	##流入停止中の時間帯の一番最後をt_hatとする,t_hat=t-Tijになるか計算が必要:ワープしている
		#		}
		#	}
		#}
	}}
	if(length(t_hat)>1){			## t_hatが複数あるとき，渋滞対策だったがダメか
		for(g in 1:length(t_hat)){
			if(sum(t_hat[g]==lam[,1])>0){
				t_hat_last <- t_hat[g]	##lamの中に同じ時間のものがある中で，一番最後の時間
		}	}
		t_hat <- t_hat_last
	}
	if(length(t_hat)==0){
		t_hat <- A[nrow(A),1]	##　仮置き
	}
	if(A[nrow(A),1]>freetime[linkno]){			## 開始直後ではなくて
	if(A[nrow(A),1]-t_hat < freetime[linkno]){	## t_hatが最低旅行時間より小さいことはない
		t_hat <- round(A[nrow(A),1] - freetime[linkno], digits = -1*log10(dt))	## とりあえず最低旅行時間を仮定
	}}
	if(sum(lam[,1]==t_hat)!=0){
		lam[lam[,1]==t_hat,2]			##t_hatのlambdaの値を算出
	} else {
		deftem <- abs(lam[,1] - t_hat)
		lam[min(deftem)==deftem, 2]
	}
}

# A <- DA_s[[10]]$al
# D <- DA_s[[10]]$dm
# lam <- ML_s[[10]]$al
# LAM_D_IJ(A, D, lam)

## 分岐率(分岐はノード5のみ)
PHAI <- function(DA, ML, j, k, network, odlist, linktime){
	i <- (1:ncol(network))[network[,j]==1]
	linkij_no <- as.numeric(LINKFIND_NO(linktime, i, j)["no"])
	numer <- 0
	donom <- 0
	for(h in 1:length(odlist)){
		dijk <- DIJKSTRA(linktime, i, odlist[h])$mat
		lamij_t <- LAM_D_IJ(DA[[h]][[linkij_no]]$al, DA[[h]][[linkij_no]]$dm, ML[[h]][[linkij_no]]$al, linkij_no)
		numer <- numer + dijk[j,k]*lamij_t
		donom <- donom + lamij_t
	}
	if(donom!=0){
		numer/donom
	} else 1/2		## 分母(流入量)が0の場合は1/2と暫定
}
# PHAI(da12, ml12, 5, 6, network, odlist, linktime)
# PHAI(DA, ML, j, kz[h], network, odlist, linktime)
# PHAI(list(al=A,dm=D),5,6,network, odlist, )
# k <- kz[h]

## 合流比(合流はノード2のみ)
# set <- eta123
ETA <- function(set, xij, yjk, i, j, linktime, network){
	k <- (1:nrow(network))[network[j,]==1]
	inodes <- (1:ncol(network))[network[,j]==1]
	icand  <- inodes[inodes!=i]
	linkxy <- cbind(linktime, xij, yjk)
	sumxij <- 0
	for(h in 1:length(icand)){
		sumxij <- sumxij + LINKFIND(linkxy, icand[h], j)[,"xij"]
	}
	eta <- set
	yjk_t <- LINKFIND(linkxy, j, k)[,"yjk"]
	if(sumxij < (1-set)*yjk_t) eta <- 1-sumxij/yjk_t
	eta
}
# ETA(eta123, m_xij, m_yjk, 5, 2, linktime, network)

#########################
#### STEP6 流出流率μの決定
## 直列
MU_STRAIGHT <- function(xij, yjk, i, j, linktime, network){
	linkxy <- cbind(linktime, xij, yjk)
	k <- (1:nrow(network))[network[j,]==1]	##到着地ノードはnumeric(0)になる
	xij_t <- LINKFIND(linkxy, i, j)[,"xij"]
	yjk_t <- LINKFIND(linkxy, j, k)[,"yjk"]
	min(xij_t, yjk_t)
}
#MU_STRAIGHT(m_xij, m_yjk, 1, 4, linktime, network)
#MU_STRAIGHT(xijz, yjkz, 1, 4, tauij, network)

## 分岐(node5)
MU_DIVERGENCE <- function(xij, yjk, i, j, linktime, network, DA, ML, odlist){
	linkxy <- cbind(linktime, xij, yjk)
	kz <- (1:nrow(network))[network[j,]==1]	## 流出候補ノード集合

	xij_t <- LINKFIND(linkxy, i, j)[,"xij"]	## 流入側
	sumy <- 0
	y_phai <- c()
	for(h in 1:length(kz)){
		sumy <- sumy + LINKFIND(linkxy, j, kz[h])[,"yjk"]
		phai <- PHAI(DA, ML, j, kz[h], network, odlist, linktime)
		y_phai <- c(y_phai, LINKFIND(linkxy, j, kz[h])[,"yjk"]/phai)
	}
	min(xij_t, sumy, y_phai)
}
#MU_DIVERGENCE(m_xij, m_yjk, 4, 5, linktime, network, da12, ml12, odlist)
# kz <- c(2,6)
# DA <- DA_d
# ML <- ML_d
# i <- 4
# j <- 5

## 合流(node2)
MU_MERGE <- function(set, xij, yjk, i, j, linktime, network){
	linkxy <- cbind(linktime, xij, yjk)
	k <- (1:nrow(network))[network[j,]==1]
	xij_t <- LINKFIND(linkxy, i, j)[,"xij"]
	yjk_t <- LINKFIND(linkxy, j, k)[,"yjk"]*ETA(set, xij, yjk, i, j, linktime, network)
	min(xij_t, yjk_t)
}
# MU_MERGE(eta123, m_xij, m_yjk, 5, 2, linktime, network)
# μ(合計値)の現時刻値が更新される
# ml_sum <- ml		# μ(合計値)の更新値を追加したものをテスト計算のために作成
#for(i in 1:length(ml_sum)){
#	matr <- ml_sum[[i]]$dm
#	mu_t <- 1
#	ml_sum[[i]]$dm <- rbind(matr, c(matr[nrow(matr),1]+dt, mu_t))
#}

M_MU <- function(xij, yjk, linktime, network, nodez, da_s, ml_s, odlist, set){
	muz <- c()
	for(g in 1:nrow(linktime)){		## 次のリンクに受け渡すときがあやしい
		n_attr <- nodez[linktime[g,"node2"]]
		if(n_attr == 1){	## 直列
			muz <- c(muz, MU_STRAIGHT(xij, yjk, linktime[g,"node1"], linktime[g,"node2"],	
					linktime, network))
		} else if (n_attr == 2){	## 合流
			muz <- c(muz, MU_MERGE(set, xij, yjk, linktime[g,"node1"], linktime[g,"node2"], linktime, network))
		} else {		## 分岐
			muz <- c(muz, MU_DIVERGENCE(xij, yjk, linktime[g,"node1"], linktime[g,"node2"], 
					linktime, network, da_s, ml_s, odlist))
		}
	}
	muz
}
# xij <- xijz
# yjk <- yjkz
# linktime <- tauij
# nodez <- node_attr
# da_s <- DA_d
# ml_s <- ML_d
# set <- eta123

#######################################
#### STEP7 FIFOに従った目的地別交通流出率μ^d
LAM_D_IJ_T <- function(A, D, lam, linkno){
	t_hat <- A[D[nrow(D),2]==A[,2],1]	## 停止はしないので，同じ値になることはないはず．だが，最初は全部0なのでダメ.離散的なので，必ずしも0にならない．
	if(sum(D[nrow(D),2]==A[,2])==0){	## たしか最初のときへの対策
	if(sum(D[nrow(D),2]<=A[,2])!=0){	## D[nrow(D),2]が流入Aを超えている場合は除外　$$そんなことは起こらないはずだが，，
		t_hat <- min(A[D[nrow(D),2]<A[,2],1])	##一番最初の時間をt_hatとする
		Afirst <- A[A[,1]==t_hat,]
		if(Afirst[2]!=0){		##渋滞時(流入量がある程度あるとき)はt_hat(一番最初の時間)だとlamが0になってしまいまずい
			if(sum(A[,2]==Afirst[2])>1){	## 流入0からの立ち上がりとか
				if(lam[lam[,1]==t_hat,2]==0){
					t_hat <- t_hat - dt #max(A[A[,2]==Afirst[2],1])	##流入停止中の時間帯の一番最後をt_hatとする,t_hat=t-Tijになるか計算が必要:ワープしている
				}
			}
		}
	}}
	if(length(t_hat)>1){			## t_hatが複数あるとき，渋滞対策だったがダメか
		for(g in 1:length(t_hat)){
			if(sum(t_hat[g]==lam[,1])>0){
				t_hat_last <- t_hat[g]	##lamの中に同じ時間のものがある中で，一番最後の時間
		}	}
		t_hat <- t_hat_last
	}
	if(length(t_hat)==0){
		t_hat <- A[nrow(A),1]	##　仮置き
	}
	if(A[nrow(A),1]>freetime[linkno]){			## 開始直後ではなくて
	if(A[nrow(A),1]-t_hat < freetime[linkno]){	## t_hatが最低旅行時間より小さいことはない
		t_hat <- round(A[nrow(A),1] - freetime[linkno], digits = -1*log10(dt))	## とりあえず最低旅行時間を仮定
	}}
	t_hat
}
# A <- DA_s[[h]]$al
# D <- DA_s[[h]]$dm
# lam <- ML_s[[h]]$al
# linkno <- h
# LAM_D_IJ_T(A, D, lam, linkno)

MU_D <- function(DA_s, DA, ML_s, ML, i, j, network, odlist, linktime){
	linkij_no <- as.numeric(LINKFIND_NO(linktime, i, j)["no"])
	t_hat <- LAM_D_IJ_T(DA_s[[linkij_no]]$al, DA_s[[linkij_no]]$dm, ML_s[[linkij_no]]$al, linkij_no)	##修正
	mu_ij_t <- ML_s[[linkij_no]]$dm[nrow(ML_s[[linkij_no]]$dm),2]		## STEP6の流出流率μ
	tal_tf <- ML_s[[linkij_no]]$al[,1]==t_hat
	lam_ij_hat <- ML_s[[linkij_no]]$al[tal_tf,2]	## t_hatのときの流入率 
	if(sum(tal_tf)==0){	## 小数点以下の誤差対策
		tal_temp <- abs(ML_s[[linkij_no]]$al[,1]-t_hat)
		lam_ij_hat <- ML_s[[linkij_no]]$al[min(tal_temp)==tal_temp,2]
	}
	for(g in 1:length(odlist)){
		tal_d_tf <- ML[[g]][[linkij_no]]$al[,1]==t_hat
		lam_ij_hat_d <- ML[[g]][[linkij_no]]$al[tal_d_tf,2]
		if(sum(tal_d_tf)==0){
			tal_d_temp <- abs(ML[[g]][[linkij_no]]$al[,1]-t_hat)
			lam_ij_hat_d <- ML[[g]][[linkij_no]]$al[tal_d_temp==min(tal_d_temp),2]
		}
		if(lam_ij_hat!=0){
			mu_ij_td <- (mu_ij_t/lam_ij_hat)*lam_ij_hat_d
		} else {
			mu_ij_td <- 0
		}
		temp <- ML[[g]][[linkij_no]]$dm
		ML[[g]][[linkij_no]]$dm <- rbind(temp, c(temp[nrow(temp),1]+dt, mu_ij_td))
	}
	ML	##目的地別の現時刻の流入率を追加したグラフ
}
# ttt <- MU_D(da, da12, ml_sum, ml12, 4, 5, network, odlist, linktime)
# DA <- DA_d
# ML <- ML_d
# i <- tauij[h,"node1"]
# j <- tauij[h,"node2"]
# linktime <- tauij

##################################
#### STEP8 目的地別交通量保存則 　##　いらない
#### STEP9 目的地別流入流率λ　算出
LAM_D <- function(i,j, odlist, od, ML, linktime, network){
	linkij_no <- as.numeric(LINKFIND_NO(linktime, i, j)["no"])	##linkijはlinktimeの中で何番目のlinkか．
	time <- max(ML[[1]][[1]]$dm[,1])
	hz <- (1:ncol(network))[network[,i]==1]	##iに流入するノード
	lam_d_ij <- c()
	for(h in 1:cnt_d){
		dq <- dQ(time, od, i, odlist[h])
		sum_mu <- 0
		if(sum(network[,i]==1)!=0){
		for(g in 1:length(hz)){
			mu_d_hi <- ML[[h]][[as.numeric(LINKFIND_NO(linktime, hz[g], i)["no"])]]$dm
			sum_mu <- sum_mu + mu_d_hi[mu_d_hi[,1]==time,2]
		}}
		lam_d_ij <- (dq+sum_mu)*DIJKSTRA(linktime, i, odlist[h])$mat[i,j]	##lam_d_ijはNULLになる，DIJKSTRAがカラだと．
		temp <- ML[[h]][[linkij_no]]$al
		ML[[h]][[linkij_no]]$al <- rbind(temp, c(time, lam_d_ij))
	}
	ML
}			
# ttt <- LAM_D(1,4, odlist, od, ml12, linktime, network)
# ttt[[1]][[1]]$al
# LAM_D(4,5, odlist, od, ml12, linktime, network)

#########################
#### STEP10 t+⊿tへの更新

## 目的合計の流入流率λ
SUM_D_al <- function(l_s, l_d){
	time <- nrow(l_d[[1]][[1]]$al)
	for(h in 1:cnt_link){
		sum_lam_ij <- 0
		for(g in 1:cnt_d){
			sum_lam_ij <- sum_lam_ij + l_d[[g]][[h]]$al[time, 2]
		}
		l_s[[h]]$al <- rbind(l_s[[h]]$al, c(l_d[[1]][[h]]$al[time, 1], sum_lam_ij))
	}
	l_s
}

## 目的別の累積交通量(A,D)
UPL_DA_d <- function(da_d, ml_d){
	time_ml <- nrow(ml_d[[1]][[1]]$al)
	time_da <- nrow(da_d[[1]][[1]]$al)
	for(h in 1:cnt_link){
		sum_d_ij <- 0
		sum_a_ij <- 0
		for(g in 1:cnt_d){
			temp_d_tn <- da_d[[g]][[h]]$dm[time_da,2] + ml_d[[g]][[h]]$dm[time_ml,2]
			da_d[[g]][[h]]$dm <- rbind(da_d[[g]][[h]]$dm, c(dt + ml_d[[g]][[h]]$dm[time_ml, 1], temp_d_tn))
			temp_a_tn <- da_d[[g]][[h]]$al[time_da,2] + ml_d[[g]][[h]]$al[time_ml,2]
			da_d[[g]][[h]]$al <- rbind(da_d[[g]][[h]]$al, c(dt + ml_d[[g]][[h]]$al[time_ml, 1], temp_a_tn))
		}
	}
	da_d
}

## 目的t合計の累積交通量(A,D)
SUM_D_DA <- function(da_s, da_d){
	time <- nrow(da_d[[1]][[1]]$al)
	for(h in 1:cnt_link){
		sum_a_ij <- 0
		sum_d_ij <- 0
		for(g in 1:cnt_d){
			sum_a_ij <- sum_a_ij + da_d[[g]][[h]]$al[time, 2]
			sum_d_ij <- sum_d_ij + da_d[[g]][[h]]$dm[time, 2]
		}
		da_s[[h]]$al <- rbind(da_s[[h]]$al, c(da_d[[1]][[h]]$al[time, 1], sum_a_ij))
		da_s[[h]]$dm <- rbind(da_s[[h]]$dm, c(da_d[[1]][[h]]$dm[time, 1], sum_d_ij))
	}
	da_s
}
