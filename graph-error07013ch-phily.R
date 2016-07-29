setwd("D:/od-mcmc/result/0701")

#########
od.err <- read.csv("D:/od-mcmc/c-result/errsd.csv", header=F)
o.err <- read.csv("D:/od-mcmc/c-result/errsd-o.csv", header=F)
d.err <- read.csv("D:/od-mcmc/c-result/errsd-d.csv", header=F)
energy <- read.csv("D:/od-mcmc/c-result/energy.csv", header=F)
cost <- read.csv("D:/od-mcmc/c-result/cost.csv", header=F)
totalod <- read.csv("D:/od-mcmc/c-result/totalod.csv", header=F)
cellz0 <- read.csv("D:/od-mcmc/c-result/cell0.csv", header=F)
ent <- read.csv("D:/od-mcmc/c-result/entropy.csv", header=F)
#########

setwd("D:/od-mcmc/c-result")
setwd("D:/od-mcmc/c-result/0716-i")
setwd("C:/mcmc_static/result-20160715_113946")
setwd("C:/mcmc-static-git/mcmc_static.git/result-20160715_214151")
setwd("C:/mcmc-static-git/mcmc_static.git/result-20160717_163631")
setwd("C:/od-mcmc/result/result-20160726_004838")

ch <- 13
i <- 1
##########
	name.od.err <- paste("errsd-",ch,"-",i,".csv", sep="")
		od.err <- read.csv(name.od.err, header=F)
	name.o.err <- paste("errsd-o-",ch,"-",i,".csv", sep="")
		o.err <- read.csv(name.o.err, header=F)
	name.d.err <- paste("errsd-d-",ch,"-",i,".csv", sep="")
		d.err <- read.csv(name.d.err, header=F)
	name.energy <- paste("energy-",ch,"-",i,".csv", sep="")
		energy <- read.csv(name.energy, header=F)
	name.cost <- paste("cost-",ch,"-",i,".csv", sep="")
		cost <- read.csv(name.cost, header=F)
	name.totalod <- paste("totalod-",ch,"-",i,".csv", sep="")
		totalod <- read.csv(name.totalod, header=F)

for(i in 2:6){
	name.od.err <- paste("errsd-",ch,"-",i,".csv", sep="")
		od.err <- rbind(od.err, read.csv(name.od.err, header=F))
	name.o.err <- paste("errsd-o-",ch,"-",i,".csv", sep="")
		o.err <- rbind(o.err, read.csv(name.o.err, header=F))
	name.d.err <- paste("errsd-d-",ch,"-",i,".csv", sep="")
		d.err <- rbind(d.err, read.csv(name.d.err, header=F))
	name.energy <- paste("energy-",ch,"-",i,".csv", sep="")
		energy <- rbind(energy, read.csv(name.energy, header=F))
	name.cost <- paste("cost-",ch,"-",i,".csv", sep="")
		cost <- rbind(cost, read.csv(name.cost, header=F))
	name.totalod <- paste("totalod-",ch,"-",i,".csv", sep="")
		totalod <- rbind(totalod, read.csv(name.totalod, header=F))
	#name.ent <- paste("entropy-",ch,"-",i,".csv", sep="")
	#	ent <- rbind(ent, read.csv(name.ent, header=F))
}

##########
	name.od.err <- paste("errsd-",ch,".csv", sep="")
		od.err <- read.csv(name.od.err, header=F)
	name.o.err <- paste("errsd-o-",ch,".csv", sep="")
		o.err <- read.csv(name.o.err, header=F)
	name.d.err <- paste("errsd-d-",ch,".csv", sep="")
		d.err <- read.csv(name.d.err, header=F)
	name.energy <- paste("energy-",ch,".csv", sep="")
		energy <- read.csv(name.energy, header=F)
	name.cost <- paste("cost-",ch,".csv", sep="")
		cost <- read.csv(name.cost, header=F)
	name.totalod <- paste("totalod-",ch,".csv", sep="")
		totalod <- read.csv(name.totalod, header=F)
##########

#alpha3 <- 10^(-10)
#alpha1 <- 13.0
nodez <- 588
#ent1 <- energy[od.err[,1]!=-99,] + alpha1 * (o.err[od.err[,1]!=-99,]^2 + d.err[od.err[,1]!=-99,]^2)*nodez + alpha3 * (cost[od.err[,1]!=-99,]-3634352.5)^2
#ent.dn <- energy + alpha1 * (o.err^2 + d.err^2)*nodez + alpha3 * (cost-3634352.5)^2
prev <- -9999

sum(od.err[,1]!=-99)
num.it <- nrow(od.err)
sum(od.err[,1]!=-99)/num.it

#################################
## GRAPH
#setwd("D:/od-mcmc/result/0715")

## Number of accepted
acount <- c()
deno <- 1000
for(i in 0:(nrow(od.err)/deno)){
	acount <- c(acount, sum(od.err[(i*deno+1):((i+1)*deno),]!= -99))
}
#postscript("numaccept1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("numaccept1.pdf", width=11, height=5)
#png(filename = "./fig/numaccept2.png", width=1100, height=500)
xlabname <- paste("Number of one term = ", deno, sep="")
plot(1:(length(acount)-1), acount[1:(length(acount)-1)], type="p", xlab=c(xlabname), 
	ylab="Number of accepted sample", pch=20, col="red", ylim=c(0,50), cex=0.1)

dev.off()


#postscript("o-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("o-err-od1.pdf", width=11, height=5)
#png(filename = "./fig/o-err1.png", width=1100, height=500)
#plot(0,0, xlim=c(800,1000), ylim=range(o.err[,1]), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
plot(0,0, xlim=c(0,num.it), ylim=c(1650,2100), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
for(i in 1000*(1:(num.it/1000))){
	if(od.err[i,]!=-99){
		points(i, o.err[i,], pch=20, col="red", cex=0.5)
		prev <- o.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.1)
		points(i, o.err[i,], pch=20, col="green", cex=0.1)
	}
}
legend("topleft", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

#postscript("d-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("d-err-od1.pdf", width=11, height=5)
#png(filename = "./fig/d-err1.png", width=1100, height=500)
#plot(0,0, xlim=c(800,1000), ylim=range(d.err[,1]), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
plot(0,0, xlim=c(0,num.it), ylim=c(1650,2100), xlab="Number of Iteration", ylab="Error of Mean Square (Destination)", col="white")
for(i in 1000*(1:(num.it/1000))){
	if(od.err[i,]!=-99){
		points(i, d.err[i,], pch=20, col="red", cex=0.5)
		prev <- d.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.1)
		points(i, d.err[i,], pch=20, col="green", cex=0.1)
	}
}
legend("topleft", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

#postscript("od-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("od-err-od1.pdf", width=11, height=5)
#png(filename = "./fig/od-err1.png", width=1100, height=500)
plot(0,0, xlim=c(0,num.it), ylim=c(70,100), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
for(i in 1000*(1:(num.it/1000))){
	if(od.err[i,]!=-99){
		points(i, od.err[i,], pch=20, col="red", cex=0.2)
		prev <- od.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.1)
	}
}
legend("topleft", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

#postscript("energy1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("energy1.pdf", width=11, height=5)
#png(filename = "./fig/energy2.png", width=1100, height=700)
#png(filename = "./fig/energy2ex.png", width=1100, height=700)
#plot(0,0, xlim=c(0,105000), ylim=range(energy[od.err[,1]!=-99,]), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
#plot(0,0, xlim=c(800,1000), ylim=c(10000,14000), xlab="Number of Iteration", ylab="Energy", col="white")
plot(0,0, xlim=c(0,num.it), ylim=c(17400,17500), xlab="Number of Iteration", ylab="Energy", col="white")
for(i in 1000*(1:(num.it/1000))){
	if(od.err[i,]!=-99){
		points(i, energy[i,], pch=20, col="red", cex=0.1)
		prev <- energy[i,]
	} else {
		points(i, energy[i,], pch=1, col="green", cex=0.1)
		points(i, prev, pch=20, col="blue", cex=0.1)
	}
}
legend("bottomright", c("accepted","rejected"), pch=1, col=c("red","blue"))
dev.off()
#abline(h=12300)

#postscript("cost1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("cost2.pdf", width=11, height=5)
#png(filename = "./fig/cost1.png", width=1100, height=500)
#plot(0,0, xlim=c(0,25000), ylim=range(cost), xlab="Number of Iteration", ylab="Total travel cost", col="white")
plot(0,0, xlim=c(0,num.it), ylim=range(cost), xlab="Number of Iteration", ylab="Total travel cost", col="white")
		#points(1:nrow(cost), cost[,1], pch=20, col="gray50", lwd=0.4, type="o", cex=0.4)
for(i in 1000*(1:(num.it/1000))){
#	if(od.err[i,]!=-99){
		points(i, cost[i,], pch=20, col="red", cex=0.2)
#		prev <- cost[i,]
#	} else {
#		points(i, prev, pch=20, col="blue", cex=0.1)
#		points(i, cost[i,], pch=1, col="green", cex=0.1)
#	}
}
#t.cost <-  3625226.6 ## 3625988.4 ## 3634352## 6447056 ## 3294329
#abline(h=t.cost, col="gray77")#
#abline(h=t.cost*1.1, col="gray77")#
#abline(h=t.cost*0.9, col="gray77")#
legend("bottomleft", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

#postscript("totalod1.eps", horizontal=F, onefile=F, paper="special", width=11, height=6)
#pdf("totalod1.pdf", width=11, height=6)
#png(filename = "totalod1.png", width=1100, height=600)
#real.od <- read.csv("C:/git-urata/od-mcmc/odtable-row2.csv", header=F)
#sumod <- sum(real.od)#+nodez*(nodez)*20
sumod <- 18503872

plot(0,0, xlim=c(0,nrow(od.err)), ylim=range(18503500, 18505500), xlab="Number of Iteration", 
	ylab="Total OD", col="white")
#for(i in 1:nrow(cost)){
#	points(i, totalod[i,], pch=20, col="green", cex=0.2)
#}
for(i in 10*(1:(nrow(od.err)/10))){
	if(od.err[i,]!=-99){
		points(i, totalod[i,], pch=20, col="red", cex=0.2)
		prev <- totalod[i,]
	} else {
#		points(i, prev, pch=20, col="blue", cex=0.1)
		points(i, totalod[i,], pch=1, col="blue", cex=0.2)
	}
}
abline(h=sumod, col="gray77")
abline(h=sumod*1.01, col="gray77")
abline(h=sumod*0.99, col="gray77")
#abline(v=70000)
legend("topright", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

## Entropy of True OD table
logfac <- function(ss){
	total <- 0
	for(i in ss:1){
		total <- log(i) + total
	}
	total
}
denofac <- 0
for(i in 1:nrow(real.od)){
	if(real.od[i,]!=0){
		denofac <- denofac + logfac(real.od[i,])
	}
}
logfac(sumod) - denofac


##########################
##########################
## package "coda"
## Convergence or not 
gap <- 3200/2
pick <- floor(75000 + gap * c(1:((nrow(od.err) - 75000)/gap)) - gap/2)

heidel.diag(energy[pick,1], eps=0.1, pvalue=0.05)

###########################
###########################
ent <- ent1
j <- 1
#postscript("entropy1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("entropy1.pdf", width=11, height=5)
#png(filename = "entropy1.png", width=1100, height=500)
plot(0,0, xlim=c(800,1000), ylim=c(12000,16000), xlab="Number of Iteration", ylab="Entropy", col="white")
for(i in 1:nrow(od.err)){
	if(od.err[i,]!=-99){
		points(i, ent[j], pch=20, col="red", cex=0.2)
		prev <- ent[j]
		j <- j + 1
	} else {
		points(i, ent.dn[i,], pch=20, col="green", cex=0.1)
		points(i, prev, pch=20, col="blue", cex=0.1)
	}
	abline(h=20307.939, col="gray77")#
}
legend("topright", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()










##################################
## OD Table
#######
## compare each cell
coldef <- function(aa){
	if(aa < 5){
		co <- "#CCFF00"
	} else if(aa < 15){
		co <- "#CCCC00"
	} else if(aa < 25){
		co <- "#CC9900"
	} else if(aa < 35){
		co <- "#CC6600"
	} else if(aa < 45){
		co <- "#CC3300"
	} else {
		co <- "#CC0000"
	}
	co
}

###########
## read OD table accepted
###########
ini.od <- read.csv("C:/git-urata/od-mcmc/odtable-row-ini2.csv", header=F)

setwd("D:/od-mcmc/c-result/0701-b-008-a3-001-a1-2/res-od/")
setwd("D:/od-mcmc/c-result/0708/res-od/")

#ac.tab <- c(0:(nrow(od.err)-1))[od.err[,1]!=-99]
ac.tab <- c(0:(nrow(od.err)-1))[od.err[,1]>0]
tabmat <- rep(-99, length=nodez^2)
for(i in ac.tab){
	ff <- paste("odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	tabmat <- rbind(tabmat, tes[,1])
	cat(i, "\n")
}
tabmat.s <- tabmat[2:nrow(tabmat),]
#save.image("D:\\od-mcmc\\result\\0701\\alldata0701.RData")

#apply(tabmat.s, MARGIN=2, FUN=range)
count0 <- function(ro){
	sum(ro==0)
}
#c0num <- apply(tabmat.s, MARGIN=2, FUN=count0)
#odtime <- read.csv("veh-time2.csv", header=T)
#plot(c0num, odtime[,3], xlim=c(0,12000))

#####
## Compare true OD table
#tabmatz <- tabmat.s[6000:nrow(tabmat.s),]
#tabmatz <- tabmatz[300*(1:floor(nrow(tabmatz)/300)),]
allr <- nrow(tabmatz)

## histgram
i <- 372	#max=44: 232, 370
frer <- tabmatz[,i]
truev <- real.od[i,]
iniv <- ini.od[i,]
mas <- table(frer)
xr <- min(frer):max(frer)

plot(0, 0, xlim=c(0,max(frer,truev,8,iniv)+2), ylim=c(0,1), col="white", 
	ylab="Proportion", xlab="Number of OD pair")
for(i in 1:length(mas)){
	rect(xr[i],0, xr[i]+1, mas[i]/allr, col="gray50")
}
points(c(truev+0.5,truev+0.5), c(0,1), type="l", col="red", lwd=1)
points(c(iniv+0.55, iniv+0.55), c(0,1), type="l", col="blue", lty=2)

## transition
i <- 408	#max=44: 232, 370
i <- i+1
frer <- tabmatz[,i]
truev <- real.od[i,]
iniv <- ini.od[i,]
plot(1:nrow(tabmatz), frer, type="o", col="black", ylim=c(0,max(frer,truev,19)+1))
abline(h=truev, col="red")
abline(h=iniv+0.03, col="blue", lty=2)

#####
## Count 0 cell
cell0 <- c()
for(i in 1:nrow(tabmat.s)){
	cell0 <- c(cell0, sum(tabmat.s[i,]==0))
}

#postscript("cell0.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("cell0.pdf", width=11, height=5)
#png(filename = "cell0.png", width=1100, height=500)
plot(1:length(cell0), cell0, cex=0.3, xlab="Accepted sample", ylab="Number of OD cell which has no trip")
abline(h=sum(real.od==0), col="gray77")	#48
dev.off()

cell0 <- c()
for(i in 1:nrow(tabmatz)){
	cell0 <- c(cell0, sum(tabmatz[i,]==0))
}
plot(1:length(cell0), cell0)
abline(h=sum(real.od==0), col="gray77")	#48

#####
## Count frequent cell
ov25 <- c()	# sample a cell which number is over 20
for(i in 1:nrow(tabmat.s)){
	ov25 <- c(ov25, sum(tabmat.s[i,real.od[,1]>25]>25))	# real 16
}
ov35 <- c()	# 
for(i in 1:nrow(tabmat.s)){
	ov35 <- c(ov35, sum(tabmat.s[i,real.od[,1]>35]>35))	# real 8
}

tabmat.o <- tabmat.s[ov25>=12 & ov35>=7,]#c(51:nrow(tabmat))[ov25>=12 & ov35>=7]
tes <- tabmat.o[6,]# tes <- real.od[,1]
plot(0,0,xlim=c(0,30), ylim=c(0,25), xlab="Origin Zone", ylab="Destination Zone", col="white")
	for(j in 1:length(tes)){
		points(1 + ((j-1) %/% 24), 1+((j-1) %% 24), col=coldef(tes[j]), type="p", pch=15, cex=1.2)
	}
legend("topright", c("0-4","5-14","15-24","25-34","35-44","45-"),
	  col=c(coldef(1),coldef(10),coldef(20),coldef(30), coldef(40), coldef(55)), pch=15)
dev.off()


plot(0,0, xlim=c(0,(nodez^2)), ylim=c(0,max(tabmat.s)), col="white", xlab="OD pair", ylab="Number")
for(i in 1:nrow(tabmat.s)){
	points(1:(nodez^2), tabmat.s[i,], type="o", cex=0.3, col="gray50")
}
points(1:(nodez^2), real.od[,1], type="l", col="red")


#####################
##########
## MCMC sample (RESAMPLING)
##      -(in-burn term) + Resampled each 600
#setwd("D:/od-mcmc/result/0704")
pick <- 35000 + 600 * c(1:((nrow(o.err) - 35000)/600)) - 600/2

pmat <- matrix(-99, ncol=nodez^2, nrow=length(pick))
for(i in 1:length(pick)){
	tab <- max(ac.tab[pick[i] >= ac.tab])
	pmat[i,] <- tabmat.s[ac.tab==tab,]
}
allr <- nrow(pmat)

## histgram
i <- 370	#max=44: 232, 370
frer <- pmat[,i]
truev <- real.od[i,]
iniv <- ini.od[i,]
mas <- table(frer)
xr <- min(frer):max(frer)

plot(0, 0, xlim=c(0,max(frer,truev,8,iniv)+2), ylim=c(0,1), col="white", 
	ylab="Proportion", xlab="Number of OD pair")
for(i in 1:length(mas)){
	rect(xr[i],0, xr[i]+1, mas[i]/allr, col="gray50")
}
points(c(truev+0.5,truev+0.5), c(0,1), type="l", col="red", lwd=1)
points(c(iniv+0.55, iniv+0.55), c(0,1), type="l", col="blue", lty=2)

## transition
i <- 370	#max=44: 232, 370
i <- i-1
for(i in 1:ncol(pmat)){
	frer <- pmat[,i]
	truev <- real.od[i,]
	iniv <- ini.od[i,]
	nameo <- floor(i/nodez)+1
	named <- i %% nodez
	if(named == 0){
		named <- nodez
		nameo <- nameo - 1
	}
	if(nameo < 10){
		OO <- paste(0, nameo, sep="")
	} else {OO <- paste(nameo)}
	if(named < 10){
		DD <- paste(0, named, sep="")
	} else {DD <- paste(named)}
	name <- paste("./cell-trans/cell", "O", OO,"-", "D", DD,".png", sep="")
	cellname <- paste("Cell:", "O", OO, "-", "D", DD, sep="")
	png(filename = name, width=900, height=700)
	plot(1:nrow(pmat), frer, type="o", col="black", ylim=c(0,max(frer,truev,59)+1),
		xlab="sample", ylab="Number of OD pair", cex=0.5)
	abline(h=truev, col="red")
	abline(h=iniv, col="blue", lty=2, lwd=0.8)
	legend("topleft", c("Sample","True OD","Initial OD"), pch=c(1,NA,NA), lty=c(1,1,2), 
		col=c("black","red","blue"), title = cellname)
	dev.off()
}

## Count under/over XX
count.under <- function(mat, xx){
	num0 <- rep(-99,length=nrow(mat))
	for(i in 1:nrow(mat)){
		num0[i] <- sum(mat[i,] < xx)
	}
	num0
}

con0 <- count.under(pmat, 1)
sum(con0)/length(con0)
sum(real.od < 1)

con0 <- count.under(pmat, 2)
sum(con0)/length(con0)
sum(real.od < 2)

con0 <- count.under(pmat, 3)
sum(con0)/length(con0)
median(con0)
sum(real.od < 3)

con0 <- count.under(pmat, 4)
sum(con0)/length(con0)
median(con0)
sum(real.od < 4)

con0 <- count.under(pmat, 5)
sum(con0)/length(con0)
median(con0)
sum(real.od < 5)

con0 <- count.under(pmat, 6)
sum(con0)/length(con0)
median(con0)
sum(real.od < 6)

con0 <- count.under(pmat, 10)
sum(con0)/length(con0)
median(con0)
sum(real.od < 10)

real.con <- c()
sim.con <- c()
for(i in 1:51){
	real.con <- c(real.con, sum(real.od < i))
	con <- count.under(pmat, i)
	sim.con <- c(sim.con, sum(con)/length(con))
}
#postscript("cumlative-number-of-cell.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("cumlative-number-of-cell.pdf", width=11, height=5)
#png(filename = "cumlative-number-of-cell.png", width=1100, height=500)
plot(0:50, real.con, type="o", ylab="Number of OD pair", xlab="Trip number")
points(0.1+c(0:50), sim.con, col="red", type="o")
legend("bottomright", c("MCMC(average)","Real"), col=c("red","black"), lwd=1, pch=1)
dev.off()

#############################################
## truncated Normal
cut <- 0
for(i in 1:108){
	cut <- cut + (pnorm(0, mean=i, sd=log(i/7+1.5))-pnorm(-0.5, mean=i, sd=log(i/7+1.5)))
}
cut
(pnorm(1, mean=0, sd=0.24)-pnorm(0.5, mean=0, sd=0.24))

cut - (pnorm(1, mean=0, sd=0.245946)-pnorm(0.5, mean=0, sd=0.245946))

prow <- c()
for(i in 0:20){
	prow <- c(prow, pnorm(i*0.25, mean=0, sd=2))
}


##########################
## package "coda"
## Convergence or not 
gap <- 3200
pick <- floor(75000 + gap * c(1:((nrow(od.err) - 75000)/gap)) - gap/2)

heidel.diag(energy[pick,1], eps=0.1, pvalue=0.05)


## Difference of COST
difcost <- c()
for(i in 2:nrow(cost)){
	difcost <- c(difcost, cost[i,] - cost[i-1,])
}

difcost.now <- difcost
(sum(difcost.now > 20000)+sum(difcost.now < -20000))/length(difcost.now)
c(1:length(difcost.now))[(difcost.now > 20000)|(difcost.now < -20000)]

od.err.50 <- od.err[50*(1:(floor(nrow(od.err)/50))),]

difcost.ori <- difcost
(sum(difcost.now > 20000)+sum(difcost.now < -20000))/length(difcost.now)

plot(1:length(difcost.ori), difcost.ori)
abline(h=20000, col="red")
abline(h=-20000, col="red")
difcost.ori.50 <- difcost.ori[50*(1:(floor(length(difcost.ori)/50)))]
points(50*(1:(floor(length(difcost.ori)/50))), difcost.ori.50, col="red")

plot(1:length(difcost.now), difcost.now)
abline(h=20000, col="red")
abline(h=-20000, col="red")
difcost.now.50 <- difcost.now[50*(1:(floor(length(difcost.now)/50)))]
points(50*(1:(floor(length(difcost.now)/50))), difcost.now.50, col="red")

difcost.now.51 <- difcost.now[1+50*(1:(floor(length(difcost.now)/50)))]
points(1+50*(1:(floor(length(difcost.now)/50))), difcost.now.51, col="green")
























################################
## OD cell order
################################
real.od <- read.csv("D:/smartgit/od-mcmc05/odtable-row2.csv", header=F)
#sort(real.od[,1], decreasing=T)

#postscript("dist-od-all1.eps", horizontal=F, onefile=F, paper="special", width=7, height=5)
#pdf("dist-od-all1.pdf", width=7, height=5)
plot(0,0,xlim=c(0,580), ylim=c(0,70), xlab="order", ylab="OD demand", col="white")
for(i in 0:99){
	ff <- paste("D:/smartgit/od-mcmc04/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	points(1:nrow(tes), sort(tes[,1], decreasing=T), type="l")
}
points(1:nrow(real.od), sort(real.od[,1], decreasing=T), type="l", col="red")
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

#postscript("dist-od-one1.eps", horizontal=F, onefile=F, paper="special", width=7, height=5)
#pdf("dist-od-one1.pdf", width=7, height=5)
plot(0,0,xlim=c(0,580), ylim=c(0,70), xlab="order", ylab="OD demand", col="white")
i <- 99
	ff <- paste("D:/smartgit/od-mcmc04/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	points(1:nrow(tes), sort(tes[,1], decreasing=T), type="o", cex=0.2)

points(1:nrow(real.od), sort(real.od[,1], decreasing=T), type="o", col="red", cex=0.2)
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

#########
## alpha1=alpha2=10^(-1), alpha3=10^(-8), beta=10^(-5)  accept:82
#########
#postscript("dist-od-all2.eps", horizontal=F, onefile=F, paper="special", width=7, height=5)
#pdf("dist-od-all2.pdf", width=7, height=5)
plot(0,0,xlim=c(0,580), ylim=c(0,70), xlab="order", ylab="OD demand", col="white")
for(i in 0:99){
	ff <- paste("D:/smartgit/od-mcmc04/res-od2/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	points(1:nrow(tes), sort(tes[,1], decreasing=T), type="l")
}
points(1:nrow(real.od), sort(real.od[,1], decreasing=T), type="l", col="red")
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

#########
## alpha1=alpha2=10^(-1), alpha3=10^(-7), beta=10^(-6)  accept:83
#########
#postscript("dist-od-all3.eps", horizontal=F, onefile=F, paper="special", width=7, height=5)
#pdf("dist-od-all3.pdf", width=7, height=5)
plot(0,0,xlim=c(0,580), ylim=c(0,70), xlab="order", ylab="OD demand", col="white")
for(i in 0:99){
	ff <- paste("D:/smartgit/od-mcmc04/res-od3/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	points(1:nrow(tes), sort(tes[,1], decreasing=T), type="l")
}
points(1:nrow(real.od), sort(real.od[,1], decreasing=T), type="l", col="red")
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()


#########
## alpha1=alpha2=10^(-1), alpha3=10^(-7), beta=10^(-6)  accept:88
#########
#postscript("dist-od-all4.eps", horizontal=F, onefile=F, paper="special", width=7, height=5)
#pdf("dist-od-all4.pdf", width=7, height=5)
plot(0,0,xlim=c(0,580), ylim=c(0,70), xlab="order", ylab="OD demand", col="white")
for(i in 0:99){
	ff <- paste("D:/smartgit/od-mcmc04-1/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	points(1:nrow(tes), sort(tes[,1], decreasing=T), type="l")
}
points(1:nrow(real.od), sort(real.od[,1], decreasing=T), type="l", col="red")
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

#postscript("dist-od-all4-log.eps", horizontal=F, onefile=F, paper="special", width=5, height=5)
#pdf("dist-od-all4-log.pdf", width=5, height=5)
plot(0,0,xlim=c(0,log(580)), ylim=c(0,log(70)), xlab="log(order)", ylab="log(OD demand)", col="white")
i <- 97
	ff <- paste("D:/smartgit/od-mcmc04-1/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	points(log(1:nrow(tes)), log(sort(tes[,1], decreasing=T)), type="p", cex=0.5)

points(log(1:nrow(real.od)), log(sort(real.od[,1], decreasing=T)), type="p", col="red", cex=0.5)
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

## compare error term
od.err <- read.csv("D:/smartgit/od-mcmc04-1/errsd.csv", header=F)
o.err <- read.csv("D:/smartgit/od-mcmc04-1/errsd-o.csv", header=F)
d.err <- read.csv("D:/smartgit/od-mcmc04-1/errsd-d.csv", header=F)

#postscript("o-err-od41.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("o-err-od41.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,100), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
for(i in 1:nrow(o.err)){
	if(o.err[i,]!=-99){
		points(i, o.err[i,], pch=20, col="red", cex=0.2)
		prev <- o.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("d-err-od41.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("d-err-od41.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,130), xlab="Number of Iteration", ylab="Error of Mean Square (Destination)", col="white")
for(i in 1:nrow(d.err)){
	if(d.err[i,]!=-99){
		points(i, d.err[i,], pch=20, col="red", cex=0.2)
		prev <- d.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("od-err-od41.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("od-err-od41.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,15), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
for(i in 1:nrow(od.err)){
	if(od.err[i,]!=-99){
		points(i, od.err[i,], pch=20, col="red", cex=0.2)
		prev <- od.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()



#########
## Sample with Oave
## alpha1=alpha2=10^(-1), alpha3=10^(-7), beta=10^(-6)  accept:98
#########
maxv <- 0
#postscript("dist-od-aveo.eps", horizontal=F, onefile=F, paper="special", width=7, height=5)
#pdf("dist-od-aveo.pdf", width=5, height=7)
plot(0,0,xlim=c(0,580), ylim=c(0,155), xlab="order", ylab="OD demand", col="white")
for(i in 0:99){
	ff <- paste("D:/smartgit/od-mcmc05/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	if(maxv < max(tes[,1])) maxv <- max(tes[,1])
	points(1:nrow(tes), sort(tes[,1], decreasing=T), type="l")
}
points(1:nrow(real.od), sort(real.od[,1], decreasing=T), type="l", col="red")
abline(v=0, col="gray70")
abline(h=0, col="gray70")
abline(v=576, col="gray70")
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

#postscript("dist-od-aveo-log.eps", horizontal=F, onefile=F, paper="special", width=5, height=5)
#pdf("dist-od-aveo-log.pdf", width=5, height=5)
plot(0,0,xlim=c(0,log(580)), ylim=c(0,log(70)), xlab="log(order)", ylab="log(OD demand)", col="white")
i <- 98
	ff <- paste("D:/smartgit/od-mcmc05/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	points(log(1:nrow(tes)), log(sort(tes[,1], decreasing=T)), type="p", cex=0.5)

points(log(1:nrow(real.od)), log(sort(real.od[,1], decreasing=T)), type="p", col="red", cex=0.5)
abline(v=0, col="gray70")
abline(h=0, col="gray70")
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

#######
## compare each cell
coldef <- function(aa){
	if(aa < 5){
		co <- "#CCFF00"
	} else if(aa < 15){
		co <- "#CCCC00"
	} else if(aa < 25){
		co <- "#CC9900"
	} else if(aa < 35){
		co <- "#CC6600"
	} else if(aa < 45){
		co <- "#CC3300"
	} else {
		co <- "#CC0000"
	}
	co
}

#postscript("dist-od-aveo-cell.eps", horizontal=F, onefile=F, paper="special", width=5.5, height=5)
#pdf("dist-od-aveo-cell.pdf", width=5.5, height=5)
i <- 98
plot(0,0,xlim=c(0,30), ylim=c(0,25), xlab="Origin Zone", ylab="Destination Zone", col="white")
	ff <- paste("D:/smartgit/od-mcmc05/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	for(j in 1:nrow(tes)){
		points(1 + ((j-1) %/% 24), 1+((j-1) %% 24), col=coldef(tes[j,1]), type="p", pch=15, cex=1.2)
	}
legend("topright", c("0-4","5-14","15-24","25-34","35-44","45-"),
	  col=c(coldef(1),coldef(10),coldef(20),coldef(30), coldef(40), coldef(55)), pch=15)
dev.off()

#postscript("dist-od-real-cell.eps", horizontal=F, onefile=F, paper="special", width=5.5, height=5)
#pdf("dist-od-real-cell.pdf", width=5.5, height=5)
plot(0,0,xlim=c(0,30), ylim=c(0,25), xlab="Origin Zone", ylab="Destination Zone", col="white")
i <- 98
	#ff <- paste("D:/smartgit/od-mcmc05/res-od/odtable-",i,".csv", sep="")
	#tes <- read.csv(ff, header=F)
	for(j in 1:nrow(tes)){
		points(1 + ((j-1) %/% 24), 1+((j-1) %% 24), col=coldef(real.od[j,1]), type="p", pch=15, cex=1.2)
	}
legend("topright", c("0-4","5-14","15-24","25-34","35-44","45-"),
	  col=c(coldef(1),coldef(10),coldef(20),coldef(30), coldef(40), coldef(55)), pch=15)
dev.off()

## for anime
for(i in 0:99){
	fname <- paste("D:/smartgit/result/0606/matrix/", i, ".png", sep="")
	png(filename=fname, width=550, height=500)
	plot(0,0,xlim=c(0,30), ylim=c(0,25), xlab="Origin Zone", ylab="Destination Zone", col="white")
		ff <- paste("D:/smartgit/od-mcmc05/res-od/odtable-",i,".csv", sep="")
		tes <- read.csv(ff, header=F)
		for(j in 1:nrow(tes)){
			points(1 + ((j-1) %/% 24), 1+((j-1) %% 24), col=coldef(tes[j,1]), type="p", pch=15, cex=1.2)
		}
	legend("topright", c("0-4","5-14","15-24","25-34","35-44","45-"),
		  col=c(coldef(1),coldef(10),coldef(20),coldef(30), coldef(40), coldef(55)), pch=15)
	dev.off()
}


## compare error term
od.err <- read.csv("D:/smartgit/od-mcmc05/errsd.csv", header=F)
o.err <- read.csv("D:/smartgit/od-mcmc05/errsd-o.csv", header=F)
d.err <- read.csv("D:/smartgit/od-mcmc05/errsd-d.csv", header=F)

#postscript("o-err-aveo.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("o-err-aveo.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,100), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
for(i in 1:nrow(o.err)){
	if(o.err[i,]!=-99){
		points(i, o.err[i,], pch=20, col="red", cex=0.2)
		prev <- o.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("d-err-aveo.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("d-err-aveo.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,130), xlab="Number of Iteration", ylab="Error of Mean Square (Destination)", col="white")
for(i in 1:nrow(d.err)){
	if(d.err[i,]!=-99){
		points(i, d.err[i,], pch=20, col="red", cex=0.2)
		prev <- d.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("od-err-aveo.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("od-err-aveo.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,15), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
for(i in 1:nrow(od.err)){
	if(od.err[i,]!=-99){
		points(i, od.err[i,], pch=20, col="red", cex=0.2)
		prev <- od.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

############################################################
#########
## Sample with Oave & Dave
## alpha1=alpha2=10^(-1), alpha3=10^(-7), beta=10^(-6)  accept:100
#########
maxv <- 0	# 82.16
#postscript("dist-od-aveod.eps", horizontal=F, onefile=F, paper="special", width=7, height=5)
#pdf("dist-od-aveod.pdf", width=5, height=7)
plot(0,0,xlim=c(0,580), ylim=c(0,155), xlab="order", ylab="OD demand", col="white")
for(i in 0:99){
	ff <- paste("D:/smartgit/od-mcmc05-2/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	if(maxv < max(tes[,1])) maxv <- max(tes[,1])
	points(1:nrow(tes), sort(tes[,1], decreasing=T), type="l")
}
points(1:nrow(real.od), sort(real.od[,1], decreasing=T), type="l", col="red")
abline(v=0, col="gray70")
abline(h=0, col="gray70")
abline(v=576, col="gray70")
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

#postscript("dist-od-aveod-log.eps", horizontal=F, onefile=F, paper="special", width=5, height=5)
#pdf("dist-od-aveod-log.pdf", width=5, height=5)
plot(0,0,xlim=c(0,log(580)), ylim=c(0,log(70)), xlab="log(order)", ylab="log(OD demand)", col="white")
i <- 99
	ff <- paste("D:/smartgit/od-mcmc05-2/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	points(log(1:nrow(tes)), log(sort(tes[,1], decreasing=T)), type="p", cex=0.5)

points(log(1:nrow(real.od)), log(sort(real.od[,1], decreasing=T)), type="p", col="red", cex=0.5)
abline(v=0, col="gray70")
abline(h=0, col="gray70")
legend("topright", c("Real","Sample"), col=c("red","black"), lty=1)
dev.off()

#postscript("dist-od-aveod-cell.eps", horizontal=F, onefile=F, paper="special", width=5.5, height=5)
#pdf("dist-od-aveod-cell.pdf", width=5.5, height=5)
i <- 99
plot(0,0,xlim=c(0,30), ylim=c(0,25), xlab="Origin Zone", ylab="Destination Zone", col="white")
	ff <- paste("D:/smartgit/od-mcmc05-2/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	for(j in 1:nrow(tes)){
		points(1 + ((j-1) %/% 24), 1+((j-1) %% 24), col=coldef(tes[j,1]), type="p", pch=15, cex=1.2)
	}
legend("topright", c("0-4","5-14","15-24","25-34","35-44","45-"),
	  col=c(coldef(1),coldef(10),coldef(20),coldef(30), coldef(40), coldef(55)), pch=15)
dev.off()

## for anime
for(i in 0:99){
	fname <- paste("D:/smartgit/result/0606/matrix-od/", i, ".png", sep="")
	png(filename=fname, width=550, height=500)
	plot(0,0,xlim=c(0,30), ylim=c(0,25), xlab="Origin Zone", ylab="Destination Zone", col="white")
		ff <- paste("D:/smartgit/od-mcmc05-2/res-od/odtable-",i,".csv", sep="")
		tes <- read.csv(ff, header=F)
		for(j in 1:nrow(tes)){
			points(1 + ((j-1) %/% 24), 1+((j-1) %% 24), col=coldef(tes[j,1]), type="p", pch=15, cex=1.2)
		}
	legend("topright", c("0-4","5-14","15-24","25-34","35-44","45-"),
		  col=c(coldef(1),coldef(10),coldef(20),coldef(30), coldef(40), coldef(55)), pch=15)
	dev.off()
}

## compare error term
od.err <- read.csv("D:/smartgit/od-mcmc05-2/errsd.csv", header=F)
o.err <- read.csv("D:/smartgit/od-mcmc05-2/errsd-o.csv", header=F)
d.err <- read.csv("D:/smartgit/od-mcmc05-2/errsd-d.csv", header=F)

#postscript("o-err-aveod.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("o-err-aveod.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,100), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
for(i in 1:nrow(o.err)){
	if(o.err[i,]!=-99){
		points(i, o.err[i,], pch=20, col="red", cex=0.2)
		prev <- o.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("d-err-aveod.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("d-err-aveod.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,130), xlab="Number of Iteration", ylab="Error of Mean Square (Destination)", col="white")
for(i in 1:nrow(d.err)){
	if(d.err[i,]!=-99){
		points(i, d.err[i,], pch=20, col="red", cex=0.2)
		prev <- d.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("od-err-aveod.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("od-err-aveod.pdf", width=11, height=5)
plot(0,0, xlim=c(0,100), ylim=c(0,15), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
for(i in 1:nrow(od.err)){
	if(od.err[i,]!=-99){
		points(i, od.err[i,], pch=20, col="red", cex=0.2)
		prev <- od.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#####################################
## Distribution (Demand of CELL)
real.10 <- real.od[(24*9+1):(24*10),1]

odtb.10 <- real.10
for(i in 0:99){
	ff <- paste("D:/smartgit/od-mcmc05-2/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	odtb.10 <- rbind(odtb.10, tes[(24*9+1):(24*10),1])
}
odtb.10 <- odtb.10[2:nrow(odtb.10),]

otb.10 <- real.10
for(i in 0:99){
	ff <- paste("D:/smartgit/od-mcmc05/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	otb.10 <- rbind(otb.10, tes[(24*9+1):(24*10),1])
}
otb.10 <- otb.10[2:nrow(otb.10),]

for(i in 1:24){
	#fname <- paste("D:/smartgit/result/0606/cell-o10-to/", i, ".pdf", sep="")
	#pdf(fname, width=5, height=5)
	fname <- paste("D:/smartgit/result/0606/cell-o10-to/", i, ".png", sep="")
	png(filename=fname, width=500, height=500)

	plot(0,0, xlim=c(0,100), ylim=c(0,152), xlab="rank", ylab="OD Demand", col="white")
	points(1:100, sort(odtb.10[,i], decreasing=T), col="blue", cex=0.5, pch=20)
	points(1:100, sort(otb.10[,i], decreasing=T), col="green", cex=0.5, pch=20)
	abline(h=real.10[i], col="red")
	text(1,real.10[i],paste(real.10[i]), pos=1, col="red")
	abline(v=c(25,50,75), col="gray77")
	legend("topright", c("real","OD poisson","O poisson"), lty=c(1,0,0), 
		pch=c(NA,20,20), col=c("red","blue","green"))
	dev.off()
}

####################################
setwd("D:/smartgit/result/0608sampled")

#postscript("dist-od-mcmc-cell.eps", horizontal=F, onefile=F, paper="special", width=5.5, height=5)
#pdf("dist-od-mcmc-cell.pdf", width=5.5, height=5)
i <- 31326
#pdf(paste("D:/smartgit/od-mcmc06/res-od/cell-mcmc-",i,".pdf", sep=""), width=5.5, height=5)
plot(0,0,xlim=c(0,30), ylim=c(0,25), xlab="Origin Zone", ylab="Destination Zone", col="white")
	ff <- paste("D:/smartgit/od-mcmc06/res-od/odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	for(j in 1:nrow(tes)){
		points(1 + ((j-1) %/% 24), 1+((j-1) %% 24), col=coldef(tes[j,1]), type="p", pch=15, cex=1.2)
	}
legend("topright", c("0-4","5-14","15-24","25-34","35-44","45-"),
	  col=c(coldef(1),coldef(10),coldef(20),coldef(30), coldef(40), coldef(55)), pch=15)
dev.off()


