setwd("D:/od-mcmc/result/0627")

#########
#########
od.err <- read.csv("D:/od-mcmc/c-result/errsd.csv", header=F)
o.err <- read.csv("D:/od-mcmc/c-result/errsd-o.csv", header=F)
d.err <- read.csv("D:/od-mcmc/c-result/errsd-d.csv", header=F)
energy <- read.csv("D:/od-mcmc/c-result/energy.csv", header=F)
cost <- read.csv("D:/od-mcmc/c-result/cost.csv", header=F)
totalod <- read.csv("D:/od-mcmc/c-result/totalod.csv", header=F)
ent <- read.csv("D:/od-mcmc/c-result/entropy.csv", header=F)

#alpha3 <- 0.1
#alpha1 <- 1.0
nodez <- 24
#ent <- entorp + alpha1 * (o.err^2 + d.err^2)*nodez + alpha3 * (cost-104950)^2

#postscript("o-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("o-err-od+20.pdf", width=11, height=5)
plot(0,0, xlim=c(0,30000), ylim=c(0,max(o.err)), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
for(i in 1:nrow(o.err)){
	if(o.err[i,]!=-99){
		points(i, o.err[i,], pch=20, col="red", cex=0.2)
		prev <- o.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
legend("bottomleft", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

#postscript("d-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("d-err-od1.pdf", width=11, height=5)
plot(0,0, xlim=c(0,30000), ylim=c(0,max(d.err)), xlab="Number of Iteration", ylab="Error of Mean Square (Destination)", col="white")
for(i in 1:nrow(d.err)){
	if(d.err[i,]!=-99){
		points(i, d.err[i,], pch=20, col="red", cex=0.2)
		prev <- d.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("od-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("od-err-od1.pdf", width=11, height=5)
plot(0,0, xlim=c(0,30000), ylim=c(0,max(od.err)), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
for(i in 1:nrow(od.err)){
	if(od.err[i,]!=-99){
		points(i, od.err[i,], pch=20, col="red", cex=0.2)
		prev <- od.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("entropy1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("entropy1.pdf", width=11, height=5)
plot(0,0, xlim=c(0,30000), ylim=range(ent[cost!=-99]), xlab="Number of Iteration", ylab="Entropy", col="white")
for(i in 1:nrow(od.err)){
	if(cost[i,]!=-99){
		points(i, ent[i,], pch=20, col="red", cex=0.2)
		prev <- ent[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
	abline(h=20307.939, col="gray77")#
}
dev.off()

#postscript("energy1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("energy1.pdf", width=11, height=5)
plot(0,0, xlim=c(0,30000), ylim=range(energy[cost!=-99]), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
#plot(0,0, xlim=c(0,30000), ylim=c(19500,20500), xlab="Number of Iteration", ylab="Energy", col="white")
for(i in 1:nrow(energy)){
	if(energy[i,]!=-99){
		points(i, energy[i,], pch=20, col="red", cex=0.2)
		prev <- energy[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
}
dev.off()

#postscript("cost1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("cost1.pdf", width=11, height=5)
plot(0,0, xlim=c(0,30000), ylim=range(cost[cost!=min(cost)]), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
#plot(0,0, xlim=c(0,30000), ylim=c(104000,106000), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
for(i in 1:nrow(cost)){
	if(cost[i,]!=-99){
		points(i, cost[i,], pch=20, col="red", cex=0.2)
		prev <- cost[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.2)
	}
	abline(h=104950, col="gray77")#
	abline(h=104950*1.001, col="gray77")#
	abline(h=104950*0.999, col="gray77")#
}
dev.off()

#postscript("totalod1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("totalod1.pdf", width=11, height=5)
real.od <- read.csv("C:/git-urata/od-mcmc/odtable-row2.csv", header=F)
sumod <- sum(real.od)#+nodez*(nodez)*20

plot(0,0, xlim=c(0,30000), ylim=c(sumod-200, sumod+100), xlab="Number of Iteration", 
	ylab="Error of Mean Square (OD cell)", col="white")
#for(i in 1:nrow(cost)){
#	points(i, totalod[i,], pch=20, col="green", cex=0.2)
#}
for(i in 1:nrow(cost)){
	if(cost[i,]!=-99){
		points(i, totalod[i,], pch=20, col="red", cex=0.1)
		prev <- totalod[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.3)
	}
	abline(h=sumod, col="gray77")
	abline(h=sumod*1.01, col="gray77")
	abline(h=sumod*0.99, col="gray77")
}
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

setwd("D:/od-mcmc/c-result/0629-b-015-a3-0005-a1-1/res-od/")

ac.tab <- c(0:(nrow(o.err)-1))[o.err[,1]!=-99]
tabmat <- rep(-99, length=nodez^2)
for(i in ac.tab){
	ff <- paste("odtable-",i,".csv", sep="")
	tes <- read.csv(ff, header=F)
	tabmat <- rbind(tabmat, tes[,1])
}
tabmat.s <- tabmat[2:nrow(tabmat),]
#apply(tabmat.s, MARGIN=2, FUN=range)
#tabmatz <- tabmat[5000:nrow(tabmat.s),]

## Compare true OD table
allr <- nrow(tabmatz)

i <- 215
frer <- tabmatz[,i]
truev <- real.od[i,]
iniv <- ini.od[i,]
mas <- table(frer)
xr <- min(frer):max(frer)

plot(0, 0, xlim=c(0,max(frer,truev,8)+2), ylim=c(0,1), col="white", 
	ylab="Proportion", xlab="Number of OD pair")
for(i in 1:length(mas)){
	rect(xr[i],0, xr[i]+1, mas[i]/allr, col="gray50")
}
points(c(truev+0.5,truev+0.5), c(0,1), type="l", col="red", lwd=1)
points(c(iniv+0.55, iniv+0.55), c(0,1), type="l", col="blue", lty=2)

##
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


