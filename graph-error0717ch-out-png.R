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
setwd("D:/od-mcmc/c-result/0717-01")
setwd("D:/od-mcmc/heap-result0717-/result-20160717_165106")
setwd("C:/mcmc_static/result-20160715_113946")
setwd("C:/mcmc-static-git/mcmc_static.git/result-20160715_214151")
setwd("D:/od-mcmc/mcmc_static.git-0717/result-20160717_193034")

#ch <- 0
for(ch in 1:4){

setwd("C:/mcmc-static-git/mcmc_static.git/result-20160717_121550")

i <- 1
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
#	name.ent <- paste("entropy-",ch,"-",i,".csv", sep="")
#		ent <- read.csv(name.ent, header=F)

for(i in 2:20){
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

#alpha3 <- 10^(-10)
#alpha1 <- 13.0
nodez <- 24
#ent1 <- energy[od.err[,1]!=-99,] + alpha1 * (o.err[od.err[,1]!=-99,]^2 + d.err[od.err[,1]!=-99,]^2)*nodez + alpha3 * (cost[od.err[,1]!=-99,]-3634352.5)^2
#ent.dn <- energy + alpha1 * (o.err^2 + d.err^2)*nodez + alpha3 * (cost-3634352.5)^2
prev <- -9999

sum(od.err[,1]!=-99)
nrow(od.err)
sum(od.err[,1]!=-99)/nrow(od.err)

#################################
## GRAPH
day <- 0717
#name.folder <- paste("D:/od-mcmc/result/0",day,"/ch",ch, sep="")
name.folder <- paste("D:/od-mcmc/result/0",day, sep="")
setwd(name.folder)
#getwd()

## Number of accepted
acount <- c()
for(i in 0:(nrow(od.err)/100)){
	acount <- c(acount, sum(od.err[(i*100+1):((i+1)*100),]!= -99))
}
name.f.num <- paste("numaccept", ch, sep="")
#postscript("numaccept1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("numaccept1.pdf", width=11, height=5)
png(filename = paste(name.f.num, ".png", sep=""), width=1100, height=500)
plot(1:length(acount), acount, type="p", xlab=c("Number of iteration(*100)"), 
	ylab="Number of accepted sample", pch=20, col="red", ylim=c(0,100), cex=0.1)

dev.off()


name.f.o <- paste("o-err-od", ch, sep="")
#postscript("o-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("o-err-od1.pdf", width=11, height=5)
png(filename = paste(name.f.o, ".png", sep=""), width=1100, height=500)
#plot(0,0, xlim=c(800,1000), ylim=c(0,10), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
plot(0,0, xlim=c(0,nrow(o.err)), ylim=c(0,10), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
for(i in 1:nrow(o.err)){
	if(od.err[i,]!=-99){
		points(i, o.err[i,], pch=20, col="red", cex=0.5)
		prev <- o.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.1)
	#	points(i, o.err[i,], pch=20, col="green", cex=0.1)
	}
}
legend("topright", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()


name.f.d <- paste("d-err-od", ch, sep="")
#postscript("d-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("d-err-od1.pdf", width=11, height=5)
png(filename = paste(name.f.d, ".png", sep=""), width=1100, height=500)
#plot(0,0, xlim=c(800,1000), ylim=c(0,10), xlab="Number of Iteration", ylab="Error of Mean Square (Origin)", col="white")
plot(0,0, xlim=c(0,nrow(d.err)), ylim=c(0,10), xlab="Number of Iteration", ylab="Error of Mean Square (Destination)", col="white")
for(i in 1:nrow(d.err)){
	if(od.err[i,]!=-99){
		points(i, d.err[i,], pch=20, col="red", cex=0.5)
		prev <- d.err[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.1)
	#	points(i, d.err[i,], pch=20, col="blue", cex=0.1)
	}
}
legend("topright", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()


name.f.od <- paste("od-err-od", ch, sep="")
#postscript("od-err-od1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("od-err-od1.pdf", width=11, height=5)
png(filename = paste(name.f.od, ".png", sep=""), width=1100, height=500)
plot(0,0, xlim=c(0,nrow(od.err)), ylim=c(0,10), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
for(i in 1:nrow(od.err)){
	if(od.err[i,]!=-99){
		points(i, od.err[i,], pch=20, col="red", cex=0.2)
		prev <- od.err[i,]
	} else {
		if(i %% 3 == 0){
		points(i, prev, pch=20, col="blue", cex=0.1)
	#	points(i, od.err[i,], pch=20, col="blue", cex=0.1)
		}
	}
}
legend("topleft", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()


name.f.ene <- paste("energy", ch, sep="")
#postscript("energy1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("energy1.pdf", width=11, height=5)
png(filename = paste(name.f.ene, ".png", sep=""), width=1100, height=500)
#plot(0,0, xlim=c(0,105000), ylim=range(energy[od.err[,1]!=-99,]), xlab="Number of Iteration", ylab="Error of Mean Square (OD cell)", col="white")
#plot(0,0, xlim=c(800,1000), ylim=c(10000,14000), xlab="Number of Iteration", ylab="Energy", col="white")
plot(0,0, xlim=c(0,nrow(energy)), ylim=c(16000,21000), xlab="Number of Iteration", ylab="Energy", col="white")
for(i in 1:nrow(energy)){
	if(od.err[i,]!=-99){
		points(i, energy[i,], pch=20, col="red", cex=0.1)
		prev <- energy[i,]
	} else {
	#	points(i, energy[i,], pch=1, col="blue", cex=0.1)
		points(i, prev, pch=20, col="blue", cex=0.1)
	}
}
legend("bottomright", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

name.f.cost <- paste("cost", ch, sep="")
#postscript("cost1.eps", horizontal=F, onefile=F, paper="special", width=11, height=5)
#pdf("cost2.pdf", width=11, height=5)
png(filename = paste(name.f.cost, ".png", sep=""), width=1100, height=500)
#plot(0,0, xlim=c(0,25000), ylim=range(cost), xlab="Number of Iteration", ylab="Total travel cost", col="white")
plot(0,0, xlim=c(0,nrow(cost)), ylim=c(3000000,4700000), xlab="Number of Iteration", ylab="Total travel cost", col="white")
		#points(1:nrow(cost), cost[,1], pch=20, col="gray50", lwd=0.4, type="o", cex=0.4)
for(i in 1:nrow(cost)){
	if(od.err[i,]!=-99){
		points(i, cost[i,], pch=20, col="red", cex=0.2)
		prev <- cost[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.1)
	#	points(i, cost[i,], pch=1, col="green", cex=0.1)
	}
}
t.cost <-  3625226.6 ## 3625988.4 ## 3634352## 6447056 ## 3294329
abline(h=t.cost, col="gray77")#
abline(h=t.cost*1.1, col="gray77")#
abline(h=t.cost*0.9, col="gray77")#
legend("bottomleft", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

name.f.total <- paste("total", ch, sep="")
#postscript("totalod1.eps", horizontal=F, onefile=F, paper="special", width=11, height=6)
#pdf("totalod1.pdf", width=11, height=6)
png(filename = paste(name.f.total, ".png", sep=""), width=1100, height=600)
real.od <- read.csv("C:/git-urata/od-mcmc/odtable-row2.csv", header=F)
sumod <- sum(real.od)#+nodez*(nodez)*20

plot(0,0, xlim=c(0,nrow(totalod)), ylim=c(sumod-70, sumod+80), xlab="Number of Iteration", 
	ylab="Total OD", col="white")
#for(i in 1:nrow(cost)){
#	points(i, totalod[i,], pch=20, col="green", cex=0.2)
#}
for(i in 1:nrow(totalod)){
	if(od.err[i,]!=-99){
		points(i, totalod[i,], pch=20, col="red", cex=0.2)
		prev <- totalod[i,]
	} else {
		points(i, prev, pch=20, col="blue", cex=0.1)
	#	points(i, totalod[i,], pch=1, col="blue", cex=0.2)
	}
}
abline(h=sumod, col="gray77")
abline(h=sumod*1.01, col="gray77")
abline(h=sumod*0.99, col="gray77")
#abline(v=70000)
legend("topright", c("accepted","rejected"), pch=20, col=c("red","blue"))
dev.off()

}
