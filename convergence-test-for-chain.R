
for(ch in 0:7){

setwd("C:/mcmc-static-git/mcmc_static.git/result-20160716_194804")

i <- 1
	name.od.err <
- paste("errsd-",ch,"-",i,".csv", sep="")
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

print(ch)
print(sum(od.err[,1]!=-99)/nrow(od.err))

##########################
## package "coda"
## Convergence or not 
gap <- 3200
pick <- floor(75000 + gap * c(1:((nrow(od.err) - 75000)/gap)) - gap/2)

print(heidel.diag(energy[pick,1], eps=0.1, pvalue=0.05))

}
