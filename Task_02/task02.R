setwd('/Users/angelavanhorn/Desktop/Evolution/Tasks/Task_02')
Data1 <- read.csv('http://jonsmitchell.com/data/beren.csv',
	stringsAsFactors=F)
Data2 <- read.csv('http://jonsmitchell.com/data/cyrus.csv',
	stringsAsFactors=F)
write.csv(Data1, 'rawdata.csv', quote=F)
length(Data1)
nrow(Data1)
ncol(Data1)
colnames(Data1)
head(Data1)
Data1 [1,]
Data1 [2,]
Data1 [1:3,]
Data1 [1:3, 4]
Data1 [1:5, 1:3]
Feeds <- which(Data1 [.9] == 'bottle')
berenMilk <- Data1 [Feeds,]
head(berenMilk)
Feeds <- which(Data1 [,'event'] =='bottle')
Feeds <- which(Data1$event =='bottle')
dayID <- apply(Data1, 1, function(x) paste(x[1:3], collapse='-'))
dateID <- sapply('dayID' , as.Date, format = "%Y-%n-%d", orgin =
	"2019-04-18")
Data1$age <- 'dataID - dataID'[which(Data1$event == 'birth')]
head(Data1)	
beren2 <- Data1
beren3 <- beren2[order(beren2$age),]
write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)
Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
varFeed <- tapply(beren3$value[Feeds], beren3$[Feeds], var)
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
Feeds[1:6]
[1]  6  7  8  9 20 21
 beren3$value[1:6]
[1]  NA  NA  NA  NA  NA 1.5
 beren3$age[1:6]
[1] "dataID - dataID" "dataID - dataID" "dataID - dataID" "dataID - dataID"
[5] "dataID - dataID" "dataID - dataID"
 berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
 boxplot(beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab= "who gave the bottle", ylab= "amount of milk consumed (oz)")
abline(h=mean(totalFeed), lty=2, col='red')

pdf('r02b-totalMilkByDay.pdf, height=4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days",ylab="ounces of milk")
abline(h=mean(totalFeed), 1ty=2, col="red")
dev.off()
source('https://jonsmitchell.com/code/plotFxn02b.R')
Mass <- which(Data1[,9]=='trait_mass')
berenMass <- Data1[Mass,]
head(berenMass)
source("http://jonsmitchell.com/code/simFxn04.R")
MatGrandpa <- makeFounder("grandma_mom")
MatGrandpa <- makeFounder("grandpa_mom")
PatGrandma <- makeFounder("grandma_da")
Alan <- makeBaby(PatGrandma, PatGrandpa)
Brenda <- makeBaby(PatGrandma, PatGrandpa)
Focus <- makeBaby(Brenda, Alan)

ToMom <- length(grep("mom", Focus))/length(Focus)
toMomMom <- length(grep("grandma_mom",Focus))/length(Focus)
ToMomDad <- length(grep("grandpa_mom", Focus))/length(Focus)

Sibling_01 <- makeBaby(Brenda, Alan)
ToSib <- length(intersect(Focus, Sibling_01))/length(Focus)
ManySiblings <- replicate(1e3, length(intersect(Focus, makeBaby(Brenda, Alan)))/length(Focus))
quantile(ManySiblings)

mean(ManySiblings)
plot(density(ManySiblings), main="", xlab="proportion shared genes")
sum(ToMomMom, ToMomDad)
pdf("003_relatePlot.pdf", height = 4, width = 4)
dev.off()
