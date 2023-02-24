install.packages("adegenet")
install.packages("poppr")
install.packages("dplyr")
install.packages("heirfstat")
install.packages("dreshape2")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("scales")

library(adegenet)
library(poppr)
library(dplyr)
library(heirfstat)
library(reshape2)
library(ggplot2)
libary(RColorBrewer)
library(scales)

lobster<-read.csv("https://jonsmitchell.com/data/lobster_genotypes.csv")
lobster_wide <- reshape(lobster, idvar =c("ID","Site"), timevar= "Locus", direction = "wide" , sep ="")
colnames(lobster_wide) <- gsub("Genotype", "", colnames(lobster_wide ))
snpgeno <- lobster_wide[ , 3:ncol(lobster_wide)]
snps_to_remove <- c("25580","32362","41521","53889","65376","8953","21197","15531","22740","28357","33066","51507","53052","53263","21880","22323","22365")
snpgeno <- snpgeno[ , !colnames(snpgeno) %in% snps_to_remove]
ind <- as.character(lobster_wide$ID)
site <- as.character(lobster_wide$Site)

lobster_gen <= df2genind (snpgeno, ploidy=2, ind.names = ind, pop = site, sep="")
lobster_gen <= missingno(lobster_gen, type="geno", cutoff=0.20)
mlg(lobster_gen)
dup_lobster <- mlg.id(lobster_gen)
lob_dups <- c()
x <- 1
for (i in dups_lobster){
	if (length(dups_lobster[i]) > 1){
		lob_dups[x] <- i
	x <- x + 1
	}
}
lob_Nodups <- indNames(lobster_gen)[! indNames(lobster_gen)[! indNames(lobster_gen) %in% lob_
	dups]
lobster_gen <- lobster_gen[lob_Nodups, ] 
summary(lobster_gen$pop)
basic_lobster <- basic.stats(lobster_gen, diploid =TRUE)
Ho_lobster <- round(apply(basic_lobster$Ho, MARGIN = 2, FUN=mean, na.
	rm=TRUE), digits=3)
He_lobster <- round(apply(basic_lobster$Hs, MARGIN=2, FUN=mean, na. 
	rm=TRUE), digits=3)
par(mar=c(4,41,1), las=1, mgp=c(2, 0.25, 0), tck=-0.005, cex.lab=1.25)
plot(He_lobster, Ho_lobster, xlab="expected H", ylab="observed H", pch=21, bg="gray", xlim=c(0.32, 0.405), ylim=c(0.32, 0.405))
abline(0,1,1ty=3)
abline(lm(Ho_lobster~He_lobster), 1ty=2, lwd=1.25, col='red')
Fis <- apply(basic_lobster$Fis, MARGIN=2, FUN=mean, na.rm=TRUE)











