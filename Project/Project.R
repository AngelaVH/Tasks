data1<-read.csv("/Users/angelavanhorn/Desktop/Evolution/Tasks/Project/Emergence-1.csv")
data2<-read.csv("/Users/angelavanhorn/Desktop/Evolution/Tasks/Project/LifespanDevelopment-1.csv")
shortlife<-("/Users/angelavanhorn/Desktop/Evolution/Tasks/Project/ShortLife.csv")
longlife<-("/Users/angelavanhorn/Desktop/Evolution/Tasks/Project/LongLife.csv")

pdf("ShortLifeLifespanandDevelopment.pdf")
plot(shortlife[[Development]], shortlife[[Lifespan]], xlab="Development Time", ylab="Lifespan")
title('Development vs. Lifespan (Shortlife)')
dev.off()
cor.test(shortlife[[Development]], shortlife[[Lifespan]], method=c("pearson", "kendall", "spearman"))
View(shortlife)



