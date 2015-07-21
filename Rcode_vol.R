#install.packages("shiny")
#require("shiny", "lattice")
#install.packages("ggplot2")
#require(ggplot2)
library("shiny")
rm(list =ls())
#load the data
mydata <- read.delim("diffexp-myc.txt", sep="\t", header = T)
#load log-foldchange and pvalue from the data.frame
Gene_id <- mydata[1]
LogFC <- mydata$logFC_WB1PMyc_Ctrl
pValue <- mydata$adj.P.Val_WB1PMyc_Ctrl

#log10
negLogPV <- -log10(pValue)


pvselection <- 2.3
fcselection <- 2


#add PV-FALSE/TRUE row
pvSelectionCol <- as.factor(negLogPV > pvselection)
fcSelectionCol <- as.factor((LogFC > fcselection | LogFC < -fcselection))
bothPostitionCol <- as.factor(((negLogPV > pvselection & (LogFC > fcselection | LogFC <  -fcselection))))

#genrate datafram
df <- cbind(cbind(  data.frame(cbind(Gene_id,LogFC,negLogPV)), 
                    pvSelectionCol, 
                    fcSelectionCol,
                    bothPostitionCol
))

plot(df$LogFC, df$negLogPV, col=c("gray", "blue")[df$bothPostitionCol], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Foldchange vs p-Value")
#xyplot(df$negLogPV~df$LogFC, col=c("gray", "green")[df$bothPostitionCol])

#add lines
abline(v=c(-fcselection,fcselection), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
abline(h=pvselection, col="black", lty=2, lwd=1)

