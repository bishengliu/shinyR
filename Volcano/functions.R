#generate volcano plot
GenerateVolcano =  function(
		pvselection, fcselection, LogFC, negLogPV, DoublePostitionCol
)
{

		  pvselection <- as.numeric(input$pvselection)
		  fcselection <- as.numeric(input$fcselection)
		  LogFC <- res[getFCCol()]
		  negLogPV <- -log10(res[getPVCol()])


		  #generate extra cloumn
		  DoublePostitionCol <- as.factor(((negLogPV >= pvselection & (abs(LogFC) >= fcselection))))
		  
		  #genrate datafram
		  df <- cbind(cbind(  data.frame(cbind(LogFC,negLogPV)), 
							  DoublePostitionCol
		  ))  
		  plot(df[[1]], df[[2]], col=c("#D9EDF7", "#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)

		  abline(h=pvselection, col="red", lty=2, lwd=1)


		  abline(v=c(-fcselection,fcselection), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))

}