  
  
  
  
  
  
  ## downloads
  ## interactive plot

  output$downloadrcPlot <- downloadHandler(
    filename = function() {
      paste('interactiveplot-', Sys.Date(), '.',input$rcfiletype, sep="")
    },
    content <- function(file){
      if (input$rcfiletype == "png")
      {
		  png(file)
		  pvselection <- as.numeric(input$pvselection)
		  fcselection <- as.numeric(input$fcselection)
		  LogFC <- res[getFCCol()]
		  negLogPV <- -log10(res[getPVCol()])
		  GeneId <- (res[geneId()])
		  if(geneName() !=0){
		  GeneName <- (res[geneName()])}
		  if(geneDes() !=0){
		  GeneDes <- (res[geneDes()])
		  }
		  
		  #generate extra cloumn
		  DoublePostitionCol <- as.factor(((negLogPV >= pvselection & (abs(LogFC) >= fcselection))))
		  
		  #genrate datafram
		  if(geneName() ==0){
			if(geneDes() ==0){
				df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV)), 
							  DoublePostitionCol))
				colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "DoublePostitionCol")
			}
			else{
				df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneDes)), 
							  DoublePostitionCol))
				colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneDes","DoublePostitionCol")
			}
		  }else{
				if(geneDes() ==0){
					df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName)), 
								  DoublePostitionCol))
					colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneName","DoublePostitionCol")
				}
				else{
					df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName, GeneDes)), 
								  DoublePostitionCol))
					colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneName", "GeneDes","DoublePostitionCol")
				}
		  }
		  #print (df)
			df1 <- subset(df, DoublePostitionCol == "TRUE")
			#rPlot(LogFC ~ negLogPV, data=df1, color='GeneId', type='point')
			p1 <- rPlot(NegLogPV ~ LogFC, data=df1, type='point', color='GeneId')
			p1$guides(x = list(title="Log10 Fold Change", min = -getFCmax() - 1, max = getFCmax() + 1))
			p1$guides(y = list(title = "Negative Log10 p-value", max = getPVmax() + 1))
			p1$addParams(width = 900, height = 370, dom = 'rchart',
				title = "Interactive Plot")
			  
			dev.off() 
      }else if (input$rcfiletype == "jpeg"){
        jpeg(file, quality = 100)
        		pvselection <- as.numeric(input$pvselection)
      fcselection <- as.numeric(input$fcselection)
	  LogFC <- res[getFCCol()]
	  negLogPV <- -log10(res[getPVCol()])
	  GeneId <- (res[geneId()])
	  if(geneName() !=0){
	  GeneName <- (res[geneName()])}
	  if(geneDes() !=0){
	  GeneDes <- (res[geneDes()])
	  }
	  
	  #generate extra cloumn
      DoublePostitionCol <- as.factor(((negLogPV >= pvselection & (abs(LogFC) >= fcselection))))
      
      #genrate datafram
	  if(geneName() ==0){
		if(geneDes() ==0){
			df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV)), 
                          DoublePostitionCol))
			colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "DoublePostitionCol")
		}
		else{
			df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneDes)), 
                          DoublePostitionCol))
			colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneDes","DoublePostitionCol")
		}
	  }else{
			if(geneDes() ==0){
				df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName)), 
							  DoublePostitionCol))
				colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneName","DoublePostitionCol")
			}
			else{
				df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName, GeneDes)), 
							  DoublePostitionCol))
				colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneName", "GeneDes","DoublePostitionCol")
			}
	  }
	  #print (df)
		df1 <- subset(df, DoublePostitionCol == "TRUE")
		#rPlot(LogFC ~ negLogPV, data=df1, color='GeneId', type='point')
		p1 <- rPlot(NegLogPV ~ LogFC, data=df1, type='point', color='GeneId')
		p1$guides(x = list(title="Log10 Fold Change", min = -getFCmax() - 1, max = getFCmax() + 1))
		p1$guides(y = list(title = "Negative Log10 p-value", max = getPVmax() + 1))
		p1$addParams(width = 900, height = 370, dom = 'rchart',
			title = "Interactive Plot")
		#p1$save('p1.html', cdn=TRUE)
		return(p1)
        dev.off() 
      }else if (input$rcfiletype == "pdf"){
        pdf(file)
		pvselection <- as.numeric(input$pvselection)
      fcselection <- as.numeric(input$fcselection)
	  LogFC <- res[getFCCol()]
	  negLogPV <- -log10(res[getPVCol()])
	  GeneId <- (res[geneId()])
	  if(geneName() !=0){
	  GeneName <- (res[geneName()])}
	  if(geneDes() !=0){
	  GeneDes <- (res[geneDes()])
	  }
	  
	  #generate extra cloumn
      DoublePostitionCol <- as.factor(((negLogPV >= pvselection & (abs(LogFC) >= fcselection))))
      
      #genrate datafram
	  if(geneName() ==0){
		if(geneDes() ==0){
			df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV)), 
                          DoublePostitionCol))
			colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "DoublePostitionCol")
		}
		else{
			df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneDes)), 
                          DoublePostitionCol))
			colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneDes","DoublePostitionCol")
		}
	  }else{
			if(geneDes() ==0){
				df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName)), 
							  DoublePostitionCol))
				colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneName","DoublePostitionCol")
			}
			else{
				df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName, GeneDes)), 
							  DoublePostitionCol))
				colnames(df) <- c("GeneId", "LogFC", "NegLogPV", "GeneName", "GeneDes","DoublePostitionCol")
			}
	  }
	  #print (df)
		df1 <- subset(df, DoublePostitionCol == "TRUE")
		#rPlot(LogFC ~ negLogPV, data=df1, color='GeneId', type='point')
		p1 <- rPlot(NegLogPV ~ LogFC, data=df1, type='point', color='GeneId')
		p1$guides(x = list(title="Log10 Fold Change", min = -getFCmax() - 1, max = getFCmax() + 1))
		p1$guides(y = list(title = "Negative Log10 p-value", max = getPVmax() + 1))
		p1$addParams(width = 900, height = 370, dom = 'rchart',
			title = "Interactive Plot")
		#p1$save('p1.html', cdn=TRUE)
        dev.off()
      }else{
        
      }
    }
  )