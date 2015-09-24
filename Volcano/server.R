
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library("shiny")
library("shinyjs")
library("rCharts")


## source plot functions
#source("functions.R")
#require(reshape2)
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 36MB.
options(shiny.maxRequestSize = 36*1024^2)
#tags$head(HTML("<script type='text/javascript' src='js/plot.js'></script>"))
server <- function(input, output, session)
{

  ###################################################################################
  ## load starter rdata object for widgets and app
  ################################################################################### 
  ## create a Progress object
  progress <- shiny::Progress$new()
  progress$set(message = "Initializing ...", value = 0)
  on.exit(progress$close())
  
  ## create a closure to update progress
  updateProgress <- function(value = NULL, detail = NULL) {
    if (is.null(value)) {
      value <- progress$getValue()
      value <- value + (progress$getMax() - value) / 5
    }
    progress$set(value = value, detail = detail)
  }
  
  
  #empty data frame for the input file 
  res <- c()
  
  #create empty vector
  vectorGeneId <- c()


getColnames <- reactive({
    infl <- input$infile      
    if (is.null(infl))
    {
      return(c())
    }else{
      res <<- read.delim(infl$datapath,sep="\t",header= TRUE)
      ## update variables
      choices <- colnames(res)
      return(choices)
    }
  })

  #add "Exclude from output"
  getNewColnames <-reactive({
	infl <- input$infile      
    if (is.null(infl))
    {
      return(c())
    }else{

      ## update variables
      choices <- c("Exclude from output", colnames(res))
      return(choices)
    }
  
  })
  
  
#get p-value column
getPVCol <- reactive({
	input$pvCol
	#validate(
	#need(input$pvCol != "", "Please select the column for p-value")
	#need(class(res[input$pvCol])== numeric, "Please select the column for p-value")
	#)
	val <- isolate(input$pvCol)
	return (val)

})
#log Fold Change column
getFCCol <- reactive({
	input$fcCol
	val <- isolate(input$fcCol)
	return (val)
})

#get the gene ID col slider
geneId <- reactive({
	input$geneIdCol
	val <- isolate(input$geneIdCol)
	return (val)
})
#get the gene name col 
geneName <- reactive({
	input$geneNameCol
	if(input$geneNameCol !="Exclude from output"){
	val <- isolate(input$geneNameCol)
	return (val)
	}else{
	return(0)
	}
})

#get the gene Description col 
geneDes <- reactive({
	input$geneDesCol
	if(input$geneDesCol !="Exclude from output"){
	val <- isolate(input$geneDesCol)
	return (val)
	}else{
	return(0)
	}	
})

#get fcmax for slider
getFCmax <- reactive ({
  input$fcCol
  if(isolate(length(input$fcCol)) == 0)
  {
    return(5)
  }else{
    x <- which(colnames(res) == input$fcCol)
    
    if (is.numeric(res[,x]))
    {
      vals <- abs(res[,x])
      val <- ceiling(max(vals))
      return(val)      
    }else{
      return(5)
    }
  }
})

#get pvmax for slider
getPVmax <- reactive ({
  input$pvCol
  if(isolate(length(input$pvCol)) == 0)
  {
    return(5)
  }else{
    x <- which(colnames(res) == input$pvCol)
    
    if (is.numeric(res[,x]))
    {
      val <- ceiling(max(-log10(res[,x])))
      return(val)      
    }else{
      return(5)
    }
  }
})


#get the log p-value slider
getPV <- reactive({
	input$pvselection
	val <- isolate(input$pvselection)
	return (val)
})
#get the log fc slider
getFV <- reactive({
	input$pfcelection
	val <- isolate(input$fcselection)
	return (val)
})

#create the link for table
createLink <- function(val1, val2) {
  #sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
  #sprintf('<button onClick="showImg(%s)" class="btn btn-primary">Info</button>',val)
  #sprintf('<button onClick="showImg(%s)" type="button" class="btn btn-info" data-toggle="modal" data-target="#myModal">Info</button>',val)
  
  
  sprintf('<div class="row"><button type="button" id="%s"  name="getGeneId" value="%s" class="btn btn-default action-button btn btn-primary shiny-bound-input openDialog" data-toggle="modal" data-target="#myModal">Generate Plots</button></row>',val1, val2)
  #shiny::actionButton("refreshPlot",label="Refresh",class='btn btn-primary')
  #shiny::actionButton(val1, val2)
}
 
 ##uiOutPut
 ## p value col selector
  output$pvCol <- renderUI({
    selectInput("pvCol", label = "Select the p-value Column", choices = getColnames())
  })
   ## fold change col selector
  output$fcCol <- renderUI({
    selectInput("fcCol", label = "Select the log Fold Change Column", choices = getColnames())
  })
  ##set the p-Value
    output$pvselection <- renderUI({
    input$pvCol
    sliderInput("pvselection", label = "Select the log p-value ", min=0, max=getPVmax(), value = 0)
  })
  ##set the fold change
    output$fcselection <- renderUI({
    input$fcCol
    sliderInput("fcselection", label = "Select the log Fold Change Column", min=0, max=getFCmax(), value = 0)

  })
 
## gene id col
  output$geneIdCol <- renderUI({
    selectInput("geneIdCol", label = "Select Gene Id Column", choices = getColnames())
  })
  
  ## gene name
 output$geneNameCol <- renderUI({
    selectInput("geneNameCol", label = "Select Gene Name Column", choices = getNewColnames())
  })
  
  ## gene id col
output$geneDesCol <- renderUI({
    selectInput("geneDesCol", label = "Select Gene Description Column", choices = getNewColnames())
  })

output$group1 <- renderUI({
    selectInput("group1", label = "Select Multiple Conditions", choices = getNewColnames(), , multiple=TRUE)
  })
  
 output$group2 <- renderUI({
    selectInput("group2", label = "Select Multiple Conditions", choices = getNewColnames(), multiple=TRUE)
  })
  
#NewGeneId
CurrentId <- reactive({
	input$NewGeneId
	val <- isolate(input$NewGeneId)
	return (val)
	})

 
Group1 <- reactive({
	input$group1
	val <- (input$group1)
	return (val)
})
Group2 <- reactive({
	input$group2
	val <- (input$group2)
	return (val)
})

#download
output$downloadvc <- renderUI({
    selectInput("vcfiletype", label = "Download Volcano Plot", choices = c('png','jpeg','pdf'))
  })  
#output$downloadrc <- renderUI({
#    selectInput("rcfiletype", label = "Download Reactive Plot", choices = c('png','jpeg','pdf'))
#  })  
output$downloadtb <- renderUI({
    selectInput("tbfiletype", label = "", choices = c('tsv','csv'))
  }) 
  

output$downloadbox <- renderUI({
    selectInput("boxfiletype", label = "", choices = c('png','jpeg','pdf'))
  }) 
 output$downloadg1 <- renderUI({
    selectInput("g1filetype", label = "", choices = c('png','jpeg','pdf'))
  }) 
 output$downloadg2 <- renderUI({
    selectInput("g2filetype", label = "", choices = c('png','jpeg','pdf'))
  })
  output$downloadlt <- renderUI({
    selectInput("ltfiletype", label = "", choices = c('tsv','csv'))
  }) 

observeEvent(input$refreshPlot, {
    
  
	output$volcano <- renderPlot(
		{
		  #input$refreshPlot
		
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
		  if(pvselection == 0 && fcselection == 0){
			plot(df[[1]], df[[2]], col=c("#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)
		  }else{
			plot(df[[1]], df[[2]], col=c("#D9EDF7", "#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)
		  }
		  #plot(df[[1]], df[[2]], col=c("#D9EDF7", "#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)

		  abline(h=pvselection, col="red", lty=2, lwd=1)


		  abline(v=c(-fcselection,fcselection), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
		  
		})
	####################################################################################
	#
	#for reactive chart
	output$rchart <- renderChart2({
	
	#input$refreshPlot
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
		p1$addParams(width = 700, height = 370, dom = 'rchart',
			title = "Interactive Plot")
		#p1$save('p1.html', cdn=TRUE)
		p1$set(legendPosition = "none")
		return(p1)

	})
	
	
	##############################################################################################
  #for table
  output$table <- renderDataTable({
	  #input$refreshPlot
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
	  
	  #print(df[df$DoublePostitionCol=="TRUE", c(1)])
	  #print(subsetdf)
	  
	  #get the geneID
	  vectorGeneId <<- as.vector(df[df$DoublePostitionCol=="TRUE", c(1)])
	  #renderUI
	  #get new list of Gene ID for modal gene list
	  output$NewGeneId <- renderUI({
			selectInput("NewGeneId", label = "Select Gene ID", choices = vectorGeneId)
	 })
	  # add link to the table
	  df$Action <-  createLink(df$GeneId, df$GeneId)

      #df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName, GeneDes)), 
      #                    DoublePostitionCol
      #))
      #subset(df, df$DoublePostitionCol=="TRUE", select = c(1, 2, 3))
	  
      df[df$DoublePostitionCol=="TRUE",c(seq(1:(length(df)-2)), length(df))]
    
		
  }, escape = FALSE)
  })
  

observeEvent(input$refreshPlot2,{
		# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}
		
		output$boxplot <- renderPlot({
		colors <- c("red","blue")
		boxplot(group1data, group2data, names=c("Group1","Group2"), col=colors, main= CurrentId, ylab="Fold Change", las=1)
		
		})
		output$conditiontable <- renderDataTable({
		
		#df <- melt(data.frame(group1,group2))
		#df <- c(rbind(group1,group2))
		#print(df)
		#len <- 1:(length(group1) + length(group2) )
		#print(len)
		len <- c()
		if(length(group1) > length(group2)){
			len <- 1:length(group1)
			for (i in (length(group2)+1):length(group1)){
				group2[i] <- ""
			}
			
		}
		else if (length(group1) == length(group2)){
			len <- 1:length(group1)
		}
		else{
			len <- 1:length(group2)
			for (i in (length(group1)+1):length(group2)){
				group1[i] <- ""
			}
			}	
		df <- cbind(len,group1, group2)
		colnames(df) <- c("Labels", "Group1", "Group2")
		df
		}, escape = FALSE)
		
		output$barplot1 <- renderPlot({
		colors <- c(rep("red",length(group1)))
		barplot(group1data, names=1:length(group1), col=colors, main= "Group1", ylab="Fold Change", las=1)
		
		})
		output$barplot2 <- renderPlot({
		colors <- c(rep("blue",length(group2)))
		barplot(group2data, names=1:length(group2), col=colors, main= "Group2", ylab="Fold Change", las=1)
		
		})
		
  
})


  ## downloads in main page
  ## volcano plot

output$downloadvcPlot <- downloadHandler(
    filename = function() {
      paste('volcanoplot-', Sys.Date(), '.',input$vcfiletype, sep="")
    },
    content <- function(file){
      if (input$vcfiletype == "png")
      {
        png(file)
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
		 if(pvselection == 0 && fcselection == 0){
			plot(df[[1]], df[[2]], col=c("#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)
		  }else{
			plot(df[[1]], df[[2]], col=c("#D9EDF7", "#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)
		  }

		  abline(h=pvselection, col="red", lty=2, lwd=1)


		  abline(v=c(-fcselection,fcselection), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
		  
        dev.off() 
      }else if (input$vcfiletype == "jpeg"){
        jpeg(file, quality = 100)
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
		 if(pvselection == 0 && fcselection == 0){
			plot(df[[1]], df[[2]], col=c("#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)
		  }else{
			plot(df[[1]], df[[2]], col=c("#D9EDF7", "#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)
		  }

		  abline(h=pvselection, col="red", lty=2, lwd=1)


		  abline(v=c(-fcselection,fcselection), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
        dev.off() 
      }else if (input$vcfiletype == "pdf"){
        pdf(file)
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
		  if(pvselection == 0 && fcselection == 0){
			plot(df[[1]], df[[2]], col=c("#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)
		  }else{
			plot(df[[1]], df[[2]], col=c("#D9EDF7", "#C55279")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Volcano Plot", pch=19)
		  }

		  abline(h=pvselection, col="red", lty=2, lwd=1)


		  abline(v=c(-fcselection,fcselection), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
        dev.off()
      }else{
        
      }
    }
  )

  
  ## downloads
  ## table data

output$downloadtbPlot <- downloadHandler(
    filename = function() {
      paste('tableplot-', Sys.Date(), '.',input$tbfiletype, sep="")
    },
    content <- function(file){
      if (input$tbfiletype == "tsv")
      {
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
		  
		  
		  mt_mtx <- df[df$DoublePostitionCol=="TRUE",c(seq(1:(length(df)-2)), (length(df)-1))]
		  
		  write.table(mt_mtx, file, row.names = FALSE, sep="\t")

      }else if (input$tbfiletype == "csv"){
	  
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
		  
		  
		 mt_mtx <- df[df$DoublePostitionCol=="TRUE",c(seq(1:(length(df)-2)), (length(df)-1))]
		write.csv(mt_mtx, file, row.names = FALSE)

      }else{
        
      }
    }
  )
  
 
 
 
 
 
 #download plot in modal
output$downloadboxPlot <- downloadHandler(
    filename = function() {
      paste('boxplot-', Sys.Date(), '.',input$boxfiletype, sep="")
    },
    content <- function(file){
      if (input$vcfiletype == "png")
      {
        png(file)
				# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}
		

		colors <- c("red","blue")
		boxplot(group1data, group2data, names=c("Group1","Group2"), col=colors, main= CurrentId, ylab="Fold Change", las=1)
		  
        dev.off() 
      }else if (input$boxfiletype == "jpeg"){
        jpeg(file, quality = 100)
        		# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}
		
		colors <- c("red","blue")
		boxplot(group1data, group2data, names=c("Group1","Group2"), col=colors, main= CurrentId, ylab="Fold Change", las=1)
        dev.off() 
      }else if (input$boxfiletype == "pdf"){
        pdf(file)
				# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}
		
		colors <- c("red","blue")
		boxplot(group1data, group2data, names=c("Group1","Group2"), col=colors, main= CurrentId, ylab="Fold Change", las=1)
        dev.off()
      }else{
        
      }
    }
  )

output$downloadg1Plot <- downloadHandler(
    filename = function() {
      paste('barplotg1-', Sys.Date(), '.',input$g1filetype, sep="")
    },
    content <- function(file){
      if (input$g1filetype == "png")
      {
        png(file)
				# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}
		
		colors <- c(rep("red",length(group1)))
		barplot(group1data, names=1:length(group1), col=colors, main= "Group1", ylab="Fold Change", las=1)
		  
        dev.off() 
      }else if (input$g1filetype == "jpeg"){
        jpeg(file, quality = 100)
        		# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}
		
		colors <- c(rep("red",length(group1)))
		barplot(group1data, names=1:length(group1), col=colors, main= "Group1", ylab="Fold Change", las=1)
        dev.off() 
      }else if (input$g1filetype == "pdf"){
        pdf(file)
				# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}

		colors <- c(rep("red",length(group1)))
		barplot(group1data, names=1:length(group1), col=colors, main= "Group1", ylab="Fold Change", las=1)
        dev.off()
      }else{
        
      }
    }
  )
  
output$downloadg2Plot <- downloadHandler(
    filename = function() {
      paste('barplotg2-', Sys.Date(), '.',input$g2filetype, sep="")
    },
    content <- function(file){
      if (input$g2filetype == "png")
      {
        png(file)
		# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}
		
		colors <- c(rep("blue",length(group2)))
		barplot(group2data, names=1:length(group2), col=colors, main= "Group2", ylab="Fold Change", las=1)
		  
        dev.off() 
      }else if (input$g2filetype == "jpeg"){
        jpeg(file, quality = 100)
        		# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}
		
		colors <- c(rep("blue",length(group2)))
		barplot(group2data, names=1:length(group2), col=colors, main= "Group2", ylab="Fold Change", las=1)
        dev.off() 
      }else if (input$g2filetype == "pdf"){
        pdf(file)
				# get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		#get the data in group1
		group1data <- c()
		for (i in 1:length(group1)){
		group1data[i] <- res[group1[i]][geneIdIndex,]
		}
		#get the data in group2
		group2data <- c()
		for (i in 1:length(group2)){
		group2data[i] <- res[group2[i]][geneIdIndex,]
		}

		colors <- c(rep("blue",length(group2)))
		barplot(group2data, names=1:length(group2), col=colors, main= "Group2", ylab="Fold Change", las=1)
        dev.off()
      }else{
        
      }
    }
  )
  
   ## downloads
  ## table data
output$downloadltPlot <- downloadHandler(
    filename = function() {
      paste('labeltableplot-', Sys.Date(), '.',input$ltfiletype, sep="")
    },
    content <- function(file){
      if (input$ltfiletype == "tsv")
      {
	  # get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		
		len <- c()
		if(length(group1) > length(group2)){
			len <- 1:length(group1)
			for (i in (length(group2)+1):length(group1)){
				group2[i] <- ""
			}
			
		}
		else if (length(group1) == length(group2)){
			len <- 1:length(group1)
		}
		else{
			len <- 1:length(group2)
			for (i in (length(group1)+1):length(group2)){
				group1[i] <- ""
			}
			}	
		df <- cbind(len,group1, group2)
		colnames(df) <- c("Labels", "Group1", "Group2")

		write.table(df, file, row.names = FALSE, sep="\t")

      }else if (input$ltfiletype == "csv"){
	    # get all geneID
		geneId <- res[geneId()]
		#get index of gene id
		CurrentId <- CurrentId()

		#CurrentGeneId <- CurrentGeneId()
		#print(CurrentGeneId)
		geneIdIndex <- which(geneId == CurrentId)

		
		group1 <- Group1()
		group2 <- Group2()
		len <- c()
		if(length(group1) > length(group2)){
			len <- 1:length(group1)
			for (i in (length(group2)+1):length(group1)){
				group2[i] <- ""
			}
			
		}
		else if (length(group1) == length(group2)){
			len <- 1:length(group1)
		}
		else{
			len <- 1:length(group2)
			for (i in (length(group1)+1):length(group2)){
				group1[i] <- ""
			}
			}	
		df <- cbind(len,group1, group2)
		colnames(df) <- c("Labels", "Group1", "Group2")
		write.csv(df, file, row.names = FALSE)

      }else{
        
      }
    }
  )
  
}

  
 



  

	

  