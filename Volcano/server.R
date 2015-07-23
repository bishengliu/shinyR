
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library("shiny")
library("rCharts")
# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 36MB.
options(shiny.maxRequestSize = 36*1024^2)

server <- function(input, output)
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
      plot(df[[1]], df[[2]], col=c("gray", "blue")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Foldchange vs p-Value")

      abline(h=pvselection, col="black", lty=2, lwd=1)


      abline(v=c(-fcselection,fcselection), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
      
    })
	####################################################################################
	#this part gives a problem
	#for reactive chart
		output$rchart <- renderChart({
	
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
		p1 <- rPlot(LogFC ~ NegLogPV, data=df1, type='point')
		#p1$save('p1.html', cdn=TRUE)
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
      #df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName, GeneDes)), 
      #                    DoublePostitionCol
      #))
      #subset(df, df$DoublePostitionCol=="TRUE", select = c(1, 2, 3))
      df[df$DoublePostitionCol=="TRUE",c(seq(1:(length(df)-1)))]
    
  })
  })
  }

