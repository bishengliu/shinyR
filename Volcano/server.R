
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library("shiny")
library("shinyjs")
library("rCharts")
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
  
  
  sprintf('<div class="row"><button type="button" id="%s"  name="getGeneId" value="%s" class="btn btn-default action-button btn btn-primary shiny-bound-input openDialog" data-toggle="modal" data-target="#myModal">Generate Plot</button></row>',val1, val2)
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
  
 ## for modal selection box
  output$cond1 <- renderUI({
    selectInput("cond1", label = "Select Condition", choices = getNewColnames())
  })
    output$cond2 <- renderUI({
    selectInput("cond2", label = "Select Condition", choices = getNewColnames())
  })
    output$cond3 <- renderUI({
    selectInput("cond3", label = "Select Condition", choices = getNewColnames())
  })
    output$cond4 <- renderUI({
    selectInput("cond4", label = "Select Condition", choices = getNewColnames())
  })
  
#geneID
CurrentId <- reactive({
	input$SelectGeneId
	if(input$SelectGeneId !=""){
	val <- input$SelectGeneId
	return (val)
	}else{
	return(0)
	}
})
  
  #get cond1 
Cond1 <- reactive({
	input$cond1
	if(input$cond1 !="Exclude from output"){
	val <- isolate(input$cond1)
	return (val)
	}else{
	return(0)
	}
})
Cond2 <- reactive({
	input$cond2
	if(input$cond2 !="Exclude from output"){
	val <- isolate(input$cond2)
	return (val)
	}else{
	return(0)
	}
})
Cond3 <- reactive({
	input$cond3
	if(input$cond3 !="Exclude from output"){
	val <- isolate(input$cond3)
	return (val)
	}else{
	return(0)
	}
})
Cond4 <- reactive({
	input$cond4
	if(input$cond4 !="Exclude from output"){
	val <- isolate(input$cond4)
	return (val)
	}else{
	return(0)
	}
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
      plot(df[[1]], df[[2]], col=c("gray", "#DF6026")[df[[3]]], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Fold change vs p-Value", pch=19)

      abline(h=pvselection, col="black", lty=2, lwd=1)


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
		p1$guides(y = list(title = "Fold change vs p-Value", max = getPVmax() + 1))
		p1$addParams(width = 900, height = 370, dom = 'rchart',
			title = "Genes of Interest")
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
	  
	  #print(df[df$DoublePostitionCol=="TRUE", c(1)])
	  #print(subsetdf)
	  
	  #get the geneID
	  vectorGeneId <<- as.vector(df[df$DoublePostitionCol=="TRUE", c(1)])
	  #print(vectorGeneId)
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
		#get gene ID
		currentGeneId <- CurrentId()
		if(currentGeneId == 0){
			output$errtext = renderText({ 
			  "Current Gene ID is empty, please manually copy Gene ID!"
			})
		}
		print("currentID")
		print(currentGeneId)
		print("IDVector")
		print(res[geneId()])
		#put the gene id into a vector for indexing
		idVector <- as.vector(res[geneId()])
		#get index of gene id
		geneIdIndex <- match(currentGeneId, idVector)
		print("CurrentIndex")
		print(geneIdIndex)
		
		if(Cond1() !=0){
			cond1 <- as.vector(res[Cond1()])[geneIdIndex]}else{cond1 <- 0}
		if(Cond2() !=0){
			cond2 <- as.vector(res[Cond2()])[geneIdIndex]}else{cond2 <- 0}
		if(Cond3() !=0){
			cond3 <- as.vector(res[Cond3()])[geneIdIndex]}else{cond3 <- 0}
		if(Cond4() !=0){
			cond4 <- as.vector(res[Cond4()])[geneIdIndex]}else{cond4 <- 0}
			
			print("Conditions")
			print(cond1)
			print(cond2)
			print(cond3)
			print(cond4)
		
		output$boxplot <- renderPlot({
		})
  
})
	
	
	
	
  }

  

	

  