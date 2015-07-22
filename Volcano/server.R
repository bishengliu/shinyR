
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library("shiny")
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
      choices <<- colnames(res)
      return(choices)
    }
  })

#get p-value column
getPVCol <- reactive({
	input$pvCol
	validate(
	need(input$pvCol != "", "Please select the column for p-value")
	#need(class(res[input$pvCol])== numeric, "Please select the column for p-value")
	)
	val <- isolate(input$pvCol)
	return (val)

})
#log Fold Change column
getFCCol <- reactive({
	input$fcCol
	validate(
	need(input$fcCol != "", "Please select the column for fold change")
	#need(class(res[input$fcCol])== numeric, "Please select the column for fold change")
	)
	val <- isolate(input$fcCol)
	return (val)
})

#get the gene ID col slider
geneId <- reactive({
	input$geneIdCol
	val <- isolate(input$geneIdCol)
	return (val)
})
#get the gene ID col slider
geneName <- reactive({
	input$geneNameCol
	val <- isolate(input$geneNameCol)
	return (val)
})

#get the gene Description col slider
geneDes <- reactive({
	input$geneDesCol
	val <- isolate(input$geneDesCol)
	return (val)
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
    sliderInput("pvselection", label = "Select the log p-value ", min=min(res[getPVCol()]), max=max(res[getPVCol()]), value = 0)
  })
  ##set the fold change
     output$fcselection <- renderUI({
    sliderInput("fcselection", label = "Select the log Fold Change Column", min=min(res[getFCCol()]), max=max(res[getFCCol()]), value = 0)
  })
 
## gene id col
  output$geneIdCol <- renderUI({
    selectInput("geneIdCol", label = "Select Gene Id Column", choices = getColnames())
  })
  
  ## gene name
  output$geneNameCol <- renderUI({
    selectInput("geneNameCol", label = "Select Gene Name Column", choices = getColnames())
  })
  
  ## gene id col
  output$geneDesCol <- renderUI({
    selectInput("geneDesCol", label = "Select Gene Description Column", choices = getColnames())
  })
  
  
  
  output$volcano <- renderPlot(
    {
	  input$refreshPlot
	
      pvselection <- as.numeric(input$pvselection)
      fcselection <- as.numeric(input$fcselection)
	  LogFC <- res[getFCCol()]
	  negLogPV <- -log10(res[getPVCol()])
	  GeneId <- (res[geneId()])
	  GeneName <- (res[geneName()])
	  GeneDes <- (res[geneDes()])

      #generate extra cloumn
      DoublePostitionCol <- as.factor(((negLogPV >= pvselection & (abs(LogFC) >= fcselection))))
      
      #genrate datafram
      df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName, GeneDes)), 
                          DoublePostitionCol
      ))
      print(df)
      
      plot(df$LogFC, df$negLogPV, col=c("gray", "blue")[df$DoublePostitionCol], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Foldchange vs p-Value")

      abline(h=pvselection, col="black", lty=2, lwd=1)


      abline(v=c(-fcselection,fcselection), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
      
    })
  #get the value for table
  output$table = renderTable({
	  input$refreshPlot
      pvselection <- as.numeric(input$pvselection)
      fcselection <- as.numeric(input$fcselection)
	  LogFC <- res[getFCCol()]
	  negLogPV <- -log10(res[getPVCol()])
	  GeneId <- (res[geneId()])
	  GeneName <- (res[geneName()])
	  GeneDes <- (res[geneDes()])

      #generate extra cloumn
      DoublePostitionCol <- as.factor(((negLogPV >= pvselection & (abs(LogFC) >= fcselection))))
      
      #genrate datafram
      df <- cbind(cbind(  data.frame(cbind(GeneId, LogFC,negLogPV, GeneName, GeneDes)), 
                          DoublePostitionCol
      ))
      #subset(df, df$DoublePostitionCol=="TRUE", select = c(1, 2, 3))
      df[df$DoublePostitionCol=="TRUE",c(1,2,3,4,5)]
    
  })
}
