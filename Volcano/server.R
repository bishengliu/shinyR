
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

server <- function(input, output)
{
  
  library("shiny")
  mydata <- read.delim("diffexp-myc.txt", sep="\t", header = T)
  #load log-foldchange and pvalue from the data.frame
  LogFC <- mydata$logFC_WB1PMyc_Ctrl
  pValue <- mydata$adj.P.Val_WB1PMyc_Ctrl
  
  #log10
  negLogPV <- -log10(pValue)
  
  
  
  
  output$volcano <- renderPlot(
    {
      Gene_id <- mydata[1]
      pvselection <- input$pvselection
      fcselection <- input$fcselection
      #left <- function(fcselection){
      #  0 - fcselection
      #}
      #left = -(fcselection)
      #right <- function(fcselection){
      #  fcselection
      #}
      
      
      #add PV-FALSE/TRUE row
      pvSelectionCol <- as.factor(negLogPV >= pvselection)
      fcSelectionCol <- as.factor((abs(LogFC) >= fcselection))
      bothPostitionCol <- as.factor(((negLogPV >= pvselection & (abs(LogFC) >= fcselection))))
      
      #genrate datafram
      df <- cbind(cbind(  data.frame(cbind(Gene_id, LogFC,negLogPV)), 
                          #pvSelectionCol, 
                          #fcSelectionCol,
                          bothPostitionCol
      ))
      
      
      plot(df$LogFC, df$negLogPV, col=c("gray", "blue")[df$bothPostitionCol], xlab="Log10 Fold Change", ylab="Negative Log10 p-value", main="Foldchange vs p-Value")
      #xyplot(df$negLogPV~df$LogFC, col=c("gray", "green")[df$bothPostitionCol])
      
      #add lines
      #negfcselection <- (0 - fcselection)
      if(fcselection ==2 ){
        abline(v=c(-2,2), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
        #text(2,0, "text")
      }
      if(fcselection ==4 ){
        abline(v=c(-4,4), col=c("red", "red"), lty=c(2,2), lwd=c(1,1))
        #text(4,0, "text")
      }
      abline(h=pvselection, col="black", lty=2, lwd=1)
      #text(0,pvselection, "text")

      
    })
  #get the value for table
  output$table = renderDataTable({
      Gene_id <- mydata[1]
      pvselection <- input$pvselection
      fcselection <- input$fcselection
      #left <- function(fcselection){
      #  0 - fcselection
      #}
      #left = -(fcselection)
      #right <- function(fcselection){
      #  fcselection
      #}
      
      
      #add PV-FALSE/TRUE row
      #pvSelectionCol <- as.factor(negLogPV >= pvselection)
      #fcSelectionCol <- as.factor((abs(LogFC) >= fcselection))
      bothPostitionCol <- as.factor(((negLogPV >= pvselection & (abs(LogFC) >= fcselection))))
      
      #genrate datafram
      df <- cbind(cbind(  data.frame(cbind(Gene_id,LogFC,negLogPV)), 
                          #pvSelectionCol, 
                          #fcSelectionCol,
                          bothPostitionCol
      ))
      #subset(df, df$bothPostitionCol=="TRUE", select = c(1, 2, 3))
      df[df$bothPostitionCol=="TRUE",c(1,2,3)]
    
  })
}