
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library("rCharts")
#install.packages("devtools")
library(devtools)
#install_github('rCharts', 'ramnathv')

shinyUI(
bootstrapPage(
  
	navbarPage(title="", windowTitle ="VolcanoPlot",header="",
            tabPanel("Volcano Plot", 
               
				fluidRow(
					column(2,
						wellPanel(
									HTML("<h4>Upload File</h4>"),
									HTML("<hr>"),
									## upload file
									fileInput('infile', '',
											  accept = c('.tsv', '.txt')
									),
									HTML("<div>
										<h4>Help on Input File! </h4>
										<ul>
											<li>Your input file must be Tab separated</li>
											<li>Your input file must have a header</li>
											<li>Make sure your input file data items are NOT wrapped with qoutes</li>
										</ul>
									</div>")
						)
					),
					column(8,
					    fluidRow(
                          HTML("<div class='alert alert-warning alert-dismissible' role='alert'>
                              <button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
                                <p align='center'><strong>Notice!</strong> some text here</p>
                              </div>")
						),
						fluidRow(
                                 shiny::actionButton("refreshPlot",label="Refresh",class='btn btn-primary')
                        ),
						fluidRow(
							column(6,
								plotOutput("volcano")
							),
							column(6,
								showOutput("rchart", "polycharts")
							#	#plotOutput("rchart")
							)
                        ),
                      fluidRow(
							dataTableOutput("table")	
                        )
					),
					column(2,
						wellPanel(
							
                        
                        uiOutput("pvCol"),
						HTML("<br />"),
						
                        uiOutput("pvselection"),
						HTML("<hr />"),
						
                        uiOutput("fcCol"),
						HTML("<br />"),
                        uiOutput("fcselection"),
						HTML("<hr />"),
						tags$p("Addition columns for table output:"),
						HTML("<br />"),
						
                        uiOutput("geneIdCol"),
						HTML("<br />"),
						
                        uiOutput("geneNameCol"),
						HTML("<br />"),
						
                        uiOutput("geneDesCol")
						)
					
					)
				)
            ),
            tabPanel("Documentation",
				fluidRow(
					column(3),
					    column(6, HTML("<p>Documentation</p>")),
					column(3)
				)
			)
             
	)
)
)
