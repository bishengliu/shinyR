
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library("rCharts")
#install.packages("reshape2")
library(devtools)
#require(reshape2)
#install_github('rCharts', 'ramnathv')

shinyUI(
bootstrapPage(
	tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
	),
	navbarPage(title="", windowTitle ="VolcanoPlot",header="",
            tabPanel("Volcano Plot", 
               fluidRow(HTML('<div id="myModal" class="modal fade" role="dialog">
										  <div class="modal-dialog">

											<!-- Modal content-->
											<div class="modal-content">
											  <div class="modal-header">
												<button type="button" class="close" data-dismiss="modal">&times;</button>
												<h4 class="modal-title text-success">Generate subplots</h4>
												
												<div class=\'alert alert-info alert-dismissible\' role=\'alert\'>
												  <button type=\'button\' class=\'close\' data-dismiss=\'alert\' aria-label=\'Close\'><span aria-hidden=\'true\'>&times;</span></button>
													<p align=\'left\'><strong>Notice and howto: </strong><br /></p>
													<hr/> 
													<p align=\'left\'><span class=\"text-danger\">Step1: </span>select gene ID; <span class=\"text-danger\"><br/>Step2: </span> select conditions for group1 and group2, multiple selections allowed; 
													  <span class=\"text-danger\"><br/>Step3: </span>click "Generate Plot" button and change the conditions as necessary.<span class=\"text-danger\"><br/>Step4: </span>download plots and table data.</p>
												</div>
												
											  </div>
											<div class="modal-body">'),
								#uiOutput("geneId"),
								#textOutput("errtext"),							
								#textInput('SelectGeneId', "Current Gene ID", ""),
								uiOutput("NewGeneId"),
								HTML("<hr />"),
								#uiOutput("cond1"),
								#uiOutput("cond2"),
								#uiOutput("cond3"),
								#uiOutput("cond4"),
								fluidRow(
									column(6,
										HTML("<h5 class=\"text-success\">Group1</h5>"),
										uiOutput("group1")
									),
									column(6,
										HTML("<h5 class=\"text-success\">Group2</h5>"),
										uiOutput("group2")
									)
								),
								
								#uiOutput("group1"),
								#uiOutput("group2"),
								shiny::actionButton("refreshPlot2",label="Generate Plot",class='btn btn-primary'),
								
								HTML("<hr />"),
								HTML("<br />"),
								
								fluidRow(
									column(1, 
									HTML("<h4 class=\"text-success\">Plots</h4>")
									),
									column(3,
										uiOutput("downloadbox"),
										downloadButton('downloadboxPlot', 'Download Boxplot', class='btn btn-primary')
									),
									column(1),
									column(3,
										uiOutput("downloadg1"),
										downloadButton('downloadg1Plot', 'Download Group1', class='btn btn-primary')
									),
									column(1),
									column(3,
										uiOutput("downloadg2"),
										downloadButton('downloadg2Plot', 'Download Group2', class='btn btn-primary')
									)
								),
								fluidRow(
									column(4,
										plotOutput("boxplot")
										#uiOutput("downloadbox"),
										#downloadButton('downloadboxPlot', 'Download Boxplot', class='btn btn-primary')
									),

									column(4,
										plotOutput("barplot1")
										#uiOutput("downloadg1"),
										#downloadButton('downloadg1Plot', 'Download Group1', class='btn btn-primary')
									),
									column(4,
										plotOutput("barplot2")
										#uiOutput("downloadg2"),
										#downloadButton('downloadg2Plot', 'Download Group2', class='btn btn-primary')
									)
								),
								HTML("<hr />"),
								fluidRow(
									column(2,
										HTML("<h4 class=\"text-success\">Label Table</h4>")
									),
									column(10,
										uiOutput("downloadlt"),
										downloadButton('downloadltPlot', 'Download Table', class='btn btn-primary')
									)
								),
								#HTML("<h4 class=\"text-success\">Label Table</h4>"),
								#uiOutput("downloadglt"),
								#downloadButton('downloadgltPlot', 'Download Table', class='btn btn-primary'),
								HTML("<br />"),
								dataTableOutput("conditiontable"),

								#
								HTML('<p></p>
											  
											  </div>
											  <div class="modal-footer">
												<button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
											  </div>
											</div>
										  </div>
										</div>')
							),
				fluidRow(
					column(2,
						wellPanel(
									HTML("<h5 class=\"text-success\"><span class=\"text-danger\"><strong>Step 1: </strong></span>Upload File</h5>"),
									HTML("<hr>"),
									## upload file
									fileInput('infile', '',
											  accept = c('.tsv', '.txt')
									),
									HTML("<div>
										<h5 class=\"text-info\">Help on Input File! </h5>
										<ul>
											<li class=\"text-info\">Your input file must be Tab separated</li>
											<li class=\"text-info\">Your input file must have a header</li>
											<li class=\"text-info\">Make sure your input file data items are NOT wrapped with qoutes</li>
										</ul>
									</div>"),
									HTML("<hr>")
									),
						wellPanel(
									HTML("<h5 class=\"text-success\"><span class=\"text-danger\"><strong>Step 2: </strong></span>Set the P-value and the Fold Change</h5>"),
									HTML("<hr />"),
									uiOutput("pvCol"),
									HTML("<br />"),
									
									uiOutput("pvselection"),
									HTML("<hr />"),
									
									uiOutput("fcCol"),
									HTML("<br />"),
									uiOutput("fcselection")
									#tags$p("Additional columns for table output:"),

									)
						#wellPanel(
									#HTML("<h4 class=\"text-success\">Download Plots</h4>"),
									#HTML("<hr>"),
									#uiOutput("downloadvc"),
									#downloadButton('downloadvcPlot', 'Download', class='btn btn-primary'),
									#HTML("<hr>"),
									#HTML("<h4>Download ReactivePlot</h4>"),
									#uiOutput("downloadrc"),
									#downloadButton('downloadrcPlot', 'Download', class='btn btn-primary')
									#HTML("<hr>"),
									#HTML("<h4>Download Table</h4>"),
									#uiOutput("downloadtb"),
									#downloadButton('downloadtbPlot', 'Download', class='btn btn-primary')
									#)

					),
					column(8,
					    fluidRow(
                          HTML("<div class='alert alert-info alert-dismissible' role='alert'>
                              <button type='button' class='close' data-dismiss='alert' aria-label='Close'><span aria-hidden='true'>&times;</span></button>
                                <p align='left'><strong>Notice and howto: </strong><br /></p>
								<hr/> 
								<p align='left'><span class=\"text-danger\">Step1: </span>upload your data file ( must be a tsv file ) on the left side panel; <span class=\"text-danger\"><br/>Step2: </span> once completed, choose the 
                                  columns for the p-vale and the log fold change. Click on refresh button to generate the volcano plot and one table, adjust p-value and log fold change if necessary; <span class=\"text-danger\"><br/>Step3: </span>download the volcano plot;
								  <span class=\"text-danger\"><br/>Step4: </span>download table data, choose additionnal collumns if necessary.</p>
								 <hr/> 
								
								 <p align='left'><span class=\"text-danger\">Generate subplots: </span>Click the \"Generate Plots\" button in the table to generate plots for each gene.</p>
                              </div>")
						),
						fluidRow(
                                 shiny::actionButton("refreshPlot",label="Refresh",class='btn btn-primary')
                        ),
						fluidRow(
							tags$hr(),
							column(6,
								plotOutput("volcano")
								#uiOutput("downloadvc")
							),
							column(6,
								showOutput("rchart", "polycharts")
								#uiOutput("downloadrc")
							)
                        ),
                      fluidRow(
							tags$hr(),
							dataTableOutput("table")
							#uiOutput("downloadtb")							
                        ),
						
					singleton(
					#don't put in the head, must be after the Modal
							tags$script(src = "plot.js"))
					),

					column(2,
						#wellPanel(
						#HTML("<h4 class=\"text-success\">Set the columns of P-value and Log FoldChange</h4>"),
                        #HTML("<hr />"),
                        #uiOutput("pvCol"),
						#HTML("<br />"),
						
                        #uiOutput("pvselection"),
						#HTML("<hr />"),
						
                        #uiOutput("fcCol"),
						#HTML("<br />"),
                        #uiOutput("fcselection")
						#tags$p("Additional columns for table output:"),

						#),
						wellPanel(
									HTML("<h5 class=\"text-success\"><span class=\"text-danger\"><strong>Step 3: </strong></span>Download Plots</h5>"),
									HTML("<hr>"),
									uiOutput("downloadvc"),
									downloadButton('downloadvcPlot', 'Download', class='btn btn-primary')
									#HTML("<hr>"),
									#uiOutput("downloadrc"),
									#downloadButton('downloadrcPlot', 'Download', class='btn btn-primary')
									#HTML("<hr>"),
									#HTML("<h4>Download Table</h4>"),
									#uiOutput("downloadtb"),
									#downloadButton('downloadtbPlot', 'Download', class='btn btn-primary')
									),
						wellPanel(
									HTML("<h5 class=\"text-success\"><span class=\"text-danger\"><strong>Step 4: </strong></span>Set the additional columns for the table output:</h5>"),
									HTML("<br />"),
									uiOutput("geneIdCol"),
									HTML("<br />"),
									
									uiOutput("geneNameCol"),
									HTML("<br />"),
									
									uiOutput("geneDesCol"),
									HTML("<hr>"),
									HTML("<h5 class=\"text-success\">Download Table</h5>"),
									#HTML("<hr>"),
									#HTML("<h4>Download Table</h4>"),
									uiOutput("downloadtb"),
									downloadButton('downloadtbPlot', 'Download', class='btn btn-primary')
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
