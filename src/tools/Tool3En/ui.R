##
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(shinythemes)
library(gridExtra)
library(extrafont)
library(shinyBS)
library(xlsx)

###### UI start

tt <- function(param, txt="[0 &lt; valid &le; 100]") {
  return(bsTooltip(param, txt, "right", options = list(container = "body")))
}

inputWidth <- "110px"

source("langParams.R")

shinyUI(bootstrapPage(
  
  theme = shinytheme("flatly"),
  
  titlePanel(gett("main.title.t3")),
  
  ###### data input tabs
  
  
  sidebarLayout(
    
    sidebarPanel(
      h4(gett("input.1")),
      br(),
      
      numericInput("oel", gett("input.2"), 100, width=inputWidth) ,
      tt("oel", gett("input.2.tooltip")),
      
      numericInput("al", gett("input.3"), 1, width=inputWidth),
      tt("al", gett("input.3.tooltip")),
      
      numericInput("target_perc", gett("input.3.1"), 95, 0, 100, width=inputWidth),
      tt("target_perc",gett("frac.1.tooltip")),
      
      numericInput("frac_threshold", gett("input.3.2"), 5, width=inputWidth),
      tt("frac_threshold",gett("perc.1.tooltip")),
      
      numericInput("conf", gett("input.4"), 90, width=inputWidth),
      tt("conf", gett("input.4.tooltip")),
      
      numericInput("psi", gett("input.5"), 30, width=inputWidth),
      tt("psi", gett("input.5.tooltip")),
      
      br(),
      h4(gett("input.6")),
      bsTooltip("Data", gett("input.6.tooltip"),
                "right", options = list(container = "body")),
      br(),
      fileInput('file1', gett("input.6.1"),
                accept = c(".xlsx")),
      strong(gett("input.6.2")),
      downloadButton('downloadData', gett("input.6.3")),
      tt("downloadData", gett("input.6.4")),
      br(),
      br(),
      uiOutput("Box.listofvars")
      #p(gett("input.6.4"))
, width = 3),
    
    mainPanel(
      
      ####waiting message
      
      singleton(tags$head(tags$script(src = "textarea.js"))),
      singleton(tags$head(tags$script(src = "google-analytics.js"))),
      tags$head(tags$style(type="text/css", "
                           #waitmessage {
                           position: fixed;
                           top: 0px;
                           left: 0px;
                           width: 100%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
                           }
              #probSituUnacceptable, [for='probSituUnacceptable'] {display: none;}
              input[type='number'] {
    -moz-appearance:textfield;
                           }
                           
                           input::-webkit-outer-spin-button,
                           input::-webkit-inner-spin-button {
                           -webkit-appearance: none;
                           }
                           ")),
      
      
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div(gett("input.8"),id="waitmessage")),
      tabsetPanel( 
        
        #
        #
        #
        #
        #
        ############GLOBAL ANALYSIS PANEL
        #
        #
        #
        #
        #
        tabPanel(gett("global.tab.name"),
          h3(gett("global.title")),
          conditionalPanel("output.fileNotUploaded",
            br(),
            br(),
            strong(span(h4(gett("data.upload")),style="color:#5dade2"))
          ),
          conditionalPanel("output.fileUploaded", 
            tabsetPanel(
            #######################GLOBAL DESCRIPTIVE
              tabPanel(gett("descriptive.tab.name"),
                h3(gett("descriptive.title")),
                tableOutput('res.desc'),
                h4(gett("descriptive.1")),
                p(htmlOutput("descriptive.2")),
                br(),
                h3(gett("descriptive.3")),
                plotOutput('qqplot'),
                br(),
                p(gett("descriptive.4")),
                ##### NEW FOR BOXPLOT
                br(),
                h3(gett("descriptive.5")),
                plotOutput('boxplot'),
                p(gett("descriptive.6")),
                br()
                
                ####END OF NEW FOR BOXPLOT
              ),
              #######################GLOBAL - RISK
                   
              tabPanel(gett("descriptive.tab2.name"),
                br(),
                p(gett("descriptive.subtab.name.2.text")),
                tabsetPanel(
                  ##########GLOBAL - RISK - EXCEEDANCE
                  tabPanel(gett("frac.tab.name"),
                    h3(gett("frac.title")),
                    br(),
                    div(style="block",class="plot-with-text",
                        div(style="display: inline-block;width:55%",class="text",
                            h4(strong(gett("frac.7"))),
                            p("\u25B9", gett("frac.8")),
                            p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("frac.9") ,"\u2265 ", textOutput("acceptableExpo1", inline=TRUE))),
                            p("\u25B9", gett("frac.10")),
                            p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",strong(textOutput("probrisk",inline=TRUE))),
                            p("\u25B9", gett("frac.11")),
                            p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong("<", textOutput("probSituUnacceptable1",inline=TRUE))),
                            p("\u25B9", gett("frac.12")),
                            p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk",inline=TRUE)))
                        ),
                        div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.1"))
                    ),
                    
                    br(),
                    
                    
                    #parameter estimates distribution
                    h4(strong(gett("frac.4"))),
                    
                    p(htmlOutput("gm1.dist", inline=TRUE), strong(textOutput("gm1",inline=TRUE))),
                    p(htmlOutput("gsd1.dist", inline=TRUE), strong(textOutput("gsd1",inline=TRUE))),
                                                                   
                    #parameter estimates exceedance
                    br(),
                    h4(strong( gett("frac.5"))),
                    p(gett("frac.6"),strong(textOutput("Frac",inline=TRUE))),
                    p(gett("frac.6.1"), strong(textOutput("Frac.ci",inline=TRUE))),
                    
                    br(),
                                       
                                       #####illustrative graphs
                                       
                    h4(strong(gett("frac.graph.1"))),
                   
                   #sequential
                   
                    p(strong(gett("frac.graph.4"))),
                   
                    p(gett("frac.graph.5")),
                   
                    p(gett("frac.graph.6")),
                    plotOutput("seqplot.frac"),
                   
                    br(),
                   
                   #risk band
                   
                    p(strong(gett("frac.graph.11"))),
                   
                   
                    p(gett("frac.graph.12.1"), textOutput("acceptableExpoDiv10_1", inline=TRUE),
                     gett("frac.graph.12.2"), textOutput("acceptableExpoDiv10_2", inline=TRUE), 
                     gett("frac.graph.12.3"), textOutput("percentile2", inline=TRUE),
                     gett("frac.graph.12.4"), textOutput("percentile3", inline=TRUE), gett("frac.graph.12.5")),
                    plotOutput("riskband.frac")
                  ),
                              ##########GLOBAL - RISK - 95th percentile
                              
                  tabPanel(htmlOutput("percentile4", inline=TRUE),
                    h3(gett("perc.title"), textOutput("percentile5", inline=TRUE)),
                    br(),
                    div(style="block",class="plot-with-text",
                        div(style="display: inline-block;width:55%",class="text",
                            h4(strong(gett("frac.7"))),
                            p("\u25B9", gett("frac.8")),
                            p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("percentile8", inline=TRUE), "\u2265 ",  gett("OEL"))),
                            p("\u25B9", gett("frac.10")),
                            p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("probrisk.perc",inline=TRUE))),
                            p("\u25B9", gett("frac.11")),
                            p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong("<", textOutput("probSituUnacceptable2",inline=TRUE))),
                            p("\u25B9", gett("frac.12")),
                            p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.perc",inline=TRUE)))
                        ),
                        div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.2"))
                    ),
                     
                    
                    
                    br(),
                     
                     #parameter estimates - distribution
                     
                     h4(strong(gett("frac.4"))),
                     
                    p(htmlOutput("gm1.perc", inline=TRUE), strong(textOutput("gm2", inline=TRUE))),
                    p(htmlOutput("gsd1.perc", inline=TRUE), strong(textOutput("gsd2",inline=TRUE))),
                     
                     br(),
                    
                    #parameter estimates - centile
                    
                     h4(strong(gett("perc.5"), textOutput("percentile6", inline=TRUE))),
                     p(gett("frac.6"),strong(textOutput("Perc",inline=TRUE))),
                     
                     p(gett("frac.6.1"), strong(textOutput("Perc.ci",inline=TRUE))),
                     
                     
                     br(),
                     
                     ###graphs
                     
                     h4(strong(gett("frac.graph.1"))),
                     
                     ##sequential plot
                     
                     p(strong(gett("frac.graph.4"))),
                     
                     p(gett("perc.graph.5")),
                    
                     p(gett("frac.graph.6")),
                     plotOutput("seqplot.perc"),
                     
                     
                     #risk plot
                     
                     br(),
                     p(strong(gett("frac.graph.11"))),
                     
                     p(gett("perc.graph.12.1"), textOutput("percentile10", inline=TRUE), htmlOutput("perc.graph.12.2.2", inline=TRUE)),
                     plotOutput("riskband.perc")
                  ),
                              
                  ##########GLOBAL - RISK - Arithmetic mean
                  tabPanel(gett("am.tab.name"),
                     h3(gett("am.title")),
                     br(),
                     div(style="block",class="plot-with-text",
                         div(style="display: inline-block;width:55%",class="text",
                             h4(strong(gett("frac.7"))),
                             p("\u25B9", gett("am.8")),
                             p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("AM"), "\u2265 ",  gett("OEL"))),
                             p("\u25B9", gett("frac.10")),
                             p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",strong(textOutput("probrisk.AM",inline=TRUE))),
                             p("\u25B9", gett("frac.11")),
                             p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong("<", textOutput("probSituUnacceptable3",inline=TRUE))),
                             p("\u25B9", gett("frac.12")),
                             p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("finalrisk.AM",inline=TRUE)))
                         ),
                         div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.3"))
                     ),
                     p(strong(gett("am.13")),gett("am.13.1")),
                     br(),
                     
                     #distrbution
                     h4(strong(gett("frac.4"))),
                     
                     p(htmlOutput("gm1.AM", inline=TRUE), strong(textOutput("gm3", inline=TRUE))),
                     p(htmlOutput("gsd1.AM", inline=TRUE), strong(textOutput("gsd3",inline=TRUE))),
                    
                     br(),
                     br(),
                     #parameters
                     h4(strong(gett("am.5"))),
                     
                     p(gett("frac.6"),strong(textOutput("AM",inline=TRUE))),
                     
                     p(gett("frac.6.1"),strong(textOutput("AM.ci",inline=TRUE))),
                     

                     ##risk decision
                     
                     br(),
                     
                     ##illustrative graphs
                     
                     h4(strong(gett("frac.graph.1"))),
                     
                    #sequential ^plot
                     
                     p(strong(gett("frac.graph.4"))),
                     p(gett("am.graph.5")),
                    
                     p(gett("frac.graph.6")),
                    
                     plotOutput("seqplot.AM"),
                     br(),
                    
                     #risk band plot
                    
                     p(strong(gett("frac.graph.11"))),
                    
                     p(htmlOutput("am.graph.12")),
                     plotOutput("riskband.am")
                  )
                ###enf of tabset panel level2 
                )
              ),
              ##wnd of tabsetpanel l1vel1
              br()
              )
            )
        ),
        
       #
       #
       #
       #
       ############COMPARATIVE
       #
       #
       #
       #
       
       
        
        tabPanel(gett("comp.tab.name"),
          h3(gett("comp.title")),
          conditionalPanel("output.fileNotUploaded",
            br(),
            br(),
            strong(span(h4(gett("data.upload")),style="color:#5dade2"))
          ),
                 
          conditionalPanel("output.fileUploaded", 
            tabsetPanel(
              #######################COMPARATIVE - ALL
              tabPanel(gett("comp.all.tab.name"),
                br(),
                p(gett("comp.1")),
                br(),
                
                #box whisker plot
                
                h4(gett("comp.2")),
                p(htmlOutput("comp.3")),
                br(),
                plotOutput('categories.boxplot'),
                br(),
                
                ##comp stats
                
                h4(gett("comp.4")),
                br(),
                p(gett("comp.5")),
                br(),
                tableOutput('table.global.dist'),
                p(gett("comp.5.2")),
                br(),
                h4(gett("gauge.t3.title")),
                br(),
                p(gett("gauge.t3")),
                plotOutput("gauge.t3"),
                h4(gett("riskband.t3.title")),
                br(),
                p(gett("riskband.t3")),
                plotOutput("riskband.t3")
              ),
              #######################COMPARATIVE - 2 categories
              tabPanel(gett("comp.2.tab.name"),
                br(),
                p(htmlOutput("comp.6")),
                br(),
                uiOutput("box.cat1"),
                uiOutput("box.cat2"),
                
                h3(gett("comp.7")),
                br(),
                p(htmlOutput("comp.8")),
                plotOutput("graph.2cat.whisk"),
                
               ###Numerical analysis
                
                ### distributional parameters
                
                
                h3(gett("comp.9")),
                
                br(),
                h4(gett("comp.12")),
               
                #html input for the expected ratio, to get label and value on same line
                htmlOutput("comp.11"),
                br(),
                br(),
                tableOutput("table.2cat.dist"),
                em(gett("comp.13")),
                br(),
                br(),
                h3(gett("comp.14")),
                br(),
                
                ###risk metrics
                
                h4(gett("comp.16"), textOutput("percentile20", inline=TRUE), gett("comp.17")),
                #html input for the expected ratio, to get label and value on same line
                htmlOutput("comp.15"),
                br(),
                br(),
                
                
                tableOutput("table.2cat.metrics1"),
                
                em(gett("comp.13")),
                
                br(),
                br(),
                
                #html input for the expected ratio, to get label and value on same line
                
                h4(gett("comp.19")),
                htmlOutput("comp.18"),
                br(),
                br(),
                tableOutput("table.2cat.metrics2"),
                em(gett("comp.13.2")),
                br(),
                br(),
                
                #####proba of an unacceptable situation
                
                h3(gett("comp.20")),
                p(gett("comp.21"), textOutput("percentile21", inline=TRUE), gett("comp.22")),
                
                tags$ol(tags$li(gett("comp.23"), "\u2265", textOutput("acceptableExpo3", inline=TRUE)),
                        tags$li(textOutput("percentile23", inline=TRUE), gett("comp.24"), textOutput("percentile24", inline=TRUE), "\u2265", gett("OEL")),
                        tags$li(gett("comp.26"), "\u2265", gett("OEL"))),
                p(gett("comp.27"), textOutput("probSituUnacceptable6", inline=TRUE), gett("comp.28")),
                br(),
                tableOutput('table.2cat.risk')
                          ##closing tabpanel level2
              )
        #closing tabset panel level2         
            )
   
        
        #closing conitional panel
          )
                      
#closing tabpanel level1                   
        ),
        
       #
       #
       #
       ############SINGLE CATEGORY
       #
       #
       #
       #
       
      tabPanel(gett("single.tab.name"),
        h3(gett("single.title")),
        conditionalPanel("output.fileNotUploaded",
          br(),
          br(),
          strong(span(h4(gett("data.upload")),style="color:#5dade2"))
        ),
        conditionalPanel("output.fileUploaded",
          br(),
          p(gett("single.1")),
          uiOutput("box.cat.single"),
                ############################SINGLE CATEGORY - DESCRIPTIVE
                
          tabsetPanel(
            tabPanel(gett("descriptive.tab.name"), 
              h3(gett("descriptive.title")),
              tableOutput('res.desc.single'),
              h4(gett("descriptive.1")),
              p(htmlOutput("descriptive.2.1")),
             
              br(),
              h3(gett("descriptive.3")),
              plotOutput('qqplot.single'),
              br(),
              p(gett("descriptive.4")),
              br()
            ),
            ######################### SINGLE CATEGORY - EXCEEDANCE
                  
            tabPanel(gett("frac.tab.name"),
              h3(gett("frac.title")),
              br(),
              div(style="block",class="plot-with-text",
                  div(style="display: inline-block;width:55%",class="text",
                      h4(strong(gett("frac.7"))),
                      p("\u25B9", gett("frac.8")),
                      p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("frac.9"), "\u2265", textOutput("acceptableExpo4", inline=TRUE))),
                      p("\u25B9", gett("frac.10")),
                      p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",strong(textOutput("probrisk.single",inline=TRUE))),
                      p("\u25B9", gett("frac.11")),
                      p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong("<", textOutput("probSituUnacceptable7",inline=TRUE))),
                      p("\u25B9", gett("frac.12")),
                      p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",strong(textOutput("finalrisk.single",inline=TRUE)))
                  ),
                  div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.1.single"))
              ),
              
              br(),
               
               ##param estimates distribution
               
               h4(strong(gett("frac.4"))),
               
              
              p(htmlOutput("gm1.frac", inline=TRUE), strong(textOutput("gm1.single",inline=TRUE))),
              p(htmlOutput("gsd1.frac", inline=TRUE), strong(textOutput("gsd1.single",inline=TRUE))),
              
               br(),
               
               ##param estimate exceedance
               
               h4(strong(gett("frac.5"))),
               
               p(gett("frac.6"),strong(textOutput("Frac.single",inline=TRUE))),
               
               p(gett("frac.6.1"),strong(textOutput("Frac.ci.single",inline=TRUE))),
               

               br(),
    
              ### graphs  
    
               h4(strong(gett("frac.graph.1"))),
    
    
              ## seq plots
    
               p(strong(gett("frac.graph.4"))),
    
               p(gett("frac.graph.5")),
               p(gett("frac.graph.6")),
               plotOutput("seqplot.frac.single"),
               br(),
    
              #risk plot
    
               p(strong(gett("frac.graph.11"))),
    
               p(gett("frac.graph.12.1"), textOutput("acceptableExpoDiv10_3", inline=TRUE), gett("frac.graph.12.2"), textOutput("acceptableExpoDiv10_4", inline=TRUE), gett("frac.graph.12.3"), textOutput("percentile26", inline=TRUE), gett("frac.graph.12.4"), textOutput("percentile27", inline=TRUE), gett("frac.graph.12.5")),
               plotOutput("riskband.frac.single"),
               br()
              ),
              ######################### SINGLE CATEGORY - pPerc
                  
              tabPanel(textOutput("percentile28", inline=TRUE),
                h3(gett("perc.title"), textOutput("percentile29", inline=TRUE)),
                br(),
                div(style="block",class="plot-with-text",
                    div(style="display: inline-block;width:55%",class="text",
                        h4(strong(gett("frac.7"))),
                        p("\u25B9", gett("frac.8")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(textOutput("percentile32", inline=TRUE), "\u2265 ",  gett("OEL"))),
                        p("\u25B9", gett("frac.10")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",strong(textOutput("probrisk.perc.single",inline=TRUE))),
                        p("\u25B9", gett("frac.11")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong("<", textOutput("probSituUnacceptable8",inline=TRUE))),
                        p("\u25B9", gett("frac.12")),
                        p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",strong(textOutput("finalrisk.perc.single",inline=TRUE)))
                    ),
                    div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.2.single"))
                ),
                
                br(),
                
                 
                 ##param estimates distribution
                 
                 h4(strong(gett("frac.4"))),
                 
                p(htmlOutput("gm1.perc2", inline=TRUE), strong(textOutput("gm1.single2",inline=TRUE))),
                p(htmlOutput("gsd1.perc2", inline=TRUE), strong(textOutput("gsd1.single2",inline=TRUE))),
                 br(),
                 
                 #param estimates - perc
                 
                 h4(strong(gett("perc.5"), textOutput("percentile30", inline=TRUE))),
                 
                 p(gett("frac.6"),strong(textOutput("Perc.single",inline=TRUE))),
                 
                 p(gett("perc.6.1"),strong(textOutput("Perc.ci.single",inline=TRUE))),

                 br(),
                 
                 ##risk decision
                 
                 ###illustrative graphs
                 
                 
                 #seq
                 h4(strong(gett("frac.graph.1"))),
                 
                 p(strong(gett("frac.graph.4"))),
                 
                 p(gett("perc.graph.5")),
                 p(gett("frac.graph.6")),
                 plotOutput("seqplot.perc.single"),
                 
                 br(),
                 
                 ##risk band
                 p(strong(gett("frac.graph.11"))),
                 
                 
                 p(gett("perc.graph.12.1"), textOutput("percentile34", inline=TRUE), htmlOutput("perc.graph.12.2", inline=TRUE)),
                 plotOutput("riskband.perc.single"),
                 br()
                ),
                ######################### SINGLE CATEGORY - AM
                tabPanel(gett("am.tab.name"),
                  h3(gett("am.title")),
                  br(),
                  div(style="block",class="plot-with-text",
                      div(style="display: inline-block;width:55%",class="text",
                          h4(strong(gett("frac.7"))),
                          p("\u25B9", gett("am.8")),
                          p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong(gett("AM"), "\u2265", gett("OEL"))),
                          p("\u25B9", gett("frac.10")),
                          p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",strong(textOutput("probrisk.AM.single",inline=TRUE))),
                          p("\u25B9", gett("frac.11")),
                          p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0", strong("<", textOutput("probSituUnacceptable9",inline=TRUE))),
                          p("\u25B9", gett("frac.12")),
                          p("\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0\u00A0",strong(textOutput("finalrisk.AM.single",inline=TRUE)))
                      ),
                      div(style="display: inline-block;width:44%;vertical-align:top", class="plot", plotOutput("risquemetre.3.single"))
                  ),
                  p(strong(gett("am.13")),gett("am.13.1")),
                   br(),
                   
                   
                   ##param estimates single
                   
                   h4(strong(gett("frac.4"))),
                   
                  p(htmlOutput("gm1.AM2", inline=TRUE), strong(textOutput("gm3.single",inline=TRUE))),
                  p(htmlOutput("gsd1.AM2", inline=TRUE), strong(textOutput("gsd3.single",inline=TRUE))),
                   br(),
                   
                   #param estimates - am
                   
                   h4(strong(gett("am.5"))),
                   
                   p(gett("frac.6"),strong(textOutput("AM.single",inline=TRUE))),
                   
                   p(gett("frac.6.1"),strong(textOutput("AM.ci.single",inline=TRUE))),
                   
                   br(),
                   
                   ###graphs
                   
                   h4(strong(gett("frac.graph.1"))),
                   
                   
                    #seq
                   
                   p(strong(gett("frac.graph.4"))),
                   p(gett("am.graph.5")),
                   p(gett("frac.graph.6")),
                   plotOutput("seqplot.AM.single"),
                   br(),
                   
                   ##risk 
                   
                   p(strong(gett("frac.graph.11"))),
                   p(htmlOutput("am.graph.12.2")),
                   plotOutput("riskband.am.single"),
                   br()
                   ##closing tablpanel level2
                  )
                #closing tabseppanel level 2
                )
                
                #closing conditional panel 
                
                )
                
                #closing tabpanel level 1
                ),
       
       
#
#
#
#
#
#        
###########INSTRUCTIONS
#
#
#
#
#
#
        tabPanel(gett("inst.tab.name"),
                 h3(gett("inst.title.t3")),
                 br(),
                 p(gett("inst.1")),
                 
                 p(gett("inst.2")),
                 
                 p(gett("inst.3.t3"))
                 
        ),
#
#
#
#
############About
#
#
#
#
#
        tabPanel(gett("about.tab.name"),
                 h3(gett("about.title.t3")),
                 br(),
                 p(htmlOutput("about.1.t3")),
                 p(gett("about.2")),
                 p(htmlOutput("about.3"))),
        
#
#
#
#
#
############BACKGROUND
#
#
#
#
#
        tabPanel(gett("back.tab.name"),
                 h3(gett("back.title.t3")),
                 br(),
                 p(gett("back.1.t3")),
                 p(htmlOutput("back.2")),
                 p(gett("back.3"))
                 
                 
                 
                 
        )
        
      )
      )
      )
    )
)



