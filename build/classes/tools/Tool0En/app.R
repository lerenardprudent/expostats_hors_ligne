#
#
#
#     SERVER+UI FOR TOOL1 EXPRESS: SEG ANALYSIS TOOL
#
#
#
#


library(shinydashboard)
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(shinythemes)
library(rjags)
library(V8)

## langage parameters

source("langParams.R")

setwd("./scripts")

##SEG SPECIFIC FUNCTIONS

source("SEG/Data formatting functions_SEG.R")

##COMMON

source("Common/Simple censored imputation functions.R")

source("Common/Descriptive numerical output functions.R")

source("Common/Descriptive graphs functions.R")

source("Common/Bayesian engine functions.R")

source("Common/Numerical output functions.R")

source("Common/Main graph functions.R")

setwd("..")

source("gauge.R")

####### HTML for data entry

inputTextarea <- function(inputId, value="", nrows, ncols) {
  tagList(
    singleton(tags$head(tags$script(src = "textarea.js"))),
    tags$textarea(id = inputId,
                  class = "inputTextarea",
                  rows = nrows,
                  cols = ncols,
                  as.character(value),
                  '28.9\n19.4\n<5.5\n149.9\n26.42\n56.1')
  )
}

nd <- gsub(" ", "", Sys.time())
nd2 <- ""
bindtextdomain(transDomain, "")

##### function for tooltips

tt <- function(param, txt="[0 &lt; valid &le; 100]") {
  return(bsTooltip(param, txt, "right", options = list(container = "body")))
}



inputWidth <- "110px"

expandMenu <- "shinyjs.myFunc = function(){$('#inputsMenu').prev('a').click();}"


######### MAIN UI

ui <- dashboardPage(
  dashboardHeader(title = gett("main.title.t1x"), titleWidth = "750px"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(id="inputsMenu", gett("tab.name.1"), icon = icon("th"),
               # Input directly under menuItem
               #selectInput("inputTest", "Input Test",
                #           choices = c("a", "b", "c", "d"), multiple=TRUE, selectize=TRUE,
                #           width = '98%'),
               
               # Input inside of menuSubItem
               #menuSubItem(icon = NULL,
              #             sliderInput("inputTest2", "Input test 2", min=0, max=10, value=5,
              #                         width = '95%')
              # )
              numericInput("oel", gett("input.2"), 100, width=inputWidth) ,
              tt("oel", gett("input.2.tooltip")),
              div(id="outer", style="display:none", 
                  numericInput("conf", gett("Confidence"), value=90),
                  numericInput("al", gett("Action level"), value=1),
                  numericInput("target_perc", gett('Percentile'), value=95),
                  numericInput("psi", gett("input.5"), value=30),
                  numericInput("frac_threshold", gett("frac.1"), value=5)
              ),
              div(class='form-group shiny-input-container',
                tags$label(style="display:block", gett("input.6")),
                inputTextarea('Data', '',10,10 )
              ),
              startExpanded = TRUE
      ),
      menuItem(id="resultsMenu", gett("tab.name.2"), tabName = "dashboard", icon = icon("calculator"),
               menuSubItem(gett("subtab.name.1"), tabName = "desc_stats", icon=icon("table")),
               menuSubItem(gett("subtab.name.2"), tabName = "param_estim", icon=icon("exchange")),
               menuSubItem(gett("subtab.name.3"), tabName = "risk_assess", icon=icon("flash")),
               startExpanded = TRUE
      ),
      menuItem(gett("about.tab.name"), tabName = "about_tab_name", icon = icon("question-circle"))
    )
  ),
  dashboardBody(
    #useShinyjs(),
    #extendShinyjs(text=expandMenu),
    tabItems(
      # First tab content
      tabItem(tabName = "desc_stats",
              fluidRow(
                box(
                  title = gett("descriptive.title"),
                  tableOutput('res.desc'),
                  h4(gett('descriptive.1')),
                  
                  htmlOutput('descriptive.2')
                ),
                box(
                  title = gett("descriptive.3"),
                  plotOutput('qqplot'),
                  p(gett("descriptive.4")),
                  
                  ##### NEW FOR BOXPLOT
                  br(),
                  h4(gett("descriptive.5")),
                  plotOutput('boxplot'),
                  p(gett("descriptive.6"))
                  
                  ####END OF NEW FOR BOXPLOT
                  
                )
              )
      ),
      tabItem(tabName = "param_estim",
              fluidRow(
                box(
                  title = gett("subtab.name.2"),
                  
## TO CLEAN AND UNIFORMIZE ACROSS ALL TOOLS
                  #param estimates - distrib
                  h5(strong(gett("frac.4"))),
                  p(textOutput("gm1.dist", inline=TRUE), strong(textOutput("gm1",inline=TRUE))),
                  p(textOutput("gsd1.dist", inline=TRUE), strong(textOutput("gsd1",inline=TRUE))),
                  
                  #param estimates - exceedance
                  h5(strong(gett("frac.5"))),
                  p(gett("frac.6"),strong(textOutput("Frac",inline=TRUE))),
                  
                  p(gett("frac.6.1"),strong(textOutput("Frac.ci",inline=TRUE))),
                  
                  ##param estimates - percentile
                  h5(strong(gett("perc.5"), textOutput("percentile1",inline=TRUE))),
                  p(gett("frac.6"),strong(textOutput("Perc",inline=TRUE))),
                  p(gett("frac.6.1"),strong(textOutput("Perc.ci",inline=TRUE))),

                  #param estimates  -  arithmetic mean
                  h5(strong(gett("am.5"))),
                  p(gett("frac.6"),strong(textOutput("AM",inline=TRUE))),
                  p(gett("frac.6.1"),strong(textOutput("AM.ci",inline=TRUE)))

                ),
              
                ### sequential plot
                box(
                  title = paste0(gett("frac.graph.4")),
                  plotOutput("seqplot.AM"),
                  p(gett("result.8")),
                  p(gett("frac.graph.6"))
                  
                )
            )
      ),
      
      
      ##### risk assessment
      
      tabItem(tabName = "risk_assess",
              fluidRow(
                
                #risk meter
                box(
                  title = gett("subtab.name.3"),
                  p(gett("risk.1")),
                  plotOutput("risquemetre"),
                  
                  h4(gett("risk.1.1")),
                  
                  p(gett("risk.9"), strong(gett("95th percentile") ,"\u2265 ", gett("OEL"))),
                  
                  p(gett("frac.10"),strong(textOutput("probrisk.perc",inline=TRUE))),
                  
                  p(gett("frac.12"),strong(htmlOutput("finalrisk.perc",inline=TRUE)))
                ),
                
                #risk framework
                box(
                  title = gett("box.name.5"),
                  p(gett("risk.2")),
                  p(gett("risk.3")),
                  p(gett("risk.4")),
                  tags$ul(
                    tags$li(HTML(paste(tags$span(style="color:green", gett("risk.5"))))),
                    tags$li(HTML(paste(tags$span(style="color:orange", gett("risk.6"))))),
                    tags$li(HTML(paste(tags$span(style="color:red", gett("risk.7")))))
                  ),
                  br(),
                  p(gett("risk.8"))
                )
                
                
                
              )
      ),
      tabItem(tabName = "about_tab_name",
              fluidRow(
                box(
                  title = gett("about.title.t1x"),
                  p(htmlOutput("about.1.t1x")),
                  p(gett("about.2.t1x")),
                  p(htmlOutput("about.3"))
                )
              )
      )
    )
  )
)

############   SERVER

server <- function(input, output) {
  
  #### TO COMMENT BY DM / ELIMINATE
  set.seed(122)
  histdata <- rnorm(500)
  
  #observe({
  #  js$myFunc()
  #})
  
  ###data formatting
  
  formatted.sample <- reactive({ 
    
    data.in <- input$Data
    
    oel <- input$oel
    
    oel.mult=1
    
    result <- data.sample.formatted <-data.formatting.SEG(data.in=data.in,
                                                          oel=oel, 
                                                          oel.mult = oel.mult)
    return(result)
    
  })
  
  ### input parameters
  
  user.input <- reactive({ 
    
    conf <- 90
    
    psi <- 30
    
    frac_threshold = 5
    
    target_perc = 95
    
    
    result <-list( conf=conf , 
                   psi = psi , 
                   frac_threshold = frac_threshold ,
                   target_perc = target_perc )
    
    return(result)
    
  })
  
  ###### simple imputation for descriptive statistics and graphs
  
  data.imputed <- reactive({ 
    
    X <- formatted.sample()
    
    result <-simple.censored.treatment(observations.formatted=X$data,
                                       notcensored=X$notcensored,
                                       leftcensored=X$leftcensored,
                                       rightcensored=X$rightcensored,
                                       intcensored=X$intcensored)
    
    return(result)
    
  })
  
  
  #################bayesian analysis
  
  bayesian.analysis <- reactive({ 
    
    
    
    # Create a Progress object
    progress <- shiny::Progress$new()
    progress$set(message = "server.X", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress <- function(detail = NULL) {
      progress$inc(amount = 1/50, detail = detail)
    }
    
    
    X <- formatted.sample()
    
    result <-fun.bayes.jags(observations=X$data , 
                            notcensored=X$notcensored, 
                            leftcensored=X$leftcensored ,
                            rightcensored=X$rightcensored ,
                            intcensored=X$intcensored , 
                            seed=X$seed , 
                            c.oel=X$c.oel , n.iter=25000 , updateProgress = updateProgress) 
    
    return(result)
    
  })
  
  ######### numerical ouputs
  
  
  num.res <- reactive({ 
    
    
    X <- bayesian.analysis()
    
    Y <- formatted.sample()
    
    Z <- user.input()
    
    
    
    result <- all.numeric(mu.chain=X$mu.chain ,
                          sigma.chain=X$sigma.chain ,
                          conf=Z$conf ,
                          c.oel=Y$c.oel ,
                          frac_threshold=Z$frac_threshold ,
                          target_perc=Z$target_perc)
    
    
    return(result)
    
  })
  
  ####### TO COMMENT / ELIMINATE BY DM
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  
  ###### DESCRIPTIVE STATS
  
  ##### table
  output$res.desc <- renderTable({
    
    X <- data.imputed() 
    
    Y <- formatted.sample()
    
    result <- fun.desc.stat(data.simply.imputed=X , c.oel = Y$c.oel )
    
    result$parameter <-
      c(gett('res.desc.1'),
        gett('res.desc.2'),
        gett('res.desc.3'),
        gett('res.desc.4'),
        gett('res.desc.5'),
        gett('res.desc.6'),
        gett('res.desc.7'),
        gett('res.desc.8'),
        gett('res.desc.9'),
        gett('res.desc.10'),
        gett('res.desc.11'),
        gett('res.desc.12'),
        gett('res.desc.13')
      )
    
    return(result)   
    
  },include.rownames=FALSE)
  
  
  ##### qqplot
  output$qqplot <- renderPlot({
    
    
    X <- data.imputed() 
    
    
    Y <- formatted.sample()
    
    p <- fun.qqplot(data.simply.imputed= X  , 
                    notcensored = Y$notcensored,
                    qqplot.1=gett("qqplot.1"),
                    qqplot.2=gett("qqplot.2"),
                    qqplot.3=gett("qqplot.3"),
                    qqplot.4=gett("qqplot.4"),
                    qqplot.5=gett("qqplot.5"),
                    qqplot.6=gett("qqplot.6"))
    
    print(p)
    
  })
  
  ###### NEW FOR BOXPLOT
  
  ##### boxplot
  output$boxplot <- renderPlot({
    
    
    X <- data.imputed() 
    
    
    Y <- formatted.sample()
    
    p <- fun.boxplot( data.simply.imputed = X, 
                      notcensored = Y$notcensored, 
                      c.oel =Y$c.oel,
                      boxplot.1=gett("d.boxplot.1"),
                      boxplot.2=gett("d.boxplot.2"),
                      boxplot.3=gett("d.boxplot.3"),
                      boxplot.4=gett("d.boxplot.4"),
                      boxplot.5=gett("d.boxplot.5"))
    
    print(p)
    
  })
  
  ###### END OF NEW FOR BOXPLOT
  
  
  ############### PARAMETER ESTIMATES
  
  output$gm1 <-renderText({
    
    X <- num.res()
    
    est <-X$gm$est
    
    lcl <-X$gm$lcl
    
    ucl <-X$gm$ucl
    
    return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
    
    
  })
  
  #confidence level
  output$conf.out.7 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  ##gsd point estimate + confidence intervall
  
  output$gsd1 <-renderText({
    
    X <- num.res()
    
    est <-X$gsd$est
    
    lcl <-X$gsd$lcl
    
    ucl <-X$gsd$ucl
    
    return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
    
  })
  
  #confidence level
  output$conf.out.8 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  #point estimate-exceedance
  output$Frac <-renderText({
    
    X <- num.res()
    
    res4<-X$frac$est
    
    return(paste(signif(res4,3)," %",sep=""))
    
  })
  
  #confidence interval-exceedance
  output$Frac.ci <-renderText({
    
    X <- num.res()
    
    lcl <-X$frac$lcl
    ucl <-X$frac$ucl
    
    return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
    
  })
  
  output$conf.out <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  
  output$conf.out.2 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  #point estimate-95th percentile
  output$Perc <-renderText({
    
    X <- num.res()
    
    res4<-X$perc$est
    
    return(paste(signif(res4,3)))
    
  })
  
  #confidence level
  output$conf.out.3 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  
  output$conf.out.4 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  #confidence interval-95th percentile
  output$Perc.ci <-renderText({
    
    X <- num.res()
    
    lcl <-X$perc$lcl
    ucl <-X$perc$ucl
    
    out <- paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep="")
    
    return(out)
    
  })
  
  
  #confidence level
  output$conf.out.5 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  
  output$conf.out.6 <-renderText({
    
    return(paste(input$conf," %",sep=""))
    
  })
  
  #point estimate-arithmetic mean
  output$AM <-renderText({
    
    X <- num.res()
    
    res4<-X$am$est
    
    return(paste(signif(res4,3)))
    
  })
  
  #confidence interval-arithmetic mean
  output$AM.ci <-renderText({
    
    X <- num.res()
    
    lcl <-X$am$lcl
    ucl <-X$am$ucl
    
    return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
    
  })
  
  # name for the 95th percentile
  output$percentile1 <-renderText({percText(input$target_perc)})

  
  # sequential plot
  output$seqplot.AM <- renderPlot({
    
    X <- num.res()
    Y <- formatted.sample()
    Z <- data.imputed()
    
    
    graph2 <- sequential.plot.frac(gm=X$gm$est , 
                                   gsd=X$gsd$est ,
                                   frac=X$frac$est , 
                                   c.oel=Y$c.oel ,
                                   data.simply.imputed=Z,
                                   seqplot.1=gett("seqplot.1"),
                                   seqplot.2=gett("seqplot.2"),
                                   seqplot.6=gett("seqplot.6"))
    
    suppressWarnings(print(graph2))
    
  })
  
  
  ######## TO BE COMMENTED / ELIMINATED BY DM
  percText <- function(perc) {
    rem <- perc %% 10
    if ( rem == 1) {
      suff <- "st"
    } else
      if ( rem == 2 ) {
        suff <- "nd"
      } else
        if ( rem == 3 ) {
          suff <- "rd"
        } else {
          suff <- "th"
        }
    return (paste0(perc, gett(suff), " ", gett("percentile")) )
  }
  
  
  ######### RISK METER
  
  output$risquemetre <- renderPlot({

    X <-num.res()
    Y <- user.input()
    
    frac <-100*(1-pnorm((log((input$oel*input$al))-bayesian.analysis()$mu)/bayesian.analysis()$sigma))
      risk <-length(frac[frac>=input$frac_threshold])/length(frac)
      dessinerRisqueMetre.G(risk, input$psi/100)
    
    #dessinerRisqueMetre(actualProb=X$frac.risk, 
    #                    minProbUnacceptable=Y$psi, 
    #                    colorProb="darkblue",
    #                    actualProb2=NULL, 
    #                    colorProb2="#4863A0")
    
  })
  
  #risk probability
  output$probrisk.perc <-renderText({
    
    X <- num.res()
    
    risk <-X$perc.risk
    
    return(paste(signif(risk,3)," %",sep=""))
    
  })
  
  #risk decision
  output$finalrisk.perc <-renderUI({
    
    X <- num.res()
    
    risk <-X$perc.risk
    
    if (risk>=30) return (HTML(paste(tags$span(style="color:red", gett("server.2")))))
    
    if (risk<30 & risk>=5) return (HTML(paste(tags$span(style="color:orange", gett("server.2.1")))))
    
    if (risk<5) return (HTML(paste(tags$span(style="color:green", gett("server.2.2")))))
    
    
  })
  
  output$about.1 <-renderUI({
    HTML(gettt("about.1",
               a(gett("ESPUM"), href='https://www.espum.umontreal.ca',target="_blank"),
               a("Université de Montréal",href='https://www.umontreal.ca',target="_blank")
         )
        )
    
    
  })
  
  output$about.3 <-renderUI({
    HTML(gettt("about.3",
               a(gett("free"), href='http://cran.r-project.org/', target='_blank'),
               a("JAGS", href='http://mcmc-jags.sourceforge.net/', target='_blank'),
               a("Shiny", href='http://www.rstudio.com/shiny/', target='_blank')
    )
    )
    
    
  })
  
  #######note NDEXPO
  
  output$result.2 <- renderUI({
    HTML(
      gettt("result.2",
            a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/', target="_blank"),
            a('Dennis Helsel',href='http://www.practicalstats.com/info2use/books.html',target="_blank")
      )
    )
  })
  
  output$gm1.dist <- renderText({
    gettt("frac.18.b",
          gett("gm.lwr"),
          paste(input$conf," %",sep=""))
  })
  
  output$gsd1.dist <- renderText({
    gettt("frac.18.b",
          gett("gsd.lwr"),
          paste(input$conf," %",sep=""))
  })
  
  output$descriptive.2 <- renderUI({
    HTML(
      gettt("descriptive.2",
            a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/', target="_blank"),
            a('Dennis Helsel',href='http://www.practicalstats.com/info2use/books.html',target="_blank")
      )
    )
  })


}

shinyApp(ui, server)
