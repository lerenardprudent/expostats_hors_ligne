library(shiny)
library(rjags)
library(readxl)
library(gridExtra)
library(extrafont)
#library(xlsx)

######################     scripts to source


setwd("./scripts")

##### determinant specific functions

source("Determinant/Data formatting functions_D.R")

source("Determinant/Bayesian engine functions_D.R")

source("Determinant/Descriptive graphs functions_D.R")

source("Determinant/Numerical output functions_D.R")

source("Determinant/Main graph functions_D.R")

#### common functions


source("Common/Simple censored imputation functions.R")

source("Common/Descriptive numerical output functions.R")

source("Common/Descriptive graphs functions.R")

source("Common/script density comparison.R")

source("Common/Numerical output functions.R")

source("Common/Main graph functions.R")

source("Common/Bayesian engine functions.R")

setwd("..")

###########loading the simulation data---------------------------

data.example <-read.csv(file="./data/data.example.csv")


##########SHINY STUFF

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
  return(paste0(perc, gett(suff), " ", gett("riskplot.17")))
}

# Define server logic 
shinyServer(function(input, output, session) {
 
  
  
  
  
#
#
#
#
################### data preparation
#
#
#
#
  
  ###lecture des données saisies
  
  niter.default <- 30000
 
  dataset.input <- reactive({
    
    inFile <- input$file1
    
    if(is.null(inFile))
      
      return(NULL)
    
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    
    read_excel(path = paste(inFile$datapath, ".xlsx", sep=""))#, sheetIndex = 1)
  })  
    
  ###trick for conditional results when no file
  
  output$fileUploaded <- reactive({
    return(!is.null(dataset.input()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$fileNotUploaded <- reactive({
    return(is.null(dataset.input()))
  })
  outputOptions(output, 'fileNotUploaded', suspendWhenHidden=FALSE)
  
  
  ####lits of variables for the dynamic list
  
  ListOfVars <- reactive({ 
    
    X <- dataset.input()
    
    return(names(X)[2:length(X)])
    
  })
  
  
  #####list of variables to select the analysis
  
  output$Box.listofvars <- renderUI({
    
    
    selectInput("listofvars",gett("server.inputs.1"),ListOfVars())
    
    
  })
  
  
  #############download
  
  output$downloadData <- downloadHandler(
    
    filename = function() { paste("data.example", '.xlsx', sep='') },
    
    content = function(file) {
     
      write.xlsx(data.example, file, sheetName="data", 
                 col.names=TRUE, row.names=FALSE, append=FALSE, showNA=TRUE)
    }
  )
  
  ############## initial formatting
  
  
  formatted.sample <- reactive({ 
    
    data.in <- dataset.input()
    
    oel <- input$oel
    
    oel.mult=input$al
    
    result <-data.formatting.D( data.in=data.in,
                                               oel=oel,oel.mult=oel.mult, 
                                               VarOfInterest=input$listofvars)
    return(result)
    
  })
  
  
  ######## input parameters
  
 
  user.input <- reactive({ 
    
    conf <- input$conf
    
    psi <- input$psi
    
    frac_threshold = input$frac_threshold
    
    target_perc = input$target_perc
    
    exp.ratio.gm = input$exp.ratio.gm
    
    ex.ratio.perc = input$exp.ratio.pPerc
    
    expdelta = input$exp.delta.f
    
    result <-list( conf=conf , 
                   psi = psi , 
                   frac_threshold = frac_threshold ,
                   target_perc = target_perc,
                   exp.ratio.gm = exp.ratio.gm ,
                   expdelta = expdelta
                   )
    
    return(result)
    
  })
  
  
  ######## simple imputation - global
  
  data.imputed <- reactive({ 
    
    X <- formatted.sample()
    
    result <-simple.censored.treatment(observations.formatted=X$data,
                                       notcensored=X$notcensored,
                                       leftcensored=X$leftcensored,
                                       rightcensored=X$rightcensored,
                                       intcensored=X$intcensored)
    
    return(result)
    
  })
  
  
  ###### data formatted single category
  
  single.cat.formatted <- reactive({ 
    
    X <-formatted.sample()
    
    Y <- input$single.cat
    
    result <-select.cat.formatted(data.formatted = X , cat = Y ) 
    
    
    return(result)
    
  })
  
  #### data imputed single category
  
  single.cat.imputed <- reactive({ 
    
    X <- single.cat.formatted()
    
    result <-simple.censored.treatment(observations.formatted=X$data,
                                                   notcensored=X$notcensored,
                                                   leftcensored=X$leftcensored,
                                                   rightcensored=X$rightcensored,
                                                   intcensored=X$intcensored)
    
    return(result)
    
  })
  
  #
  #          
  #          
  #          
  #          
  #################bayesian analysis  (GLOBAL)
  #  
  #          
  #          
  #        
  
   
                 
      bayesian.analysis.global <- reactive({ 
        
        
        X <-formatted.sample()  
        
        
        # Create a Progress object
        progress <- shiny::Progress$new()
        progress$set(message = gett("server.1"), value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        
        # Create a callback function to update progress.
        # Each time this is called:
        # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
        #   distance. If non-NULL, it will set the progress to that value.
        # - It also accepts optional detail text.
        updateProgress <- function(detail = NULL) {
          progress$inc(amount = 1/25, detail = detail)
        }
        
        bayesian.analysis.global <-fun.bayes.jags(observations=X$data , 
                                                  notcensored=X$notcensored, 
                                                  leftcensored=X$leftcensored ,
                                                  rightcensored=X$rightcensored ,
                                                  intcensored=X$intcensored , 
                                                  seed=X$seed , 
                                                  c.oel=X$c.oel , n.iter=25000 , updateProgress= updateProgress) 
        
        return(bayesian.analysis.global)
        
      })      
      
      
  #
  #          
  #          
  #          
  #          
  #################bayesian analysis   - determinants
  #  
  #          
  #          
  #   
      
      
                  
  bayesian.analysis.groups <- reactive({ 
    
    
  X <- formatted.sample() 
  
    # Create a Progress object
    progress1 <- shiny::Progress$new()
    progress1$set(message = gett("server.1"), value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress1$close())
    
    # Create a callback function to update progress.
    # Each time this is called:
    # - If `value` is NULL, it will move the progress bar 1/5 of the remaining
    #   distance. If non-NULL, it will set the progress to that value.
    # - It also accepts optional detail text.
    updateProgress1 <- function(detail = NULL) {
      progress1$inc(amount = 1/30, detail = detail)
    }
    
    
    
    
    res4 <- fun.bayes.jags.D( data.formatted = X ,
                              n.iter=niter.default ,
                              updateProgress = updateProgress1)  
    
    return(res4)
    
  })                
     
  #
  #          
  #          
  #          
  #          
  #################bayesian analysis   -  single categ0ry
  #  
  #          
  #          
  #   
  
  single.cat.bayes <- reactive({ 
    
    X <-bayesian.analysis.groups()
    
    Y <- input$single.cat
    
    result <-bayesian.output.B.single(bayesian.output.D = X, cat = Y) 
    
    
    return(result)
    
  })            
  
  #                
  #                
  #                
  #                  
  ########################################GLOBAL ANALYSIS
  #
  #
  #
  #
  
  
  ################## GLOBAL ANALYSIS - DESCRIPTIVE STATS
  
          #####descriptive table
          
             output$res.desc <- renderTable({
            
            
               X <-formatted.sample()
               
               Y <- data.imputed()
               
               if ( nrow(Y$imputed$data) > 0 ) {
                 result <-fun.desc.stat(data.simply.imputed = Y, c.oel = X$c.oel)
                } else {
                  result <- data.frame(matrix(nrow=13))
                }
               
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
        
          
          #####QQPLOT
          
          
          output$qqplot <- renderPlot({
            
            X <-formatted.sample()
            
            Y <- data.imputed()
            
            if ( nrow(Y$imputed$data) > 0 ) {
            p <- fun.qqplot.group.D(data.simply.imputed = Y, 
                                    notcensored = X$notcensored , 
                                    cats = X$var,
                                    qqplot.1=gett("qqplot.1"),
                                    qqplot.2=gett("qqplot.2"),
                                    qqplot.3=gett("qqplot.3"),
                                    qqplot.4=gett("qqplot.4"),
                                    qqplot.5=gett("qqplot.5"),
                                    qqplot.6=gett("qqplot.6"))
            
            } else {
              #Vide
              p <- data.frame()
              p<-ggplot(p) + geom_blank()
            }
            suppressWarnings(print(p))
          })
          

  ################## GLOBAL ANALYSIS - RISK        
          
          ######### numerical results
          
          num.res.global <- reactive({ 
            
            
            X <-bayesian.analysis.global()
            Y <-formatted.sample()
            Z <- user.input()
            
            result <- all.numeric(mu.chain=X$mu.chain ,
                                  sigma.chain=X$sigma.chain ,
                                  conf=Z$conf ,
                                  c.oel=Y$c.oel ,
                                  frac_threshold=Z$frac_threshold ,
                                  target_perc=Z$target_perc)
            
            
            return(result)
            
          })
          
          ################## GLOBAL ANALYSIS - RISK - EXCEEDANCE
          
            ###riskmeter
          
          output$risquemetre.1 <- renderPlot({
           
            X <-num.res.global()
            Y <- user.input()
            
            dessinerRisqueMetre(actualProb=X$frac.risk, 
                                minProbUnacceptable=Y$psi, 
                                colorProb="darkblue",
                                actualProb2=NULL, 
                                colorProb2="#4863A0")
            
          })
          
          
           ##gm point estimate + confidence intervall
          
          output$gm1 <-renderText({
            
            X <- num.res.global()
            
            est <-X$gm$est
            
            lcl <-X$gm$lcl
            
            ucl <-X$gm$ucl
            
            return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
            
            
          })
          
          #confidence level
          output$conf.out.1 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          ##gsd point estimate + confidence intervall
          
          output$gsd1 <-renderText({
            
            X <- num.res.global()
            
            est <-X$gsd$est
            
            lcl <-X$gsd$lcl
            
            ucl <-X$gsd$ucl
            
            return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
            
          })
          
          #confidence level
          output$conf.out.2 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          #exceedance point estimate
          output$Frac <-renderText({
            
            X <- num.res.global()
            
            res4<-X$frac$est
            
            return(paste(signif(res4,3)," %",sep=""))
            
          })
          
          #confidence level
          output$conf.out.3 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          
          output$conf.out.4 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          
          #confidence interval
          output$Frac.ci <-renderText({
            
            X <- num.res.global()
            
            lcl <-X$frac$lcl
            ucl <-X$frac$ucl
            
            return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
            
          })
          
          #risk probability
          output$probrisk <-renderText({
            
            X <- num.res.global()
            
            risk <-X$frac.risk
            
            return(paste(signif(risk,3),"%",sep=""))
            
          })
          
          #risk decision
          output$finalrisk <-renderText({
            
            X <- num.res.global()
            
            risk <-X$frac.risk
            
            if (risk>=input$psi) return(gett("server.2"))
            
            else return(gett("server.3"))
            
            
          })
          
          ########graphique de type sequentiel
          
          output$seqplot.frac <- renderPlot({
            
            X <- num.res.global()
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
          
          
          #############graphique bandes de risque INRS
          
          
          output$riskband.frac <- renderPlot({
            
            X <- formatted.sample()
            Y <- bayesian.analysis.global()
            Z <- user.input()
            
            
            graph8 <-riskband.plot.frac(mu.chain=Y$mu.chain,
                                        sigma.chain=Y$sigma.chain,
                                        c.oel=X$c.oel,
                                        psi=Z$psi,
                                        frac_threshold = Z$frac_threshold,
                                        riskplot.1=gett("riskplot.1"),
                                        riskplot.2=gett("riskplot.2"))
            
            suppressWarnings(print(graph8))
          })
          
          output$riskband.t3 <- renderPlot({
            
            X <- formatted.sample()
            
            Y <- bayesian.analysis.groups()
            
            Z <- user.input()
            
            bands <- riskband.perc.byband(bayesian.ouput.D = Y, c.oel=X$c.oel, target_perc=Z$target_perc, sorting=TRUE,
                                          riskplot.3=paste0("<1%\n", gett("OEL")),
                                          riskplot.4=paste0("1-10%\n", gett("OEL")),
                                          riskplot.5=paste0("10-50%\n", gett("OEL")),
                                          riskplot.6=paste0("50-100%\n", gett("OEL")),
                                          riskplot.7=paste0(">", gett("OEL")),
                                          riskband.byband.1 = gett("Category"),
                                          riskband.byband.2 = gett("Probability"),
                                          riskband.byband.3 = gett("Critical percentile category"))
            
            suppressWarnings(print(bands))
          })
          
          ### AJOUT GETT ###
          output$gauge.t3 <- renderPlot({
            
            X <- formatted.sample()
            
            Y <- bayesian.analysis.groups()
            
            Z <- user.input()
            
            gauge <- risk.gauge(bayesian.ouput.D = Y , c.oel=X$c.oel, user.input=Z,ggplot.cat.1 = gett("Category"),
                                ggplot.cat.2 = gett("Overexposure risk"))
            
            suppressWarnings(print(gauge))
          })
          
               
          ################## GLOBAL ANALYSIS - RISK - 95th percentile
          
          ###riskmeter
          
          output$risquemetre.2 <- renderPlot({

            X <-num.res.global()
            Y <- user.input()
            
            dessinerRisqueMetre(actualProb=X$perc.risk, 
                                minProbUnacceptable=Y$psi, 
                                colorProb="darkblue",
                                actualProb2=NULL, 
                                colorProb2="#4863A0")
          })
          
          
          ##gm point estimate + confidence intervall
          
          output$gm2 <-renderText({
            
            X <- num.res.global()
            
            est <-X$gm$est
            
            lcl <-X$gm$lcl
            
            ucl <-X$gm$ucl
            
            return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
            
            
          })
          
          #confidence level
          output$conf.out.5 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          ##gsd point estimate + confidence intervall
          
          output$gsd2 <-renderText({
            
            X <- num.res.global()
            
            est <-X$gsd$est
            
            lcl <-X$gsd$lcl
            
            ucl <-X$gsd$ucl
            
            return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
            
          })
          
          #confidence level
          output$conf.out.6 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          
          
          #pPercpoint estimate
          output$Perc <-renderText({
            
            X <- num.res.global()
            
            res5<-X$perc$est
            
            return(paste(signif(res5,3)))
            
          })
          
          #confidence level
          output$conf.out.7 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          
          output$conf.out.8 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          
          #confidence interval
          output$Perc.ci <-renderText({
            
            X <- num.res.global()
            
            lcl <-X$perc$lcl
            
            ucl <-X$perc$ucl
            
            return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
            
          })
          

          #risk probability
          output$probrisk.perc <-renderText({
            
            X <- num.res.global()
            
            risk <-X$perc.risk
            
            return(paste(signif(risk,3),"%",sep=""))
            
          })
          
          #risk decision
          output$finalrisk.perc <-renderText({
            
            X <- num.res.global()
            Y <- user.input()
            
            risk <-X$perc.risk
            
            if (risk>=input$psi) return(gett("server.2"))
            
            else return(gett("server.3"))
            
            
          }) 
          
          ###### sequential plot
          
          
          output$seqplot.perc <- renderPlot({
            
            X <- num.res.global()
            Y <- formatted.sample()
            Z <- data.imputed()
            W <- user.input()
            
            
            graph2 <- sequential.plot.perc(gm=X$gm$est , 
                                           gsd=X$gsd$est ,
                                           perc=X$perc$est , 
                                           c.oel=Y$c.oel ,
                                           data.simply.imputed=Z,
                                           target_perc=W$target_perc,
                                           seqplot.1=gett("seqplot.1"),
                                           seqplot.3=gett("seqplot.3"),
                                           seqplot.4=gett("seqplot.4"),
                                           seqplot.6 = gett("seqplot.6"))
            
            suppressWarnings(print(graph2))
            
          })
          
          
          ##############risk band graph
          
          ### AJOUT GETT ###
          output$riskband.perc <- renderPlot({
            
            X <- formatted.sample()
            Y <- bayesian.analysis.global()
            Z <- user.input()
            
            
            graph8 <-riskband.plot.perc(mu.chain=Y$mu.chain,
                                        sigma.chain=Y$sigma.chain,
                                        c.oel=X$c.oel,
                                        target_perc = Z$target_perc,
                                        psi=Z$psi,
                                        riskplot.2=gett("riskplot.2"),
                                        riskplot.3=gett("riskplot.3"),
                                        riskplot.4=gett("riskplot.4"),
                                        riskplot.5=gett("riskplot.5"),
                                        riskplot.6=gett("riskplot.6"),
                                        riskplot.7=gett("riskplot.7"),
                                        riskplot.8=gett("riskplot.8"))
            
            suppressWarnings(print(graph8))
          })
          
        
          ################## GLOBAL ANALYSIS - RISK - ARITHMETIC MEAN
          
          ###riskmeter
          
          output$risquemetre.3 <- renderPlot({
            
            X <-num.res.global()
            Y <- user.input()
            
            dessinerRisqueMetre(actualProb=X$am.risk, 
                                minProbUnacceptable=Y$psi, 
                                colorProb="darkblue",
                                actualProb2=NULL, 
                                colorProb2="#4863A0")
          })
          
          
          ##gm point estimate + confidence intervall
          
          output$gm3 <-renderText({
            
            X <-num.res.global()
            
            est <-X$gm$est
            
            lcl <-X$gm$lcl
            
            ucl <-X$gm$ucl
            
            return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
            
            
          })
          
          #confidence level
          output$conf.out.9 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          ##gsd point estimate + confidence intervall
          
          output$gsd3 <-renderText({
            X <-num.res.global()
            
            est <-X$gsd$est
            
            lcl <-X$gsd$lcl
            
            ucl <-X$gsd$ucl
            
            return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
            
          })
          
          #confidence level
          output$conf.out.10 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          
          
          #AM point estimate
          output$AM <-renderText({
            
            X <-num.res.global()
            
            est <- X$am$est
            
            return(paste(signif(est,3)))
            
          })
          
          #confidence level
          output$conf.out.11 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          
          output$conf.out.12 <-renderText({
            
            return(paste(input$conf," %",sep=""))
            
          })
          
          
          #confidence interval
          output$AM.ci <-renderText({
            
            X <-num.res.global()
            
            lcl <- X$am$lcl
            
            ucl <- X$am$ucl
            
            return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
            
          })
          
          #risk probability
          output$probrisk.AM <-renderText({
            
            X <-num.res.global()
            
            risk <-X$am.risk
            
            return(paste(signif(risk,3),"%",sep=""))
            
          })
          
          #risk decision
          output$finalrisk.AM <-renderText({
            
            X <-num.res.global()
            
            risk <-X$am.risk
            
            if (risk>=input$psi) return(gett("server.2"))
            
            else return(gett("server.3"))
            
            
          }) 
          
          
          #######################sequential graph
          
          output$seqplot.AM <- renderPlot({
            
            X <- num.res.global()
            Y <- formatted.sample()
            Z <- data.imputed()
            
            
            
            graph6 <- sequential.plot.am(gm=X$gm$est , 
                                         gsd=X$gsd$est ,
                                         am=X$am$est , 
                                         c.oel=Y$c.oel ,
                                         data.simply.imputed=Z,
                                         seqplot.1=gett("seqplot.1"),
                                         seqplot.3=gett("seqplot.3"),
                                         seqplot.5=gett("seqplot.5"),
                                         seqplot.6=gett("seqplot.6"))
            suppressWarnings(print(graph6))
            
          })
          
          ###########################risk band graph
          
          
          output$riskband.am <- renderPlot({
            
            
            X <- formatted.sample()
            Y <- bayesian.analysis.global()
            Z <- user.input()
            
            graph8 <-riskband.plot.am(mu.chain=Y$mu.chain,
                                      sigma.chain=Y$sigma.chain,
                                      c.oel=X$c.oel,
                                      psi=Z$psi,
                                      riskplot.2=gett("riskplot.2"),
                                      riskplot.3=gett("riskplot.3"),
                                      riskplot.4=gett("riskplot.4"),
                                      riskplot.5=gett("riskplot.5"),
                                      riskplot.6=gett("riskplot.6"),
                                      riskplot.7=gett("riskplot.7"),
                                      riskplot.9=gett("riskplot.9"))
            
            suppressWarnings(print(graph8))
            
          })
          
          
                 
#                
#                
#                  
########################################COMPARATIVE  ANALYSIS
#
#
#
#
          
          
          ################## COMPARATIVE ANALYSIS - all categories
          
            
          
                ####BOX WHISKER PLOT
          
                
                      ########################simulated data
                      
                      
                      output$categories.boxplot <- renderPlot({
                      
                        X <- formatted.sample()
                        
                        Y <- bayesian.analysis.groups()
                        
                        Z <- data.imputed()
                        
                        p <- boxplot.by.cat(data.formatted = X , 
                                            data.simply.imputed = Z , 
                                            bayesian.output.D = Y,
                                            boxplot.cat.1=gett("boxplot.cat.1"),
                                            boxplot.cat.2=gett("boxplot.cat.2"),
                                            boxplot.cat.3=gett("boxplot.cat.3"))
                              
                        suppressWarnings(print(p))
                      
                      }) 

          
          
                #####Comparative table  - distributional parameters
                      
                      output$table.global.dist <- renderTable({
                        
                        X <- formatted.sample()
                        
                        Y <- bayesian.analysis.groups()
                        
                        Z <- user.input()
                        
                        return(fun.comp.table.D(bayesian.analysis.D = Y, 
                                              c.oel = X$c.oel, 
                                              user.input = Z,
                                              comp.d.1=gett("comp.d.1"),
                                              comp.d.2=gett("comp.d.2"),
                                              comp.d.3=gett("comp.d.3"),
                                              comp.d.4=gett("comp.d.4"),
                                              comp.d.5=gett("comp.d.5"),
                                              comp.d.6=gett("comp.d.6"),
                                              comp.d.7=gett("comp.d.7")))
                        
                      },include.rownames=FALSE)
                      
          
          
                
                      
                      
        ################################# COMPARATIVE ANALYSIS - 2 CATEGORIES
                      
                      
                      ####lits of categoriies for Variable A
                      
                      ListOfCat1 <- reactive({ 
                        
                        x <-unlist(dataset.input()[,1])
                        
                        ##limitation to categories with 3 detected values
                        
                        cat.values <- unlist(dataset.input()[,input$listofvars])
                        
                        table.analysis <-data.frame(table(as.character(cat.values)),stringsAsFactors = FALSE)
                        
                        names(table.analysis) <-c("Var1","Freq")
                        
                        table.analysis$status <-FALSE
                        
                        for (i in 1:length(table.analysis[,1])) {
                          
                          x.prime <-x[cat.values==table.analysis$Var1[i]]
                          
                          n.det <-suppressWarnings(length(x.prime[!is.na(as.numeric(x.prime))]))
                          
                          if (n.det>=3) table.analysis$status[i] <- TRUE
                        }
                        
                        res<-names(table(cat.values)[table.analysis$status])
                        
                        return(res)
                        
                      })
                      
                      
                      output$box.cat1 <- renderUI({
                        
                        
                        selectInput("cat1",label=strong(span(gett("twocat.1"),style="color:#7D1B7E")),ListOfCat1())
                        
                        
                      })    
                      
                      
                      ####lits of categoriies for Variable B
                      
                      ListOfCat2 <- reactive({ 
                        
                        x <-unlist(dataset.input()[,1])
                        
                        ##limitation to categories with 3 detected values
                        
                        cat.values <-unlist(dataset.input()[,input$listofvars])
                        
                        table.analysis <-data.frame(table(as.character(cat.values)),stringsAsFactors = FALSE)
                        
                        names(table.analysis) <-c("Var1","Freq")
                        
                        table.analysis$status <-FALSE
                        
                        for (i in 1:length(table.analysis[,1])) {
                          
                          x.prime <-x[cat.values==table.analysis$Var1[i]]
                          
                          n.det <-suppressWarnings(length(x.prime[!is.na(as.numeric(x.prime))]))
                          
                          if (n.det>=3) table.analysis$status[i] <- TRUE
                        }
                        
                        res<-names(table(cat.values)[table.analysis$status])
                        
                        res <-res[res!=input$cat1]
                        
                        return(res)
                        
                      })
                      
                      output$box.cat2 <- renderUI({
                        
                        
                        selectInput("cat2",label=strong(span(gett("twocat.2"),style="color:#4863A0")),ListOfCat2())
                        
                        
                      })   
                      
                    
         ############# graph illustrating differences between categopries - BOX and WHISKER
                      
                      graph.2cat.whisk_reactive <-reactive({
                        
                        X <-formatted.sample()
                        
                        Y <- bayesian.analysis.groups()
                        
                        Z <- user.input()
                        
                        p <- boxplot.2.cat(data.formatted = X , 
                                           bayesian.output.D = Y, 
                                           cat1 =  input$cat1, 
                                           cat2 = input$cat2,
                                           boxplot.2cat.1=gett("boxplot.2cat.1"),
                                           boxplot.2cat.2=gett("boxplot.2cat.2"),
                                           boxplot.2cat.3=gett("boxplot.2cat.3"),
                                           boxplot.2cat.4=gett("boxplot.2cat.4"),
                                           boxplot.2cat.5=gett("Category"))
                        
                        suppressWarnings(print(p))
                      })
                      
                      output$graph.2cat.whisk <- renderPlot({
                        
                        graph.2cat.whisk_reactive()
                        
                      })            
                      
                      
            
                        
          ###################### changes in GM and GSD            
          
          
          ### TO COMMENT BY DM                        
          algn <- NULL #c("l","l","c","c","c","c","c")

                   output$table.2cat.dist <- renderTable({
                        
                     Y <- bayesian.analysis.groups()
                     
                     Z <- user.input()
                     
                     table.res <- fun.2cat.dist(bayesian.output.D = Y ,  
                                                cat1 =  input$cat1, 
                                                cat2 = input$cat2, 
                                                user.input = Z,
                                                comp.2cat.1 = gett("comp.d.1"),
                                                comp.2cat.2 = gett("comp.d.2"),
                                                comp.2cat.3 = gett("comp.d.7"),
                                                comp.2cat.4 = gett("comp.2cat.4"))
                          
                        return(table.res)
                        
                        
                      },include.rownames=FALSE,
                        align=algn)
          
          ###################### changes in AMand pPerc            
                   
                   output$table.2cat.metrics1 <- renderTable({
                     
                     Y <- bayesian.analysis.groups()
                     
                     Z <- user.input()
                     
                     table.res <- fun.2cat.metrics(bayesian.output.D = Y ,  
                                                   cat1 =  input$cat1, 
                                                   cat2 = input$cat2, 
                                                   user.input = Z,
                                                   comp.2cat.1 = gett("comp.d.am.1"),
                                                   comp.2cat.2 = gett("comp.d.am.2"),
                                                   comp.2cat.3 = gett("comp.d.7"),
                                                   comp.2cat.4 = gett("comp.2cat.4"))
                     
                     
                     return(table.res)
                     
                     
                   },include.rownames=FALSE,
                   align=algn)
                   
                   
       ###################### changes in AF            
                   
                   output$table.2cat.metrics2 <- renderTable({
                     
                     X <-formatted.sample()
                     
                     Y <- bayesian.analysis.groups()
                     
                     Z <- user.input()
                     
                     table.res <- fun.2cat.f(data.formatted = X,
                                             bayesian.output.D = Y ,  
                                                   cat1 =  input$cat1, 
                                                   cat2 = input$cat2, 
                                                   user.input = Z,
                                             comp.2cat.1 = gett("comp.d.4"),
                                             comp.2cat.3 = gett("comp.d.7"),
                                             comp.2cat.4 = gett("comp.2cat.4"))
                     
                     
                     return(table.res)
                     
 
                   },include.rownames=FALSE,
                   align=algn) 
                   
                   ###################### changes P acceptablee          
                   
                   output$table.2cat.risk <- renderTable({
                     
                     
                     X <-formatted.sample()
                     
                     Y <- bayesian.analysis.groups()
                     
                     Z <- user.input()
                     
                     table.res <- fun.2cat.risk(data.formatted = X,
                                                bayesian.output.D = Y ,  
                                             cat1 =  input$cat1, 
                                             cat2 = input$cat2, 
                                             user.input = Z,
                                             comp.2cat.1 = gett("comp.d.4"),
                                             comp.2cat.2 = gett("comp.d.5"),
                                             comp.2cat.3 = gett("comp.d.6"),
                                             comp.2cat.4 = gett("comp.d.7"))
                     
                     
                     return(table.res)
                     
                   },include.rownames=TRUE,
                   align=algn) #c("l","l","c","c","c")) 
                   
                   
                   
                   
#                
#                
#                  
########################################SINGLE CATEGORY  ANALYSIS
#
#
#
#
                   
                   
################################### defining the category of interest
                   
                   
                   ####lits of categoriies for Variable A
                   
                   ListOfCat.single <- reactive({ 
                     
                     x <-unlist(dataset.input()[,1])
                     
                     ##limitation to categories with 3 detected values
                     
                     cat.values <-unlist(dataset.input()[,input$listofvars])
                     
                     table.analysis <-data.frame(table(as.character(cat.values)),stringsAsFactors = FALSE)
                     
                     names(table.analysis) <-c("Var1","Freq")
                     
                     table.analysis$status <-FALSE
                     
                     for (i in 1:length(table.analysis[,1])) {
                       
                       x.prime <-x[cat.values==table.analysis$Var1[i]]
                       
                       n.det <-suppressWarnings(length(x.prime[!is.na(as.numeric(x.prime))]))
                       
                       if (n.det>=3) table.analysis$status[i] <- TRUE
                     }
                     
                     res<-names(table(cat.values)[table.analysis$status])
                     
                     return(res)
                     
                   })
                   
                   
                   output$box.cat.single <- renderUI({
                     
                     
                     selectInput("single.cat",label=strong(span(gett("onecat.1"),style="color:#7D1B7E")),ListOfCat.single())
                     
                     
                   })                    
                   
##############################  SINGLE CATEGORY - DESCRIPTIVE
                   
                   #####descriptive table
                   
                   output$res.desc.single <- renderTable({
                     
                     X <- single.cat.formatted()
                     
                     Y <- single.cat.imputed()
                     
                     result <-fun.desc.stat(data.simply.imputed = Y , c.oel = X$c.oel )
                     
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
                   
                   
                   #####QQPLOT
                   
                   
                   output$qqplot.single <- renderPlot({
                     
                     X <- single.cat.formatted()
                     
                     Y <- single.cat.imputed()
                     
                     p<-fun.qqplot(data.simply.imputed = Y , 
                                   notcensored = X$notcensored,
                                   qqplot.1=gett("qqplot.1"),
                                   qqplot.2=gett("qqplot.2"),
                                   qqplot.3=gett("qqplot.3"),
                                   qqplot.4=gett("qqplot.4"),
                                   qqplot.5=gett("qqplot.5"),
                                   qqplot.6=gett("qqplot.6"))
                     
                     suppressWarnings(print(p))
                     
                   })
  ############################### single category - BAYESIAN RESULTS FOR ONE CAT
                   
                 
       num.res.single <- reactive({ 
                     
                     
         X <- single.cat.bayes() 
         
         Y <- single.cat.formatted()
         
         Z <- single.cat.imputed()
         
         W <- user.input()
         
         result <-all.numeric(mu.chain=X$mu.chain ,
                               sigma.chain=X$sigma.chain ,
                               conf=W$conf ,
                               c.oel=Y$c.oel ,
                               frac_threshold=W$frac_threshold ,
                               target_perc=W$target_perc)
                     
                     
                     return(result)
                     
                   })                
                   
                   
                                
 ##############################  SINGLE CATEGORY - EXCEEDAnce
 
                   
                   ###riskmeter
                   
                   output$risquemetre.1.single <- renderPlot({

                     X <-num.res.single()
                     Y <- user.input()
                     
                     dessinerRisqueMetre(actualProb=X$frac.risk, 
                                         minProbUnacceptable=Y$psi, 
                                         colorProb="darkblue",
                                         actualProb2=NULL, 
                                         colorProb2="#4863A0")
                   })
                   
                   
                   ##gm point estimate + confidence intervall
                   
                   output$gm1.single <-renderText({
                     
                     X <- num.res.single()
                     
                     est <-X$gm$est
                     
                     lcl <-X$gm$lcl
                     
                     ucl <-X$gm$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                   })
                   
                   output$gm1.single2 <-renderText({
                     
                     X <- num.res.single()
                     
                     est <-X$gm$est
                     
                     lcl <-X$gm$lcl
                     
                     ucl <-X$gm$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                   })
                   
                   #confidence level
                   output$conf.out.1.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   ##gsd point estimate + confidence intervall
                   
                   output$gsd1.single <-renderText({
                     
                     X <- num.res.single()
                     
                     est <-X$gsd$est
                     
                     lcl <-X$gsd$lcl
                     
                     ucl <-X$gsd$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                   })
                   
                   output$gsd1.single2 <-renderText({
                     
                     X <- num.res.single()
                     
                     est <-X$gsd$est
                     
                     lcl <-X$gsd$lcl
                     
                     ucl <-X$gsd$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                   })
                   
                   #confidence level
                   output$conf.out.2.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   
                   #exceedance point estimate
                   output$Frac.single <-renderText({
                     
                     X <- num.res.single()
                     
                     res4<-X$frac$est
                     
                     return(paste(signif(res4,3)," %",sep=""))
                     
                   })
                   
                   #confidence level
                   output$conf.out.3.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   output$conf.out.4.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   #confidence interval
                   output$Frac.ci.single <-renderText({
                     
                     X <- num.res.single()
                     
                     lcl <-X$frac$lcl
                     ucl <-X$frac$ucl
                     
                     return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
                     
                   })
                   
                  
                   #risk probability
                   output$probrisk.single <-renderText({
                     
                     X <- num.res.single()
                     
                     risk <- X$frac.risk
                     
                     return(paste(signif(risk,3)," %",sep=""))
                     
                   })
                   
                   #risk decision
                   output$finalrisk.single <-renderText({
                     
                     X <- num.res.single()
                     
                     risk <-X$frac.risk
                     
                     if (risk>=input$psi) return(gett("server.2"))
                     
                     else return(gett("server.3"))
                     
                     
                   })
                   
                   ########graphique de type sequentiel
                   
                   output$seqplot.frac.single <- renderPlot({
                     
                     X <- num.res.single()
                     Y <- single.cat.formatted()
                     Z <- single.cat.imputed()
                     
                     ### AJOUT GETT ###
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
                   
                   
                   #############RISK BAND GRAPH
                   
                   
                   output$riskband.frac.single <- renderPlot({
                     
                     X <- single.cat.formatted()
                     Y <- single.cat.bayes() 
                     Z <- user.input()
                     
                     
                     graph8 <-riskband.plot.frac(mu.chain=Y$mu.chain,
                                                 sigma.chain=Y$sigma.chain,
                                                 c.oel=X$c.oel,
                                                 frac_threshold = Z$frac_threshold,
                                                 psi=Z$psi,
                                                 riskplot.1=gett("riskplot.1"),
                                                 riskplot.2=gett("riskplot.2"))
                     
                     suppressWarnings(print(graph8))
                   })
 
################## SINGLE CATEGROY ANALYSIS - RISK - 95th percentile
                   
                   ###riskmeter
                   
                   output$risquemetre.2.single <- renderPlot({
                     
                     X <-num.res.single()
                     Y <- user.input()
                     
                     dessinerRisqueMetre(actualProb=X$perc.risk, 
                                         minProbUnacceptable=Y$psi, 
                                         colorProb="darkblue",
                                         actualProb2=NULL, 
                                         colorProb2="#4863A0")
                   })
                   
                   
                   ##gm point estimate + confidence intervall
                   
                   output$gm2.single <-renderText({
                     
                     X <- num.res.single()
                     
                     est <-X$gm$est
                     
                     lcl <-X$gm$lcl
                     
                     ucl <-X$gm$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                     
                   })
                   
                   #confidence level
                   output$conf.out.5.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   ##gsd point estimate + confidence intervall
                   
                   output$gsd2.single <-renderText({
                     
                     X <- num.res.single()
                     
                     est <-X$gsd$est
                     
                     lcl <-X$gsd$lcl
                     
                     ucl <-X$gsd$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                   })
                   
                   #confidence level
                   output$conf.out.6.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   
                   #pPercpoint estimate
                   output$Perc.single <-renderText({
                     
                     X <- num.res.single()
                     
                     res5<-X$perc$est
                     
                     return(paste(signif(res5,3)))
                     
                   })
                   
                   #confidence level
                   output$conf.out.7.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   output$conf.out.8.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   #confidence interval
                   output$Perc.ci.single <-renderText({
                     
                     X <- num.res.single()
                     
                     lcl <-X$perc$lcl
                     
                     ucl <-X$perc$ucl
                     
                     return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
                     
                   })
                   
                   #risk probability
                   output$probrisk.perc.single <-renderText({
                     
                     X <- num.res.single()
                     
                     risk <-X$perc.risk
                     
                     return(paste(signif(risk,3),"%",sep=""))
                     
                   })
                   
                   #risk decision
                   output$finalrisk.perc.single <-renderText({
                     
                     X <- num.res.single()
                     Y <- user.input()
                     
                     risk <-X$perc.risk
                     
                     if (risk>=input$psi) return(gett("server.2"))
                     
                     else return(gett("server.3"))
                     
                     
                   }) 
                   
                   
                   ###### sequential plot
                   
                   
                   output$seqplot.perc.single <- renderPlot({
                     
                     X <- num.res.single()
                     Y <- single.cat.formatted()
                     Z <- single.cat.imputed()
                     W <- user.input()
                     
                     
                     graph2 <- sequential.plot.perc(gm=X$gm$est , 
                                                    gsd=X$gsd$est ,
                                                    perc=X$perc$est , 
                                                    c.oel=Y$c.oel ,
                                                    data.simply.imputed=Z,
                                                    target_perc=W$target_perc,
                                                    seqplot.1=gett("seqplot.1"),
                                                    seqplot.3=gett("seqplot.3"),
                                                    seqplot.4=gett("seqplot.4"),
                                                    seqplot.6 = gett("seqplot.6"))
                     
                     suppressWarnings(print(graph2))
                     
                   })
                   
                   
                   ##############risk band graph
                   
                   
                   output$riskband.perc.single <- renderPlot({
                     
                     X <- single.cat.formatted()
                     Y <- single.cat.bayes() 
                     Z <- user.input()
                     
                     
                     graph8 <-riskband.plot.perc(mu.chain=Y$mu.chain,
                                                 sigma.chain=Y$sigma.chain,
                                                 c.oel=X$c.oel,
                                                 target_perc = Z$target_perc,
                                                 psi=Z$psi,
                                                 riskplot.2=gett("riskplot.2"),
                                                 riskplot.3=gett("riskplot.3"),
                                                 riskplot.4=gett("riskplot.4"),
                                                 riskplot.5=gett("riskplot.5"),
                                                 riskplot.6=gett("riskplot.6"),
                                                 riskplot.7=gett("riskplot.7"),
                                                 riskplot.8=gett("riskplot.8"))
                     
                     suppressWarnings(print(graph8))
                   })
                   
 ################## SINGLE CATEGORY ANALYSIS - RISK - ARITHMETIC MEAN
                   
                   ###riskmeter
                   
                   output$risquemetre.3.single <- renderPlot({
                    
                      X <-num.res.single()
                     Y <- user.input()
                     
                     dessinerRisqueMetre(actualProb=X$am.risk, 
                                         minProbUnacceptable=Y$psi, 
                                         colorProb="darkblue",
                                         actualProb2=NULL, 
                                         colorProb2="#4863A0")
                   })
                   
                   
                   ##gm point estimate + confidence intervall
                   
                   output$gm3.single <-renderText({
                     
                     X <-num.res.single()
                     
                     est <-X$gm$est
                     
                     lcl <-X$gm$lcl
                     
                     ucl <-X$gm$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                     
                   })
                   
                   #confidence level
                   output$conf.out.9.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   ##gsd point estimate + confidence intervall
                   
                   output$gsd3.single <-renderText({
                     
                     X <-num.res.single()
                     
                     est <-X$gsd$est
                     
                     lcl <-X$gsd$lcl
                     
                     ucl <-X$gsd$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                   })
                   
                   #confidence level
                   output$conf.out.10.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   
                   #AM point estimate
                   output$AM.single <-renderText({
                     
                     X <-num.res.single()
                     
                     est <- X$am$est
                     
                     return(paste(signif(est,3)))
                     
                   })
                   
                   #confidence level
                   output$conf.out.11.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   output$conf.out.12.single <-renderText({
                     
                     return(paste(input$conf," %",sep=""))
                     
                   })
                   
                   
                   #confidence interval
                   output$AM.ci.single <-renderText({
                     
                     X <-num.res.single()
                     
                     lcl <- X$am$lcl
                     
                     ucl <- X$am$ucl
                     
                     return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
                     
                   })
                   
                   #risk probability
                   output$probrisk.AM.single <-renderText({
                     
                     X <-num.res.single()
                     
                     risk <-X$am.risk
                     
                     return(paste(signif(risk,3),"%",sep=""))
                     
                   })
                   
                   #risk decision
                   output$finalrisk.AM.single <-renderText({
                     
                     X <-num.res.single()
                     
                     risk <-X$am.risk
                     
                     if (risk>=input$psi) return(gett("server.2"))
                     
                     else return(gett("server.3"))
                     
                     
                   }) 
                   
                   
                   #######################sequential graph
                   
                   
                   
                   output$seqplot.AM.single <- renderPlot({
                     
                     X <- num.res.single()
                     Y <- single.cat.formatted()
                     Z <- single.cat.imputed()
                     
                     graph6 <- sequential.plot.am(gm=X$gm$est , 
                                                  gsd=X$gsd$est ,
                                                  am=X$am$est , 
                                                  c.oel=Y$c.oel ,
                                                  data.simply.imputed=Z,
                                                  seqplot.1=gett("seqplot.1"),
                                                  seqplot.3=gett("seqplot.3"),
                                                  seqplot.5=gett("seqplot.5"),
                                                  seqplot.6=gett("seqplot.6"))
                     suppressWarnings(print(graph6))
                     
                   })
                   
                   ###########################risk band graph
                   
                   
                   output$riskband.am.single <- renderPlot({
                      
                     X <- single.cat.formatted()
                     Y <- single.cat.bayes()
                     Z <- user.input()
                     
                     graph8 <-riskband.plot.am(mu.chain=Y$mu.chain,
                                               sigma.chain=Y$sigma.chain,
                                               c.oel=X$c.oel,
                                               psi=Z$psi,
                                               riskplot.2=gett("riskplot.2"),
                                               riskplot.3=gett("riskplot.3"),
                                               riskplot.4=gett("riskplot.4"),
                                               riskplot.5=gett("riskplot.5"),
                                               riskplot.6=gett("riskplot.6"),
                                               riskplot.7=gett("riskplot.7"),
                                               riskplot.9=gett("riskplot.9"))
                     
                     suppressWarnings(print(graph8))
                   })
                   
                   
                   ####### TO COMMENT BY DM
                   
                   output$acceptableExpo1 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
                   output$acceptableExpo2 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
                   output$acceptableExpo3 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
                   output$acceptableExpo4 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
                   output$acceptableExpo5 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
                   output$acceptableExpo6 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
                   output$acceptableExpo7 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
                   output$acceptableExpo8 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
                   output$probSituUnacceptable1 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$probSituUnacceptable2 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$probSituUnacceptable3 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$probSituUnacceptable4 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$probSituUnacceptable5 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$probSituUnacceptable6 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$probSituUnacceptable7 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$probSituUnacceptable8 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$probSituUnacceptable9 <-renderText({return(paste(input$psi,"%",sep="")) })
                   output$acceptableExpoDiv10_1 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
                   output$acceptableExpoDiv10_2 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
                   output$acceptableExpoDiv10_3 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
                   output$acceptableExpoDiv10_4 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
                   output$acceptableExpoDiv10_5 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
                   output$acceptableExpoDiv10_6 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
                   output$percentile1 <-renderText({percText(input$target_perc)})
                   output$percentile2 <-renderText({percText(input$target_perc)})
                   output$percentile3 <-renderText({percText(input$target_perc)})
                   output$percentile4 <-renderText({percText(input$target_perc)})
                   output$percentile5 <-renderText({percText(input$target_perc)})
                   output$percentile6 <-renderText({percText(input$target_perc)})
                   output$percentile7 <-renderText({percText(input$target_perc)})
                   output$percentile8 <-renderText({percText(input$target_perc)})
                   output$percentile9 <-renderText({percText(input$target_perc)})
                   output$percentile10 <-renderText({percText(input$target_perc)})
                   output$percentile11 <-renderText({percText(input$target_perc)})
                   output$percentile12 <-renderText({percText(input$target_perc)})
                   output$percentile13 <-renderText({percText(input$target_perc)})
                   output$percentile14 <-renderText({percText(input$target_perc)})
                   output$percentile15 <-renderText({percText(input$target_perc)})
                   output$percentile16 <-renderText({percText(input$target_perc)})
                   output$percentile17 <-renderText({percText(input$target_perc)})
                   output$percentile18 <-renderText({percText(input$target_perc)})
                   output$percentile19 <-renderText({percText(input$target_perc)})
                   output$percentile20 <-renderText({percText(input$target_perc)})
                   output$percentile21 <-renderText({percText(input$target_perc)})
                   output$percentile22 <-renderText({percText(input$target_perc)})
                   output$percentile23 <-renderText({percText(input$target_perc)})
                   output$percentile24 <-renderText({percText(input$target_perc)})
                   output$percentile25 <-renderText({percText(input$target_perc)})
                   output$percentile26 <-renderText({percText(input$target_perc)})
                   output$percentile27 <-renderText({percText(input$target_perc)})
                   output$percentile28 <-renderText({percText(input$target_perc)})
                   output$percentile29 <-renderText({percText(input$target_perc)})
                   output$percentile30 <-renderText({percText(input$target_perc)})
                   output$percentile31 <-renderText({percText(input$target_perc)})
                   output$percentile32 <-renderText({percText(input$target_perc)})
                   output$percentile33 <-renderText({percText(input$target_perc)})
                   output$percentile34 <-renderText({percText(input$target_perc)})
                   output$percentile35 <-renderText({percText(input$target_perc)})
                   output$percentile36 <-renderText({percText(input$target_perc)})
                   output$percentile37 <-renderText({percText(input$target_perc)})
                   output$percentile38 <-renderText({percText(input$target_perc)})
                   
                   output$comp.6 <- renderUI({
                     HTML( gettt("comp.6",
                                 strong(span(paste(gett("CATEGORY"), "1"),style="color:#7D1B7E")),
                                 strong(span(paste(gett("CATEGORY"), "2"),style="color:#4863A0")),
                                 strong(span(paste(gett("CATEGORY"), "2"),style="color:#4863A0")),
                                 strong(span(paste(gett("CATEGORY"), "1"),style="color:#7D1B7E")),
                                 strong(span(paste(gett("CATEGORY"), "2"),style="color:#4863A0")),
                                 strong(span(paste(gett("CATEGORY"), "1"),style="color:#7D1B7E"))
                          ) 
                      )
                   })
                   
                   output$comp.8 <- renderUI({
                     HTML( gettt("comp.8",
                                 strong(span(paste(gett("CATEGORY"), "1"),style="color:#7D1B7E")),
                                 strong(span(paste(gett("CATEGORY"), "2"),style="color:#4863A0")),
                                 a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/',target="_blank"))
                     )
                   })
                   
                   output$comp.11 <- renderUI({
                     HTML( paste0('<label style="display: inline">',
                           gett('comp.11'),
                           '</label><input id="exp.ratio.gm" type="number" value="0.5" min="0.001" max="1000" style="text-align:center"/>'
                          )
                     )
                   })
                   
                   output$comp.15 <- renderUI({
                     HTML( '<label style="display: inline">',
                           gett('comp.15.1'),
                            percText(input$target_perc),
                           gett('comp.15.2'),
                          '</label><input id="exp.ratio.pPerc" type="number" value="0.5" min="0.001" max="1000" style="text-align:center"/>'
                     )
                   })
                   
                   output$comp.18 <- renderUI({
                     HTML( '<label style="display: inline">',
                           gett('comp.18'),
                           '</label><input id="exp.delta.f" type="number" value="10" min="0.001" max="100" style="text-align:center"/>'
                     )
                   })
                   
                   output$back.2 <- renderUI({
                     HTML( gettt("back.2", a(gett('here'),href='http://www.expostats.ca/site/en/info.html',target="_blank") ) )
                   })
                   
                   output$locale <-renderText({
                     loc <- paste0("LOCALE = ", Sys.getlocale("LC_MESSAGES"))
                     return(loc)
                   })
                   
                   output$descriptive.2 <- renderUI({
                     HTML(
                       gettt("descriptive.2",
                             a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/', target="_blank"),
                             a('Dennis Helsel',href='http://www.practicalstats.com/info2use/books.html',target="_blank")
                       )
                     )
                   })
                   
                   output$descriptive.2.1 <- renderUI({
                     HTML(
                       gettt("descriptive.2",
                             a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/', target="_blank"),
                             a('Dennis Helsel',href='http://www.practicalstats.com/info2use/books.html',target="_blank")
                       )
                     )
                   })
                   
                   output$frac.4.1 <- renderUI({
                     HTML(
                       gettt("frac.4.1",
                             gett("geometric mean")
                       )
                     )
                   })
                   
                   output$frac.4.1.2 <- renderUI({
                     HTML(
                       gettt("frac.4.1",
                             gett("geometric mean")
                       )
                     )
                   })
                   
                   output$perc.graph.12.3 <- renderUI({
                     HTML(
                       gettt("perc.graph.12.3",
                             a('AIHA',href='https://www.aiha.org',target="_blank")
                       )
                     )
                   })
                   
                   output$perc.graph.12.3.2 <- renderUI({
                     HTML(
                       gettt("perc.graph.12.3",
                             a('AIHA',href='https://www.aiha.org',target="_blank")
                       )
                     )
                   })
                   
                   output$am.graph.12 <- renderUI({
                     HTML(
                       gettt("am.graph.12",
                             a('AIHA',href='https://www.aiha.org',target="_blank")
                       )
                     )
                   })
                   
                   output$am.graph.12.2 <- renderUI({
                     HTML(
                       gettt("am.graph.12",
                             a('AIHA',href='https://www.aiha.org',target="_blank")
                       )
                     )
                   })
                   
                   output$comp.3 <- renderUI({
                     HTML(
                       gettt("frac.25.t3",
                             a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/',target="_blank")
                       )
                     )
                   })
                   
                   output$about.1 <- renderUI({
                     HTML(
                       gettt("about.1",
                            a("l'École de santé publique",href='https://www.espum.umontreal.ca',target="_blank"),
                            a("Université de Montréal",href='https://www.umontreal.ca')
                       )
                     )
                   })
                   
                   output$gm1.dist <- renderText({
                     gettt("frac.18",
                           gett("gm.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gsd1.dist <- renderText({
                     gettt("frac.18",
                           gett("gsd.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gm1.frac <- renderText({
                     gettt("frac.18",
                           gett("gm.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gsd1.frac <- renderText({
                     gettt("frac.18",
                           gett("gsd.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gm1.perc <- renderText({
                     gettt("frac.18",
                           gett("gm.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gsd1.perc <- renderUI({
                     gettt("frac.18",
                           gett("gsd.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gm1.perc2 <- renderText({
                     gettt("frac.18",
                           gett("gm.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gsd1.perc2 <- renderUI({
                     gettt("frac.18",
                           gett("gsd.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gm1.AM <- renderText({
                     gettt("frac.18",
                           gett("gm.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gsd1.AM <- renderText({
                     gettt("frac.18",
                           gett("gsd.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gm1.AM2 <- renderText({
                     gettt("frac.18",
                           gett("gm.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   output$gsd1.AM2 <- renderText({
                     gettt("frac.18",
                           gett("gsd.lwr"),
                           paste(input$conf," %",sep=""))
                   })
                   
                   
                   ##gsd point estimate + confidence interval
                   
                   output$gsd1 <-renderText({
                     
                     X <- num.res.global()
                     
                     est <-X$gsd$est
                     
                     lcl <-X$gsd$lcl
                     
                     ucl <-X$gsd$ucl
                     
                     return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                     
                   })

                   output$about.3 <- renderUI({
                     HTML(
                       gettt("about.3",
                             a("R",href='http://cran.r-project.org/',target="_blank"),
                             a("Shiny",href='http://www.rstudio.com/shiny/',target='_blank'),
                             a("JAGS",href='http://mcmc-jags.sourceforge.net', target='_blank')
                       ))
                   })
                   
                   output$perc.graph.12.2 <- renderUI({ HTML(gettt("perc.graph.12.2", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
                   output$perc.graph.12.2.2 <- renderUI({ HTML(gettt("perc.graph.12.2", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
                   output$perc.graph.12.2.3 <- renderUI({ HTML(gettt("perc.graph.12.2", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
                   
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
                   
                   output$plot.frac.grp <- renderPlot({
                     Y <- formatted.sample()
                     Z <- user.input()
                     bayesian.analysis.cat <- fun.bayes.jags.D( data.formatted = Y,
                                                                n.iter=niter.default ,
                                                                updateProgress= NULL) 
                     CHOIX_USER <- TRUE
                     
                     p <- plot.frac.grp(bayesian.output.D = bayesian.analysis.cat,
                                    c.oel = Y$c.oel,
                                    frac_threshold = Z$frac_threshold,
                                    sorting = CHOIX_USER,
                                    gett("plot.grp.1"),
                                    gett("plot.grp.2"),
                                    gett("plot.grp.3"),
                                    gett("plot.grp.4"),
                                    gett("plot.grp.5")
                                    )
                     
                     return(p)
                     
                   })
                   
                   output$plot.perc.grp <- renderPlot({
                     
                     Y <- formatted.sample()
                     Z <- user.input()
                     bayesian.analysis.cat <- fun.bayes.jags.D( data.formatted = Y,
                                                                n.iter=niter.default ,
                                                                updateProgress= NULL) 
                     CHOIX_USER <- TRUE
                     
                     p <- plot.perc.grp(bayesian.output.D = bayesian.analysis.cat,
                                   c.oel = Y$c.oel,
                                   target_perc = Z$target_perc,
                                   sorting = CHOIX_USER,
                                   gett("plot.grp.1"),
                                   gett("plot.grp.2"),
                                   gett("plot.grp.3"),
                                   gett("plot.grp.4"),
                                   gett("plot.grp.5"),
                                   gett("plot.grp.6"),
                                   gett("plot.grp.7"),
                                   gett("plot.grp.8"),
                                   gett("plot.grp.9"),
                                   gett("plot.grp.10"),
                                   gett("plot.grp.11")
                     )
                     return(p)
                     
                   })
                   
                   output$plot.perc.am <- renderPlot({
                     
                     Y <- formatted.sample()
                     Z <- user.input()
                     bayesian.analysis.cat <- fun.bayes.jags.D( data.formatted = Y,
                                                                n.iter=niter.default ,
                                                                updateProgress= NULL) 
                     CHOIX_USER <- TRUE
                     
                     p <- plot.am.grp(bayesian.output.D = bayesian.analysis.cat,
                                      c.oel = Y$c.oel,
                                      sorting = CHOIX_USER,
                                      gett("plot.grp.1"),
                                      gett("plot.grp.2"),
                                      gett("plot.grp.3"),
                                      gett("plot.grp.4"),
                                      gett("plot.grp.5"),
                                      gett("plot.grp.6"),
                                      gett("plot.grp.7"),
                                      gett("plot.grp.8"),
                                      gett("plot.grp.9"),
                                      gett("plot.grp.10"),
                                      gett("plot.grp.11")
                     )
                     return(p)
                     
                   })
                   
                   ###### END OF NEW FOR BOXPLOT
})
