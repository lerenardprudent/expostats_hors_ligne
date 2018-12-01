#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
################################################################################


#
#
#
#     SERVER FOR TOOL2 : BETWEEN WORKER ANALYSIS TOOL
#
#
#
#



library(shiny)
library(rjags)

######################     scripts to source

setwd("./scripts")

##SEG SPECIFIC FUNCTIONS


##Between worker SPECIFIC FUNCTIONS

source("BetweenWorker/Data formatting functions_B.R")

source("BetweenWorker/Descriptive graphs functions_B.R")

source("BetweenWorker/Bayesian engine functions_B.R")

source("BetweenWorker/Main graph functions_B.R")

source("BetweenWorker/Numerical output functions_B.R")

##COMMON

source("Common/Simple censored imputation functions.R")

source("Common/Descriptive numerical output functions.R")

source("Common/Descriptive graphs functions.R")

source("Common/Bayesian engine functions.R")

source("Common/Numerical output functions.R")

source("Common/Main graph functions.R")

setwd("..")

####################   SHINY STUFF

# Define server logic 
shinyServer(function(input, output) {
  
  
  ###data formatting
  
  formatted.sample <- reactive({ 
    
    data.in <- input$Data
    
    oel <- input$oel
    
    oel.mult=input$al
    
    result <- data.formatting.B(data.in=data.in,
                                oel= oel, 
                                oel.mult = oel.mult)
    return(result)
    
  })
  
  ### input parameters
  
  user.input <- reactive({ 
    
    conf <- input$conf
    
    psi <- input$psi
    
    frac_threshold <- input$frac_threshold
    
    target_perc <- input$target_perc
    
    rappap_cover <- input$rappap_cover
    
    wwct <- input$wwct
    
    #prior <- input$prior.sigma
    uninformed_prior <- FALSE #!strtoi(prior)
    
    result <-list( conf=conf , 
                   psi = psi , 
                   frac_threshold = frac_threshold ,
                   target_perc = target_perc ,
                   rappap_cover = rappap_cover , 
                   wwct = wwct,
                   uninformed_prior = uninformed_prior)
    
    return(result)
    
  })
  
  ###### simple imputation for descriptive statistics and graphs - GROUP analysis
  
  data.imputed <- reactive({ 
    
    X <- formatted.sample()
    
    result <-simple.censored.treatment(observations.formatted=X$data$x,
                                       notcensored=X$notcensored,
                                       leftcensored=X$leftcensored,
                                       rightcensored=X$rightcensored,
                                       intcensored=X$intcensored)
    
    return(result)
    
  })
  
 
  
  
####### selection of individuals n>3
  
        ####lits of worker for the dynamic list
        
        ListOfWorkers <- reactive({ 
          
          X <- formatted.sample()
          
          Freq.table <- data.frame(table(X$data$worker))
          
          names(Freq.table) <-c("nom","n")
          
          return(as.character(Freq.table$nom[Freq.table$n>2]))
          
        })
        
        ### FRAC
        output$Box1 <- renderUI({
          
          selectInput("workers",gett("server.w.1"),ListOfWorkers())
          
        })
        
        
        
####### selection of individuals any n (bayesian analysis)        
        
          ListOfWorkers.full <- reactive({ 
          
            X <- formatted.sample()
            
            Freq.table <- data.frame(table(X$data$worker))
            
            names(Freq.table) <-c("nom","n")
            
            return(as.character(Freq.table$nom))
          
        })
        
        ### FRAC
        output$Box2 <- renderUI({
          
          selectInput("workers.full",gett("server.w.1"),ListOfWorkers.full())
          
        })
        
        
        ### PERC
        output$Box3 <- renderUI({
          
          selectInput("workers.full.pPerc",gett("server.w.1"),ListOfWorkers.full())
          
          
        })
        
        ### AM
        output$Box4 <- renderUI({
          selectInput("workers.full.am",gett("server.w.1"),ListOfWorkers.full())
          
          
        })
  

   
        
        
  #
  #
  #
  #
  ############################# DESCRIPTIVE STATISTICS
  ##
  #
  #
  #
  #
  #
  
 
  
  
  #####descriptive statistics - GROUP
  
  
        #####descriptive table
        
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
        
            
        
        #######simple qqplot
            
            
            output$qqplot <- renderPlot({
              
              X <- data.imputed() 
              
              Y <- formatted.sample()
              
              p <- fun.qqplot.group( data.simply.imputed = X , 
                               notcensored = Y$notcensored , 
                               workers = Y$data$worker,
                               qqplot.1=gett("qqplot.1"),
                               qqplot.2=gett("qqplot.2"),
                               qqplot.3=gett("qqplot.3"),
                               qqplot.4=gett("qqplot.4"),
                               qqplot.5=gett("qqplot.5"),
                               qqplot.6=gett("qqplot.6"))
              
              print(p)
              
            })
            

  #####descriptive statistics - individuals        
  
            
          
            ####Descriptive stats for selected worker workers with >5 observations         
            
            output$res.desc.ind <- renderTable({
              
              X <- formatted.sample()
              
              X.worker <- select.worker.formatted(data.formatted= X ,
                                                  worker.id =input$workers)
              
              data.imputed.worker <-simple.censored.treatment(observations.formatted=X.worker$data$x,
                                                              notcensored=X.worker$notcensored,
                                                              leftcensored=X.worker$leftcensored,
                                                              rightcensored=X.worker$rightcensored,
                                                              intcensored=X.worker$intcensored)
              
              Y <- data.imputed.worker
              
              result <- fun.desc.stat(data.simply.imputed=Y , 
                                      c.oel = X.worker$c.oel )
              
              result$parameter <-c(gett('res.desc.1'),
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
            
            
            #######simple qqplot for selected worker workers with >2 observations
            
            
            output$qqplot.ind <- renderPlot({
              
              X <- formatted.sample()
              
              X.worker <- select.worker.formatted(data.formatted= X ,
                                                  worker.id =input$workers)
              
              data.imputed.worker <-simple.censored.treatment(observations.formatted=X.worker$data$x,
                                                              notcensored=X.worker$notcensored,
                                                              leftcensored=X.worker$leftcensored,
                                                              rightcensored=X.worker$rightcensored,
                                                              intcensored=X.worker$intcensored)
              
              Y <- data.imputed.worker
              
              p <- fun.qqplot(data.simply.imputed = Y  , 
                              notcensored = X.worker$notcensored,
                              qqplot.1=gett("qqplot.1"),
                              qqplot.2=gett("qqplot.2"),
                              qqplot.3=gett("qqplot.3"),
                              qqplot.4=gett("qqplot.4"),
                              qqplot.5=gett("qqplot.5"),
                              qqplot.6=gett("qqplot.6"))
              
              print(p)
              
            })
                      
  
 
  #
  #          
  #          
  #          
  #          
  #################bayesian analysis 
  #  
  #          
  #          
  #          
                  bayesian.analysis <- reactive({ 
                    
                    
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
                      progress$inc(amount = 1/50, detail = detail)
                    }
                    
                    X <- formatted.sample()
                    
                    Y <- user.input()
                    
                    result <-fun.bayes.jags.B(data.formatted = X , 
                                              n.iter=100000 , 
                                              updateProgress = updateProgress,
                                              Y$uninformed_prior) 
                    result$uninf_prior <- Y$uninformed_prior
                    
                    return(result)
                    
                 })

  
                  
  ###### numerical analysis for the group
                  
  num.res.group <- reactive({ 
                    
                    
                    X <- bayesian.analysis()
                    
                    Y <- formatted.sample()
                    
                    Z <- user.input()
                    
                    result <- all.numeric(mu.chain=X$mu ,
                                          sigma.chain=X$stot , 
                                          conf=Z$conf ,
                                          c.oel=Y$c.oel , 
                                          frac_threshold=Z$frac_threshold ,
                                          target_perc=Z$target_perc)
                    
                    
                    return(result)
                    
                  })
  
  
  ###### numerical analysis - between worker results
  
  num.res.between <- reactive({ 
    
    
    X <- bayesian.analysis()
    
    Y <- formatted.sample()
    
    Z <- user.input()
    
    result <- all.numeric.B(bayesian.output.B = X,
                            user.input.B = Z,
                            c.oel = Y$c.oel,
                            frac.ind.threshold = input$frac_threshold_ind , 
                            perc.ind.threshold = input$perc_threshold_ind, 
                            am.ind.threshold = input$am_threshold_ind)
    
    
    return(result)
    
  })
  
                  
  ###### numerical analysis for an individual worker (FRAC / PERC / AM)
  
  num.res.worker <- reactive({ 
    
    
    X <- bayesian.analysis()
    
    Y <- formatted.sample()
    
    Z <- user.input()
    
    mu.chain.worker <- X$mu.workers[X$worker.index$numberinoutput[X$worker.index$worker==input$workers.full],]
    
    sigma.chain.worker <-X$sw
    
    result <- all.numeric(mu.chain=mu.chain.worker ,
                          sigma.chain=sigma.chain.worker , 
                          conf=Z$conf ,
                          c.oel=Y$c.oel , 
                          frac_threshold=Z$frac_threshold ,
                          target_perc=Z$target_perc)
    
    
    return(result)
    
  })  
  
  
  num.res.worker.perc <- reactive({ 
    
    
    X <- bayesian.analysis()
    
    Y <- formatted.sample()
    
    Z <- user.input()
    
    mu.chain.worker <- X$mu.workers[X$worker.index$numberinoutput[X$worker.index$worker==input$workers.full.pPerc],]
    
    sigma.chain.worker <-X$sw
    
    result <- all.numeric(mu.chain=mu.chain.worker ,
                          sigma.chain=sigma.chain.worker , 
                          conf=Z$conf ,
                          c.oel=Y$c.oel , 
                          frac_threshold=Z$frac_threshold ,
                          target_perc=Z$target_perc)
    
    
    return(result)
    
  }) 
  
  
  
  num.res.worker.am <- reactive({ 
    
    
    X <- bayesian.analysis()
    
    Y <- formatted.sample()
    
    Z <- user.input()
    
    mu.chain.worker <- X$mu.workers[X$worker.index$numberinoutput[X$worker.index$worker==input$workers.full.am],]
    
    sigma.chain.worker <-X$sw
    
    result <- all.numeric(mu.chain=mu.chain.worker ,
                          sigma.chain=sigma.chain.worker , 
                          conf=Z$conf ,
                          c.oel=Y$c.oel , 
                          frac_threshold=Z$frac_threshold ,
                          target_perc=Z$target_perc)
    
    
    return(result)
    
  })                                
  #                
  #                
  #                  
  ########################################Bayesian analysis output - EXCEEDANCE
  #
  #
  #
  
                  
  #############EXCEEDANCE - GROUP
                  
                  
                #point estimate
                output$Frac <-renderText({
                  
                  X <- num.res.group()
                  
                  res4<-X$frac$est
                  
                  return(paste(signif(res4,3)," %",sep=""))
                  
                })
                
                #confidence level
                output$conf.out <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                
                output$conf.out.2 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                
                #confidence interval
                output$Frac.ci <-renderText({
                  
                  X <- num.res.group()
                  
                  lcl <-X$frac$lcl
                  ucl <-X$frac$ucl
                  
                  return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
                  
                })
                
                #risk probability
                output$probrisk <-renderText({
                  
                  X <- num.res.group()
                  
                  risk <-X$frac.risk
                  
                  return(paste(signif(risk,3),"%",sep=""))
                  
                })
                
                #risk decision
                output$finalrisk <- renderText({
                  X <- num.res.group()
                  return(calcFinalRisk(X$frac.risk))
                })
                output$finalrisk.ind <- renderText({
                  X <-num.res.worker()
                  return(calcFinalRisk(X$frac.risk))
                })
                
                
                ##gm point estimate + confidence intervall
                
                output$gm1 <-renderText({
                  
                  X <- num.res.group()
                  
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
                  
                  X <- num.res.group()
                  
                  est <-X$gsd$est
                  
                  lcl <-X$gsd$lcl
                  
                  ucl <-X$gsd$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
                #confidence level
                output$conf.out.8 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                

  #########EXCEEDANCE - BETWEEN WORKER PARAMETERS

                
                #confi interval
                output$conf.out.ind.1 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                #rho
                output$rho.1 <-renderText({
                  
                  X <- num.res.between()
                  
                  est <-X$rho$est
                  
                  lcl <-X$rho$lcl
                  
                  ucl <-X$rho$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
                
                #rho prob >X
                
                output$rho.greater0.2 <-renderText({
                  
                  X <- num.res.between()
                  
                  res <- X$rho.greaterthan
                  
                  return(paste(signif(res,2),"%",sep=""))
                  
                })
                
                #confi interval
                output$conf.out.ind.2 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                #GSD between
                output$GSDB.1 <-renderText({
                  
                  X <- num.res.between()
                  
                  est <-X$gsdb$est
                  
                  lcl <-X$gsdb$lcl
                  
                  ucl <-X$gsdb$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
  
                #confi interval
                output$conf.out.ind.3 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
    output$gsdw.frac <- renderText({  return( paste(gettt("frac.18", gett("GSDw.lwr")) )) })
		output$gsdb.frac <- renderText({  return( paste(gettt("frac.18", gett("GSDb.lwr")) )) })
		output$rho.frac <- renderText({ return( paste(gettt("frac.18", gett("rho.lwr")) )) })
		
		output$gsdw.frac.2 <- renderText({  return( paste(gettt("frac.18", gett("GSDw.lwr")) )) })
		output$gsdb.frac.2 <- renderText({  return( paste(gettt("frac.18", gett("GSDb.lwr")) )) })
		output$rho.frac.2 <- renderText({ return( paste(gettt("frac.18", gett("rho.lwr")) )) })
		
		output$gsdw.frac.3 <- renderText({  return( paste(gettt("frac.18", gett("GSDw.lwr")) )) })
		output$gsdb.frac.3 <- renderText({  return( paste(gettt("frac.18", gett("GSDb.lwr")) )) })
		output$rho.frac.3 <- renderText({ return( paste(gettt("frac.18", gett("rho.lwr")) )) })

		output$frac.gm.ind <- renderText({
        		gettt("selected_worker", gett("gm.lwr"), input$conf) 
        	})

		output$frac.gsd.ind <- renderText({
        		gettt("selected_worker", gett("gsd.lwr"), input$conf)
        	})

		output$frac.gm.ind.2 <- renderText({
        		gettt("selected_worker", gett("gm.lwr"), input$conf)
        	})

		output$frac.gsd.ind.2 <- renderText({
        		gettt("selected_worker", gett("gsd.lwr"), input$conf)
        	})

		output$frac.gm.ind.3 <- renderText({
        		gettt("selected_worker", gett("gm.lwr"), input$conf)
        	})

		output$frac.gsd.ind.3 <- renderText({
        		gettt("selected_worker", gett("gsd.lwr"), input$conf)
        	})

		output$frac.20 <- renderText({
        		gettt("frac.20", gett("rho"))        	})

		output$perc.18 <- renderText({
        		gettt("frac.18", gett("GSDw"))
        	})

		output$perc.19 <- renderText({
        		gettt("frac.19", gett("GSDb"))
        	})

		output$perc.20 <- renderText({
        		gettt("frac.20", gett("rho"))
        	})

		output$am.18 <- renderText({
        		gettt("frac.18", gett("GSDw"))
        	})

		output$am.19 <- renderText({
        		gettt("frac.19", gett("GSDb"))
        	})

		output$am.20 <- renderText({
        		gettt("frac.20", gett("rho"))
        	})

		output$frac.25 <- renderUI({
        		HTML( gettt("frac.25", a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/', target="_blank")))
        	})
		
		output$perc.25 <- renderUI({
        		HTML( gettt("frac.25", a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/', target="_blank")))
        	})

		output$am.25 <- renderUI({
        		HTML( gettt("frac.25", a('NDexpo',href='http://www.expostats.ca/site/app-local/NDExpo/', target="_blank")))
        	})

		output$frac.graph.12.2.2 <- renderUI({ HTML(gettt("perc.graph.12.2", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
		output$frac.graph.12.2.3 <- renderUI({ HTML(gettt("perc.graph.12.2", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
		
		output$perc.graph.12.2 <- renderUI({ HTML(gettt("perc.graph.12.2", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
		
		output$am.graph.12 <- renderUI({ HTML(gettt("am.graph.12", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
		output$am.graph.12.2 <- renderUI({ HTML(gettt("am.graph.12", a('AIHA',href='https://www.aiha.org',target="_blank"))) })
		
		output$perc.graph.12.3 <- renderUI({
                        HTML( gettt("perc.graph.12.3", a('AIHA',href='https://www.aiha.org',target="_blank")))
                })

		output$back.2 <- renderUI({
                        HTML( gettt("back.2", a('elsewhere',href='http://www.expostats.ca/site/en/info.html',target="_blank")))
                })

							

                #GSD WITHIN
                output$GSDW.1 <-renderText({
                  
                  X <- num.res.between()
                  
                  est <-X$gsdw$est
                  
                  lcl <-X$gsdw$lcl
                  
                  ucl <-X$gsdw$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
                
		
                ##Rappaport ratio 
                
                          #confi interval
                          output$conf.out.ind.4 <-renderText({
                            
                            return(paste(input$conf," %",sep=""))
                            
                          })
                          
                          #95% rappaport estimate
                          output$rap.1 <-renderText({
                            
                            X <- num.res.between()
                            
                            est <-X$Rappap.ratio$est
                            
                            lcl <-X$Rappap.ratio$lcl
                            
                            ucl <-X$Rappap.ratio$ucl
                            
                            return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                            
                          })
                          
                          #proba Rappaport  > 2
                          
                          output$rap.greater2 <-renderText({
                            
                            X <- num.res.between()
                            
                            res <- X$Rappap.ratio.greaterthan2
                            
                            return(paste(signif(res,2),"%",sep=""))
                            
                          })
                          
                          #proba Rappaport  > 10
                          
                          output$rap.greater10 <-renderText({
                            
                            X <- num.res.between()
                            
                            res <- X$Rappap.ratio.greaterthan10
                            
                            return(paste(signif(res,2),"%",sep=""))
                            
                          })
                
                ## OLD STUFF TO ELIMINATE ???
                
                          #confi interval
                          output$conf.out.ind.5 <-renderText({
                            
                            return(paste(input$conf," %",sep=""))
                            
                          })
                          
                 ###Probability that F>5% for one random worker 
                          
                          #confi interval
                          output$conf.out.ind.6 <-renderText({
                            
                            return(paste(input$conf," %",sep=""))
                            
                          })
                          
                          #probability that F>5% for one random worker 
                          output$ProbFracInd <-renderText({
                            
                            X <- num.res.between()
                            
                            est <-X$ind.frac.risk$est
                            
                            lcl <-X$ind.frac.risk$lcl
                            
                            ucl <-X$ind.frac.risk$ucl
                            
                            return(paste(signif(est,2),"% [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                            
                          })
                          
                          
                          #proba that (probability that F>5% for one random worker) > 30%
                          
                          output$ProbFracInd.greater <-renderText({
                            
                            X <- num.res.between()
                            
                            res <- X$ind.frac.risk.proboverX
                            
                            return(paste(signif(res,2),"%",sep=""))
                            
                          })
  
                          
                          
                          
  ######### EXCEEEDANCE   -   Individual worker statistics
                          
   
                          
                                #confi interval
                                output$conf.out.ind.7 <-renderText({
                                  
                                  return(paste(input$conf," %",sep=""))
                                  
                                })
                          
                                ##gm point estimate + confidence intervall
                                
                                output$gm.ind.1 <-renderText({
                                  
                                  X <- num.res.worker()
                                  
                                  est <-X$gm$est
                                  
                                  lcl <-X$gm$lcl
                                  
                                  ucl <-X$gm$ucl
                                  
                                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                                  
                                  
                                })
                                
                                
                  ####### HTML STUFF FOR DM TO COMMENT /CLEAN
                                
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

                output$gsd1.perc <- renderText({
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
                                
                  
                                #confi interval
                                output$conf.out.ind.8 <-renderText({
                                  
                                  return(paste(input$conf," %",sep=""))
                                  
                                })
                                
                                ##gsd within point estimate + confidence intervall
                                
                                output$gsd.ind.1 <-renderText({
                                  
                                  X <- num.res.worker()
                                  
                                  est <-X$gsd$est
                                  
                                  lcl <-X$gsd$lcl
                                  
                                  ucl <-X$gsd$ucl
                                  
                                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                                  
                                  
                                })
                                
                                
                                #point estimate
                                output$Frac.worker <-renderText({
                                  
                                  X <- num.res.worker()
                                  
                                  res4 <-X$frac$est
                                  
                                  return(paste(signif(res4,3)," %",sep=""))
                                  
                                })
                                
                                
                                #confi interval
                                output$conf.out.ind.9 <-renderText({
                                  
                                  return(paste(input$conf," %",sep=""))
                                  
                                })
                                
                                #confidence interval
                                output$Frac.worker.ci <-renderText({
                                  
                                  X <- num.res.worker()
                                  
                                  lcl <-X$frac$lcl
                                  ucl <-X$frac$ucl
                                  
                                  return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
                                  
                                })
                                
                                
                                #risk probability
                                output$probrisk.worker <-renderText({
                                  
                                  X <- num.res.worker()
                                  
                                  risk <-X$frac.risk
                                  
                                  return(paste(signif(risk,3)," %",sep=""))
                                  
                                })

                          
                          
  #############  EXCEEDANCE - GROUP GRAPHICs 
                          
                          
                  #############  RISKMETER        
                  
                  output$risquemetre.group.frac <- renderPlot({
                    
                    X <-num.res.group()
                    Y <- user.input()
                    
                    dessinerRisqueMetre(actualProb=X$frac.risk, 
                                        minProbUnacceptable=Y$psi, 
                                        colorProb="darkblue",
                                        actualProb2=NULL, 
                                        colorProb2="#4863A0")
                    
                          })        
                          
                          
            ############# CALENDAR PLOT
  
  
                output$calendarplot <- renderPlot({
                  
                  X <- num.res.group()
                  
                  graph1 <- calendar.plot(frac.est=X$frac$est,
                                          calplot.1=gett("calplot.1"),
                                          calplot.2=gett("calplot.2"),
                                          calplot.3=gett("calplot.3"))
                  
                  print(graph1)
                  
                })
  
          #############  SEQUENTIAL PLOT
  
  
                output$seqplot.frac <- renderPlot({
                  
                  X <- num.res.group()
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
                
                
                #############   DISTRIBUTION PLOT
                
                
                output$distplot.frac <- renderPlot({
                  
                  
                  X <- num.res.group()
                  Y <- formatted.sample()
                  Z <- bayesian.analysis()
                  
                  ### exceedance chain and best estimate
                  
                  frac.dep.vec <-100*(1-pnorm((log(Y$c.oel)-Z$mu)/Z$stot))
                  
                  frac.dep.est <-X$frac$est
                  
                  ###vectors for the graphical iterations
                  
                  constrainedmu <-Z$mu[Z$mu<quantile(Z$mu,0.9) &
                                               Z$mu>quantile(Z$mu,0.1)]
                  
                  constrainedsigma <-Z$stot[Z$stot<quantile(Z$stot,0.9) &
                                                     Z$stot>quantile(Z$stot,0.1)]
                  
                  
                  ###iteration corresponding to the best estimate
                  
                  medmu <-Z$mu[which(abs(frac.dep.vec-frac.dep.est)==min(abs(frac.dep.vec-frac.dep.est)))][1]
                  
                  medsigma<-Z$stot[which(abs(frac.dep.vec-frac.dep.est)==min(abs(frac.dep.vec-frac.dep.est)))][1]  
                  
                  ###vector of mu and sigma for the graphical iterations
                  
                  vec.mu <-c(medmu,sample(constrainedmu,size=48),medmu)
                  vec.sigma <-c(medsigma,sample(constrainedsigma,size=48),medsigma)
                  
                  
                  
                  ####graph
                  
                  mu <-vec.mu[input$iter.frac]
                  sigma <-vec.sigma[input$iter.frac]
                  
                  frac.dep.graph <-100*(1-pnorm((log(Y$c.oel)-mu)/sigma))
                  
                  
                  graph4 <- distribution.plot.frac(gm=exp(mu) , 
                                                   gsd=exp(sigma)  ,
                                                   frac=frac.dep.graph , 
                                                   c.oel=Y$c.oel,
                                                   distplot.1=gett("distplot.1"),
                                                   distplot.2=gett("distplot.2"),
                                                   distplot.3=gett("distplot.3"),
                                                   distplot.4=gett("distplot.4"),
                                                   distplot.5=gett("distplot.5"))
                  
                  suppressWarnings(print(graph4))
                  
              
                
                  
                })
                
                #############  RISKBAND PLOT
                
                
                output$riskband.frac <- renderPlot({
                  
                  X <- formatted.sample()
                  Y <- bayesian.analysis()
                  Z <- user.input()
                  
                  
                  graph8 <-riskband.plot.frac(mu.chain=Y$mu,
                                              sigma.chain=Y$stot,
                                              c.oel=X$c.oel,
                                              psi=Z$psi,
                                              frac_threshold = Z$frac_threshold,
                                              riskplot.1=gett("riskplot.1"),
                                              riskplot.2=gett("riskplot.2"))
                  
                  suppressWarnings(print(graph8))
                })

  
  
                  
                  
 ######################  EXCEEDANCE BETWEEN WORKER GRAPHICS 
                  
         
        ###### WORKERS BOXPLOT
  
                        output$worker.boxplot.1 <- renderPlot({
                          
                          X <- formatted.sample()
                          
                          Y <- data.imputed()
                          
                          Z <- bayesian.analysis()
                          
                          W <- ListOfWorkers.full()
                          
                          p1 <-boxplot.by.worker(data.formatted = X , 
                                            data.simply.imputed = Y , 
                                            bayesian.output.B = Z , 
                                            worker.list = W,
                                            boxplot.work.1=gett("boxplot.work.1"),
                                            boxplot.work.2=gett("boxplot.work.2"),
                                            boxplot.work.3=gett("boxplot.work.3"))
                          
                          suppressWarnings(print(p1))
                        
                        }) 
                        
                        
          ############# INDIVIDUAL RISK BAND PLOTS
                        
                        
                        output$riskband.frac.ind <- renderPlot({
                          
                          X <- formatted.sample()
                          
                          Y <- user.input()
                          
                          Z <- bayesian.analysis()
                          
                          p1 <- individual.riskband.plot.frac(mu.chain=Z$mu,
                                                        sw.chain=Z$sw,
                                                        sb.chain=Z$sb,
                                                        c.oel=X$c.oel,
                                                        frac_threshold = Y$frac_threshold,
                                                        psi = input$frac_threshold_ind,
                                                        riskplot.work.1=gett("riskplot.work.1"),
                                                        riskplot.work.2=gett("riskplot.work.2"))
                          
                          suppressWarnings(print(p1))
                        })
                        
                        
                        
                        
                        
                        
                        
                        
                        
################################# EXCEEDANCE  - individual worker graphics
                        
                        
        #############  RISKMETER        
                        
                        output$risquemetre.ind.frac <- renderPlot({
                          
                          X <-num.res.worker()
                          Y <- user.input()
                          
                          
                          dessinerRisqueMetre(actualProb=X$frac.risk, 
                                              minProbUnacceptable=Y$psi, 
                                              colorProb="darkblue",
                                              actualProb2=NULL, 
                                              colorProb2="#4863A0")
                        })        
                        
                        
       ##### SEQUENTIAL PLOT
                        
                        
           output$seqplot.frac.worker <- renderPlot({
                          
               X <- num.res.worker()
               Y <- formatted.sample()
                          
               ##imputation for selected worker
                          
              Y.worker <- select.worker.formatted(data.formatted= Y ,
                                                              worker.id =input$workers.full)
                          
              data.imputed.worker <-simple.censored.treatment(observations.formatted=Y.worker$data$x,
                                                                          notcensored=Y.worker$notcensored,
                                                                          leftcensored=Y.worker$leftcensored,
                                                                          rightcensored=Y.worker$rightcensored,
                                                                          intcensored=Y.worker$intcensored)
                          
                          
              Z <- data.imputed.worker
                          
                          
                          graph2 <- sequential.plot.frac(gm=X$gm$est , 
                                                         gsd=X$gsd$est ,
                                                         frac=X$frac$est , 
                                                         c.oel=Y.worker$c.oel ,
                                                         data.simply.imputed=Z,
                                                         seqplot.1=gett("seqplot.1"),
                                                         seqplot.2=gett("seqplot.2"),
                                                         seqplot.6=gett("seqplot.6"))
                          
                          suppressWarnings(print(graph2))
                          
                        })
                        
                        
  
                        
          ##### RISKBAND PLOT
                        
                        
                        output$riskband.frac.worker <- renderPlot({
                          
                          X <- bayesian.analysis()
                          
                          Y <- formatted.sample()
                          
                          Z <- user.input()
                          
                          mu.chain.worker <- X$mu.workers[X$worker.index$numberinoutput[X$worker.index$worker==input$workers.full],]
                          
                          sigma.chain.worker <-X$sw
                          
                          
                          graph8 <-riskband.plot.frac(mu.chain=mu.chain.worker,
                                                      sigma.chain=sigma.chain.worker,
                                                      c.oel=Y$c.oel,
                                                      frac_threshold = Z$frac_threshold,
                                                      psi=Z$psi,
                                                      riskplot.1=gett("riskplot.1"),
                                                      riskplot.2=gett("riskplot.2"))
                          
                          suppressWarnings(print(graph8))
                          
                        })
                        
                        
                          
  #
  #
  #
  #
  #
  #
  #
  #
  #  ########################################Bayesian analysis output - 95th percentile
  
  #
  #
  #
  #
  #
  #
  #
  #
  

  ################### 95th percentile - GROUP statistics                      
                                  
                                    
            #point estimate
            output$Perc <-renderText({
              
              X <- num.res.group()
              
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
            
            
            #confidence interval
            output$Perc.ci <-renderText({
              
              X <- num.res.group()
              
              lcl <-X$perc$lcl
              
              ucl <-X$perc$ucl
              
              return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
              
            })
            
            #confidence limit
            output$Perc.ucl <-renderText({
              
              X <- num.res.group()
              
              ucl <-X$perc$ucl
              
              return(paste(signif(ucl,3)))
              
            })
            
            
            #risk probability
            output$probrisk.perc <-renderText({
              
              X <- num.res.group()
              
              risk <-X$perc.risk
              
              return(paste(signif(risk,3),"%",sep=""))
              
            })
            
            #risk decision
            output$finalrisk.perc <-renderText({
              X <- num.res.group()
              return(calcFinalRisk(X$perc.risk))
            })
            output$finalrisk.perc.ind <-renderText({
              X <-num.res.worker.perc()
              return(calcFinalRisk(X$perc.risk))
            })
            
            ##gm point estimate + confidence intervall
            
            output$gm2 <-renderText({
              
              X <- num.res.group()
              
              est <-X$gm$est
              
              lcl <-X$gm$lcl
              
              ucl <-X$gm$ucl
              
              return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
              
              
            })
            
            #confidence level
            output$conf.out.98 <-renderText({
              
              return(paste(input$conf," %",sep=""))
              
            })
            
            ##gsd point estimate + confidence intervall
            
            output$gsd2 <-renderText({
              
              X <- num.res.group()
              
              est <-X$gsd$est
              
              lcl <-X$gsd$lcl
              
              ucl <-X$gsd$ucl
              
              return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
              
            })   
            
            #confidence level
            output$conf.out.10 <-renderText({
              
              return(paste(input$conf," %",sep=""))
              
            })
            
  
  
################### 95th percentile - BEETWEEN WORKER STATISTICS                      
  
                
                #confi interval
                output$conf.out.ind.13 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                #rho
                output$rho.2 <-renderText({
                  
                  X <- num.res.between()
                  
                  est <-X$rho$est
                  
                  lcl <-X$rho$lcl
                  
                  ucl <-X$rho$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
                
                #rho prob >20
                
                output$rho.greater0.2.2 <-renderText({
                  
                  X <- num.res.between()
                  
                  res <- X$rho.greaterthan
                  
                  return(paste(signif(res,2),"%",sep=""))
                  
                })
                
                #confi interval
                output$conf.out.ind.12 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                #GSD between
                output$GSDB.2 <-renderText({
                  
                  X <- num.res.between()
                  
                  est <-X$gsdb$est
                  
                  lcl <-X$gsdb$lcl
                  
                  ucl <-X$gsdb$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
                
                #confi interval
                output$conf.out.ind.11 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                #GSD WITHIN
                output$GSDW.2 <-renderText({
                  
                  X <- num.res.between()
                  
                  est <-X$gsdw$est
                  
                  lcl <-X$gsdw$lcl
                  
                  ucl <-X$gsdw$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
                
                ##Rappaport ratio 
                
                #confi interval
                output$conf.out.ind.14 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                #rappaport estimate
                output$rap.2 <-renderText({
                  
                  X <- num.res.between()
                  
                  est <-X$Rappap.ratio$est
                  
                  lcl <-X$Rappap.ratio$lcl
                  
                  ucl <-X$Rappap.ratio$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
                #proba Rappaport  > 2
                
                output$rap.greater2.2 <-renderText({
                  
                  X <- num.res.between()
                  
                  res <- X$Rappap.ratio.greaterthan2
                  
                  return(paste(signif(res,2),"%",sep=""))
                  
                })
                
                #proba Rappaport > 10
                
                output$rap.greater10.2 <-renderText({
                  
                  X <- num.res.between()
                  
                  res <- X$Rappap.ratio.greaterthan10
                  
                  return(paste(signif(res,2),"%",sep=""))
                  
                })
                
                #### OLD STUFF TO ELIMINATE ???
                
                #confi interval
                output$conf.out.ind.15 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                
                ###Probability that perc>OEL for one random worker 
                
                #confi interval
                output$conf.out.ind.10 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                #probability that perc>OEL for one random worker 
                output$ProbPpercInd <-renderText({
                  
                  X <- num.res.between()
                  
                  est <-X$ind.perc.risk$est
                  
                  lcl <-X$ind.perc.risk$lcl
                  
                  ucl <-X$ind.perc.risk$ucl
                  
                  return(paste(signif(est,2),"% [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                })
                
                
                #proba that (probability that Pperc>OEL for one random worker) >30%
                
                output$ProbPpercInd.greater <-renderText({
                  
                  X <- num.res.between()
                  
                  res <- X$ind.perc.risk.proboverX
                  
                  return(paste(signif(res,2),"%",sep=""))
                  
                })
                
                
  
  ################### 95th percentile - INDIVIDUAL WORKER STATISTICS                      
  
                
                
                ####stats
                
                #confi interval
                output$conf.out.ind.16 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                ##gm point estimate + confidence intervall
                
                output$gm.ind.2 <-renderText({
                  
                  X <- num.res.worker.perc()
                  
                  est <-X$gm$est
                  
                  lcl <-X$gm$lcl
                  
                  ucl <-X$gm$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                  
                })
                
                
                #confi interval
                output$conf.out.ind.17 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                ##gsd within point estimate + confidence intervall
                
                output$gsd.ind.2 <-renderText({
                  
                  X <- num.res.worker.perc()
                  
                  est <-X$gsd$est
                  
                  lcl <-X$gsd$lcl
                  
                  ucl <-X$gsd$ucl
                  
                  return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
                  
                  
                })
                
                
                #point estimate
                output$Pperc.worker <-renderText({
                  
                  X <- num.res.worker.perc()
                  
                  res4 <-X$perc$est
                  
                  return(paste(signif(res4,3)))
                  
                  
                })
                
                
                #confi interval
                output$conf.out.ind.18 <-renderText({
                  
                  return(paste(input$conf," %",sep=""))
                  
                })
                
                #confidence interval
                output$Pperc.worker.ci <-renderText({
                  
                  X <- num.res.worker.perc()
                  
                  lcl <-X$perc$lcl
                  ucl <-X$perc$ucl
                  
                  return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
                  
                })
                
                
                #risk probability
                output$probrisk.worker.2 <-renderText({
                  
                  X <- num.res.worker.perc()
                  
                  risk <-X$perc.risk
                  
                  return(paste(signif(risk,3),"%",sep=""))
                  
                })
                
 
  
  ################## 95th PERCENTILE  -  GROUP GRAPHICS
  
            
                #############  RISKMETER        
                
                output$risquemetre.group.pPerc <- renderPlot({
                  
                  X <-num.res.group()
                  Y <- user.input()
                  
                  dessinerRisqueMetre(actualProb=X$perc.risk, 
                                      minProbUnacceptable=Y$psi, 
                                      colorProb="darkblue",
                                      actualProb2=NULL, 
                                      colorProb2="#4863A0")
                })        
                
                 
                #############graphique de type SEQUENTIEL
            
            
            output$seqplot.perc <- renderPlot({
              
              X <- num.res.group()
              Y <- formatted.sample()
              Z <- data.imputed()
              W <- user.input()
              
              
              graph2 <- sequential.plot.perc(gm=X$gm$est , 
                                             gsd=X$gsd$est ,
                                             perc=X$perc$est , 
                                             c.oel=Y$c.oel ,
                                             data.simply.imputed=Z,
                                             target_perc = W$target_perc,
                                             seqplot.1=gett("seqplot.1"),
                                             seqplot.3=gett("seqplot.3"),
                                             seqplot.4=gett("seqplot.4"),
                                             seqplot.6 = gett("seqplot.6"))
              
              suppressWarnings(print(graph2))
              
            })
            
            
            #############graphique de type distribution
            
            
            output$distplot.perc <- renderPlot({
              
              X <- num.res.group()
              Y <- formatted.sample()
              Z <- bayesian.analysis()
              W <- user.input()
              
              perc.vec <-exp(Z$mu + qnorm(W$target_perc/100)*Z$stot)
              
              perc.est <-X$perc$est
              
              ###vectors for the graphical iterations
              
              constrainedmu <-Z$mu[Z$mu<quantile(Z$m,0.9) &
                                           Z$mu>quantile(Z$mu,0.1)]
              
              constrainedsigma <-Z$stot[Z$stot<quantile(Z$stot,0.9) &
                                                 Z$stot>quantile(Z$stot,0.1)]
              
              
              ###iteration corresponding to the best estimate
              
              medmu <-Z$mu[which(abs(perc.vec-perc.est)==min(abs(perc.vec-perc.est)))][1]
              
              medsigma<-Z$stot[which(abs(perc.vec-perc.est)==min(abs(perc.vec-perc.est)))][1]  
              
              ###vector of mu and sigma for the graphical iterations
              
              vec.mu <-c(medmu,sample(constrainedmu,size=48),medmu)
              vec.sigma <-c(medsigma,sample(constrainedsigma,size=48),medsigma)
              
              ####graph
              
              mu <-vec.mu[input$iter.perc]
              sigma <-vec.sigma[input$iter.perc]
              
              perc.graph <-exp(mu+qnorm(W$target_perc/100)*sigma)
              
              
              graph4 <- distribution.plot.perc(gm=exp(mu) , 
                                               gsd=exp(sigma)  ,
                                               perc=perc.graph , 
                                               target_perc=W$target_perc ,
                                               c.oel=Y$c.oel,
                                               distplot.1=gett("distplot.1"),
                                               distplot.2=gett("distplot.2"),
                                               distplot.4=gett("distplot.4"),
                                               distplot.5=gett("distplot.5"),
                                               distplot.6=gett("distplot.6"))
              
              suppressWarnings(print(graph4))
              
            })
            
            
            
            ############# RISKBAND GRAPH
            
            
            output$riskband.perc <- renderPlot({
              
              X <- formatted.sample()
              Y <- bayesian.analysis()
              Z <- user.input()
              
              
              graph8 <-riskband.plot.perc(mu.chain=Y$mu,
                                          sigma.chain=Y$stot,
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
            
  
 ################## 95th PERCENTILE  -  BETWEEN WORKER GRAPHICS
            
           
            
            ###### WORKERS BOXPLOT
            
            output$worker.boxplot.2 <- renderPlot({
              
              X <- formatted.sample()
              
              Y <- data.imputed()
              
              Z <- bayesian.analysis()
              
              W <- ListOfWorkers.full()
              
              p1 <-boxplot.by.worker(data.formatted = X , 
                                     data.simply.imputed = Y , 
                                     bayesian.output.B = Z , 
                                     worker.list = W,
                                     boxplot.work.1=gett("boxplot.work.1"),
                                     boxplot.work.2=gett("boxplot.work.2"),
                                     boxplot.work.3=gett("boxplot.work.3")
                                     )
              
              suppressWarnings(print(p1))
              
            }) 
            

           ############# INDIVIDUAL RISK BAND PLOTS
            
            
            output$riskband.pPerc.ind <- renderPlot({
              
              X <- formatted.sample()
              
              Y <- user.input()
              
              Z <- bayesian.analysis()
              
              p1 <- individual.riskband.plot.perc(mu.chain=Z$mu,
                                                  sw.chain=Z$sw,
                                                  sb.chain=Z$sb,
                                                  c.oel=X$c.oel,
                                                 target_perc = Y$target_perc,
                                                  psi = input$perc_threshold_ind,
                                                 riskplot.work.1=gett("riskplot.work.1"),
                                                 riskplot.work.2=gett("riskplot.work.2"))
              
              suppressWarnings(print(p1))
            })
            
            
            
   ############################## 95th percentile - individual worker graphics
            
            
            #############  RISKMETER        
            
            output$risquemetre.ind.pPerc <- renderPlot({
              
              X <-num.res.worker.perc()
              Y <- user.input()
              
              
              dessinerRisqueMetre(actualProb=X$perc.risk, 
                                  minProbUnacceptable=Y$psi, 
                                  colorProb="darkblue",
                                  actualProb2=NULL, 
                                  colorProb2="#4863A0")
            })        
            
            
            
            
            #####SEQUENTIAL PLOT
            
            
            output$seqplot.pPerc.worker <- renderPlot({
              
              
              X <- num.res.worker.perc()
              Y <- formatted.sample()
              W <- user.input()

              ##imputation for selected worker
              
              Y.worker <- select.worker.formatted(data.formatted= Y ,
                                                  worker.id = input$workers.full.pPerc)
              
              data.imputed.worker <-simple.censored.treatment(
                observations.formatted=Y.worker$data$x,                                                              notcensored=Y.worker$notcensored,
                leftcensored=Y.worker$leftcensored,
                rightcensored=Y.worker$rightcensored,
                intcensored=Y.worker$intcensored)
              
              
              Z <- data.imputed.worker
              
              
              graph2 <- sequential.plot.perc(gm=X$gm$est , 
                                             gsd=X$gsd$est ,
                                             perc=X$perc$est , 
                                             c.oel=Y.worker$c.oel ,
                                             data.simply.imputed=Z,
                                             target_perc = W$target_perc,
                                             seqplot.1=gett("seqplot.1"),
                                             seqplot.3=gett("seqplot.3"),
                                             seqplot.4=gett("seqplot.4"),
                                             seqplot.6 = gett("seqplot.6"))
              
              suppressWarnings(print(graph2))
              
            })
            
            
            
            
            #####RISKBAND PLOT
            
            
            output$riskband.pPerc.worker <- renderPlot({
              
              X <- bayesian.analysis()
              
              Y <- formatted.sample()
              
              Z <- user.input()
              
              mu.chain.worker <- X$mu.workers[X$worker.index$numberinoutput[X$worker.index$worker==input$workers.full.pPerc],]
              
              sigma.chain.worker <-X$sw
              
              
              graph8 <-riskband.plot.perc(mu.chain=mu.chain.worker,
                                          sigma.chain=sigma.chain.worker,
                                          c.oel=Y$c.oel,
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
            
            
            
            
            
            
            
  #
  #
  #          
  #
  #######################################Bayesian analysis output - ARITHMETIC MEAN
  #
  #        
  #         
  #
  #
  #
  
            
            
############################# ARITHMETIC MEAN - GROUP STATISTICS            
            
                            
        #point estimate
        output$AM <-renderText({
          
          X <- num.res.group()
          
          res4<-X$am$est
          
          return(paste(signif(res4,3)))
          
        })
        
        #confidence level
        output$conf.out.5 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        
        output$conf.out.6 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        
        #confidence interval
        output$AM.ci <-renderText({
          
          X <- num.res.group()
          
          lcl <-X$am$lcl
          
          ucl <-X$am$ucl
          
          return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
          
        })
        
        #confidence limit
        output$AM.ucl <-renderText({
          
          X <- num.res.group()
          
          ucl <-X$am$ucl
          
          return(paste(signif(ucl,3)))
          
        })
        
        
        #risk probability
        output$probrisk.AM <-renderText({
          
          X <- num.res.group()
          
          risk <-X$am.risk
          
          return(paste(signif(risk,3),"%",sep=""))
          
        })
        
        #risk decision
        output$finalrisk.AM <-renderText({
          X <- num.res.group()
          return(calcFinalRisk(X$am.risk))
        })
        output$finalrisk.am.ind <-renderText({
          X <-num.res.worker.am()
          return(calcFinalRisk(X$am.risk))
        })
        
        ##gm point estimate + confidence intervall
        
        output$gm3 <-renderText({
          
          X <- num.res.group()
          
          est <-X$gm$est
          
          lcl <-X$gm$lcl
          
          ucl <-X$gm$ucl
          
          return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
          
        })
        
        #confidence level
        output$conf.out.11 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        ##gsd point estimate + confidence intervall
        
        output$gsd3 <-renderText({
          
          X <- num.res.group()
          
          est <-X$gsd$est
          
          lcl <-X$gsd$lcl
          
          ucl <-X$gsd$ucl
          
          return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
        })
        
        #confidence level
        output$conf.out.12 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
  
  ############################# ARITHMETIC MEAN - BETWEEN WORKER  STATISTICS            
  
  
        
        #confi interval
        output$conf.out.ind.23 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        #rho
        output$rho.3 <-renderText({
          
          X <- num.res.between()
          
          est <-X$rho$est
          
          lcl <-X$rho$lcl
          
          ucl <-X$rho$ucl
          
          return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
        })
        
        
        #rho prob >20
        
        output$rho.greater0.2.3 <-renderText({
          
          X <- num.res.between()
          
          res <- X$rho.greaterthan
          
          return(paste(signif(res,2),"%",sep=""))
          
        })
        
        #confi interval
        output$conf.out.ind.22 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        #GSD between
        output$GSDB.3 <-renderText({
          
          X <- num.res.between()
          
          est <-X$gsdb$est
          
          lcl <-X$gsdb$lcl
          
          ucl <-X$gsdb$ucl
          
          return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
        })
        
        
        #confi interval
        output$conf.out.ind.21 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        #GSD WITHIN
        output$GSDW.3 <-renderText({
          
          X <- num.res.between()
          
          est <-X$gsdw$est
          
          lcl <-X$gsdw$lcl
          
          ucl <-X$gsdw$ucl
          
          return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
        })
        
        
        ##Rappaport ratio (95%)
        
        #confi interval
        output$conf.out.ind.24 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        #rappaport estimate
        output$rap.3 <-renderText({
          
          X <- num.res.between()
          
          est <-X$Rappap.ratio$est
          
          lcl <-X$Rappap.ratio$lcl
          
          ucl <-X$Rappap.ratio$ucl
          
          return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
        })
        
        #proba Rappaport > 2
        
        output$rap.greater2.3 <-renderText({
          
          X <- num.res.between()
          
          res <- X$Rappap.ratio.greaterthan2
          
          return(paste(signif(res,2),"%",sep=""))
          
        })
        
        #proba Rappaport  > 10
        
        output$rap.greater10.3 <-renderText({
          
          X <- num.res.between()
          
          res <- X$Rappap.ratio.greaterthan10
          
          return(paste(signif(res,2),"%",sep=""))
          
        })
        
        ## OLD STUFF TO ELIMINATE ???
        
        #confi interval
        output$conf.out.ind.25 <-renderText({
          
          return(paste(input$conf," %",sep=""))
        })
        
        ###Probability that Pperc>OEL for one random worker 
        
        #confi interval
        output$conf.out.ind.19 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        #probability that AM>OEL for one random worker 
        output$ProbAMInd <-renderText({
          
          X <- num.res.between()
          
          est <-X$ind.am.risk$est
          
          lcl <-X$ind.am.risk$lcl
          
          ucl <-X$ind.am.risk$ucl
          
          return(paste(signif(est,2),"% [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
        })
        
        
        #proba that (probability that AM>OEL for one random worker) >30%
        
        output$ProbAMInd.greater <-renderText({
          
          X <- num.res.between()
          
          res <- X$ind.am.risk.proboverX
          
          return(paste(signif(res,2),"%",sep=""))
          
        })
        
        
  
  ############################# ARITHMETIC MEAN - INDIVIDUAL  WORKER  STATISTICS            
        
        
        #confi interval
        output$conf.out.ind.26 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        ##gm point estimate + confidence intervall
        
        output$gm.ind.3 <-renderText({
          
          X <- num.res.worker.am()
          
          est <-X$gm$est
          
          lcl <-X$gm$lcl
          
          ucl <-X$gm$ucl
          
          return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
          
        })
        
        
        #confi interval
        output$conf.out.ind.27 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        ##gsd within point estimate + confidence intervall
        
        output$gsd.ind.3 <-renderText({
          
          X <- num.res.worker.am()
          
          est <-X$gsd$est
          
          lcl <-X$gsd$lcl
          
          ucl <-X$gsd$ucl
          
          return(paste(signif(est,2)," [ ",signif(lcl,2), " - ",signif(ucl,2)," ]"))
          
          
        })
        
        
        #point estimate
        output$am.worker <-renderText({
          
          X <- num.res.worker.am()
          
          res4 <-X$am$est
          
          return(paste(signif(res4,3)))
          
          
        })
        
        
        #confi interval
        output$conf.out.ind.28 <-renderText({
          
          return(paste(input$conf," %",sep=""))
          
        })
        
        #confidence interval
        output$am.worker.ci <-renderText({
          
          X <- num.res.worker.am()
          
          lcl <-X$am$lcl
          ucl <-X$am$ucl
                        
          
          return(paste("[ ",signif(lcl,3)," - ",signif(ucl,3)," ]",sep=""))
          
        })
        
        
        #risk probability
        output$probrisk.worker.3 <-renderText({
          
          X <- num.res.worker.am()
          
          risk <-X$am.risk
          
          return(paste(signif(risk,3),"%",sep=""))
          
        })
        
        

  ############################# ARITHMETIC MEAN - GROUP GRAPHICS           
        
        #############  RISKMETER        
        
        output$risquemetre.group.am <- renderPlot({
          
          X <-num.res.group()
          Y <- user.input()
          
          dessinerRisqueMetre(actualProb=X$am.risk, 
                              minProbUnacceptable=Y$psi, 
                              colorProb="darkblue",
                              actualProb2=NULL, 
                              colorProb2="#4863A0")
          
        })        
        
        
        
              #############SEQUENTIAL PLOT
              
              
              output$seqplot.AM <- renderPlot({
                
                X <- num.res.group()
                Y <- formatted.sample()
                Z <- data.imputed()
                
                
                graph2 <- sequential.plot.am(gm=X$gm$est , 
                                               gsd=X$gsd$est ,
                                              am=X$am$est , 
                                               c.oel=Y$c.oel ,
                                               data.simply.imputed=Z,
                                             seqplot.1=gett("seqplot.1"),
                                             seqplot.3=gett("seqplot.3"),
                                             seqplot.5=gett("seqplot.5"),
                                             seqplot.6=gett("seqplot.6"))
                
                suppressWarnings(print(graph2))
                
              })
              
              
              ############# DISTRIBUTION PLOT
              
              
              output$distplot.AM <- renderPlot({
                
                X <- num.res.group()
                Y <- formatted.sample()
                Z <- bayesian.analysis()
                W <- user.input()
                
                ### exceedance chain and best estimate
                
                am.vec <-exp(Z$mu + 0.5*Z$stot^2)
                
                am.est <-X$am$est
                
                ###vectors for the graphical iterations
                
                constrainedmu <-Z$mu[Z$mu<quantile(Z$mu,0.9) &
                                             Z$mu>quantile(Z$mu,0.1)]
                
                constrainedsigma <-Z$stot[Z$stot<quantile(Z$stot,0.9) &
                                                   Z$stot>quantile(Z$stot,0.1)]
                
                
                ###iteration corresponding to the best estimate
                
                medmu <-Z$mu[which(abs(am.vec-am.est)==min(abs(am.vec-am.est)))][1]
                
                medsigma<-Z$stot[which(abs(am.vec-am.est)==min(abs(am.vec-am.est)))][1]  
                
                ###vector of mu and sigma for the graphical iterations
                
                vec.mu <-c(medmu,sample(constrainedmu,size=48),medmu)
                vec.sigma <-c(medsigma,sample(constrainedsigma,size=48),medsigma)
                
                ####graph
                
                mu <-vec.mu[input$iter.am]
                sigma <-vec.sigma[input$iter.am]
                
                am.graph <-exp(mu+0.5*sigma^2)
                
                
                graph4 <- distribution.plot.am(gm=exp(mu) , 
                                               gsd=exp(sigma)  ,
                                               am=am.graph , 
                                               c.oel=Y$c.oel,
                                               distplot.1=gett("distplot.1"),
                                               distplot.2=gett("distplot.2"),
                                               distplot.4=gett("distplot.4"),
                                               distplot.5=gett("distplot.5"),
                                               distplot.7=gett("distplot.7"))
                
                suppressWarnings(print(graph4))
                
              })
              
              
              ############# RISKBAND
              
              
              output$riskband.am <- renderPlot({
                
                X <- formatted.sample()
                Y <- bayesian.analysis()
                Z <- user.input()
                
                
                graph8 <-riskband.plot.am(mu.chain=Y$mu,
                                            sigma.chain=Y$stot,
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
              
              
              
        


############################# ARITHMETIC MEAN - BETWEEN WORKER GRAPHICS           


###### WORKERS BOXPLOT

            output$worker.boxplot.3 <- renderPlot({
            
              X <- formatted.sample()
              
              Y <- data.imputed()
              
              Z <- bayesian.analysis()
              
              W <- ListOfWorkers.full()
              
              p1 <-boxplot.by.worker(data.formatted = X , 
                                     data.simply.imputed = Y , 
                                     bayesian.output.B = Z , 
                                     worker.list = W,
                                     boxplot.work.1=gett("boxplot.work.1"),
                                     boxplot.work.2=gett("boxplot.work.2"),
                                     boxplot.work.3=gett("boxplot.work.3"))
              
              suppressWarnings(print(p1))
            
          }) 
          
 
  ############ INDIVIDUAL RISK BAND PLOTS
  
  
        output$riskband.am.ind <- renderPlot({
          
          X <- formatted.sample()
          
          Y <- user.input()
          
          Z <- bayesian.analysis()
          
          p1 <- individual.riskband.plot.am(mu.chain=Z$mu,
                                              sw.chain=Z$sw,
                                              sb.chain=Z$sb,
                                              c.oel=X$c.oel,
                                              psi = input$am_threshold_ind,
                                            riskplot.work.1=gett("riskplot.work.1"),
                                            riskplot.work.2=gett("riskplot.work.2"))
          
          suppressWarnings(print(p1))
        })
        
  

############################# ARITHMETIC MEAN - INDIVIDUAL WORKER GRAPHICS            

        #############  RISKMETER        
        
        output$risquemetre.ind.am <- renderPlot({
          
          X <-num.res.worker.am()
          Y <- user.input()
          
          
          dessinerRisqueMetre(actualProb=X$am.risk, 
                              minProbUnacceptable=Y$psi, 
                              colorProb="darkblue",
                              actualProb2=NULL, 
                              colorProb2="#4863A0")
        })        
        
        
        
        #####SEQUENTIAL PLOT
        
        
        output$seqplot.am.worker <- renderPlot({
          
          
          X <- num.res.worker.am()
          Y <- formatted.sample()
         
          ##imputation for selected worker
          
          Y.worker <- select.worker.formatted(data.formatted= Y ,
                                              worker.id =input$workers.full.am)
          
          data.imputed.worker <-simple.censored.treatment(observations.formatted=Y.worker$data$x,
                                                          notcensored=Y.worker$notcensored,
                                                          leftcensored=Y.worker$leftcensored,
                                                          rightcensored=Y.worker$rightcensored,
                                                          intcensored=Y.worker$intcensored)
          
          
          Z <- data.imputed.worker
          
          
          graph2 <- sequential.plot.am(gm=X$gm$est , 
                                         gsd=X$gsd$est ,
                                         am=X$am$est , 
                                         c.oel=Y.worker$c.oel ,
                                         data.simply.imputed=Z,
                                       seqplot.1=gett("seqplot.1"),
                                       seqplot.3=gett("seqplot.3"),
                                       seqplot.5=gett("seqplot.5"),
                                       seqplot.6=gett("seqplot.6"))
          
          suppressWarnings(print(graph2))
          
        })
        
        
        
        
        #####RISKBAND PLOT
        
        
        
        output$riskband.am.worker <- renderPlot({
          
          X <- bayesian.analysis()
          
          Y <- formatted.sample()
          
          Z <- user.input()
          
          mu.chain.worker <- X$mu.workers[X$worker.index$numberinoutput[X$worker.index$worker==input$workers.full.am],]
          
          sigma.chain.worker <-X$sw
          
          
          graph8 <-riskband.plot.am(mu.chain=mu.chain.worker,
                                      sigma.chain=sigma.chain.worker,
                                      c.oel=Y$c.oel,
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
        
        
        
        

######HTML STUFF TO ANNOTATE BY DM
        
        output$acceptableExpo1 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo2 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo3 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo4 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo5 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo6 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo7 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo8 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo9 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpo10 <-renderText({return(paste(input$frac_threshold,"%",sep="")) })
        output$acceptableExpoDiv10_1 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
        output$acceptableExpoDiv10_2 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
        output$acceptableExpoDiv10_3 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
        output$acceptableExpoDiv10_4 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
        output$acceptableExpoDiv10_5 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
        output$acceptableExpoDiv10_6 <-renderText({return(paste(input$frac_threshold/10,"%",sep="")) })
        
        output$probSituUnacceptable1 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$probSituUnacceptable2 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$probSituUnacceptable3 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$probSituUnacceptable4 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$probSituUnacceptable5 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$probSituUnacceptable6 <-renderText({return(paste(input$psi,"%",sep="")) })
        
        output$indivOverexpThresh1 <-renderText({return(paste(input$frac_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh1.1 <-renderText({return(paste(input$frac_threshold_ind/100,"%",sep="")) })
        output$indivOverexpThresh1.2 <-renderText({return(paste(input$frac_threshold_ind/100,"%",sep="")) })
        output$indivOverexpThresh1.3 <-renderText({return(paste(input$perc_threshold_ind/100,"%",sep="")) })
        output$indivOverexpThresh1.4 <-renderText({return(paste(input$perc_threshold_ind/100,"%",sep="")) })
        output$indivOverexpThresh1.5 <-renderText({return(paste(input$am_threshold_ind/100,"%",sep="")) })
        output$indivOverexpThresh1.6 <-renderText({return(paste(input$am_threshold_ind/100,"%",sep="")) })
        output$indivOverexpThresh2.1 <-renderText({return(paste(input$frac_threshold_ind/10,"%",sep="")) })
        output$indivOverexpThresh2.2 <-renderText({return(paste(input$frac_threshold_ind/10,"%",sep="")) })
        output$indivOverexpThresh2.3 <-renderText({return(paste(input$perc_threshold_ind/10,"%",sep="")) })
        output$indivOverexpThresh2.4 <-renderText({return(paste(input$perc_threshold_ind/10,"%",sep="")) })
        output$indivOverexpThresh2.5 <-renderText({return(paste(input$am_threshold_ind/10,"%",sep="")) })
        output$indivOverexpThresh2.6 <-renderText({return(paste(input$am_threshold_ind/10,"%",sep="")) })
        output$indivOverexpThresh3.1 <-renderText({return(paste(input$frac_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh3.2 <-renderText({return(paste(input$frac_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh3.3 <-renderText({return(paste(input$perc_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh3.4 <-renderText({return(paste(input$perc_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh3.5 <-renderText({return(paste(input$am_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh3.6 <-renderText({return(paste(input$am_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh2 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$indivOverexpThresh3 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$indivOverexpThresh4 <-renderText({return(paste(input$perc_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh4.1 <-renderText({return(paste(input$perc_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh5 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$indivOverexpThresh6 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$indivOverexpThresh7 <-renderText({return(paste(input$am_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh7.1 <-renderText({return(paste(input$am_threshold_ind,"%",sep="")) })
        output$indivOverexpThresh8 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$indivOverexpThresh9 <-renderText({return(paste(input$psi,"%",sep="")) })
        output$indivOverThreshDiv10_1 <-renderText({return(paste(input$psi/10,"%",sep="")) })
        output$indivOverThreshDiv10_2 <-renderText({return(paste(input$psi/10,"%",sep="")) })
        output$indivOverThreshDiv10_3 <-renderText({return(paste(input$psi/10,"%",sep="")) })
        output$indivOverThreshDiv10_4 <-renderText({return(paste(input$psi/10,"%",sep="")) })
        output$indivOverThreshDiv10_5 <-renderText({return(paste(input$psi/10,"%",sep="")) })
        output$indivOverThreshDiv10_6 <-renderText({return(paste(input$psi/10,"%",sep="")) })
        output$indivOverThreshDiv100_1 <-renderText({return(paste(input$psi/100,"%",sep="")) })
        output$indivOverThreshDiv100_2 <-renderText({return(paste(input$psi/100,"%",sep="")) })
        output$indivOverThreshDiv100_3 <-renderText({return(paste(input$psi/100,"%",sep="")) })
        output$indivOverThreshDiv100_4 <-renderText({return(paste(input$psi/100,"%",sep="")) })
        output$indivOverThreshDiv100_5 <-renderText({return(paste(input$psi/100,"%",sep="")) })
        output$indivOverThreshDiv100_6 <-renderText({return(paste(input$psi/100,"%",sep="")) })
        
        output$indivThresh1 <- renderText({return(paste(input$frac_threshold_ind,"%",sep="")) })
        
	percText <- function(perc) {
                rem <- perc %% 10
                if ( rem == 1) {
                  suff <- gett("st")
                } else
                if ( rem == 2 ) {
                  suff <- gett("nd")
                } else
                if ( rem == 3 ) {
                  suff <- gett("rd")
                } else {
                  suff <- gett("th")
                }
                return (paste0(perc, suff, " ", gett("percentile") ) )
              }
        
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
        output$rapRatio1 <-renderText({return(paste(input$rappap_cover,"%",sep="")) })
        output$rapRatio2 <-renderText({return(paste(input$rappap_cover,"%",sep="")) })
        output$rapRatio3 <-renderText({return(paste(input$rappap_cover,"%",sep="")) })
        output$rapRatio4 <-renderText({return(paste(input$rappap_cover,"%",sep="")) })
        output$rapRatio5 <-renderText({return(paste(input$rappap_cover,"%",sep="")) })
        
        output$ww1 <-renderText(return(input$wwct))
        output$ww2 <-renderText(return(input$wwct))
        output$ww3 <-renderText(return(input$wwct))
        
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
        output$locale <-renderText({
          loc <- paste0("LOCALE = ", Sys.getlocale("LC_MESSAGES"))
          return(loc)
        })
        
        output$rho.frac.prob <- renderText({
          paste(gett("frac.21.1"), input$wwct)
        })
        
        output$rho.perc.prob <- renderText({
          paste(gett("frac.21.1"), input$wwct)
        })
        
        output$about.1.t2 <- renderUI({
          HTML(
            gettt("about.1.t2",
                  a(gett("ESPUM"),href='https://www.espum.umontreal.ca',target="_blank"), a("Université de Montréal",href='https://www.umontreal.ca'),target='_blank')
          )
        })
        
        output$about.3 <- renderUI({
          HTML(
            gettt("about.3",
                  a("R",href='http://cran.r-project.org/',target="_blank"),
                  a("Shiny",href='http://www.rstudio.com/shiny/',target='_blank'),
                  a("JAGS",href='http://mcmc-jags.sourceforge.net', target='_blank')
            ))
        })
        
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
        
        calcFinalRisk <- function(risk) {
          Y <- user.input()
          
          if (risk>=Y$psi) return(gett("server.2"))
          
          else return(gett("server.3"))
        }
})





