.libPaths('vendor/R/library')
Sys.setenv(JAGS_HOME = "vendor/JAGS")
library(rjags)

env <- environment()
if ( is.null(env$toolName) ) {
  env$toolName <- "tool3Fr"
}

toolPath <- paste0('src/tools/', env$toolName)
shiny::runApp(toolPath, port=env$port, launch.browser = 'T')
