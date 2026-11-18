# app/app.R — modern launcher so Shiny always sees ui & server
library(shiny)
source("global.R", local = TRUE)
source("ui.R",     local = TRUE)   # defines object 'ui'
source("server.R", local = TRUE)   # defines object 'server'
shinyApp(ui = ui, server = server)
