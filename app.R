# install.packages(c("DT", "ggplot2", "plotly", "bsicons"))
# library(c("DT", "ggplot2", "plotly", "bsicons"))

library(shiny)
library(bslib)
library(DT) 

heart <- readRDS("data/heart.rds")

###############################################################################
ui <- page_sidebar( ###########################################################
  title = tags$span(
    tags$img(src = "logo.png", 
             height = "30px", 
             # filter: invert(1) -> inverts the logo color so it appears white against the dark header.
             style = "margin-right: 10px; filter: invert(1);"), 
    "Heart Attack Dashboard"
  ),
  theme = bs_theme(bootswatch = "pulse"), # gives us a pink/red accent — fitting for a heart attack dashboard.
  sidebar = sidebar(
    "Filters coming soon..."
  ),
  navset_tab(
    nav_panel("Overview", "Overview content coming soon..."),
    nav_panel("Explore", "Explore content coming soon..."),
    # nav_panel("Data", "Data content coming soon...")
    nav_panel("Data", DT::dataTableOutput("data_table"))
    
  )
)

###############################################################################
server <- function(input, output, session) { ##################################
  
  output$data_table <- DT::renderDataTable({
    heart
  })
  
  
  
}


###############################################################################
shinyApp(ui = ui, server = server)


