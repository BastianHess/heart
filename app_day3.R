# install.packages(c("DT", "ggplot2", "plotly", "bsicons"))
# library(c("DT", "ggplot2", "plotly", "bsicons"))

library(shiny)
library(bslib)
library(DT) 
library(ggplot2)
library(plotly)

source("R/helpers.R")
source("R/mod_download_plot.R")

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
  theme = bs_theme(bootswatch = "pulse"), # pulse# gives us a pink/red accent — fitting for a heart attack dashboard.
  # Try changing the theme: replace "pulse" with "flatly", "cosmo", or "superhero" (dark mode).
  
  # # add a dropdown to select the theme
  # shinythemes::themeSelector(),
  sidebar = sidebar(
    selectInput(
      inputId = "outcome",
      label = "Outcome:",
      choices = c("All", "Survived", "Died")
    ),
    selectInput(
      inputId = "diagnosis",
      label = "Diagnosis:",
      choices = c("All", sort(unique(as.character(heart$DIAGNOSIS)))),
      selected = "All"
    ),
    selectInput(
      inputId = "drg",
      label = "DRG:",
      choices = c("All", sort(unique(as.character(heart$DRG)))),
      selected = "All"
    ),
    sliderInput(
      inputId = "age_range",
      label = "Age Range:",
      min = min(heart$AGE),
      max = max(heart$AGE),
      value = c(min(heart$AGE), max(heart$AGE))
    ),
    
    actionButton(
      inputId = "reset",
      label = "Reset",
      icon = bsicons::bs_icon("arrow-counterclockwise")
    )
    
    
  ),
  navset_tab(
    nav_panel("Overview", 
              layout_column_wrap(
                width = 1/2,
                value_box(
                  title = "Female Mortality",
                  value = textOutput("f_mortality"),
                  theme = "danger",
                  showcase = bsicons::bs_icon("gender-female")
                  # The showcase parameter adds an icon to each value box using 
                  # the bsicons package. The bsicons::bs_icon() syntax calls the 
                  # function directly without loading the entire library.
                ),
                value_box(
                  title = "Male Mortality",
                  value = textOutput("m_mortality"),
                  theme = "primary",
                  showcase = bsicons::bs_icon("gender-male")
                ),
                value_box(
                  title = "Overall Mortality",
                  value = textOutput("o_mortality"),
                  theme = "primary",
                  showcase = bsicons::bs_icon("gender-ambiguous")
                )
              ), # end of layout-column wrap
              card(
                card_header("Age Distribution"),
                plotOutput("age_hist"),
                mod_download_plot_ui("dl_age", label = "Download")  # this is the new downloadbutton
              ),
              card(
                card_header("Age Distribution"),
                plotOutput("age_hist_2")
              )
              
              
    ),
    
    
    nav_panel("Explore", 
              plotlyOutput("scatter_plot")),
   
    
    nav_panel("Charges",
              layout_column_wrap(
                width = 1/3,
                
                value_box(
                  title = "Average Charges",
                  value = textOutput("avg_charges"),
                  theme = "warning"
                ),
                
                value_box(
                  title = "Average Length of Stay",
                  value = textOutput("avg_los"),
                  theme = "primary"
                ),
                
                value_box(
                  title = "Average Cost per Day",
                  value = textOutput("cost_per_day"),
                  theme = "success"
                )
              ),
              
              card(
                card_header("Daily Charges Distribution"),
                plotOutput("daily_cost_boxplot", height = "500px")
              )
    ),
     # nav_panel("Data", "Data content coming soon...")
    nav_panel("Data", DT::dataTableOutput("data_table"))
    
  )
)

##############################################################################################################################################################
server <- function(input, output, session) { #################################################################################################################
  
  
  filtered_data <- reactive({
    d <- heart
    if (input$outcome != "All") {
      d <- d[d$DIED == input$outcome, ]
    }
    if (input$diagnosis != "All") {
      d <- d[as.character(d$DIAGNOSIS) == input$diagnosis, ]
    }
    if (input$drg != "All") {
      d <- d[as.character(d$DRG) == input$drg, ]
    }
    d <- d[d$AGE >= input$age_range[1] & d$AGE <= input$age_range[2], ]
    d
  })
  
  financial_data <- reactive({
    d <- filtered_data()
    
    # remove missing charges
    d <- d[!is.na(d$CHARGES), ]
    
    d
  })
  
  daily_cost_data <- reactive({
    d <- financial_data()
    
    # remove bad LOS
    d <- d[d$LOS > 0, ]
    
    # compute cost per day
    d$COST_PER_DAY <- d$CHARGES / d$LOS
    
    # remove invalid values
    d <- d[is.finite(d$COST_PER_DAY), ]
    
    d
  })
  
  output$daily_cost_boxplot <- renderPlot({
    d <- daily_cost_data()
    req(nrow(d) >= 2)
    
    ggplot(d, aes(x = SEX, y = COST_PER_DAY, fill = SEX)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      facet_wrap(~ DRG, scales = "free_y") +
      labs(
        x = "Sex",
        y = "Cost per Day ($)",
        title = "Distribution of Daily Charges"
      ) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 11),
        legend.position = "none"
      )
  })
  
  output$avg_charges <- renderText({
    d <- financial_data()
    req(nrow(d) > 0)
    
    avg <- mean(d$CHARGES)
    paste0("$", format(round(avg, 2), big.mark = ","))
  })
  
  output$avg_los <- renderText({
    d <- filtered_data()
    req(nrow(d) > 0)
    
    avg <- mean(d$LOS)
    paste0(round(avg, 1), " days")
  })
  
  output$cost_per_day <- renderText({
    d <- financial_data()
    d <- d[d$LOS > 0, ]
    req(nrow(d) > 0)
    
    avg <- mean(d$CHARGES / d$LOS)
    paste0("$", format(round(avg, 2), big.mark = ","))
  })
  
  
  
  output$data_table <- DT::renderDataTable({
    filtered_data()
  })
  
  # Female stats
  # output$f_mortality <- renderText({
  #   d <- filtered_data()[filtered_data()$SEX == "Female", ]
  #   paste0(round(100 * sum(d$DIED == "Died") / nrow(d), 1), "%")
  # })
  
  # new; mit ausgelagertem filtern:
  output$f_mortality <- renderText({
    compute_mortality(filtered_data()[filtered_data()$SEX == "Female", ])
  })
  
  
  # # Male stats
  # output$m_mortality <- renderText({
  #   d <- filtered_data()[filtered_data()$SEX == "Male", ]
  #   paste0(round(100 * sum(d$DIED == "Died") / nrow(d), 1), "%")
  # })
  
  # new:
  # Male stats
  output$m_mortality <- renderText({
    compute_mortality(filtered_data()[filtered_data()$SEX == "Male", ])
  })
  
  
  output$o_mortality <- renderText({
    # d <- filtered_data()[filtered_data()$SEX == "Male", ]
    d <- filtered_data()
    paste0(round(100 * sum(d$DIED == "Died") / nrow(d), 1), "%")
  })

  ## split this up into two parts:
  ## The plot code is the same — we just moved it into a reactive() so both
  ## renderPlot and the download module can use it.
  # output$age_hist <- renderPlot({
  #   req(nrow(filtered_data()) >= 2) # prevents the plot from crashing when 
  #   # filters produce too few rows  req() silently stops rendering when its 
  #   # condition is not met.
  #   ggplot(filtered_data(), aes(x = AGE, fill = DIED)) +
  #     geom_density(alpha = 0.5) + # shows a smooth curve of the distribution, making it easy to compare shapes
  #     labs(x = "Age", y = "Density", fill = "DIED") +
  #     facet_wrap(~ SEX) + # creates separate panels for each sex.
  #     theme_minimal() +
  #     theme(
  #       axis.title = element_text(size = 16),
  #       axis.text = element_text(size = 14)
  #     )
  # })
  
  # Create the age plot as a reactive (reusable)
  age_plot <- reactive({
    req(nrow(filtered_data()) >= 2)
    ggplot(filtered_data(), aes(x = AGE, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Age", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  # Display the plot
  output$age_hist <- renderPlot({
    age_plot()
  })
  
  mod_download_plot_server("dl_age", filename = "age_distribution", figure = age_plot)
  
  output$age_hist_2 <- renderPlot({
    req(nrow(filtered_data()) >= 2) # prevents the plot from crashing when 
    # filters produce too few rows  req() silently stops rendering when its 
    # condition is not met.
    ggplot(filtered_data(), aes(x = AGE, fill = DIED)) +
      geom_density(alpha = 0.5) + # shows a smooth curve of the distribution, making it easy to compare shapes
      labs(x = "Age", y = "Density", fill = "SEX") +
     # facet_wrap(~ SEX) + # creates separate panels for each sex.
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  # Challenge. Filter to “Died” only. What do you notice about the length of 
  # stay? Try changing y = LOS to y = CHARGES for a different perspective.
  
  output$scatter_plot <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) >= 1)
    if(nrow(df) > 1000) {
      df <- df[sample(nrow(df), 1000), ] # this is only for the course, so that it renders quickly
    }
 #    browser()  #  to stop and check/look at things
                 #  call: summary(df) and  write.csv(df, "temp.csv")
    p <- ggplot(df, aes(x = AGE, y = LOS, color = SEX)) +
      geom_point(alpha = 0.3) + # this makes overlapping points visible.
      labs(x = "Age", y = "Length of Stay (days)", color = "Sex") +
      geom_smooth(method = "lm", se = FALSE) +
      theme_minimal()
    ggplotly(p)
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "outcome", selected = "All")
    updateSelectInput(session, "diagnosis", selected = "All")
    updateSelectInput(session, "drg", selected = "All")
    updateSliderInput(session, "age_range",
                      value = c(min(heart$AGE), max(heart$AGE)))
  })
  
  
} # End of server function


###############################################################################
shinyApp(ui = ui, server = server)


