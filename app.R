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
                card_header("Sex Comparison Snapshot"),
                tableOutput("sex_snapshot")
              ),
              card(
                card_header("Mortality Rate by Sex"),
                plotOutput("mortality_gap_plot")
              ),
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
    
    
    nav_panel("Length of Stay",
              layout_column_wrap(
                width = 1/2,
                value_box(
                  title = "Female Avg LOS",
                  value = textOutput("f_los_mean"),
                  theme = "danger",
                  showcase = bsicons::bs_icon("gender-female")
                ),
                value_box(
                  title = "Male Avg LOS",
                  value = textOutput("m_los_mean"),
                  theme = "primary",
                  showcase = bsicons::bs_icon("gender-male")
                )
              ),
              card(
                card_header("LOS Distribution"),
                plotOutput("los_hist"),
                mod_download_plot_ui("dl_los", label = "Download")
              ),
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header("LOS by Sex (Boxplot)"),
                  plotOutput("los_by_sex_box")
                ),
                card(
                  card_header("Age by Sex (Boxplot)"),
                  plotOutput("age_by_sex_box")
                )
              )
    ),
    
    
    nav_panel("Explore", 
              plotlyOutput("scatter_plot"),
              card(
                card_header("Top Diagnoses by Sex"),
                plotOutput("diagnosis_by_sex_plot")
              )),
   
    
    
     # nav_panel("Data", "Data content coming soon...")
    nav_panel("Data", DT::dataTableOutput("data_table")),
    nav_panel("About",
              card(
                card_header("About This Dashboard"),
                tags$p("This dashboard explores outcomes, length of stay, and costs for 12,844 heart attack patients treated in New York State in 1993. The data are provided in the app's ", tags$code("data/heart.rds"), " file."),
                tags$p("Key findings on why women have higher heart-attack mortality include:"),
                tags$ul(
                  tags$li("Later age and higher comorbidity burden at presentation, which worsens outcomes."),
                  tags$li("Symptom patterns that may be less classic, contributing to delays in recognition and treatment."),
                  tags$li("Different underlying disease patterns such as MINOCA (myocardial infarction with non-obstructive coronary arteries), which can be harder to diagnose and treat."),
                  tags$li("Worse short-term outcomes in younger women, influenced by both cardiac and non-cardiac factors."),
                  tags$li("An awareness gap that can delay care-seeking and risk recognition.")
                )
              )
    )
    
  )
)

###############################################################################
server <- function(input, output, session) { ##################################
  
  
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

  format_pct <- function(x, digits = 1) {
    if (is.na(x)) return("N/A")
    paste0(round(100 * x, digits), "%")
  }

  format_num <- function(x, digits = 1, suffix = "") {
    if (is.na(x)) return("N/A")
    paste0(round(x, digits), suffix)
  }

  format_currency <- function(x) {
    if (is.na(x)) return("N/A")
    paste0("$", format(round(x, 2), big.mark = ","))
  }

  output$sex_snapshot <- renderTable({
    d <- filtered_data()
    sexes <- c("Female", "Male")
    has_charges <- "CHARGES" %in% names(d)
    rows <- lapply(sexes, function(sex) {
      ds <- d[d$SEX == sex, ]
      n <- nrow(ds)
      mortality <- if (n > 0) sum(ds$DIED == "Died", na.rm = TRUE) / n else NA
      age_med <- if (n > 0) median(ds$AGE, na.rm = TRUE) else NA
      los_med <- if (n > 0) median(ds$LOS, na.rm = TRUE) else NA
      charges_med <- if (n > 0 && has_charges) median(ds$CHARGES, na.rm = TRUE) else NA
      data.frame(
        Sex = sex,
        Patients = n,
        `Mortality Rate` = format_pct(mortality),
        `Median Age` = format_num(age_med),
        `Median LOS` = format_num(los_med, suffix = " days"),
        `Median Charges` = if (has_charges) format_currency(charges_med) else "N/A",
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  }, rownames = FALSE)

  output$mortality_gap_plot <- renderPlot({
    d <- filtered_data()
    req(nrow(d) > 0)
    sexes <- c("Female", "Male")
    mortality <- sapply(sexes, function(sex) {
      ds <- d[d$SEX == sex, ]
      if (nrow(ds) == 0) return(NA_real_)
      sum(ds$DIED == "Died", na.rm = TRUE) / nrow(ds)
    })
    df <- data.frame(
      SEX = factor(sexes, levels = sexes),
      MORTALITY = mortality
    )
    ggplot(df, aes(x = SEX, y = MORTALITY, fill = SEX)) +
      geom_col(alpha = 0.8, width = 0.6, na.rm = TRUE) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      labs(x = "Sex", y = "Mortality Rate") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
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
  
  output$f_los_mean <- renderText({
    d <- filtered_data()[filtered_data()$SEX == "Female", ]
    if (nrow(d) == 0) return("N/A")
    paste0(round(mean(d$LOS, na.rm = TRUE), 1), " days")
  })
  
  output$m_los_mean <- renderText({
    d <- filtered_data()[filtered_data()$SEX == "Male", ]
    if (nrow(d) == 0) return("N/A")
    paste0(round(mean(d$LOS, na.rm = TRUE), 1), " days")
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
  
  # Create the LOS plot as a reactive (reusable)
  los_plot <- reactive({
    req(nrow(filtered_data()) >= 2)
    ggplot(filtered_data(), aes(x = LOS, fill = DIED)) +
      geom_density(alpha = 0.5) +
      labs(x = "Length of Stay (days)", y = "Density", fill = "DIED") +
      facet_wrap(~ SEX) +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
  })
  
  output$los_hist <- renderPlot({
    los_plot()
  })
  
  mod_download_plot_server("dl_los", filename = "los_distribution", figure = los_plot)

  output$los_by_sex_box <- renderPlot({
    d <- filtered_data()
    req(nrow(d) >= 2)
    ggplot(d, aes(x = SEX, y = LOS, fill = SEX)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      labs(x = "Sex", y = "Length of Stay (days)") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
      )
  })

  output$age_by_sex_box <- renderPlot({
    d <- filtered_data()
    req(nrow(d) >= 2)
    ggplot(d, aes(x = SEX, y = AGE, fill = SEX)) +
      geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
      labs(x = "Sex", y = "Age") +
      theme_minimal() +
      theme(
        legend.position = "none",
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)
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

  output$diagnosis_by_sex_plot <- renderPlot({
    d <- filtered_data()
    req(nrow(d) >= 2)
    req("DIAGNOSIS" %in% names(d))
    diag_counts <- sort(table(d$DIAGNOSIS), decreasing = TRUE)
    if (length(diag_counts) == 0) return(NULL)
    top_n <- min(6, length(diag_counts))
    top_diag <- names(diag_counts)[1:top_n]
    d <- d[d$DIAGNOSIS %in% top_diag & !is.na(d$SEX), ]
    tab <- table(d$DIAGNOSIS, d$SEX)
    df <- as.data.frame(tab)
    names(df) <- c("DIAGNOSIS", "SEX", "COUNT")
    df$SHARE <- ave(df$COUNT, df$SEX, FUN = function(x) x / sum(x))
    ggplot(df, aes(x = DIAGNOSIS, y = SHARE, fill = SEX)) +
      geom_col(position = "dodge", alpha = 0.85) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      labs(x = "Diagnosis", y = "Share of Patients") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 25, hjust = 1),
        legend.title = element_blank()
      )
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
