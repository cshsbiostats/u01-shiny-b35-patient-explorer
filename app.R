library(tidyverse)
library(shiny)
library(bslib)
library(ggsankey)
library(patchwork)
library(bsicons)
library(plotly)

data <- read_csv(here::here('data/u01_b35_data.csv'),
                 show_col_types = FALSE)

trt_options <- data |> pull(trt) |> unique() |> sort()

ae_options <- data |> 
  pull(ae) |> 
  unique() |> 
  sort()

grade_options <- data |> 
  pull(ae_grade) |> 
  unique() |> 
  sort()

main <- layout_sidebar(
  fillable = TRUE,
  sidebar = sidebar(
    selectInput(
      inputId = 'select_trt',
      label = tooltip(
        span(
          "1. Select Treatment",
          bs_icon("info-circle")
        ),
        "Please select from the drop down the treatment to visualize AE data.",
        placement = "right"
      )
      ,
      choices = trt_options
    ),
    selectInput(
      inputId = 'select_ae',
      label = tooltip(
        span(
          "2. Select AE",
          bs_icon("info-circle")
        ),
        "Please select from the drop down the adverse event to visualize",
        placement = "right"
      ),
      choices = ae_options
    ),
    selectInput(
      inputId = 'select_grade',
      label = tooltip(
        span(
          "3. Select Grade",
          bs_icon("info-circle")
        ),
        "Please select from the drop down the inital grade of interest to visualize",
        placement = "right"
      ),
      choices = grade_options
    ),
    sliderInput(
      inputId = 'select_timeframe',
      label = tooltip(
        span(
          '4. Select Timeframe',
          bs_icon("info-circle")
        ),
        "Please select from the slider the time frame to visualize AE data.",
        placement = "right"
      ),
      min = 6,
      max = 60,
      value = c(6, 36),
      step = 6
    ),
    actionButton(
      'btn_ae_visualize',
      'Visualize',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    ),
    downloadButton(
      'report',
      'Download Report',
      icon = icon(name = 'chart-bar', lib = 'font-awesome')
    )
  ),
  navset_card_tab(
    title = 'Results',
    full_screen = TRUE,
    nav_panel(
      title = 'Sankey Diagram',
      card_body(class = "p-0",
                plotOutput('sankey_plot')),
      card_body(htmlOutput('summary_descr')),
    ),
    nav_panel(title = 'Grade Duration',
              card_body(class = "p-0",
                        plotlyOutput('grade_duration'))),
    nav_panel(title = 'Toxicity Index'
              ,
              card_body(class = "p-0",
                        plotlyOutput('ti_hist')))
  )
  
)

ui <- page_fillable(
  theme = bslib::bs_theme(preset = 'shiny', version = 5),
  main
)

shinyApp(ui, function(input, output) {
  
  results <- eventReactive(input$btn_ae_visualize, {
    
    out <- make_patient_ae_sankey_results(
      data = data,
      selected_treatment = input$select_trt,
      selected_ae = input$select_ae,
      selected_timeframe = input$select_timeframe,
      selected_grade = input$select_grade
    )
    
  })
  
  observeEvent(input$btn_ae_visualize, {
    
    req(results())
    
    
    
    output$sankey_plot <- renderPlot({
      
      results()$sankey
      
    })
    
    output$ti_hist <- renderPlotly({
      
      results()$ti_hist |> ggplotly()
      
    })
    
    output$summary_descr <- renderPrint({
      
      results()$summary_description
    })
    
    output$grade_duration <- renderPlotly({
      
      results()$grade_duration |> ggplotly()
    })
    
  })
  
  output$report <- downloadHandler(
    filename = \(x) {paste0('pro_ctcae_ae_sankey_', Sys.Date(), '.pdf')},
    content = function(file) {
      
      rmarkdown::render(
        'explore_qol_template.Rmd',
        output_file = file,
        params = list('results' = results()),
        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
})
