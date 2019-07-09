library(shiny)
library(tidyverse)


ui <- fluidPage(
    h1("Welcome to ggplotr."),
    h3("Step 1: Select data"),
  hr(),
  
    sidebarPanel(h4("Choose a dataset:"), # Choose data
      selectInput(
        "dataset",
        NULL,
        choices = ls('package:datasets'),
        selected = "iris"
      )

    ),
    mainPanel(
      h4("Have a look a the data:"), # Look at data
      value = 2,
      radioButtons(
        "choice",
        NULL,
        choices = c(
          "Dataset" = 1,
          "Structure" = 2,
          "Summary" = 3
        )
      ),
      wellPanel(
      conditionalPanel(condition = "input.choice==1", verbatimTextOutput("dat")),
      conditionalPanel(condition = "input.choice==2", verbatimTextOutput("struct")),
      conditionalPanel(condition = "input.choice==3", verbatimTextOutput("summary"))
    )
  ),
  fluidRow(
    column(6,
           h3("Step 2: Choose a plot type"), # Choose plot type
           tabsetPanel(id = 'choosetab',
              tabPanel("1-Variable", value = 1,
                       radioButtons("plottype1", NULL, choices = c("Histogram" = "geom_histogram", "Dotplot" = "geom_dotplot", 
                                                                   "Density" = "geom_density", "Frequency"  = "geom_freqpoly"),
                                             selected = NULL,
                                             inline = FALSE, width = NULL)),
              tabPanel("2-Variable", value = 2,
                       radioButtons("plottype2", NULL, choices = c("Scatterplot" = "geom_point", "Boxplot" = "geom_boxplot", 
                                                                   "Line" = "geom_line", "Violinplot" = "geom_violin", 
                                                                   "Barplot" = "geom_col"), selected = NULL,
                                             inline = FALSE, width = NULL))
           )
           , offset = 0),
    column(6,                                 # Choose variable(s)
           conditionalPanel(
             condition = "input.choosetab == 1",
             h3("Step 3: Select variable"),
             uiOutput("varx_only")
           ),
           conditionalPanel(
             condition = "input.choosetab == 2",
             h3("Step 3: Select variables"),
             uiOutput("varx"), uiOutput("vary")
           )
           , offset = 0)
  ), # Output plot
  fluidRow(
    column(4,
      conditionalPanel(
        condition = "input.choosetab == 1",
        plotOutput("plot1var") 
      ),         
      conditionalPanel(
        condition = "input.choosetab == 2",
        plotOutput("plot2var") 
      )
           
           ,offset = 1),
    column(2,
    h3("Step 4: Customize plot"), # Customize plot
    textInput("plot_title", label = h5("Title:")),
    textInput("sub_title", label = h5("Subtitle:")),
    textInput("caption", label = h5("Caption:")),
    hr()
  
    ,offset = 1),
    column(2,
           conditionalPanel(condition = "input.plottype1 == 'geom_histogram' && input.choosetab == 1",
        numericInput("binwidth", label = h5("Bin width:"), value = 2),
        hr()
           ),
        conditionalPanel(condition = "input.plottype2 == 'geom_point' && input.choosetab == 2",
                          checkboxInput("regline", label = h5("Show regression line?"), value = TRUE),
                         hr()
        ),
        wellPanel(
                  h5("Choose group to colour by:"),
                  uiOutput("var_col"))
           , offset = 0),
    column(2,
           wellPanel(
           downloadButton("eksport", "Export plot"), # Export button
           radioButtons("eksporttyp", NULL, list("png", "pdf")))
           ,offset = 0)
  )
)



server <- function(input, output) {
  geomtype1 <- reactive({input$plottype1})

  # Get the value of the dataset that is selected by user from the list of datasets
  data <- reactive({
    get(input$dataset)
  })
  
  # Output the dataset
  output$dat <- renderPrint({
    data()
  })
  
  # Pulling the list of variables for choice of variable x, for 1 variable plot
  output$varx_only <- renderUI({
    selectInput("variablex_only", "select the X variable", choices = names(data()))
  })
  
  output$var_col <- renderUI({
    selectInput("variable_col", NULL, choices = names(data()))
  })
  
  # Pulling the list of variables for choice of variable x, for 2 variable plot
  output$varx <- renderUI({
    selectInput("variablex", "Select the X variable", choices = names(data()))
  })
  
  # Pulling the list of variables for choice of variable y, for 2 variable plot
  output$vary <- renderUI({
    selectInput("variabley", "Select the Y variable", choices = names(data()))
    
  })
  
  # Output dataset structure
  output$struct <- renderPrint({
    str(get(input$dataset))
  })
  
  # Output dataset summary
  output$summary <- renderPrint({
    summary(get(input$dataset))
  })
  
  # plots
  # 1 variable plot
  output$plot1var <- renderPlot({
    geomtype1 <- switch(input$plottype1,
                   geom_histogram = geom_histogram(binwidth = input$binwidth),
                   geom_dotplot = geom_dotplot(),
                   geom_density = geom_density(),
                   geom_freqpoly = geom_freqpoly())
    ggplot(data(), aes_string(x = input$variablex_only, color = input$variablex_only)) +
      geomtype1 + labs(title = input$plot_title,
                          subtitle = input$sub_title,
                          caption = input$caption)
  })
  # 2 variable plot
  output$plot2var <- renderPlot({
    geomtype2 <- switch(input$plottype2,
                        geom_point = geom_point(),
                        geom_boxplot = geom_boxplot(),
                        geom_line = geom_line(),
                        geom_violin = geom_violin(),
                        geom_col = geom_col())
   ggplot(data(), aes_string(x = input$variablex, y = input$variabley, color = input$variable_col)) +
      geomtype2 + labs(title = input$plot_title,
                         subtitle = input$sub_title,
                         caption = input$caption)
  })
  #download button
  output$eksport <- downloadHandler(
    filename =  function() {
      paste("plott", input$eksporttyp, sep=".")
    },
    
    content = function(file) {
      if(input$eksporttyp == "png")
        png(file) 
      else
        pdf(file)
      paste()
      dev.off()  
    })
}

shinyApp(ui = ui, server = server)
