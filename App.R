library(shiny)
library(shinyBS)
library(shinydashboard)
library(tidyverse)
library(RODBC)
library(DT)


# UI ----
ui <- fluidPage(
    h1("Welcome to ggplotr."),
    h5("An app for interactive visualisation of data."),
    
    br(), 
    
    h3("Step 1: Data"),
  mainPanel(
    tabsetPanel(id = "step1",
      tabPanel("R datasets", value = 1, h4("Choose a dataset:"), # Choose R data
               selectInput(
                 "dataset",
                 NULL,
                 choices = ls('package:datasets'),
                 selected = "iris")),
      tabPanel("Connect to a SQL database", value = 2, # Connect to SQL database
               br(),
               fluidRow(
                 column(4,
                   radioButtons("connect_type",label="Connection method:",
                                choices=c("odbcConnect"="odbcC","odbcDriverConnect"="odbcDC")),
                   br(),
                   wellPanel(
                     uiOutput("database_input"),
                     uiOutput("username_input"),
                     uiOutput("pass_input")  
                   ),
                   br(),
                   bsButton("login", label = "Login",style="info",disabled=FALSE),
                   uiOutput("login_fail"),
                 offset = 0),
                 column(3,
                   h4("How to log into your SQL database:"),
                   "This login connects to your SQL server via the RODBC package for R. A prerequisite is that you've connected your local machine to the SQL server via the local ODBC driver, for example", tags$link( a("like this.", target="_blank", href="https://www.youtube.com/watch?v=2xQX76nEdvo")),
                   "Use a 64-bit ODBC driver and 64-bit R.",
                        offset = 0)
                 
               )
               
      )
    )
  ),
  
  br(),

    mainPanel(
      conditionalPanel(condition = "input.step1 == 2",
        fluidRow(
    box(title="Type SQL Query", status = "info", width=12, solidHeader = T,collapsible = T,
    tagList(
    tags$style(type="text/css", "textarea {width:100%; margin-top: 5px; resize: vertical;}"),
    tags$textarea(id = "sql_query", placeholder = "SELECT * FROM table_name WHERE Variable X > n", rows = 4, value="")
                             ),
      bsButton("do_sql", label = "Run",disabled=TRUE,style="primary", icon = icon("ban"))
                         )),
  br(),
        fluidRow(
          tabBox(width = 12,id="tabset1",
            tabPanel("Table", 
              DT::dataTableOutput("view") 
                                )
                         )
                       ),
  br(),
        fluidRow(
          
          plotOutput("plotsql"),
          uiOutput("sql_varx_only")
          
        )
      ),
      conditionalPanel(condition = "input.step1 == 1",
        h4("Have a look a the data:"), # Look at data
          value = 2,
            radioButtons("choice", NULL, choices = c("Dataset" = 1, "Structure" = 2, "Summary" = 3)
                       ),
                      
                       wellPanel(
                         conditionalPanel(condition = "input.choice==1", verbatimTextOutput("dat")),
                         conditionalPanel(condition = "input.choice==2", verbatimTextOutput("struct")),
                         conditionalPanel(condition = "input.choice==3", verbatimTextOutput("summary"))
                       )
      )
  ), # main panel end
  
  conditionalPanel(condition = "input.step1 == 1",
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
    column(2,                                 # Choose variable(s)
           conditionalPanel(
             condition = "input.choosetab == 1",
             h3("Step 3: Select a variable"),
             uiOutput("varx_only")
           ),
           conditionalPanel(
             condition = "input.choosetab == 2",
             h3("Step 3: Select variables"),
             uiOutput("varx"), uiOutput("vary")
           )
          
           , offset = 0),
    column(2, 
           wellPanel(
             h5("Choose a variable to colour by:"), # Choose a variable to colour by
             uiOutput("var_col"))
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
    h3("Step 4: Customize the plot"), # Customize plot
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
        )
           , offset = 0),
    column(2,
           wellPanel(
           downloadButton("eksport", "Export plot"), # Export button
           radioButtons("eksporttyp", NULL, list("png", "pdf")))
           ,offset = 0))
  )
)


# SERVER ----
server <- function(input, output, session) {
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
    selectInput("variablex_only", "X variable:", choices = names(data()))
  })
  
  output$sql_varx_only <- renderUI({
    selectInput("sql_variablex_only", "X variable:", choices = names(sqldata()))
  })
  
  output$var_col <- renderUI({
    selectInput("variable_col", NULL, choices = names(data()))
  })
  
  # Pulling the list of variables for choice of variable x, for 2 variable plot
  output$varx <- renderUI({
    selectInput("variablex", "X variable:", choices = names(data()))
  })
  
  # Pulling the list of variables for choice of variable y, for 2 variable plot
  output$vary <- renderUI({
    selectInput("variabley", "Y variable:", choices = names(data()))
    
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
  # 1 variable sql plot
  output$plotsql <- renderPlot({
    
    ggplot(sqldata(), aes_string(x = input$sql_variablex_only)) +
      geom_histogram() 
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
  values <- reactiveValues(loginState=-1, plotState=0, warns=0)
  
# SQL connection ----
  # Input database name/connection string  
  
  output$database_input =  renderUI({
    switch(input$connect_type,
           odbcC={textInput("database", label = "Database Name", placeholder = "mydatabase", value = "")},
           odbcDC={
             tagList(
               tags$label(id = "database","Connection String"),
               tags$textarea(id = "database", placeholder = 
                               "(No Quotations Marks!)  e.g. driver={SQL Server};
                             server=servername\\\\instancename,port;database=testing;trusted_connection=true"
                             ,rows=5, value="asdad"))
           },
           return()
               )
           })
  
  # Input database username  
  
  output$username_input =  renderUI({
    switch(input$connect_type,
           odbcC={textInput("username", label = "Username", placeholder = "myusername",value = "")},
           return()
    )
  })
  
  # Input database password  
  
  output$pass_input =  renderUI({
    
    switch(input$connect_type,
           odbcC={passwordInput("pass", label="Password", placeholder = "mypassword", value = "", width = NULL)},
           return()
    )
    
  })
  
  # Error message: login failure 
  
  output$login_fail =  renderUI({
    if(values$warns[1]!=0){
      helpText(paste0("Login Failed: ",values$warns))
    }else{return()}
  })
  
  # The following block of code runs when the shiny session is terminated
  # Any active db connection is closed
  
  session$onSessionEnded(function() {
    observe({
      if(values$loginState!=-1){
        odbcClose(login())
      }
    })
  })
  
  
  # Logging in  
  
  login <- eventReactive(input$login, {
    if(input$login==0){
      return(-1)}
    isolate({
      if(values$loginState==-1){
        switch(isolate(input$connect_type),
               odbcC = {con <- tryCatch(
                 RODBC::odbcConnect(input$database , uid = input$username, pwd = input$pass),
                 warning=function(c) c$message)
               },
               odbcDC= {con <- tryCatch(RODBC::odbcDriverConnect(input$database),
                                        warning=function(c) c$message)
               }
        )
        if(is.character(con)){
          values$warns=con
          con=-1
          # it's slightly ugly, but I'm more comfortable with the convention that a failed login
          # equates to -1
        }
        validate(
          need(con!=-1, paste0("Login Failed"))
        )
      }else{RODBC::odbcClose(values$loginState)
        return(-1)}
      con})
  },ignoreNULL=FALSE)
  
  # Changing login and sql button colour
  
  observeEvent(login(), ({
    b <- login()
    values$loginState <- login()
    values$warns <- 0
    
    if(b==-1){
      updateButton(session, "login",label="Login" ,disabled = FALSE, style = "info")
      updateButton(session, "do_sql", disabled = TRUE, style = "primary")
    }else{
      session$sendInputMessage("database", list(value=""))
      updateTextInput(session,"username",value="")
      updateTextInput(session,"pass",value="")
      updateButton(session, "login", label="Logout",disabled = FALSE, style = "danger")
      updateButton(session, "do_sql", disabled = FALSE, style = "success",icon=icon("refresh"))
    }
  }))
  sqldata <- eventReactive(input$do_sql, {
    # remove whitespace from start and end of string
    query_input <- gsub("^\\s+|\\s+$", "", input$sql_query)
    # Preventing malicious SQL injections/errors
    validate(
      need(!grepl(";",query_input), "For security reason, queries including a semi-colon are not allowed!")
    )
    # No compound queries (using ;) and only queries starting with select
    # I realise this prevents queries like "with tab1 as (...) select * from tab1"
    validate(
      need(tolower(gsub(" .*$", "",query_input))=="select", 
           "Only queries starting with a select (case insensitive) are allowed")
    )
    withProgress(message = 'Querying...',{ query_output=sqlQuery(login(),paste0(query_input,";"))})
    validate(
      need(!is.character(query_output), paste0("Query failed: ",query_output[1]))
    )
    query_output
  })
  
  # Output table from SQL query
  
  output$view = DT::renderDataTable({
    table.data <- sqldata()
    print(head(table.data))
    table.data[sapply(table.data, is.character)] <- lapply(table.data[sapply(table.data, is.character)], 
                                                           as.factor)
    datatable(table.data,filter="top",rownames=FALSE,extensions="Buttons",
              selection = list(target = 'column'),options = list(paging = TRUE, scrollX = T,
                                                                 dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf')))
  })
}

# SHINY APP ----
shinyApp(ui = ui, server = server)
