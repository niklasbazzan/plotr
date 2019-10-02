library(shiny)
library(shinyBS)
library(shinydashboard)
library(tidyverse)
library(RODBC)
library(DT)


# UI ----
ui <- fluidPage(
        fluidRow(
          column(10,
            h1("plotr"),
            h5("An app for interactive visualisation of data."),
          offset = 0
          )
        ),
   
    br(), 
    
    h3("Step 1: Data"),
  mainPanel(
    tabsetPanel(id = "step1",
      tabPanel("R Datasets", value = 1, # Choose R data
               br(),
               selectInput(
                 "dataset",
                 "Choose a dataset",
                 choices = ls('package:datasets'),
                 selected = "iris")),
      tabPanel("Upload a CSV File", value = 2, # Upload a dataset
               br(),
               fileInput("file1", "Choose a CSV File",
                         multiple = TRUE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv")),
               # Input: Checkbox if file has header ----
               checkboxInput("header", "Data has header", TRUE),
               
               # Input: Select separator ----
               radioButtons("sep", "Separator",
                            choices = c(Comma = ",",
                                        Semicolon = ";",
                                        Tab = "\t"),
                            selected = ",")
               ),
      tabPanel("Connect to a SQL Database", value = 3, # Connect to SQL database
               br(),
               fluidRow(
                 column(5,
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
                 column(7,
                   h4("How to connect to a SQL database:"),
                   "This login connects to your SQL server via the RODBC package for R. A prerequisite is that you've connected your local machine to the SQL server via your local ODBC driver, for example", tags$link( a("like this.", target="_blank", href="https://www.youtube.com/watch?v=K3GZidOwGmM&")),
                   "Use a 64-bit ODBC driver and 64-bit R.",
                        offset = 0)
                 
               )
               
      )
    )
  ),
  
  br(),

    mainPanel(
      conditionalPanel(condition = "input.step1 == 1|2", # Look at R packages data
                       h4("Have a look a the data:"), 
                       value = 2,
                       radioButtons("choice", NULL, choices = c("Dataset" = 1, "Structure" = 2, "Summary" = 3), selected = 2),
                       
                       wellPanel(
                         conditionalPanel(condition = "input.choice==1", verbatimTextOutput("dat")),
                         conditionalPanel(condition = "input.choice==2", verbatimTextOutput("struct")),
                         conditionalPanel(condition = "input.choice==3", verbatimTextOutput("summary"))
                       )
      ),
      conditionalPanel(condition = "input.step1 == 3", # Look at SQL data
        fluidRow(
    box(title="Type SQL Query", status = "info", width=12, solidHeader = T,collapsible = T,
    tagList(
    tags$style(type="text/css", "textarea {width:100%; margin-top: 5px; resize: vertical;}"),
    tags$textarea(id = "sql_query", placeholder = "SELECT * FROM table_name WHERE Variable X > n", 
                  rows = 4, value="")
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
                       )
      
      ),
  br()  
  ), 
  
  fluidRow(
    column(6,
      h3("Step 2: Choose a plot type"), # Choose plot type
        tabsetPanel(id = 'choosetab',
          tabPanel("1-Variable", value = 1,
              radioButtons("plottype1", NULL, 
                    choices = c("Histogram" = "geom_histogram", "Dotplot" = "geom_dotplot", 
                        "Density" = "geom_density", "Frequency line"  = "geom_freqpoly", 
                          "Barplot" = "geom_bar"), selected = NULL, inline = FALSE, width = NULL)),
          tabPanel("2-Variable", value = 2,
              radioButtons("plottype2", NULL, 
                    choices = c("Scatterplot" = "geom_point", "Boxplot" = "geom_boxplot", 
                        "Line" = "geom_line", "Violinplot" = "geom_violin"), selected = NULL,
                                             inline = FALSE, width = NULL))
           )
           , offset = 0)
  ),
  
  br(),
  
  fluidRow(
    column(4,                                 # Choose variable(s)
           conditionalPanel(
             condition = "input.choosetab == 1",
                h3("Step 3: Select a variable"),
             wellPanel(
                uiOutput("varx_only")
                       )
           ), 
      conditionalPanel(
      condition = "input.choosetab == 2",
      h3("Step 3: Select variables"),
      wellPanel(
        uiOutput("varx"), uiOutput("vary")
      )
    ), offset = 0),
    column(4,
    conditionalPanel(
      condition = "input.choosetab == 2",
      h3("Colour by variable:"), # Choose a variable to colour by
      wellPanel(
        uiOutput("var_col")), offset = 4))
    ),
      
   # Output plot
fluidRow(
  column(8,
         conditionalPanel(
           condition = "input.choosetab == 1",
           plotOutput("plot1var") 
         ),         
         conditionalPanel(
           condition = "input.choosetab == 2",
           plotOutput("plot2var") 
         )
         
         , offset = 1)),
  
  br(),
  
  fluidRow(
    column(3,
    h3("Step 4: Customize the plot"), # Customize plot
    wellPanel(
      h4("Titles"),
        textInput("plot_title", label = h5("Title:")),
        textInput("sub_title", label = h5("Subtitle:")),
        textInput("caption", label = h5("Caption:"))
    ), offset = 0),
    column(3,
    wellPanel(
      h4("Axis labels"),
        radioButtons("xlab", "x", choices = c("Automatic", "Manual", "None")),
        conditionalPanel(condition = "input.xlab == 'Manual'",
          textInput("x_title", label = NULL)
        ),
        radioButtons("ylab", "y", choices = c("Automatic", "Manual", "None")),
        conditionalPanel(condition = "input.ylab == 'Manual'",
                       textInput("y_title", label = NULL)
      )
        
    ),
    hr(),
    wellPanel(
      h4("Axes range"),
      radioButtons("xaxesrange", "x", choices = c("Automatic", "Manual")),
      conditionalPanel(condition = "input.xaxesrange == 'Manual'",
              numericInput("x_min", label = h5("x-axis minimum:"), value = 0),
              numericInput("x_max", label = h5("x-axis maximum:"), value = 100)
      ),
      radioButtons("yaxesrange", "y", choices = c("Automatic", "Manual")),
      conditionalPanel(condition = "input.yaxesrange == 'Manual'",
          numericInput("y_min", label = h5("y-axis minimum:"), value = 0),
          numericInput("y_max", label = h5("y-axis maximum:"), value = 100)
      )
    ),
hr(),
    offset = 0),
    column(3,
           
           wellPanel(
             h4("Theme"),
             radioButtons("plottheme", NULL,
choices = c("Grey" = "theme_grey", "Classic" = "theme_classic","Minimal" = "theme_minimal", 
            "Dark" = "theme_dark", "Light" = "theme_light", "B&W" = "theme_bw", 
            "Linedraw" = "theme_linedraw", "Void" = "theme_void"))
           ), 
hr(),
  offset = 0),
    column(3,
      conditionalPanel(condition = "input.plottype1 == 'geom_histogram' && input.choosetab == 1",
        wellPanel(numericInput("binwidth", label = h5("Histogram: bin width:"), value = 1)),
hr()
    ),
      conditionalPanel(condition = "input.choosetab == 2",
        wellPanel(checkboxInput("regline", label = h5("Show regression line?"), value = FALSE)),
hr()
      ),   
    offset = 0), 
hr()
   ),
hr(),
fluidRow(column(3,
                h3("Export the plot:"),
  wellPanel(
    downloadButton("downloadPlot"), hr(),
    "Note: You may have to add '.png' to your filename."
  ),
offset = 0)
),
hr(),
h5("Feedback, bugs, suggestions?", tags$a(href = "niklas.bazzan@protonmail.com", "Reach out."))
 
)


# SERVER ----
server <- function(input, output, session) {

  # Changes the value of the data object depending on tab choice at Step 1 (R datasets or SQL data)
  data <- reactive({
    if (input$step1 == 1){
      data <- get(input$dataset)
    } 
    else if (input$step1 == 2){
      data <- csv_data()
    }
    else if (input$step1 == 3){
      data <- sqldata()
    }
  })
  
  # Printing R dataset
  output$dat <- renderPrint({
    print(data())
  })
  
  # Printing R dataset structure
  output$struct <- renderPrint({
    str(data())
  })
  
  # Printing R dataset summary
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Pulling the list of variables for choice of variable x, for 1 variable plot
  output$varx_only <- renderUI({
    selectInput("variablex_only", "X variable:", choices = names(data()))
  })
  
  # Pulling the list of variables to group colours by
  
  output$var_col <- renderUI({
    selectInput("variable_col", NULL, choices = c("NULL", names(data()))
                )
  })
  
  # Pulling the list of variables for choice of variable x, for 2 variable plot
  output$varx <- renderUI({
    selectInput("variablex", "X variable:", choices = names(data()))
  })
  
  # Pulling the list of variables for choice of variable y, for 2 variable plot
  output$vary <- renderUI({
    selectInput("variabley", "Y variable:", choices = names(data()))
    
  })
  
  # CSV upload ----
  
 csv_data <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  observeEvent(input$file1,{
    inFile <<- csv_data()
    })
  
  # plots ----
  
  # 1 variable plot
 onevarplot <- reactive({
    geomtype1 <- switch(input$plottype1,
                   geom_histogram = geom_histogram(binwidth = input$binwidth, stat = "count"),
                   geom_dotplot = geom_dotplot(),
                   geom_density = geom_density(),
                   geom_freqpoly = geom_freqpoly(),
                   geom_bar = geom_bar(stat = "count"))
    plot_theme <- switch(input$plottheme,
                         theme_grey = theme_grey(),
                         theme_classic = theme_classic(),
                         theme_dark = theme_dark(),
                         theme_light = theme_light(),
                         theme_bw = theme_bw(),
                         theme_minimal = theme_minimal(),
                         theme_linedraw = theme_linedraw(),
                         theme_void = theme_void())
    ggplot(data(), aes_string(x = input$variablex_only, color = input$variablex_only)) +
      geomtype1 + 
     labs(title = input$plot_title, x = xlabel1(), y = ylabel1(),
                          subtitle = input$sub_title, caption = input$caption) + 
     plot_theme +
     coord_cartesian(xlim = xaxisrange(), ylim= yaxisrange())
  })

  output$plot1var <- renderPlot({
    req(onevarplot())
    onevarplot()
  })
  
  # 2 variable plot
  twovarplot <- reactive({
    geomtype2 <- switch(input$plottype2,
                        geom_point = geom_point(),
                        geom_boxplot = geom_boxplot(),
                        geom_line = geom_line(),
                        geom_violin = geom_violin())
    plot_theme <- switch(input$plottheme,
                         theme_grey = theme_grey(),
                         theme_classic = theme_classic(),
                         theme_dark = theme_dark(),
                         theme_light = theme_light(),
                         theme_bw = theme_bw(),
                         theme_minimal = theme_minimal(),
                         theme_linedraw = theme_linedraw(),
                         theme_void = theme_void())
   ggplot(as.data.frame(data()), aes_string(x = input$variablex, y = input$variabley, color = input$variable_col)) +
      geomtype2 + 
     labs(title = input$plot_title, x = xlabel2(), y = ylabel2(),
                         subtitle = input$sub_title, caption = input$caption) + 
     plot_theme +
     coord_cartesian(xlim = xaxisrange(), ylim= yaxisrange()) +
     reg_line()
     

  })
  
  output$plot2var <- renderPlot({
    req(twovarplot())
    twovarplot()
  })
  
# One plot object 
  theplot <- reactive({
    switch(input$choosetab,
           "1" = onevarplot(),
           "2" = twovarplot())
  })
  
# Plot download handler
  output$downloadPlot <- downloadHandler(
    filename = function(){
      paste("plot", 'png', sep = ".")
    },
    content = function(file){
      req(theplot())
      ggsave(file, plot = theplot())
    }
  )
  
  # x & y labels (label1 is for 1 variable plot, label2 is for 2 variable plot )
  
  xlabel1 <- reactive({
    if (input$xlab == "Automatic"){
      xlabel1 <- input$variablex_only
    } else if(input$xlab == "Manual"){
      xlabel1 <- input$x_title}
      else if (input$xlab == "None"){
      ylabel2 <- NULL}
  })
  xlabel2 <- reactive({
    if (input$xlab == "Automatic"){
      xlabel2 <- input$variablex
    } else if(input$xlab == "Manual"){
      xlabel2 <- input$x_title}
      else if (input$xlab == "None"){
      ylabel2 <- NULL}
  })
  ylabel1 <- reactive({
    if (input$ylab == "Automatic"){
      ylabel1 <- "Count"
    } else if (input$ylab == "Manual"){
      ylabel1 <- input$y_title}
      else if (input$ylab == "None"){
      ylabel2 <- NULL}
  })
  ylabel2 <- reactive({
    if (input$ylab == "Automatic"){
      ylabel2 <- input$variabley
    } else if (input$ylab == "Manual"){
      ylabel2 <- input$y_title}
      else if (input$ylab == "None"){
      ylabel2 <- NULL}
  })
  
  # Regression line
reg_line <- reactive({
    if (input$regline == TRUE){
      reg_line <- geom_smooth(method = "lm", se = FALSE, col = "grey")
    } else{
      reg_line <- NULL}
})

# Axis ranges

xaxisrange <- reactive({
  if(input$xaxesrange == "Automatic"){
    xaxisrange <- NULL
  }else{
    xaxisrange <- c(input$x_min, input$x_max)
}
})

yaxisrange <- reactive({
  if(input$yaxesrange == "Automatic"){
    yaxisrange <- NULL
  }else{
    yaxisrange <- c(input$y_min, input$y_max)
  }
})
  

  
  
# SQL connection ----
  # Input database name/connection string  
  
  values <- reactiveValues(loginState=-1, plotState=0, warns=0)
  
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
      need(!grepl(";",query_input), "For security reasons, queries including a semi-colon are not allowed!")
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
