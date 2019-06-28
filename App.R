library(shiny)

analysethis <- mtcars

ui <- fluidPage(
h2("analysethis, a database of cars"),
hr(),
fluidRow(
  mainPanel(
    NULL, DT::dataTableOutput("mytable")
  ),
  sidebarPanel(
    checkboxGroupInput("show_vars", "Velg hvilke kolonner du vil se:",
                       names(analysethis), selected = names(analysethis))
  )
),
hr(),
fluidRow(
  column(6,
  h2("1. Velg visualisering"),
  radioButtons("visualisation", NULL, choices = c("Spredningsplott", "Histogram", "Stolpediagram",  "Kumulativ fordeling"), selected = NULL,
               inline = FALSE, width = NULL), offset = 0),
  column(6,
  h2("2. Velg variabler"),
  conditionalPanel(
    condition = "input.visualisation == 'Spredningsplott'", 
    selectInput('xcol', 'x Variable', names(analysethis)),
    selectInput('ycol', 'y Variable', names(analysethis), selected = names(analysethis)[[2]])
    ),
  conditionalPanel(
    condition = "input.visualisation == 'Histogram'", 
    radioButtons("histbuttons", NULL, choices = c(names(analysethis)), selected = NULL,
                 inline = FALSE, width = NULL)
  ),
  conditionalPanel(
    condition = "input.visualisation == 'Stolpediagram'", 
    selectInput('xcol', 'x Variable', names(analysethis)),
    selectInput('ycol', 'y Variable', names(analysethis), selected = names(analysethis)[[2]])
  ),
  conditionalPanel(
    condition = "input.visualisation == 'Kumulativ fordeling'", 
    radioButtons("kumulativbuttons", NULL, choices = c(names(analysethis)), selected = NULL,
                 inline = FALSE, width = NULL)
    )
  )
),
fluidRow(
  column(5,
    h2("3. Resultat:"),
    conditionalPanel(
      condition = "input.visualisation == 'Spredningsplott'", plotOutput('scatter')
    ),
    conditionalPanel(
      condition = "input.visualisation == 'Histogram'", plotOutput('hist')
      ),
  conditionalPanel(
    condition = "input.visualisation == 'Stolpediagram'", plotOutput('bar')
      ),
  conditionalPanel(
    condition = "input.visualisation == 'Kumulativ fordeling'", plotOutput('kumulativ')
  ),
  conditionalPanel(
    condition = "input.features == 'Stats'", verbatimTextOutput('summary')
  )
),
  column(5,
h2("4. Tilpass"),
        
textInput("plottitle", label = h5("Tittel:")),
         hr(), verbatimTextOutput("value"),

checkboxGroupInput("features", "Legg til elementer:",
                   c("Stats", "Trendline"), selected = NULL),

hr(),

downloadButton("eksport", "Last ned visualisering"),
radioButtons("eksporttyp", NULL, list("png", "pdf"))
         , offset = 1)
  )
)

  

server <- function(input, output){
  analysethis2 = analysethis[sample(nrow(analysethis)), ]
  output$mytable <- DT::renderDataTable({
    DT::datatable(analysethis2[, input$show_vars, drop = FALSE])
  })
#spredningsplott
  scatterdata <- reactive({
    analysethis[, c(input$xcol, input$ycol)]
  })
  output$scatter <- renderPlot({
    plot(scatterdata(), main = paste(input$plottitle))
  })

#histogram
  histdata <- reactive({
    analysethis[, c(input$histbuttons)]
  })
  output$hist <- renderPlot({
    hist(histdata(), main = paste(input$plottitle), xlab = input$histbuttons)
  })

#stolpediagram
    bardata <- reactive({list(
      xbar = input$xcol,
      ybar = input$ycol,
      names = c("xbar", "ybar"),
      class = data.frame
      )
  })
  output$bar <- renderPlot({
    barplot(bardata$xbar, bardata$ybar, main = paste(input$plottitle))
  })
#kumulativ fordeling
  kumulativdata <- reactive({
    cumsum(analysethis[,(input$kumulativbuttons)])
  })
  output$kumulativ <- renderPlot({
    plot(kumulativdata(), main = paste(input$plottitle), xlab = input$kumulativbuttons)
  })
#generic plot object
  genericplot <- reactive({
    if(input$visualisation == "Spredningsplott"){
genericplot <- plot(analysethis[, c(input$xcol, input$ycol)], main = paste(input$plottitle))
    }
    else if(input$visualisation == "Histogram"){
genericplot <- hist(analysethis[, c(input$histbuttons)], main = paste(input$plottitle), xlab = input$histbuttons)
    }
    else if(input$visualisation == "Stolpediagram"){
genericplot <- hist(analysethis[, c(input$histbuttons)], main = paste(input$plottitle), xlab = input$histbuttons)
    }
    else if(input$visualisation == "Kumulativ fordeling"){
genericplot <- plot(kumulativdata(), main = paste(input$plottitle), xlab = input$kumulativbuttons)
    }
  })
#generic data object
  genericdata <- reactive({
    if(input$visualisation == "Spredningsplott"){
      genericdata <- analysethis[, c(input$xcol, input$ycol)]
    }
    else if(input$visualisation == "Histogram"){
      genericdata <- analysethis[, c(input$histbuttons)]
    }
    else if(input$visualisation == "Stolpediagram"){
      genericdata <- analysethis[, c(input$histbuttons)]
    }
    else if(input$visualisation == "Kumulativ fordeling"){
      genericdata <- cumsum(analysethis[,(input$kumulativbuttons)])
    }
  })
#summary feature
  output$summary <- renderPrint({
    summary(genericdata())
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
      paste(genericplot())
      dev.off()  
    })
}

shinyApp(ui, server)

