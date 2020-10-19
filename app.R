library(shinydashboard)
library(shiny)
library("randomForest")
source("fun.R")
library(shinycssloaders)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- dashboardPage( skin = "blue",
                           header<-dashboardHeader(title='CEEI PERFORMANCE PREDICTION PLATFORM', titleWidth = 400),
                           sidebar <- dashboardSidebar(
                             sidebarMenu(
                               width=350,
                               menuItem("CEEI", icon = icon("send",lib='glyphicon'),
                                        
                                        href = "http://www.theceei.com"),
                              # img(src='ceei.jpg', align = "left"),
                               fileInput("data","Choose csv:",multiple="false"),
                               numericInput("empid1","Enter Employee ID",value="90000142")  
                             )),
                           
                           dashboardBody(tags$style(type="text/css",
                                                    ".shiny-output-error { visibility: hidden; }",
                                                    ".shiny-output-error:before { visibility: hidden; }"
                           ),
                           h1("Performance Prediction Platform"),
                           
                          
                           
                          
      # Show a plot of the generated distribution
      fluidRow(
            h3("Random Forest Classifier"),
            h4("The following are the parameters of employee that are used to predict their performance band:"),
            h6("Age, Education level"),
            h6("Performance Rating 1, Performance Rating 2"),
            h6("Performance Rating 3, Performance Rating 4"),
            h6("Building Team Commitment, Strategic Thinking"),
            h6("Leads Decision Making and Delivers Results"),
            h6("Analytical Thinking, Customer Relations"),
            h6("Service Quality and Planning, Solution Selling"),
            h6("In Market Execution, Sales Planning and Forecasting"),
            h6("Negotiation, Actionable Insights"),
            h6("Solving Problems, Engage, Applied Thinking"),
            h6("Change, Drive, Average Comp Score"),
            br(),
            h1(textOutput("final_pred") %>% withSpinner(color="#0dc5c1")),
            plotlyOutput("final_plot1")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  fileemp <- reactive({
    infile <- input$data
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    empdata<-data.frame(read.csv(infile$datapath))
  })
  randomforest_model<-function(data1,para){
    data=data[,-c(1:3,29:30)]
    ind = sample(2, nrow(data), replace=TRUE, prob=c(0.7,0.3))
    trainData = data[ind==1,]
    testData = data[ind==2,]
    rf_model = randomForest(Local.Band~., data=trainData, ntree=100, proximity=T)
    plot(margin(rf_model, testData$Local.Band))
    Pred = predict(rf_model, newdata=testData)
    table(Pred, testData$Local.Band)
    # CM = table(Pred, testData$Local.Band)
    # accuracy = (sum(diag(CM)))/sum(CM)
  }
  output$final_pred<-renderText({
    ip=fileemp()
    ans<-final(ip,input$empid1)
    ans
    paste("Performance Prediction for employee ID: ",input$empid1," is ",ans," (Local Band)")
  })
  
  output$final_plot1<-renderPlotly({
    ip1=fileemp()
    p<-final_plot(ip1,input$empid1)
    p
  })
}
# Run the application
shinyApp(ui = ui, server = server)
