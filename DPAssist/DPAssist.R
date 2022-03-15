library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(party)
library(caret)
library(plotly)
library(curl)
library(RCurl)
library(e1071)
library(rpart)
library(ranger)
library(shiny)
library(digest)
library(shinyjs)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(ggplot2)
library(sodium)
library(plotly)
library(DT)
library(data.table)
library(dplyr)
library(rio)
#library(uuid)
library(expss)
library(anytime)
library(rhandsontable)
library(RColorBrewer)
library(rpivotTable)
library(rvest)
library(formattable)
library(DBI)
library(odbc)
library(validate)
library(lubridate)
library(networkD3)
library(htmlwidgets)
library(htmltools)
library(treemapify)


ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "DPA Assist"
                    ),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Risk Assessment",tabName = "Risk", icon = icon("th")),
                        menuItem("Data Respondent", tabName = "Respondent", icon = icon("dashboard")),
                        menuItem("Data Processor",tabName = "Processor", icon = icon("file-download")),
                        menuItem("Consent Management",tabName = "Consent", icon = icon("check-circle")),
                        menuItem("Data Lifecycle Management",tabName = "Retention", icon = icon("database"))
                      )
                    ),
                    dashboardBody(
                      # Boxes need to be put in a row (or column)
                      tabItems(
                        # First tab content
                        tabItem(tabName = "Respondent",
                                fluidRow(
                                  ###### Application statistics tab #####
                                  fluidPage(
                                    title = 'Statistics',
                                    icon = icon('dollar'),
                                    br(),
                                    box(title = "Application Statistics",status ="primary", solidHeader = TRUE, collapsible= TRUE,width = 12, height = 400,
                                        br(),
                                        fluidRow(
                                          infoBoxOutput('numberofcontracts',width = 4)
                                        ),
                                        fluidRow(
                                          valueBoxOutput("totalstage1ecl",width = 4),
                                          valueBoxOutput("totalstage2ecl",width = 4),
                                          valueBoxOutput("totalstage3ecl",width = 4)),
                                        fluidRow(
                                          useSweetAlert(),
                                          column(6, offset=3, align="center",
                                                 actionButton(inputId = "onbal_run","Fetch Statistics",icon("calculator"),
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                    ),#end of box
                                    box(title = "Search for your Responses",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                        div(helpText("Please input your Respondent ID to retrieve your data"),
                                            textInput(inputId = 'searchRespInfo',label = 'Select Respondent ID'),
                                            br(),
                                            fluidRow(
                                              useSweetAlert(),
                                              column(6, offset=3, align="center",
                                                     actionButton(inputId = "db_fetch_RespInfo","Fetch Data",icon("calculator"),
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                            
                                        )),
                                    
                                    box(tableOutput(outputId = "report1Respvalues"),downloadLink(outputId = "downloadRespvalues"),
                                        title = "Responses per Survey Filled",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 12,bordered=TRUE),
                                    
                                    box(title = "Make a request to the data processor",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                        div(helpText("Please input the ID of the survey you would like to amend"),
                                            textInput(inputId = 'enterSurveyID',label = 'Select Survey ID'),
                                            selectInput("right_request", "Request",
                                                        c("",  "Deletion", "Amendment", "Restrict Processing", "Access")),
                                            br(),
                                            fluidRow(
                                              useSweetAlert(),
                                              column(6, offset=3, align="center",
                                                     actionButton(inputId = "db_submit_right","Submit Request",icon("calculator"),
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                            
                                        ))
                                    
                                    
                                    
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "Processor",
                                
                                fluidRow(
                                  ###### Application statistics tab #####
                                  fluidPage(
                                    title = 'Processor Statistics',
                                    icon = icon('dollar'),
                                    br(),
                                    box(title = "Survey Statistics",status ="primary", solidHeader = TRUE, collapsible= TRUE,width = 12, height = 400,
                                        br(),
                                        fluidRow(
                                          infoBoxOutput('numberofcontracts12',width = 4)
                                        ),
                                        fluidRow(
                                          valueBoxOutput("totalsurveys",width = 3),
                                          valueBoxOutput("totalresponses",width = 3),
                                          valueBoxOutput("totalrequests",width = 3),
                                          valueBoxOutput("totalpendingrequests",width = 3)),
                                        fluidRow(
                                          useSweetAlert(),
                                          column(6, offset=3, align="center",
                                                 actionButton(inputId = "fetch_survey_info","Fetch Statistics",icon("calculator"),
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                    ),#end of box
                                    box(title = "Search for your Surveys",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                        div(helpText("Please input your Researcher ID to retrieve your data"),
                                            textInput(inputId = 'searchResInfo',label = 'Select Researcher ID'),
                                            br(),
                                            fluidRow(
                                              useSweetAlert(),
                                              column(6, offset=3, align="center",
                                                     actionButton(inputId = "db_fetch_ResInfo","Fetch Data",icon("calculator"),
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                            
                                        )),
                                    box(tableOutput(outputId = "report1Resvalues"),downloadLink(outputId = "downloadResvalues"),
                                        title = "Survey Data Recorded",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 12,bordered=TRUE),
                                    
                                    
                                    box(title = "No of requests pending and No of requests",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                        div(
                                          fluidRow(
                                            column(6,offset = 0,plotlyOutput('approvalpieplot')),
                                            column(6,offset = 0,plotlyOutput('responsesline')),
                                            column(6,offset = 0,plotOutput('approvalpieplot2'))
                                            
                                          )
                                          
                                        ))
                                    
                                    
                                    
                                  )
                                )
                        ),
                        tabItem(tabName = "Consent",
                               
                                fluidRow(
                                  ###### Application statistics tab #####
                                  fluidPage(
                                    title = 'Consent Statistics',
                                    icon = icon('dollar'),
                                    br(),
                                    box(title = "Survey Statistics",status ="primary", solidHeader = TRUE, collapsible= TRUE,width = 12, height = 400,
                                        br(),
                                        fluidRow(
                                          infoBoxOutput('numberofcontracts1',width = 4)
                                        ),
                                        fluidRow(
                                          valueBoxOutput("totalresponses1",width = 3),
                                          valueBoxOutput("totalobtainedconsent",width = 3),
                                          valueBoxOutput("totalpendingconsent",width = 3),
                                          valueBoxOutput("totalwithdrawnconsent",width = 3)),
                                        fluidRow(
                                          useSweetAlert(),
                                          column(6, offset=3, align="center",
                                                 actionButton(inputId = "fetch_consent_info","Fetch Statistics",icon("calculator"),
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                    ),#end of box
                                    
                                    
                                    box(title = "Graphs",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                        div(
                                          fluidRow(
                                            column(6,offset = 0,plotlyOutput('approvalpieplot1')),
                                            column(6,offset = 0,plotlyOutput('diffconsentdays')),
                                            column(12,offset = 0,plotlyOutput('eclpieplot21'))
                                            
                                          )
                                          
                                        ))
                                    
                                    
                                    
                                  )
                                )
                        ),
                        tabItem(tabName = "Retention",
                               
                                fluidRow(
                                  ###### Application statistics tab #####
                                  fluidPage(
                                    title = 'Data Statistics',
                                    icon = icon('dollar'),
                                    br(),
                                    box(title = "Data Statistics",status ="primary", solidHeader = TRUE, collapsible= TRUE,width = 12, height = 400,
                                        br(),
                                        fluidRow(
                                          infoBoxOutput('numberofcontracts34',width = 4)
                                        ),
                                        fluidRow(
                                          valueBoxOutput("totalnodatasets",width = 4),
                                          valueBoxOutput("totalarchiveddata",width = 4),
                                          valueBoxOutput("totaldatapending",width = 4)
                                          ),
                                        fluidRow(
                                          useSweetAlert(),
                                          column(6, offset=3, align="center",
                                                 actionButton(inputId = "fetch_lifecycle_info","Fetch Statistics",icon("calculator"),
                                                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                    ),#end of box
                                    
                                    
                                    box(title = "Graphs",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                        div(
                                          fluidRow(
                                            column(6,offset = 0,plotlyOutput('dataarchived')),
                                            column(6,offset = 0,plotOutput('daystakentoarchive')),
                                            column(12,offset = 0,plotlyOutput('eclpieplot271'))
                                            
                                          )
                                          
                                        ))
                                    
                                    
                                    
                                  )
                                )
                        ),
                        tabItem(tabName = "Risk",
                               
                                fluidPage(
                                  fluidRow(
                                    box(title = "Search for your Datasets",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                        div(helpText("Please input your Researcher ID to retrieve your data"),
                                            textInput(inputId = 'searchResearcherInfo',label = 'Select Researcher ID'),
                                            br(),
                                            fluidRow(
                                              useSweetAlert(),
                                              column(6, offset=3, align="center",
                                                     actionButton(inputId = "db_fetch_ResearcherInfo","Fetch Data",icon("calculator"),
                                                                  style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                            
                                        )),
                                    
                                    box(tableOutput(outputId = "report1Researchervalues"),downloadLink(outputId = "downloadResearchervalues"),
                                        title = "Datasets stored",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 12,bordered=TRUE),
                                    box(title = "Dataset Statistics",status ="primary", solidHeader = TRUE, collapsible= TRUE,width = 12, height = 400,
                                        br(),
                                        fluidRow(
                                          infoBoxOutput('numberofcontracts5',width = 4)
                                        ),
                                        fluidRow(
                                          valueBoxOutput("totaldatasets",width = 4),
                                          valueBoxOutput("totalopenrequests",width = 4),
                                          valueBoxOutput("totalarchivaldue",width = 4))
                                       
                                    ),
                                    box(title = "Graphs",status ="primary", solidHeader = TRUE, collapsible= TRUE,width = 12, height = 400,
                                        br(),
                                        fluidRow(
                                          column(6,offset = 0,plotOutput('datasetResponses')),
                                          column(6,offset = 0,plotlyOutput('requiresaudit')),
                                          column(12,offset = 0,plotlyOutput('consentdays'))
                                        )
                                    )
                                    
                                  )
                                )
                              
                        ),
                        
                        tabItem(tabName = "ScatterPlot",
                                h2("ScatterPlots"),
                                fluidRow(
                                  box(title = "Scatter Plots",  width = 9,  background = "red",
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      plotlyOutput("view2")
                                  ),
                                  box(title = "X Axis Variable",  width = 3,  background = "red", height=110,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "three",label=h6(""),
                                                  choices = c("age","dataset_rows","consent_approved"),selected = "age")
                                  ),
                                  box(title = "Y Axis Variable",  width = 3,  background = "red", height=110,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "four",label=h6(""),
                                                  choices = c("age","dataset_rows","consent_approved","dataset_age",
                                                              "retention_length","data_ownership","loan_status","grade"),selected = "consent_approved")
                                  ),
                                  box(title = "Fill Variables",  width = 3,  background = "red", height=120,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "five",label=h6(""),
                                                  choices = c("dataset_age","retention_length","data_ownership","loan_status","grade"),selected = "dataset_age")
                                  ),
                                  box(title = "Control",  background = "black", width = 3, height=270,
                                      status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                      sliderInput("slider4", label=h5("Dataset Length"), 0, 29091, 4500),
                                      sliderInput("slider2", label=h5("Opacity"), 0, 1, 0.85)
                                  )
                                )
                        ),
                        
                        tabItem(tabName = "Histogram",
                                h2("Histograms"),
                                fluidRow(
                                  box(title = "Histogram",  width = 9,  background = "red",
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      plotlyOutput("view3")
                                  ),
                                  box(title = "X Axis Variable",  width = 3,  background = "red", height=120,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "six",label=h6(""),
                                                  choices = c("age","dataset_rows","consent_approved"),selected = "age")
                                  ),
                                  box(title = "Opacity Control",  background = "black", width = 3, height=135,
                                      status = "warning", solidHeader = TRUE, collapsible = TRUE,
                                      sliderInput("slider3", "Opacity:", 0, 1, 0.85)
                                  ),
                                  box(title = "Fill Variable",  width = 3,  background = "red", height=120,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      selectInput(inputId = "seven",label=h6(""),
                                                  choices = c("dataset_age","retention_length","data_ownership","loan_status","grade"),selected = "data_ownership")
                                  )
                                )
                        ),
                        tabItem(tabName = "Model",
                                h2("Model"),
                                fluidRow(
                                  box(title = "Bar Plots",  width = 9,  background = "red",
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      plotOutput("view4")
                                  )
                                )
                        ),
                        
                        # Second tab content
                        tabItem(tabName = "Summary",
                                h2("Summary"),
                                fluidRow(
                                  box(title = "Dataset Summary", background = "purple",
                                      width = 10,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("summary1")
                                  ),
                                  box(title = "Model Summary", background = "purple",width = 7, 
                                      status = "primary",solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("summary")
                                  )
                                )
                        ),
                        tabItem(tabName = "Results",
                                h2("Prediction"),
                                fluidRow(
                                  box(title = "Entered Observation", background = "orange",width = 8,
                                      status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                      tableOutput("table")
                                  ),
                                  box(title = "Prediction", background = "red", 
                                      status = "primary",  solidHeader = TRUE, collapsible = TRUE,
                                      verbatimTextOutput("pred")
                                  )
                                )
                        )
                      )
                    )
)

server <- function(input, output) {
  datainput = reactive({
    
    loan_data <- read.csv(curl("https://github.com/anagar20/Credit-Risk-Modeling/raw/master/loan_data.csv"))
    index_highage=which(loan_data$age>122)
    new_data <- loan_data[-index_highage, ]
    loan_data = new_data
    
    
    loan_data$retention_length <- rep(NA, length(loan_data$emp_length))
    loan_data$retention_length[which(loan_data$emp_length <= 2)] <- "0-2"
    loan_data$retention_length[which(loan_data$emp_length > 2 & loan_data$emp_length <= 4)] <- "2-4"
    loan_data$retention_length[which(loan_data$emp_length > 4 & loan_data$emp_length <= 6)] <- "4-6"
    loan_data$retention_length[which(loan_data$emp_length > 6 & loan_data$emp_length <= 10)] <- "6-10"
    loan_data$retention_length[which(loan_data$emp_length > 10 & loan_data$emp_length <= 15)] <- "10-15"
    loan_data$retention_length[which(loan_data$emp_length > 15 & loan_data$emp_length <= 30)] <- "15-30"
    loan_data$retention_length[which(loan_data$emp_length > 30 & loan_data$emp_length <= 45)] <- "30-45"
    loan_data$retention_length[which(loan_data$emp_length > 45)] <- "45+"
    loan_data$retention_length[which(is.na(loan_data$emp_length))] <- "Unknown"
    loan_data$retention_length <- as.factor(loan_data$retention_length)
    loan_data$emp_length=NULL
    
    
    loan_data$dataset_age <- rep(NA, length(loan_data$int_rate))
    loan_data$dataset_age[which(loan_data$dataset_age <= 8)] <- "0-8"
    loan_data$dataset_age[which(loan_data$dataset_age > 8 & loan_data$dataset_age <= 11)] <- "8-11"
    loan_data$dataset_age[which(loan_data$dataset_age > 11 & loan_data$dataset_age <= 13.5)] <- "11-13.5"
    loan_data$dataset_age[which(loan_data$dataset_age > 13.5)] <- "13.5+"
    loan_data$dataset_age[which(is.na(loan_data$dataset_age))] <- "Unknown"
    loan_data$dataset_age <- as.factor(loan_data$dataset_age) 
    loan_data$dataset_age=NULL
    
    loan_data$loan_status = as.factor(loan_data$loan_status)
    loan_data
  })
  
  test = reactive({
    if (input$submit > 0) {
      df <- data.frame(dataset_rows = as.integer(input$dataset_rows),grade = (input$grade),
                       data_ownership = (input$data_ownership),
                       consent_approved = (input$consent_approved),age = as.integer(input$age),
                       retention_length = (input$retention_length),dataset_age = (input$dataset_age))
      return(list(df=df))
    }
    
  })
  
  fn2 = reactive({
    train = (datainput())
    train_x = train[ ,-1]
    train_y = train[ , 1]
    myFolds <- createFolds(train_y, k = input$cv)
    
    myControl = trainControl(
      method = "cv",
      verboseIter = TRUE,
      index = myFolds
    )
    
    model <- train(
      x=train_x, y=train_y,
      method = input$model,
      tuneLength = input$tl,
      trControl = myControl,
      preProcess = c("zv","center","scale","pca")
    )
    
    model
  })
  
  new_theme = theme(panel.background = element_blank(),
                    axis.line.x   = element_line(color='black'),
                    axis.line.y   = element_line(color='black'),
                    axis.ticks    = element_line(color='black'),
                    axis.title.x  = element_text(family="Times",face = "bold", size = 12),
                    axis.title.y  = element_text(family="Times",face = "bold", size = 12),
                    axis.text     = element_text(family="Trebuchet MS",face = "italic", size = 10),
                    legend.title  = element_text(family="Times",face = "bold", size = 8),
                    legend.text   = element_text(family="Trebuchet MS",face = "italic", size = 8))
  
  
  output$view1 = renderPlotly({
    if (input$submit > 0) {
      #barplot(table(datainput()$Sex), main="BarPlot - Sex Distribution", names.arg = c("Female","Male"),col="grey")
      p1 = ggplot(datainput(), aes_string(x = input$two, fill = input$one)) +
        geom_bar(alpha = input$slider1) + theme_igray()+ new_theme
      ggplotly(p1) 
    }
    else{
      NULL
    }
  })
  
  output$view2 = renderPlotly({
    if (input$submit > 0) {
      data = datainput()
      dat = data[1:input$slider4, ]
      #barplot(table(datainput()$Sex), main="BarPlot - Sex Distribution", names.arg = c("Female","Male"),col="grey")
      p2 = ggplot(dat, aes_string(x = input$three,y =input$four,  col = input$five)) +
        geom_point(alpha = input$slider2) + theme_igray()+ new_theme
      ggplotly(p2) 
    }
    else{
      NULL
    }
  })
  
  output$view3 = renderPlotly({
    if (input$submit > 0) {
      #barplot(table(datainput()$Sex), main="BarPlot - Sex Distribution", names.arg = c("Female","Male"),col="grey")
      p3 = ggplot(datainput(), aes_string(x = input$six, fill = input$seven)) +
        geom_histogram(alpha = input$slider3) + theme_igray()+ new_theme
      ggplotly(p3) 
    }
    else{
      NULL
    }
  })
  
  output$view4 = renderPlot({
    if (input$submit > 0) {
      model = fn2()
      plot(model)
    }
    else{
      NULL
    }
  })
  
  
  output$summary1 = renderPrint({
    if (input$submit > 0) {
      summary(datainput())
    }
    else{
      NULL
    }
  })
  
  output$table = renderTable({
    if (input$submit > 0) {
      test()$df
    }
    else{
      NULL
    }
  })
  
  output$pred <- renderPrint({
    if (input$submit > 0) {
      test = test()$df
      loan_data = datainput()
      loan_data_1 = loan_data[ ,c(2,3,4,5,6,7,8)]
      ww =(rbind(loan_data_1,test))
      test = ww[29092, ]
      model = fn2()
      pp = predict(model,test)
      if(df$consent_approved <= 5000) {
        print('Data is of low risk per assessment')
      }
      else{
        print('Data is of high risk per assessment')
      }
    }
    else{
      NULL
    }
  })
  
  output$summary <- renderPrint({
    if (input$submit > 0) {
      (fn2())
    }
    
    else{
      NULL
    }
    
  })
  
  ##################################################################Data Subject Right############################################################
  ### Checking Surveys Respondend to ###
  observeEvent(input$onbal_run,{
    
    
    if (isFALSE(exists("SurveyResponses"))){output$totalstage1ecl<-renderValueBox(valueBox(subtitle = "All Responses",value = tags$p("3", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))
    }else {output$totalstage1ecl<-renderValueBox(valueBox(subtitle = "All Applications",value = tags$p(paste(prettyNum(count(Student_Info)),"Survey Responses",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))}
    
    if (isFALSE(exists("Approved_applications"))){output$totalstage2ecl<-renderValueBox(valueBox(subtitle = "Total Requests",value = tags$p("2", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="olive"))
    }else {output$totalstage2ecl<-renderValueBox(valueBox(subtitle = "Total Requests",value = tags$p(paste(prettyNum(difference,big.mark=","),"Requests to Data Procesors",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="red"))}
    
    if (isFALSE(exists("Approved_applications"))){output$totalstage3ecl<-renderValueBox(valueBox(subtitle = "Pending Requests",value = tags$p("1", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))
    }else {output$totalstage3ecl<-renderValueBox(valueBox(subtitle = "Pending Request",value = tags$p(paste(prettyNum(count(Approved_applications),big.mark=","),"Pending Request",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))}
    
  })
  #StdID variable
  observe({
    
    respondentID<-input$searchRespInfo
    .GlobalEnv$staffID<- respondentID
    surveyID<-input$searchResInfo
    .GlobalEnv$surveyID<- surveyID
    ResearcherID<-input$searchResearcherInfo
    .GlobalEnv$ResearcherID<- ResearcherID
    
  })
  #render Survey responses for respondent
  observeEvent(input$db_fetch_RespInfo,{
    RespInfo<-subset(Responses,Responses$RespondentID == staffID)
    .GlobalEnv$RespInfo<- RespInfo
    output$report1Respvalues<-renderTable({RespInfo})
    
  })
  #Save Request
  observeEvent(input$db_submit_right, {
    rightsql <- sqlInterpolate(con, 'INSERT INTO Rights_action ([SurveyID],[ResearcherID],[RespondentID],[RequestID],[Type_of_Request],[Date_of_Request],[Approve],[Date_of_Aproval]) VALUES (?SurveyID, ?ResearcherID, ?RespondentID, ?RequestID,?Type_of_Request, ?Date_of_Request, ?Approve,?Date_of_Aproval)',
                               SurveyID = input$enterSurveyID, ResearcherID = "RS001",  RespondentID = input$searchRespInfo,  RequestID = "RQ017", Type_of_Request = input$right_request, Date_of_Request = "11/09/2021", Approve ="",Date_of_Aproval="")
    
    dbExecute(con, rightsql)
    #@dbtrigger$trigger()
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Your request was saved successfully"),
                            br(),
                            div(tags$b(paste0("We will notify the data processor of the request made"), style = "color: green;"))
    ))
  })
  
  ### Checking Surveys Respondend to in data processor page###
  observeEvent(input$fetch_survey_info,{
    
    
    if (isFALSE(exists("SurveyResponses"))){output$totalsurveys<-renderValueBox(valueBox(subtitle = "All Surveys",value = tags$p("1", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))
    }else {output$totalsurveys<-renderValueBox(valueBox(subtitle = "All Surveys",value = tags$p(paste(prettyNum(count(Student_Info)),"Survey Responses",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))}
    
    if (isFALSE(exists("SurveyResponses"))){output$totalresponses<-renderValueBox(valueBox(subtitle = "Total Respondents",value = tags$p("63", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="olive"))
    }else {output$totalresponses<-renderValueBox(valueBox(subtitle = "Total Respondents",value = tags$p(paste(prettyNum(difference,big.mark=","),"Requests to Data Procesors",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="red"))}
    
    if (isFALSE(exists("SurveyResponses"))){output$totalrequests<-renderValueBox(valueBox(subtitle = "Total Requests",value = tags$p("16", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))
    }else {output$totalrequests<-renderValueBox(valueBox(subtitle = "Total Requests",value = tags$p(paste(prettyNum(count(Approved_applications),big.mark=","),"Pending Request",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))}
    
    if (isFALSE(exists("SurveyResponses"))){output$totalpendingrequests<-renderValueBox(valueBox(subtitle = "Pending Requests",value = tags$p("7", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="purple"))
    }else {output$totalpending_requests<-renderValueBox(valueBox(subtitle = "Pending Requests",value = tags$p(paste(prettyNum(count(Approved_applications),big.mark=","),"Pending Request",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="purple"))}
    
  })
  #render Survey responses for respondent
  observeEvent(input$db_fetch_ResInfo,{
    ResInfo<-subset(Surveys,Surveys$SurveyID == SurveyID)
    .GlobalEnv$RespInfo<- RespInfo
    output$report1Resvalues<-renderTable({ResInfo})
    
    output$approvalpieplot<-renderPlotly({
      not_approved<-sum(Rights_action$Approve=="")
      print(not_approved)
      approved<-sum(Rights_action$Approve=="Y")
      print(approved)
      
      labels = c('Approved','Pending')
      values = c(approved, not_approved)
      df <- data.frame(labels, values)
      
      fig <- df %>% plot_ly(labels = ~labels, values = ~values)
      
      fig <- fig %>% add_pie(hole = 0.6)
      
      fig <- fig %>% layout(
        showlegend = T,
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      fig
    })
    
    #####Bar Chart
    output$approvalpieplot2<-renderPlot({
      access = sum(Rights_action$Type_of_Request=="Access")
      rectify = sum(Rights_action$Type_of_Request=="Rectify")
      deletion<-sum(Rights_action$Type_of_Request=="Deletion")
      portability<-sum(Rights_action$Type_of_Request=="Portability")
      restrict<-sum(Rights_action$Type_of_Request=="Restrict Processing")
      
      
      Request_Type = c("Rectify", "Access", "Deletion","Portability","Restrict Processing")
      values = c(rectify,access,deletion,portability,restrict)
      df <- data.frame(Request_Type, values)
      
      ggplot(df, aes(area = values, fill = Request_Type,
                     label = paste(Request_Type, values, sep = "\n"))) +
        ggtitle("Categories of data requests") +
        geom_treemap() +
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15) +
        theme(legend.position = "right")
      plotly::ggplotly()
    })
    ####Line graph
    output$responsesline<-renderPlotly({
      fig <- plot_ly(x = ~Rights_action$RequestID, y = ~Rights_action$Days_taken_to_approve, type = 'scatter', mode = 'lines', fill = 'tozeroy')
      fig <- fig %>% layout(xaxis = list(title = 'Request_ID'),
                            yaxis = list(title = 'Days taken to respond'),
                            title = "Days taken to respond to requests ")
      
      
      fig
    })
    
    
  })
  
  
  
  ##### Fetch survey info###############################
  ### Checking Surveys Respondend to in data processor page###
  observeEvent(input$fetch_consent_info,{
    
    
    if (isFALSE(exists("SurveyResponses"))){output$totalresponses1<-renderValueBox(valueBox(subtitle = "Respondents",value = tags$p("63", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))
    }else {output$totalresponses1<-renderValueBox(valueBox(subtitle = "Respondents",value = tags$p(paste(prettyNum(count(Student_Info)),"Survey Responses",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))}
    
    if (isFALSE(exists("SurveyResponses"))){output$totalobtainedconsent<-renderValueBox(valueBox(subtitle = "Consent Obtained",value = tags$p("36", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="olive"))
    }else {output$totalobtainedconsent<-renderValueBox(valueBox(subtitle = "Consent Obtained",value = tags$p(paste(prettyNum(difference,big.mark=","),"Requests to Data Procesors",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="red"))}
    
    if (isFALSE(exists("SurveyResponses"))){output$totalpendingconsent<-renderValueBox(valueBox(subtitle = "Consent Pending",value = tags$p("25", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))
    }else {output$totalpendingconsent<-renderValueBox(valueBox(subtitle = "Consent Pending",value = tags$p(paste(prettyNum(count(Approved_applications),big.mark=","),"Pending Request",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))}
    
    if (isFALSE(exists("SurveyResponses"))){output$totalwithdrawnconsent<-renderValueBox(valueBox(subtitle = "Consent Withdrawn",value = tags$p("2", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="purple"))
    }else {output$totalwithdrawnconsent<-renderValueBox(valueBox(subtitle = "Consent Withdrawn",value = tags$p(paste(prettyNum(count(Approved_applications),big.mark=","),"Pending Request",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="purple"))}
    
    ###########Scatter
    
    output$approvalpieplot1<-renderPlotly({
      obtained <-sum(Consent$Consent.Given=="Obtained")
      pending<-sum(Consent$Consent.Given=="Pending")
      withdrawn<-sum(Consent$Consent.Given=="Withdrawn")
      consent_type<-c("Obtained","Pending","Withdrawn")
      consent_values<-c(obtained,pending,withdrawn)
      consent_df<-data.frame(consent_type, consent_values)
      
      ######Scatter#######
      fig <- plot_ly(consent_df, x = ~consent_values, y = ~consent_type, name = "Number of responses with consent obtained", type = 'scatter',
                     mode = "markers", marker = list(color = "blue"))
      fig <- fig %>% layout(
        title = "Number of responses with consent obtained",
        xaxis = list(title = "No of responses"),
        margin = list(l = 100)
      )
      
      fig
    })
    ##############Dumbell Plots
    output$diffconsentdays<-renderPlotly({
      theme_set(theme_bw())  
      
      # Data Prep
      
      # Diverging Barcharts
      # Diverging Barcharts
      ggplot(Consent_DF, aes(x=RespondentID, y=mpg_z, label="Consent")) + 
        geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
        scale_fill_manual(name="Consent", 
                          labels = c("Consent obtained", "Consent not obtained"), 
                          values = c("above"="#00ba38", "below"="#f8766d")) + 
        labs(
          title= "Respondent Consent") +
        ylim(-1.5, 1.5)+ 
        coord_flip()
    })
    
  })
  #render Risk metrics responses for researcher
  observeEvent(input$db_fetch_ResearcherInfo,{
    DatasetInfo<-subset(Risk,Risk$ResearcherID == ResearcherID)
    .GlobalEnv$RespInfo<- DatasetInfo
    output$report1Researchervalues<-renderTable({DatasetInfo})
    
    if (isFALSE(exists("Risk"))){output$totaldatasets<-renderValueBox(valueBox(subtitle = "Total Datasets",value = tags$p("3", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))
    }else {output$totaldatasets<-renderValueBox(valueBox(subtitle = "Total Datasets",value = tags$p(paste(prettyNum(3)), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))}
    
    if (isFALSE(exists("Risk"))){output$totalopenrequests<-renderValueBox(valueBox(subtitle = "Open Requests",value = tags$p("45", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="olive"))
    }else {output$totalopenrequests<-renderValueBox(valueBox(subtitle = "Open Requests",value = tags$p(paste(prettyNum(45,big.mark=",")), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="red"))}
    
    if (isFALSE(exists("Risk"))){output$totalarchivaldue<-renderValueBox(valueBox(subtitle = "Datasets due for archival",value = tags$p("1", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))
    }else {output$totalarchivaldue<-renderValueBox(valueBox(subtitle = "Datasets due for archival",value = tags$p(paste(prettyNum(1,big.mark=",")), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))}
    
    
    ######Bar Plot
    output$datasetResponses<-renderPlot({
      p <- ggplot(Risk, aes(x = Risk$DatasetID, y = Risk$Responses))+
        geom_col(aes(fill = Risk$DatasetID), width = 0.7)
      p + coord_flip()
      p + ggtitle("Responses per datatype") +
        xlab("Dataset ID") + ylab("No of responses")
      
    })
    ######Pie Plot
    output$requiresaudit<-renderPlotly({
      
      audit_x<-c("Requires Audit","Does not require audit")
      Yes = sum(Risk$Audit_Required=="Y")
      No = sum(Risk$Audit_Required=="N")
      audit_y = c(Yes,No)
      audit_data<-data.frame(audit_x,audit_y)
      print(audit_data)
      
      
      fig <- plot_ly(audit_data, labels = ~audit_x, values = ~audit_y, type = 'pie')
      fig <- fig %>% layout(title = 'Datasets scheduled for audit',
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      fig
    })
    
    ########Dumbell
    output$consentdays<-renderPlotly({
      consent_dataset<-Consent[1:25,]
      print(consent_dataset)
      
      fig <- plot_ly(consent_dataset, color = I("gray80"))
      fig <- fig %>% add_segments(x = ~consent_dataset$Date_of_response, xend = ~consent_dataset$Date_of_consent, y = ~consent_dataset$RespondentID, yend = ~consent_dataset$RespondentID, showlegend = FALSE)
      fig <- fig %>% add_markers(x = ~consent_dataset$Date_of_response, y = ~consent_dataset$RespondentID, name = "Date of response", color = I("red"))
      fig <- fig %>% add_markers(x = ~consent_dataset$Date_of_consent, y = ~consent_dataset$RespondentID, name = "Date consent given", color = I("green"))
      fig <- fig %>% layout(
        title = "Days between response and consent approval",
        xaxis = list(title = "Days of the month"),
        margin = list(l = 65)
      )
      
      fig
    })
    
  })
  #render Lifecycle
  observeEvent(input$fetch_lifecycle_info,{
   
    
    if (isFALSE(exists("Risk"))){output$totalnodatasets<-renderValueBox(valueBox(subtitle = "Total Datasets",value = tags$p("3", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))
    }else {output$totalnodatasets<-renderValueBox(valueBox(subtitle = "Total Datasets",value = tags$p(paste(prettyNum(3)), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))}
    
    if (isFALSE(exists("Risk"))){output$totalarchiveddata<-renderValueBox(valueBox(subtitle = "Archived data",value = tags$p("0", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="olive"))
    }else {output$totalarchiveddata<-renderValueBox(valueBox(subtitle = "Archived data",value = tags$p(paste(prettyNum(0,big.mark=",")), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="green"))}
    
    if (isFALSE(exists("Risk"))){output$totaldatapending<-renderValueBox(valueBox(subtitle = "Datasets due for archival",value = tags$p("1", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))
    }else {output$totaldatapending<-renderValueBox(valueBox(subtitle = "Datasets due for archival",value = tags$p(paste(prettyNum(1,big.mark=",")), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))}
    
    
    ######Bar Plot
    output$dataarchived<-renderPlotly({
      dataset1<-sum(Responses$SurveyID=="SVY001")
      dataset2<-sum(Responses$SurveyID=="SVY002")
      dataset3<-sum(Responses$SurveyID=="SVY003")
      data_rows<-c(dataset1, dataset2, dataset3)
      data_col<-c("Dataset1","Dataset2","Dataset3")
      dataset_df<-data.frame(data_col,data_rows)
      print(dataset_df)
      
      library(plotly)
      
      fig <- plot_ly(
        x = data_col,
        y = data_rows,
        name = "Datasets",
        type = "bar"
      )
      fig <- fig %>% layout(xaxis = list(title = 'Datasets'),
                            yaxis = list(title = 'Total no of rows'),
                            title = "No of records in datasets")
      
      fig
     
      
    })
    ######Lollipop Plot
    output$daystakentoarchive<-renderPlot({
      ggplot(Risk, aes(x=DatasetID, y=Days_to_Archival)) + 
        geom_point(size=3) + 
        geom_segment(aes(x=DatasetID, 
                         xend=DatasetID, 
                         y=0, 
                         yend=Days_to_Archival)) + 
        geom_point(size = 4, pch = 21, bg = 4, col = 1) +
        labs(title="Days to archival") + 
        theme(axis.text.x = element_text(angle=65, vjust=0.6))
      
    })
    
    ########Dumbell
    
    
  })
  
}

shinyApp(ui, server)