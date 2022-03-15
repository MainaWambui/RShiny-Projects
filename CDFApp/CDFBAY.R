# Packages ----------------------------------------------------------------


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

con <- dbConnect(odbc(), "ncba_da", Database = "Central")
Student_Info<-(dbReadTable(con, "Student_Info_PartA1"))
Student_Info$BursaryApplicationID<-trimws(Student_Info$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
.GlobalEnv$Student_Info<- Student_Info
Approved_applications <-dbReadTable(con, "Approved_Applications")
Approved_applications$BursaryApplicationID<-trimws(Approved_applications$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
.GlobalEnv$Approved_applications<- Approved_applications


header <- dashboardHeader( title = "CDF BAY",uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"),collapsed = FALSE) 
body <- dashboardBody(shinyjs::useShinyjs(),uiOutput("body"))

#### Login Page #####
loginpage <- fluidPage(
  setBackgroundImage(src =  'cbaimage2.jpg',shinydashboard = TRUE),
  div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      
      wellPanel(
        style = "background: white; opacity: 0.9;",
        fluidRow(column(width = 12,offset = 4,imageOutput("ncbalogo",width = "200%",height = "200%"))),
        br(),
        #tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
        br(),
        textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
        passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
        br(),
        div(
          style = "text-align: center;",
          actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
          shinyjs::hidden(
            div(id = "nomatch",
                tags$p("Oops! Incorrect username or password!",
                       style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                       class = "text-center"))),
          br()
        ))
  ))
#### Credentials and Dashboard Specifics ####
credentials = data.frame(
  username_id = c("1", "2"),
  passod   = sapply(c("1", "2"),password_store),
  permission  = c("basic", "advanced"), 
  stringsAsFactors = F
)


# Define UI for application that draws a histogram
ui <- fluidPage(
  dashboardPage(header,sidebar, body,skin = "blue"))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Login Server side function
  login = FALSE
  
  USER <- reactiveValues(login = login)
  #### Logo ####
  output$ncbalogo<- renderImage({
    list(
      src = "www/ncba.jpg",
      contentType = "image/jpg",
      width = 140,
      height = 120
    )},deleteFile = FALSE
  )
  #### Login Event ####
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          .GlobalEnv$Username<- Username
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        }
      }
    }    
  })
  #### Logout Event #####
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  
  
  # Main UI Setup ----------------------------------------------------
  # Sidebar setup 
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE && Username == '1'){ 
      #### side bar ####
      sidebarMenu(
        menuItem("Student Portal", tabName = "modelinput", icon = icon("mortar-board"),startExpanded=FALSE,
                 menuSubItem("Student Profile", tabName = "historicaldatainput", icon = icon("globe-africa")),
                 menuSubItem("Application Form", tabName = "currentdatainput", icon = icon("file-text-o")),
                 menuSubItem("Application Progress",tabName = "parametersinput",icon = icon("calendar-alt"))),
                
               menuItem('About', tabName = 'aboutifrs9engine',icon = icon('info-circle'))
      )
    }else if (USER$login == TRUE && Username == '2') {
      sidebarMenu(
        menuItem("Polling Station Commitee", tabName = "polling-station", icon = icon("calculator")),
        menuItem("CDF Staff", tabName = "results", icon = icon("chart-line"),
                 menuSubItem("Staff Profile",tabName = "staff-profile",icon = icon("poll")),
                 menuSubItem("Pending Applications", tabName = "tables", icon = icon("table")),
                 # menuSubItem('Time Analysis',tabName = 'timeanalysis', icon = icon('calendar-alt')),
                 menuSubItem("Approved Applications", tabName = "calculation", icon = icon("file-download"))
        ),# end of menuitem
        menuItem('About', tabName = 'aboutifrs9engine',icon = icon('info-circle'))
      ) 
    }
  })
  
  #Body Setup -----------------------------------------------------------------------------------------
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      #### dashboard body ####
      setBackgroundImage(NULL ,shinydashboard = TRUE)
      tabItems(
        ####### Student data loading tab ######
        tabItem(tabName = "historicaldatainput",
                tags$head(
                  tags$style("label{font-family: Verdana;}")),
                tags$h1(strong("Student Data")),
                fluidRow(
                  tabsetPanel(
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("Student Profile"),
                  
                             box(title = "Please fill in your student information if it's the first time using the system to set your profile",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,
                                 collapsed = FALSE, div(
                                   
                                   textInput("fname", "First Name", ""),
                                   textInput("mname", "Middle Name", ""),
                                   textInput("lname", "Last Name", ""),
                                   textInput("Age", "Age"),
                                   textInput("CurrentSchool", "Current School"),
                                   textInput("StudentID", "Student ID"),
                                   checkboxInput("bursary", "I've applied for a bursary before", FALSE),
                                   sliderInput("num_years", "Number of years in current school", 0, 8, 2, ticks = FALSE),
                                   selectInput("school_level", "Current School Level",
                                               c("",  "Primary", "Secondary", "College", "University")),
                                   actionButton('stdInfotodb', 'Submit'),
                                   tableOutput('dbtablestdinfo')
                                 ))),
                          tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("Student Information"),
                             box(title = "Search for your student profile",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,div(helpText("Please input your Student ID to retrieve your profile"),
                                 textInput(inputId = 'report1periodscenario',label = 'Enter Student ID'),
                                 br())),
                             fluidRow(
                               useSweetAlert(),
                               column(6, offset=3, align="center",
                                      actionButton(inputId = "db_fetch","Fetch Data",icon("calculator"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                             box(tableOutput(outputId = "report1outputexposurevalues"),downloadLink(outputId = "report1downloadexposurevalues"),
                                 title = "Student Data",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=F))
                  ))),
        #About Page
        tabItem(tabName = 'aboutifrs9engine',
                setShadow("card"),
                fluidRow(
                  column(
                    width = 12,
                    align = "center",
                    box(title = "ABOUT CDF BAY Application",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                        div(
                          helpText("This application was designed as part of SWE 6110 course. In case of any questions or application help, please contact the Admin at ewambium@usiu.ac.ke")
                        ))
                  )
                )#end of row     
        ),#end of tabitem
        
        #Polling Station Applications Tab
        tabItem(tabName = "polling-station",
                tags$head(
                  tags$style("label{font-family: Verdana;}")),
                tags$h1(strong("Polling Station Applications")),
                fluidRow(
                  tabsetPanel(
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("Student Information"),
                             box(title = "Applications filed under your station ",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,div(helpText("Please input your Polling Station to retrieve applications filed for your station"),
                                   selectInput(inputId = 'polling',choices = unique(as.character(Student_Info$Polling_Station)),selected = '',label = 'Select Polling Station'),
                                   selectInput(inputId = 'Application',choices = unique(as.character(Student_Info$BursaryApplicationID)),selected = '',label = 'Select Bursary Application ID'),
                                    br())),
                             fluidRow(
                               useSweetAlert(),
                               column(6, offset=3, align="center",
                                      actionButton(inputId = "db_fetch_pollingstd","Fetch Applications",icon("calculator"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                             br(),
                             box(tableOutput(outputId = "reportPollingApplications"),downloadLink(outputId = "report1PollingApplications"),
                                 title = "Student Data",status = "primary",solidHeader = TRUE,collapsible = TRUE,width = 12,bordered=TRUE),
                             box(title = "FOR OFFICIAL USE BY THE POLLING STATION VETTING COMMITTEE",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                 div(
                                   id = "form",
                                   checkboxInput("form_signed", "This form was dully filled and signed", FALSE),
                                   checkboxInput("form_attachment", "All support documents have been attached", FALSE),
                                   checkboxInput("form_bursary", "Recommended for Bursary", FALSE),
                                   textInput("Rec_Reason", "Reasons for non recommendation", "")
                                 ),
                                 div(
                                   id = "form",
                                   textInput("Polling_chair", "Chairperson's Name", ""),
                                   textInput("Polling_cdate", "Date", ""),
                                   fileInput("file5", "Please attach official signature",
                                             accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf"))
                                   
                                 ),
                                 div(
                                   id = "form",
                                   textInput("PollingS_name", "Secretary's Name", ""),
                                   textInput("PollingS_Date", "Date", ""),
                                   fileInput("file5", "Please attach official signature",
                                             accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf"))
                                 ),
                                 div(
                                   id = "form",
                                   textInput("PollingM_name", "Member's Name", ""),
                                   textInput("Pollingm_date", "Date", ""),
                                   fileInput("file5", "Please attach official signature",
                                             accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf"))
                                 ),
                                 fluidRow(
                                   useSweetAlert(),
                                   column(6, offset=3, align="center",
                                          actionButton(inputId = "db_post_pollingstd","Post Decision",icon("calculator"),
                                                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))))
                             )
                    
                    
                  ))),
        #Staff Profile Tab
        tabItem(tabName = "staff-profile",
                tags$head(
                  tags$style("label{font-family: Verdana;}")),
                tags$h1(strong("Staff Data")),
                fluidRow(
                  tabsetPanel(
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("Staff Profile"),
                             
                             box(title = "Please fill in your information if it's the first time using the system to set your profile",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,
                                 collapsed = FALSE, div(
                                   
                                   textInput("Staff_name", "First Name", ""),
                                   textInput("Staff_mname", "Middle Name", ""),
                                   textInput("Staff_lname", "Last Name", ""),
                                   textInput("Staff_No", "Employee Number"),
                                   textInput("Staff_Role", "Current Role"),
                                   textInput("Staff_Mobile", "Mobile_No"),
                                   fluidRow(
                                     useSweetAlert(),
                                     column(6, offset=3, align="center",
                                            actionButton(inputId = "db_staffInfo","Save Data",icon("calculator"),
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                   tableOutput('dbtablestaffinfo')
                                 ))),
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("Staff Information"),
                             box(title = "Search for your Staff profile",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                 div(helpText("Please input your Staff ID to retrieve your profile"),
                                     textInput(inputId = 'searchstaffInfo',label = 'Select Staff ID'),
                                     br())),
                             fluidRow(
                               useSweetAlert(),
                               column(6, offset=3, align="center",
                                      actionButton(inputId = "db_fetch_staffInfo","Fetch Data",icon("calculator"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                             box(tableOutput(outputId = "report1Staffvalues"),downloadLink(outputId = "downloadStaffvalues"),
                                 title = "StaffData",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=F))
                  ))),
        #Retrieve Student Applications Tab
        tabItem(tabName = "tables",
                tags$head(
                  tags$style("label{font-family: Verdana;}")),
                tags$h1(strong("Bursary Applications")),
                fluidRow(
                  tabsetPanel(
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("Student Information"),
                             box(title = "Applications Filed ",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,div(helpText("Please select the ApplicationID to retrieve the students information"),
                                                                                                                                                                           
                                 selectInput(inputId = 'stdApplicationID',choices = unique(as.character(Student_Info$BursaryApplicationID)),selected = '',label = 'Select Bursary Application ID'),
                                  br())),
                             fluidRow(
                               useSweetAlert(),
                               column(6, offset=3, align="center",
                                      actionButton(inputId = "db_fetch_stdApplications","Fetch Applications",icon("calculator"),
                                                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                             box(tableOutput(outputId = "reportBursaryApplications"),downloadLink(outputId = "report1BursaryApplications"),
                                 title = "Student Data",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             box(tableOutput(outputId = "reportPartA2"),downloadLink(outputId = "report1PartA2"),
                                 title = "Student Data",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             box(tableOutput(outputId = "reportPartA22"),downloadLink(outputId = "report1PartA22"),
                                 title = "Student Additional Information",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             #box(tableOutput(outputId = "reportPartA3"),downloadLink(outputId = "report1PartA3"),
                                 #title = "Student Education Funding History",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             box(tableOutput(outputId = "reportPartA32"),downloadLink(outputId = "report1PartA32"),
                                 title = "Student Academic Performance",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             #box(tableOutput(outputId = "reportPartB1"),downloadLink(outputId = "report1PartB1"),
                                 #title = "Student Academic Performance",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             box(tableOutput(outputId = "reportPartB2"),downloadLink(outputId = "report1PartB2"),
                                 title = "Student Referrees",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             box(tableOutput(outputId = "reportPartC"),downloadLink(outputId = "report1PartC"),
                                 title = "Student Declarations",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             #box(tableOutput(outputId = "reportPartD"),downloadLink(outputId = "report1PartD"),
                                #title = "Student Verifications",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             box(tableOutput(outputId = "reportPartE"),downloadLink(outputId = "report1PartE"),
                                 title = "Polling Station Verifications",status = "primary",solidHeader = FALSE,collapsible = F,width = 12,bordered=T),
                             br(),
                             box(title = "FOR OFFICIAL USE BY THE CONSTITUENCY EDUCATION BURSARY SUB COMMITTEE",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,
                                 collapsed = FALSE, div(
                                   
                                   selectInput("is_approved", "Bursary Recommendation",
                                               c("",  "Recommended for Bursary award", "Not recommended for Bursary award")),
                                   textInput("approval_amount", "Bursary awarded Kshs.", ""),
                                   textInput("approval_reason", "Reason", ""),
                                   textInput("Sec_name", "Secretary's Name", ""),
                                   textInput("approval_Date", "Date Approved"),
                                   fluidRow(
                                     useSweetAlert(),
                                     column(6, offset=3, align="center",
                                            actionButton(inputId = "db_approvalInfo","Finalise Decision",icon("calculator"),
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
                                   tableOutput('dbapprovalInfo')
                                 ))
                             
                    )
                    
                    
                  ))),
        ###### Application statistics tab #####
        tabItem(style = "background: white; opacity: 0.9;",
                tabName = "calculation",
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
                  )#end of box
                )#end of fluid page
        ),#end of tab item
        
        ###### Std Application Progress #####
        tabItem(style = "background: white; opacity: 0.9;",
                tabName = "parametersinput",
                fluidPage(
                  title = 'Application Status',
                  icon = icon('dollar'),
                  br(),
                  box(title = "Application Progress",status ="primary", solidHeader = TRUE, collapsible= TRUE,width = 12, height = 400,
                      div(helpText("Please Enter your ApplicationID to retrieve your application status"),
                          textInput(inputId = 'apprApplicationID', label ='Enter the ApplicationID'),
                          br()),
                      br(),
                      fluidRow(
                        infoBoxOutput('Application Data',width = 4)
                      ),
                      fluidRow(
                        valueBoxOutput("totalstage1ecl1",width = 4),
                        valueBoxOutput("totalstage2ecl1",width = 4),
                        valueBoxOutput("totalstage3ecl1",width = 4)),
                      fluidRow(
                        useSweetAlert(),
                        column(6, offset=3, align="center",
                               actionButton(inputId = "onbal_run1","Submit ApplicationID",icon("calculator"),
                                            style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                  )#end of box
                )#end of fluid page
        ),#end of tab item
        
          tabItem(tabName = "currentdatainput",
                tags$head(
                  tags$style("label{font-family: Verdana;}")),
                tags$h1(strong("Application Form")),
                fluidRow(
                  tabsetPanel(
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("PART A"),
                               box(title = "TO BE FILLED BY THE APPLICANT / PARENT / GUARDIAN",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,
                                 collapsed = FALSE,div(
                                   id = "form",
                                   
                                   textInput("stdname", "Name of Student (as it appears in ID/official documents)", ""),
                                   radioButtons("gender", "GENDER", choices = c(Male ='M', Female = 'F', Intersex = 'I')),
                                   textInput("stdDOB", "DATE OF BIRTH", ""),
                                   textInput("IDNo", "ID. NO./PASSPORT NO", ""),
                                   textInput("School", "NAME OF SCHOOL /COLLEGE / UNIVERSITY", ""),
                                   textInput("regNo", "ADMISSION/REGISTRATION NUMBER", ""),
                                   textInput("CampBranch", "CAMPUS/ BRANCH: (for tertiary institution and University)", ""),
                                   textInput("Dept", "FACULTY / DEPARTMENT: (for tertiary institution and University)", ""),
                                   textInput("Course", "COURSE OF STUDY: (for tertiary institution and University)", ""),
                                   selectInput("study_type", "MODE OF STUDY",
                                               c("",  "Regular", "Parallel", "Boarding", "Day")),
                                   textInput("class", "CLASS / GRADE/ YEAR OF STUDY", ""),
                                   sliderInput("course_dur", "COURSE DURATION(In Years)", 0, 8, 2, ticks = FALSE),
                                   textInput("Comp_year", "EXPECTED YEAR AND MONTH OF COMPLETION", ""),
                                   textInput("mob_no", "MOBILE /TELEPHONE NUMBER", ""),
                                   textInput("polling_stn", "POLLING STATION", ""),
                                   textInput("ward", "WARD", ""),
                                   textInput("loc", "LOCATION", ""),
                                   textInput("subloc", "SUB LOCATION (for tertiary institution and University)", ""),
                                   textInput("phys_addr", "PHYSICAL ADDRESS (for tertiary institution and University)", ""),
                                   textInput("perm_addr", "PERMANENT ADDRESS (for tertiary institution and University)", ""),
                                   textInput("post_addr", "INSTITUTION'S POSTAL ADDRESS (for tertiary institution and University)", ""),
                                   textInput("tel_no", "INSTITUTION'S TELEPHONE NUMBER (for tertiary institution and University)", ""),
                                   textInput("appl_amnt", "AMOUNT APPLIED FOR (Kshs.) (for tertiary institution and University)", "")
                                   
           
                                 )),
                             box(title = "FAMILY BACKGROUND",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,
                                 collapsed = FALSE, div(
                                   id = "form",
                                   
                                   
                                   selectInput("Fam_status", "Kindly indicate your family status:",
                                               c("",  "Both Parents Dead", "One Parent Dead", "Both Parents Alive", "Single Parent", "Others (state)")),
                                   textInput("Siblings", "Number of siblings ( alive)", ""),
                                   textInput("Fam_Income", "Estimated Family income (annually Kshs.)", ""),
                                   textInput("Fam_expenses", "Estimated Family expenses (annually Kshs.)", ""),
                                   fileInput("stamp", "Please attach official stamp with date",
                                             accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                   textInput("Father_name", "Father's Full Name:", ""),
                                   textInput("Father_address", "Father's Address", ""),
                                   textInput("Father_telno", "Father's Telephone Number:", ""),
                                   textInput("Father_Occupation", "Father's Occupation", ""),
                                   selectInput("Father_Emptype", "Father's Type of employment (Tick appropriately)",
                                               c("",  "Permanent", "Contractual", "Casual", "Retired", "Self employed", "None")),
                                   textInput("Father_IncomeSource", "Father's Main source of income", ""),
                                   textInput("Mothers_Name", "Mother's Full Name:", ""),
                                   textInput("Mothers_Address", "Mother's Address", ""),
                                   textInput("Mothers_TelNo", "Mother's Telephone Number:", ""),
                                   textInput("Mothers_Occ", " Mother's Occupation", ""),
                                   selectInput("Mothers_EmpType", "Mother's Type of employment (Tick appropriately)",
                                               c("",  "Permanent", "Contractual", "Casual", "Retired", "Self employed", "None")),
                                   textInput("Mothers_IncomeSource", " Mother's Main source of income", ""),
                                   textInput("Guardian_Name", "Guardian's Full Name:", ""),
                                   textInput("Guardian_Address", "Guardian's Address", ""),
                                   textInput("Guardian_TelNo", "Guardian's Telephone Number:", ""),
                                   textInput("Guardian_Occ", "Guardian's Occupation", ""),
                                   selectInput("Guardian_Emp_type", "Guardian's Type of employment (Tick appropriately)",
                                               c("",  "Permanent", "Contractual", "Casual", "Retired", "Self employed", "None")),
                                   textInput("Guardian_IncomeSource", "Guardian's Main source of income", "")
                                   
                                                                    )),
 
                             box(title = "APPLICANT'S ADDITIONAL INFORMATION",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,
                                 collapsed = FALSE, div(
                                   id = "form",
                                   
                                   
                                   textInput("Application_Reason", "Why are you applying for bursary assistance?", ""),
                                   selectInput("Financial_support_Ind", "Have you received any financial support / bursaries from NG-CDF in the past?",
                                               c("",  "Yes", "No")),
                                   textInput("Last_Financial_Support", "If yes, specify how much and when you last received the support.", ""),
                                   selectInput("Other_Bursaries", "Have you received any financial support / bursaries from other organizations in the past?",
                                               c("",  "Yes", "No")),
                                   textInput("Bursary_Details", "If yes, please provide details.", ""),
                                   selectInput("Illness_Ind", "Do you suffer from any chronic illness?",
                                               c("",  "Yes", "No")),
                                   textInput("Illness_Details", "If yes, please provide details/evidence.", ""),
                                   selectInput("Parent_Disability_Ind", "Do your parents / guardians have any form of disability?",
                                               c("",  "Yes", "No")),
                                   textInput("Parent_Disability_Info", "If yes, please describe the disability.", ""),
                                   selectInput("Parent_Illness_Ind", "Do your parents / guardians suffer from any chronic illness?",
                                               c("",  "Yes", "No")),
                                   textInput("Parent_Illness_Info", "If yes, please provide details", "")
                                   
                                 )),
                             box(title = "EDUCATION FUNDING HISTORY",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                 div(
                                   id = "form",
                                   helpText("State the main source of funding for your education in the past as below:"),
                                   textInput("Main_Source_Sec", "In secondary school:", ""),
                                   textInput("Main_Source_College", "In college", ""),
                                   textInput("Main_Source_Uni", "In the university:", ""),
                                   helpText("Indicate other sources of funding if any"),
                                   textInput("Other_Source_Sec", "In secondary school:", ""),
                                   textInput("Other_Source_College", "In college", ""),
                                   textInput("Other_Source_Uni", "In the university:", ""),
                                   fluidRow(
                                     useSweetAlert(),
                                     column(6, offset=3, align="center",
                                            actionButton(inputId = "submitpartA1","Submit Part A",icon("calculator"),
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                   
                                   
                               
                                 ))),
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("PART B"),
                             
                             box(title = "APPLICANT'S ACADEMIC PERFORMANCE",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,
                                 collapsed = FALSE,div(
                                   id = "form",
                                   
                                   selectInput("Academic_Perfomance", "What is your average academic performance?",
                                               c("",  "Excellent", "Very Good", "Good", "Fair", "Poor")),
                                   checkboxInput("Sent_Away", "Have you been sent away from school? Check if yes", FALSE),
                                   textInput("Reason", "if yes, provide reasons for your absence", ""),
                                   textInput("Weeks_Absent", "Specify the number of weeks you stayed away from school", ""),
                                   textInput("Annual_Fees", "Annual fees as per fees structure Kshs", ""),
                                   textInput("Last_Fee", "Last semester's / Term's fee balance Kshs"),
                                   textInput("Current_Fee", "This semester's / Term's fee balance Kshs", ""),
                                   textInput("Next_Fee", "Next semester's / Term's fee balance Kshs"),
                                   textInput("HELB_Loan", "Loan from HELB", "")
                                   
                                 )),
                             box(title = "Referees",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,
                                 collapsed = FALSE,div(
                                   id = "form",
                                   
                                   textInput("Ref1Name", "1st Referee's Name", ""),
                                   textInput("Ref1Address", "1st Referee's Address", ""),
                                   textInput("Ref1no", "1st Referee's Phone Number", ""),
                                   textInput("Ref2Name", "2nd Referee's Name", ""),
                                   textInput("Ref2Address", "2nd Referee's Address", ""),
                                   textInput("Ref2no", "2nd Referee's Phone Number", ""),
                                   fluidRow(
                                     useSweetAlert(),
                                     column(6, offset=3, align="center",
                                            actionButton(inputId = "submitpartB1","Submit Part B",icon("calculator"),
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                   
                                   
                                 ))
                            ),
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("PART C"),
                            
                             box(title = "STUDENT DECLARATION",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                 div(
                                   helpText("I declare that I have read this form / this form has been read to me and I hereby confirm that the information given herein is true to the best of my knowledge and belief; I understand that any false information provided shall lead to my automatic disqualification by the committee."),
                                   id = "form",
                                   textInput("std_Name", "Student Name", ""),
                                   textInput("std_Date", "Date", "")
                                   
                                 )),
                             box(title = "PARENT DECLARATION",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                 div(
                                   helpText("I declare that I have read this form / this form has been read to me and I hereby confirm that the information given herein is true to the best of my knowledge and belief; I understand that any false information provided shall lead to my automatic disqualification by the committee."),
                                   
                                   id = "form",
                                   
                                   textInput("Parent_Name", "Parent Name", ""),
                                   textInput("Parent_date", "Date", ""),
                                   textInput("Parent_ID", "ID Number", ""),
                                   fluidRow(
                                     useSweetAlert(),
                                     column(6, offset=3, align="center",
                                            actionButton(inputId = "submitpartC1","Submit Declaration",icon("calculator"),
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                 ))),
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("PART D"),
                             box(title = "RELIGIOUS LEADER VERIFICATION",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                 div(
                                  id = "form",
                                   
                                   textInput("Religion", "Name of Religion", ""),
                                   radioButtons("Religion_Type", "Type of religion", choices = c(Christian ='Christian', Muslim = 'Muslim', Hindu = 'Hindu', Any_Other= 'Any Other')),
                                   textInput("Religion_Other", "If other specify", ""),
                                   textInput("Religious_Leader_Comments", "Comment on the status of the family / parents of the applicant", ""),
                                   checkboxInput("Religious_Leader_Verification", "I CERTIFY THAT THE INFORMATION GIVEN HEREIN IS TRUE", FALSE),
                                   textInput("Rel_Leader_name", "Regious Leader's Name", ""),
                                  fileInput("file4", "Please attach signature",
                                            accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                  fileInput("file5", "Please attach official stamp with date",
                                            accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf"))
                                 )),
                             box(title = "CHIEF /ASSISTANT CHIEF VERIFICATION",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                 div(
                                   id = "form",
                                   
                                   textInput("Chief_Name", "Name of the area chief / Assistant chief", ""),
                                   textInput("Chief_Location", "Location / sub location", ""),
                                   textInput("Chief_comment", "Comment on the status of the family / parents of the applicant", ""),
                                   checkboxInput("Chief_Verification", "I CERTIFY THAT THE INFORMATION GIVEN HEREIN IS TRUE", FALSE),
                                   
                                   fileInput("file4", "Please attach signature",
                                             accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                   fileInput("file5", "Please attach official stamp with date",
                                             accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                   fluidRow(
                                     useSweetAlert(),
                                     column(6, offset=3, align="center",
                                            actionButton(inputId = "submitpartD1","Submit Part D",icon("calculator"),
                                                         style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                                   
                                   
                                 ))
                                 ),
                    tabPanel(style = "background: white; opacity: 0.9;",
                             title = strong("PART E"),
                                 box(title = "Attachements",status = "primary",solidHeader = TRUE,width = 12,collapsible = TRUE,collapsed = FALSE,
                                     div(
                                       id = "form",
                                       
                                       fileInput("file5", "Please attach Students' transcript / Report Form",
                                                 accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                       fileInput("file5", "Please attach Photocopy of parents' / guardians National Identity Card",
                                                 accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                       fileInput("file5", "Please attach Photocopy of students' National Identity Card ( mandatory for post school students)",
                                                 accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                       fileInput("file5", "Please attach Photocopy of birth certificate",
                                                 accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                       fileInput("file5", "Please attach secondary / college / university ID card",
                                                 accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                       fileInput("file5", "Please attach Parents death certificate / burial permit ( mandatory for orphans)",
                                                 accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                       fileInput("file5", "Please attach Current fees structure ( mandatory for all applicants)",
                                                 accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                       fileInput("file5", "Please attach Admission letters (mandatory for colleges and universities)",
                                                 accept = c( "png/pdf","text/comma-separated-values,text/plain",".pdf")),
                                       actionButton("submit", "Submit", class = "btn-primary")
                                     )))
                  )))
    ) # End of maintab
        #####Application form tab############################
      
      ############Submit student info ##########################
      
      
      }
    else {
      loginpage
    }
  })
  #save student profile to DB
  observeEvent(input$stdInfotodb, {
    sql <- sqlInterpolate(con, 'INSERT INTO StudentInfo ([FirstName],[MiddleName],[LastName],[Age],[CurrentSchool],[StudentID],[Bursary],[Yearsinschool],[SchoolLevel]) VALUES (?FirstName, ?MiddleName, ?LastName, ?Age,?CurrentSchool, ?StudentID, ?Bursary, ?Yearsinschool, ?SchoolLevel)',
                          FirstName = input$fname, MiddleName = input$mname,  LastName = input$lname,  Age = input$Age, CurrentSchool = input$CurrentSchool,StudentID = input$StudentID, Bursary= input$bursary, Yearsinschool= input$num_years, SchoolLevel= input$school_level )
    dbExecute(con, sql)
    #dbtrigger$trigger()
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Your student information was saved successfully"),
                            br(),
                            div(tags$b(paste0("You have saved the Students data from the system database"), style = "color: green;"))
    ))
  })
  
  # view student information
  vals <- reactiveValues()
  vals1 <- reactiveValues()
  
  observeEvent(input$db_fetch,{
    #con <- dbConnect(odbc(), "ncba_da", Database = "Central")
    outsql <- dbGetQuery(con, 'SELECT [FirstName],[MiddleName],[LastName],[CurrentSchool],[StudentID],[Age],[Bursary],[YearsinSchool],[SchoolLevel]  FROM [StudentInfo]')

    StdInfo<- subset(outsql,outsql$StudentID == stdID)
    .GlobalEnv$StdInfo<- StdInfo
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Student Information Displayed"),
                            br(),
                            div(tags$b(paste0("The data has been fetched from the system database"), style = "color: green;"))
    ))
  })
  
  #render Student profile in student portal
  observeEvent(input$db_fetch,{
    output$report1outputexposurevalues<-renderTable({StdInfo})
    
  }) 
  
  
  #StdID variable
  observe({
    stdID <- input$report1periodscenario
    .GlobalEnv$stdID <- stdID
    PollingID<-input$polling
    .GlobalEnv$PollingID<- PollingID
    staffID<-input$searchstaffInfo
    .GlobalEnv$staffID<- staffID
    ApplicationID<-input$Application
    .GlobalEnv$ApplicationID<- ApplicationID
    sdApplicationID<-input$stdApplicationID
    stdApplicationID<-trimws(sdApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$stdApplicationID<- stdApplicationID
    appApplicationID<-input$apprApplicationID
    apprApplicationID<-trimws(appApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$apprApplicationID<- apprApplicationID
  })
  observe({
    
    today <- Sys.Date()
    Bursary_Date<<- format(today, format="%d");
    Registration_No<- input$regNo
    BursaryApp_ID <<- paste("NG_CDF", Registration_No, Bursary_Date,  sep ='-')
    Bursary_Application_ID  <- input$BursaryApp_ID
    Student_Name <- input$stdname
    Gender<- input$gender
    Date_of_Birth<- input$stdDOB
    ID_No<- input$IDNo
    School<- input$School
    Registration_No<- input$regNo
    Campus_Branch<- input$CampBranch
    Department<- input$Dept
    Course_Duration<- input$Course
    Mobile_No<- input$mob_no
    Polling_Station<- input$polling_stn
    Ward<- input$ward
    Location<- input$loc
    SubLocation<- input$subloc
    Physical_address<- input$phys_addr
    Institution_Postal_address<- input$post_addr
    Telephone_No<- input$tel_no
    Application_Amount<- input$appl_amnt
    Family_Status<- input$Fam_status
    No_Of_Siblings<- input$Siblings
    Family_Income<- input$Fam_Income
    Family_Expenses<- input$Fam_expenses
    Fathers_Name<- input$Father_name
    Fathers_Address<- input$Father_address
    Fathers_Tel_No<- input$Father_telno
    Fathers_Occupation<- input$Father_Occupation
    Fathers_Employment_Type<- input$Father_Emptype
    Fathers_Income_Source<- input$Father_IncomeSource
    Mothers_Name<- input$Mothers_Name
    Mothers_Address<- input$Mothers_Address
    Mothers_Tel_No<- input$Mothers_TelNo
    Mothers_Occupation<- input$Mothers_Occ
    Mothers_Employment_Type<- input$Mothers_EmpType
    Mothers_Income_Source<- input$Mothers_IncomeSource
    Guardians_Name<- input$Guardian_Name
    Guardians_Address<- input$Guardian_Address
    Guardians_Tel_No<- input$Guardian_TelNo
    Guardians_Occupation<- input$Guardian_Occ
    Guardians_Employment_Type<- input$Guardian_Emp_type
    Guardians_Income_Source<- input$Guardian_IncomeSource
    Application_Reason<- input$Application_Reason
    Financial_Support_Indicator<- input$Financial_support_Ind
    Last_Financial_Support<- input$Last_Financial_Support
    Other_Bursaries<- input$Other_Bursaries
    Bursary_Details<- input$Bursary_Details
    Student_Illness_Indicator<- input$Illness_Ind
    Student_Illness_Details<- input$Illness_Details
    Parent_Disabilty_Indicator<- input$Parent_Disability_Ind
    Parent_Disabilty_Details<- input$Parent_Disability_Info
    Parent_Illness_Indicator<- input$Parent_Illness_Ind
    Parent_Illness_Details<- input$Parent_Illness_Info
    Main_Funding_Secondary<- input$Main_Source_Sec
    Main_Funding_College<- input$Main_Source_College
    Main_Funding_University<- input$Main_Source_Uni
    Other_Funding_Secondary<- input$Other_Source_Sec
    Other_Funding_College<- input$Other_Source_College
    Other_Funding_University<- input$Other_Source_Uni
    
    
    
    
  })

  #save student partA form details to DB
  observeEvent(input$submitpartA1, {
    ###Construct query to insert values into database table
    qry = paste0("INSERT INTO Student_Info_PartA1 (Student,Gender,Date_of_Birth,ID_No,School,Registration_No,Campus_Branch,Department,Course_Duration,Mobile_No,Polling_Station,Ward,Location,Physical_address,Institution_Postal_address,Telephone_No,Application_Amount,Course_Name,Sub_Location,BursaryApplicationID)",
                 "VALUES ('",paste(input$stdname,"'",",","'",input$gender,"'",",","'",input$stdDOB,"'",",","'",input$IDNo,"'",",","'",input$School,"'",",","'",input$regNo,"'",",","'",input$CampBranch,"'",",","'",input$Dept,"'",",","'",input$course_dur,"'",",","'",input$mob_no,"'",",","'",input$polling_stn,"'",",","'",input$ward,"'",",","'",input$loc,"'",",","'",input$physaddr,"'",",","'",input$postaddr,"'",",","'",input$telno,"'",",","'",input$applamnt,"'",",","'",input$Course,"'",",","'",input$subloc,"'",",","'",BursaryApp_ID,"')"))
    qry2 = paste0("INSERT INTO Student_Info_PartA2 (BursaryApplicationID,Family_Status,Sibling_No,Family_Income,Family_Expenses,Fathers_Name,Fathers_Address,Fathers_Tel_No,Fathers_Occupation,Fathers_Employment_Type,Fathers_Income_Source)",
                  "VALUES ('",paste(BursaryApp_ID,"'",",","'",input$Fam_status,"'",",","'",input$Siblings,"'",",","'",input$Fam_Income,"'",",","'",input$Fam_expenses,"'",",","'",input$Father_name,"'",",","'",input$Father_address,"'",",","'",input$Father_telno,"'",",","'",input$Father_Occupation,"'",",","'",input$Father_Emptype,"'",",","'",input$Father_IncomeSource,"')"))
    qry21 = paste0("INSERT INTO Student_Info_PartA22 (BursaryApplicationID,Mothers_Name,Mothers_Address,Mothers_Tel_No,Mothers_Occupation,Mothers_Employment_Type,Mothers_Income_Source,Guardians_Name,Guardians_Address,Guardians_Tel_No,Guardians_Occupation,Guardians_Employment_Type,Guardians_Income_Source)",
                   "VALUES ('",paste(BursaryApp_ID,"'",",","'",input$Mothers_Name,"'",",","'",input$Mothers_Address,"'",",","'",input$Mothers_TelNo,"'",",","'",input$Mothers_Occ,"'",",","'",input$Mothers_EmpType,"'",",","'",input$Mothers_IncomeSource,"'",",","'",input$Guardian_Name,"'",",","'",input$Guardian_Address,"'",",","'",input$Guardian_TelNo,"'",",","'",input$Guardian_Occ,"'",",","'",input$Guardian_Emp_type,"'",",","'",input$Guardian_IncomeSource,"')"))
    qry3 = paste0("INSERT INTO Student_Info_PartA3 (BursaryApplicationID,Application_Reason,Financial_Support_Indicator,Last_Financial_Support,Other_Bursaries,Bursary_Details,Student_Illness_Indicator,Student_Illness_Details,Parents_Disability_Indicator,Parents_Disability_Details,Parents_Illness_Indicator,Parents_Illness_Details)",
                  "VALUES ('",paste(BursaryApp_ID,"'",",","'",input$Application_Reason,"'",",","'",input$Financial_support_Ind,"'",",","'",input$Last_Financial_Support,"'",",","'",input$Other_Bursaries,"'",",","'",input$Bursary_Details,"'",",","'",input$Illness_Ind,"'",",","'",input$Illness_Details,"'",",","'",input$Parent_Disability_Ind,"'",",","'",input$Parent_Disability_Info,"'",",","'",input$Parent_Illness_Ind,"'",",","'",input$Parent_Illness_Info,"')"))
    qry32 = paste0("INSERT INTO Student_Info_PartA32 (BursaryApplicationID,Main_Funding_Secondary,Main_Funding_College,Main_Funding_University,Other_Funding_Secondary,Other_Funding_College,Other_Funding_University)",
                   "VALUES ('",paste(BursaryApp_ID,"'",",","'",input$Main_Source_Sec,"'",",","'",input$Main_Source_College,"'",",","'",input$Main_Source_Uni,"'",",","'",input$Other_Source_Sec,"'",",","'",input$Other_Source_College,"'",",","'",input$Other_Source_Uni,"')"))
    
    
    ###Query to send to database
   dbSendQuery(con, statement = qry)
   dbSendQuery(con, statement = qry2)
   dbSendQuery(con, statement = qry21)
   dbSendQuery(con, statement = qry3)
   dbSendQuery(con, statement = qry32)
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Student Information Part A data Inserted"),
                            br(),
                            div(tags$b(paste0("You have Inserted the data into the system database"), style = "color: green;"))
    ))
  })
  #save student partB form details to DB
  observeEvent(input$submitpartB1, {
    ###Construct query to insert values into database table
    qryB = paste0("INSERT INTO Student_Info_PartB1 (BursaryApplicationID,Academic_Performance,Been_Sent_Away,Reason_Absent,Weeks_Absent,Annual_Fees,Last_Term_Fees,Current_Term_Fee,Next_Sem_Fee,Helb_Loan)",
                 "VALUES ('",paste(BursaryApp_ID,"'",",","'",input$Academic_Perfomance,"'",",","'",input$Sent_Away,"'",",","'",input$Reason,"'",",","'",input$Weeks_Absent,"'",",","'",input$Annual_Fees,"'",",","'",input$Last_Fee,"'",",","'",input$Current_Fee,"'",",","'",input$Next_Fee,"'",",","'",input$HELB_Loan,"')"))
    qryB2 = paste0("INSERT INTO Student_Info_PartB2 (BursaryApplicationID,Referee_1_Name,Referee_1_Address,Referee_1_Telno,Referee_2_Name,Referee_2_Address,Referee_2_Telno)",
                  "VALUES ('",paste(BursaryApp_ID,"'",",","'",input$Ref1Name,"'",",","'",input$Ref1Address,"'",",","'",input$Ref1no,"'",",","'",input$Ref2Name,"'",",","'",input$Ref2Address,"'",",","'",input$Ref2no,"')"))
    
    ###Query to send to database
    dbSendQuery(con, statement = qryB)
    dbSendQuery(con, statement = qryB2)

    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Student Information Part B data Inserted"),
                            br(),
                            div(tags$b(paste0("You have Inserted the data into the system database"), style = "color: green;"))
    ))
  })
  #save student partC form details to DB
  observeEvent(input$submitpartC1, {
    ###Construct query to insert values into database table
    qryC = paste0("INSERT INTO Student_Info_PartC (BursaryApplicationID,Student_Name,Student_Declaration_Date,Parent_Name,Parent_Declaration_Date,Parent_IDNo)",
                  "VALUES ('",paste(BursaryApp_ID,"'",",","'",input$std_Name,"'",",","'",input$std_Date,"'",",","'",input$Parent_Name,"'",",","'",input$Parent_date,"'",",","'",input$Parent_ID,"')"))
     
    ###Query to send to database
    dbSendQuery(con, statement = qryC)
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Student Declarations Part C data Inserted"),
                            br(),
                            div(tags$b(paste0("You have Inserted the data into the system database"), style = "color: green;"))
    ))
  })
  #save student partC form details to DB
  observeEvent(input$submitpartD1, {
    ###Construct query to insert values into database table
    qryD = paste0("INSERT INTO Student_Info_PartD (BursaryApplicationID,Religion,Religion_Other,Religious_Leader_Comments,Religious_Leader_Name,Chiefs_Name,Chief_Location,Chief_Comment, Religion_Type, Chief_Verification, Religious_Leader_Verification)",
                  "VALUES ('",paste(BursaryApp_ID,"'",",","'",input$Religion,"'",",","'",input$Religion_Other,"'",",","'",input$Religious_Leader_Comments,"'",",","'",input$Rel_Leader_name,"'",",","'",input$Chief_Name,"'",",","'",input$Chief_Location,"'",",","'",input$Chief_comment,"'",",","'",input$Religion_Type,"'",",","'",input$Chief_Verification,"'",",","'",input$Religious_Leader_Verification,"')"))
    
    ###Query to send to database
    dbSendQuery(con, statement = qryD)
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Student Declarations Part D data Inserted"),
                            br(),
                            div(tags$b(paste0("You have Inserted the data into the system database"), style = "color: green;"))
    ))
  })

  #Get Application Information for polling station
  observeEvent(input$db_fetch_pollingstd, {
    polloutsql <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartA1]')
    polloutsql$BursaryApplicationID<-trimws(polloutsql$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$polloutsql<- polloutsql
    
    PollingStudentsID<- subset(polloutsql,polloutsql$Polling_Station == PollingID)
    .GlobalEnv$PollingStudentsID<- PollingStudentsID
    #ApplicationID<-PollingStudentsID$BursaryApplicationID
    
    PollingStudentInfo<-subset(polloutsql,polloutsql$BursaryApplicationID == ApplicationID)
    .GlobalEnv$PollingStudentInfo<- PollingStudentInfo
    #print(PollingStudentInfo)
    
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("All Applications made to your polling station have been loaded"),
                            br(),
                            div(tags$b(paste0("You have loaded the Applications data from the system database"), style = "color: green;"))
    ))
  })
  
  #render Student profile for polling station officials
  observeEvent(input$db_fetch_pollingstd,{
    output$reportPollingApplications<-renderTable({PollingStudentInfo})
    
  })
  #save Polling approvals profile to DB
  observeEvent(input$db_post_pollingstd, {
    pollingsql <- sqlInterpolate(con, 'INSERT INTO Student_Info_PartE ([BursaryApplicationID]
      ,[Form_Signed]
      ,[Documents_Attached]
      ,[Recommended_Bursary]
      ,[Recommendation_Reason]
      ,[Polling_Chairperson]
      ,[Chair_Signed]
      ,[Secretary_Name]
      ,[Sec_Signed]
      ,[Member_Name]
      ,[Member_Signed]) VALUES (?SBAID, ?form_signed, ?form_attachment, ?form_bursary,?Rec_Reason, ?Polling_chair,?Polling_cdate, ?PollingS_name,?PollingS_Date,?PollingM_name,?Pollingm_date)',
                               SBAID = input$Application, form_signed = input$form_signed,  form_attachment = input$form_attachment,  form_bursary = input$form_bursary, Rec_Reason = input$Rec_Reason, Polling_chair = input$Polling_chair, Polling_cdate = input$Polling_cdate, PollingS_name = input$PollingS_name, PollingS_Date = input$PollingS_Date, PollingM_name = input$PollingM_name, Pollingm_date = input$Pollingm_date)
     
    dbExecute(con, pollingsql)
    #@dbtrigger$trigger()
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Your information was saved successfully"),
                            br(),
                            div(tags$b(paste0("You have saved the Recommendation to the system database"), style = "color: green;"))
    ))
  })
  #save Staff profile to DB
  observeEvent(input$db_staffInfo, {
    staffsql <- sqlInterpolate(con, 'INSERT INTO StaffInfo ([Employees_First_Name],[Employees_Middle_Name],[Employees_Last_Name],[Staff_No],[Staff_Role],[Staff_Mobile_No]) VALUES (?SFirstName, ?SMiddleName, ?SLastName, ?StaffNo,?StaffRole, ?StaffMobile)',
                          SFirstName = input$Staff_name, SMiddleName = input$Staff_mname,  SLastName = input$Staff_lname,  StaffNo = input$Staff_No, StaffRole = input$Staff_Role, StaffMobile = input$Staff_Mobile)
    
    dbExecute(con, staffsql)
    #@dbtrigger$trigger()
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Your information was saved successfully"),
                            br(),
                            div(tags$b(paste0("You have saved the Employee's data from the system database"), style = "color: green;"))
    ))
  })
  vals1 <- reactiveValues()
  
  observeEvent(input$db_fetch_staffInfo,{
    staffsql <- dbGetQuery(con, 'SELECT [Employees_First_Name],[Employees_Middle_Name],[Employees_Last_Name],[Staff_No],[Staff_Role],[Staff_Mobile_No] FROM [StaffInfo]')
    .GlobalEnv$staffsql<- staffsql
    StaffInfo<- subset(staffsql,staffsql$Staff_No == staffID)
    .GlobalEnv$StaffInfo<- StaffInfo
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Staff Data have been loaded"),
                            br(),
                            div(tags$b(paste0("You have loaded the Staff data from the system database"), style = "color: green;"))
    ))
    
  }) 
  
  #render Staff profile for polling station officials
  observeEvent(input$db_fetch_staffInfo,{
    output$report1Staffvalues<-renderTable({StaffInfo})
    
  })
  
  #Get Application Information
  observeEvent(input$db_fetch_stdApplications, {
    stdInfosql <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartA1]')
    stdInfosql$BursaryApplicationID<-trimws(stdInfosql$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$stdInfosql<- stdInfosql
    PartA2 <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartA2]')
    PartA2$BursaryApplicationID<-trimws(PartA2$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$PartA2<- PartA2
    PartA22 <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartA22]')
    PartA22$BursaryApplicationID<-trimws(PartA22$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$PartA22<- PartA22
    #PartA3 <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartA3]')
    #PartA3$BursaryApplicationID<-trimws(PartA3$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    #.GlobalEnv$PartA3<- PartA3
    PartA32 <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartA32]')
    PartA32$BursaryApplicationID<-trimws(PartA32$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$PartA32<- PartA32
    #PartB1 <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartB1]')
    #PartB1$BursaryApplicationID<-trimws(PartB1$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    #.GlobalEnv$PartB1<- PartB1
    PartB2 <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartB2]')
    PartB2$BursaryApplicationID<-trimws(PartB2$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$PartB2<- PartB2
    PartC <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartC]')
    PartC$BursaryApplicationID<-trimws(PartC$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$PartC<- PartC
    #PartD <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartD]')
    #PartD$BursaryApplicationID<-trimws(PartD$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    #.GlobalEnv$PartD<- PartD
    PartE <- dbGetQuery(con, 'SELECT * FROM [Student_Info_PartE]')
    PartE$BursaryApplicationID<-trimws(PartE$BursaryApplicationID, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
    .GlobalEnv$PartE<- PartE
    
    
    CDFStudentInfo<-subset(stdInfosql,stdInfosql$BursaryApplicationID == stdApplicationID)
    .GlobalEnv$CDFStudentInfo<- CDFStudentInfo
    stdPartA2<-subset(PartA2,PartA2$BursaryApplicationID == stdApplicationID)
    .GlobalEnv$stdPartA2<- stdPartA2
    stdPartA22<-subset(PartA22,PartA22$BursaryApplicationID == stdApplicationID)
    .GlobalEnv$stdPartA22<- stdPartA22
    #stdPartA3<-subset(PartA3,PartA3$BursaryApplicationID == stdApplicationID)
    #.GlobalEnv$stdPartA3<- stdPartA3
    stdPartA32<-subset(PartA32,PartA32$BursaryApplicationID == stdApplicationID)
    .GlobalEnv$stdPartA32<- stdPartA32
    #stdPartB1<-subset(PartB1,PartB1$BursaryApplicationID == stdApplicationID)
    #.GlobalEnv$stdPartB1<- stdPartB1
    stdPartB2<-subset(PartB2,PartB2$BursaryApplicationID == stdApplicationID)
    .GlobalEnv$stdPartB2<- stdPartB2
    stdPartC<-subset(PartC,PartC$BursaryApplicationID == stdApplicationID)
    .GlobalEnv$stdPartC<- stdPartC
    #stdPartD<-subset(PartD,PartD$BursaryApplicationID == stdApplicationID)
    #.GlobalEnv$stdPartD<- stdPartD
    stdPartE<-subset(PartE,PartE$BursaryApplicationID == stdApplicationID)
    .GlobalEnv$stdPartE<- stdPartE
    
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("All Applications have been loaded"),
                            br(),
                            div(tags$b(paste0("You have loaded the Applications data from the system database"), style = "color: green;"))
    ))
  })
  #render Std data for CDF Staff
  observeEvent(input$db_fetch_stdApplications,{
    output$reportBursaryApplications<-renderTable({CDFStudentInfo})
    output$reportPartA2<-renderTable({stdPartA2})
    output$reportPartA22<-renderTable({stdPartA22})
    #output$reportPartA3<-renderTable({stdPartA32})
    output$reportPartA32<-renderTable({stdPartA32})
    #output$reportPartB1<-renderTable({stdPartB1})
    output$reportPartB2<-renderTable({stdPartB2})
    output$reportPartC<-renderTable({stdPartC})
    #output$reportPartD<-renderTable({stdPartD})
    output$reportPartE<-renderTable({stdPartE})
    
  })
  #Approved Apps
  observeEvent(input$onbal_run,{
                
                difference = (count(Student_Info)-count(Approved_applications))
    
               
                 if (isFALSE(exists("Student_Info"))){output$totalstage1ecl<-renderValueBox(valueBox(subtitle = "All Applications",value = tags$p("No Data Yet", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))
                 }else {output$totalstage1ecl<-renderValueBox(valueBox(subtitle = "All Applications",value = tags$p(paste(prettyNum(count(Student_Info)),"Total Applications",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))}
                 
                 if (isFALSE(exists("Approved_applications"))){output$totalstage2ecl<-renderValueBox(valueBox(subtitle = "Pending Applications",value = tags$p("No Data Yet", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="olive"))
                 }else {output$totalstage2ecl<-renderValueBox(valueBox(subtitle = "Pending Applications",value = tags$p(paste(prettyNum(difference,big.mark=","),"Pending Applications",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="red"))}
                 
                 if (isFALSE(exists("Approved_applications"))){output$totalstage3ecl<-renderValueBox(valueBox(subtitle = "Approved Applications",value = tags$p("No Data Yet", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))
                 }else {output$totalstage3ecl<-renderValueBox(valueBox(subtitle = "Approved Applications",value = tags$p(paste(prettyNum(count(Approved_applications),big.mark=","),"Approved Applications",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))}
                 
})
  
  #Application Status
  observeEvent(input$onbal_run1,{
    stdApplicationStatus <-subset(Approved_applications,Approved_applications$BursaryApplicationID == apprApplicationID)
    .GlobalEnv$stdApplicationStatus<- stdApplicationStatus
    NoApps<-subset(Student_Info,Student_Info$BursaryApplicationID == apprApplicationID)
    .GlobalEnv$NoApps<- NoApps
    
    if (isFALSE(exists("NoApps"))){output$totalstage1ecl1<-renderValueBox(valueBox(subtitle = "All Applications",value = tags$p("Application Yet to be Approved", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))
    }else {output$totalstage1ecl1<-renderValueBox(valueBox(subtitle = "All Applications",value = tags$p(paste(prettyNum(count(NoApps)),"Total Applications",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="light-blue"))}
    
    if (isFALSE(exists("stdApplicationStatus"))){output$totalstage2ecl1<-renderValueBox(valueBox(subtitle = "Approved Applications",value = tags$p("Application Yet to be Approved", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="olive"))
    }else {output$totalstage2ecl1<-renderValueBox(valueBox(subtitle = "Approved Applications",value = tags$p(paste(prettyNum(count(stdApplicationStatus),big.mark=","),"Approved Applications",sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="red"))}
    
    if (isFALSE(exists("Approved_applications"))){output$totalstage3ecl1<-renderValueBox(valueBox(subtitle = "Approved Amount",value = tags$p("Application Yet to be Approved", style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))
    }else {output$totalstage3ecl1<-renderValueBox(valueBox(subtitle = "Approved Amount",value = tags$p(paste("Kshs.", prettyNum(stdApplicationStatus$Approval_Amount,big.mark=","),sep=" "), style = "font-size: 50%;",style = "padding-bottom:15px;"),icon = icon("calculator fa-1x"), color="yellow"))}
    
  })
  #save Staff profile to DB
  observeEvent(input$db_approvalInfo, {
    staffsql <- sqlInterpolate(con, 'INSERT INTO Approved_Applications ([BursaryApplicationID],[Is_Approved],[Approval_Amount],[Approval_Reason],[Sec_Name],[Approval_Date]) VALUES (?BAID, ?is_approved, ?approval_amount, ?approval_reason,?Sec_name, ?approval_Date)',
                               BAID = input$stdApplicationID, is_approved = input$is_approved,  approval_amount = input$approval_amount,  approval_reason = input$approval_reason, Sec_name = input$Sec_name, approval_Date = input$approval_Date)
    
    dbExecute(con, staffsql)
    #@dbtrigger$trigger()
    
    ###shinyModal to show to user when the update to the database table is successful
    showModal( modalDialog( title=paste0("Your information was saved successfully"),
                            br(),
                            div(tags$b(paste0("You have saved the Final Decision to the system database"), style = "color: green;"))
    ))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
