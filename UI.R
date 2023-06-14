library(shiny)
library(shinydashboard)
library(shinythemes)



shinyUI(

  fluidPage(
  theme=shinytheme("cerulean"),# returns URL of a shiny theme
  themeSelector(), ## not required but can be used to select the theme
  dashboardPage(
    dashboardHeader(title = "Math VS Portuguese Language"),
    dashboardSidebar(
      sidebarMenu(
      menuItem("About",tabName = "About", icon = icon("book")),
      menuItem("Portuguese", tabName = "Portuguese", icon = icon("dashboard")),
      menuItem("Math", tabName = "Math", icon = icon("chart-simple"))
      
    )),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "Portuguese",h1("Portuguese"),
                 fluidRow(
                   tabBox(
                          tabPanel("Correlation", plotOutput("matrix")),
                          tabPanel("Settlement", plotOutput("plot1")),
                          tabPanel("Family", plotOutput("plot2")),
                          tabPanel("Parents Edu.", plotOutput("plot3")),
                          tabPanel("Higher Edu.", plotOutput("plot4")),
                          tabPanel("Edu. Support", plotOutput("plot5")),
                          tabPanel("Alcohol", plotOutput("plot6")),
                          tabPanel("Romantic", plotOutput("plot7")),
                          tabPanel("Alone", plotOutput("plot8")),
                          tabPanel("Friends", plotOutput("plot9")),
                          tabPanel("Absences", plotOutput("plot10")),
                          tabPanel("Study Time", plotOutput("plot11"))
                          ),
                   
                     
          
        
      )
      
    ),
    tabItem(tabName = "Math",
            h1("Math"),
            fluidRow(
              tabBox(
                tabPanel("Correlation", plotOutput("matrixmath")),
                tabPanel("Settlement", plotOutput("plotmath2")),
                tabPanel("Family", plotOutput("plotmath3")),
                tabPanel("Parents Edu.", plotOutput("plotmath4")),
                tabPanel("Higher Edu.", plotOutput("plotmath5")),
                tabPanel("Edu. Support", plotOutput("plotmath6")),
                tabPanel("Alcohol", plotOutput("plotmath7")),
                tabPanel("Romantic", plotOutput("plotmath8")),
                tabPanel("Alone", plotOutput("plotmath9")),
                tabPanel("Friends", plotOutput("plotmath10")),
                tabPanel("Absences", plotOutput("plotmath11")),
                tabPanel("Study Time", plotOutput("plotmath12"))
                
                
              ),
              
              
                  
            
                
              
              
            )
    
  ),
  tabItem(tabName = "About",
          h1("About"),
          fluidPage(
          fluidRow(
          mainPanel(
            tabsetPanel(
              type = "tab",
              tabPanel("Math Data",
                       DT::dataTableOutput("mathdata"),
                       
              ),
              tabPanel("Portuguese Language Data",
                       
                       DT::dataTableOutput("pordata")
              )
            )
            
          )
          
          ),
          
            
          br(),
          br(),
            
            
            sidebarLayout(
              sidebarPanel(
                selectInput(inputId = "VarX",
                            label = "Select X-axis Variable:",
                            choices = list('school',
                                           'sex',
                                           'age',
                                           'address',
                                           'famsize',
                                           'Pstatus',
                                           'Medu' ,
                                           'Fedu' ,
                                           "Mjob" ,
                                           "Fjob",
                                           "reason" ,
                                           "guardian" ,
                                           "traveltime" ,
                                           "studytime" ,
                                           "failures" ,
                                           "schoolsup" ,
                                           "famsup" ,
                                           "paid",
                                           "activities" ,
                                           "nursery" ,
                                           "higher" ,
                                           "internet" ,
                                           "romantic" ,
                                           'famrel' ,
                                           'freetime' ,
                                           "goout" ,
                                           "Dalc" ,
                                           "Walc" ,
                                           "health",
                                           "absences" ,
                                           "G1" ,
                                           "G2" ,
                                           "G3" ,
                                           "avrg_G"
                                           )),
                        
              ),
              
              mainPanel(
                plotOutput("interplot")
              )
            )
          )
          )
          
          )
)
)
)
)



##~get(input$Var3)