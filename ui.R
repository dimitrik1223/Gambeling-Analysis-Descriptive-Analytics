#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http:/shiny.rstudio.com/


library(shiny)

# Load packages
if(!require("shiny")) install.packages("shiny"); library("shiny")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("plotly")) install.packages("plotly"); library("plotly")
if(!require("readxl")) install.packages("readxl"); library("readxl")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("DT")) install.packages("DT"); library("DT")
if(!require("haven")) install.packages("haven"); library("haven")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("tidyr")) install.packages("tidyr"); library("tidyr")
if(!require("stringr")) install.packages("stringr"); library("stringr")
if(!require("maps")) install.packages("maps"); library("maps")
if(!require("maptools")) install.packages("maptools"); library("maptools")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("shinydashboard")) install.packages("shinydashboard"); library("shinydashboard")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("shinythemes")) install.packages("shinythemes"); library("shinythemes")
if(!require("dashboardthemes")) install.packages("dashboardthemes"); library("dashboardthemes")
if(!require("sqldf")) install.packages("sqldf"); library("sqldf")
if(!require("rworldmap")) install.packages("rworldmap"); library("rworldmap")
if(!require("priceR")) install.packages("priceR"); library("priceR")
if(!require("rsconnect")) install.packages("rsconnect"); library("rsconnect")
if(!require("mapproj")) install.packages("mapproj"); library("mapproj")



countries_apx <- read_excel("appendice.xlsx", sheet = "country_nm")
countries_apx = unique(countries_apx$"Country Name")
countries_apx = sort(countries_apx)

product_apx <- read_excel("appendice.xlsx", sheet = "prod")
language_apx <- read_excel("appendice.xlsx", sheet = "lang")
app_nm_apx <- read_excel("appendice.xlsx", sheet = "app_nm")

df_donut <- read.csv("df_country.csv")

load("base_table.Rdata")

#Load base table:
#load("base_table.RData")
  
###################################### UI ###################################################

header <- dashboardHeader(title = "Marketing Analysis")

sidebar <- dashboardSidebar( 
  
  #Sidebar
  sidebarMenu(
    menuItem("Overview", tabName = "overview", icon = icon("r-project")),
    menuItem("Summary", icon = icon("map"), tabName = "summary"),
    menuItem("Demographics", icon = icon("map"), tabName = "demographics"),
    menuItem("Product", tabName = "product", icon = icon("gamepad")),
    menuItem("Gambling", icon = icon("dice"), tabName = "gambleing"),
    menuItem("Customers Profiling", icon = icon("smile"), tabName = "customers"),
    menuItem("Conclusion", icon = icon("smile"), tabName = "conclusion"),
    menuItem("Basetable", tabName = "basetable", icon = icon("table")),
    menuItem("Play Area - Experimental", tabName = "distribution", icon = icon("users"))
      )
  )


body <- dashboardBody(
  ### changing theme
  shinyDashboardThemes(
    theme = "purple_gradient"
  ),
  
  
  ############ Tab: Overview #################
  tabItems( 
    tabItem (tabName = "overview",
     #        ,
    #         h3("IESEG School of Management"),
    ##         br(),
    #         h3("The most relevant insights from a business perspective that our analysis brought to the forefront are as follow:"),
    #         br(),
             mainPanel(
               fluidRow(
                 align = "center",
                 img(src='ieseg.png'),
                 h1(tags$span(style = "color: #fcba03;", "IESEG School of Management")),
                 br(),br(),
                 h1(tags$span(style = "color: #ffffff;", "Business Analysis Tools: Open Source")),
                 
                 br(),br(),
                 h1(tags$span(style = "color: #ffffff;", "Descriptive Analytics for an Online Gambling Company")),
                 br(),br(),br(),
                 h3(tags$span(style = "color: #ffffff;", "A group project by:")),
                 h3(tags$span(style = "color: #ffffff;", "Inderpreet RANA, Dimitri KESTENBAUM & Nixia sancy JOHN")),
                 
               ), width = 12
             )
    
             
             
             
             
    ),
    
    ############ Tab: summary #################    
    tabItem (tabName = "summary",
             fluidRow(
             h1("Summary"),
             box(
               h2(valueBoxOutput("profit_box")),
               h2(valueBoxOutput("buy_box")),
               h2(valueBoxOutput("sell_box")),
               h2(valueBoxOutput("wins_box")),
               h2(valueBoxOutput("stakes_box")),
               h2(valueBoxOutput("bets_box")),
             
             )             
             )
             
             
    ),
    
    
    
    
    ############ Tab: base table #################    
    tabItem (tabName = "basetable",
             downloadButton("downloadData", "Download"),
             div(style = 'overflow-x: scroll', DT::dataTableOutput('Basetable'))
             ),
    
    
    
    ############ Tab: Demographics #################     
    tabItem (tabName = "demographics",  fluidRow(
      
      # Demographics page header
      titlePanel("Geo Analysis"),
    
      tabsetPanel(
        tabPanel("Global Presence", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Global Presence"), 
                                   plotOutput("map", width = "100%")
                          )))),
        tabPanel("EU Presence", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("EU Presence"), 
                                   plotOutput("eu_map", width = "100%")
                          )))),
        tabPanel("Top Countries by Users", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Top countries by users"), 
                                   plotlyOutput("demographics", width = "100%")
                          )))),
        tabPanel("Top Countries - Gender Distribution", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Top countries by users - Excluding Germany"), 
                                   plotlyOutput("demographics2", width = "100%")
                          )))),
        tabPanel("Gender Distribution - Germany", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Gender Distribution in Germany"), 
                                   plotOutput("demographics_de6", width = "100%")
                          )))),
        
        tabPanel("Germany - Average bets by Gender", br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Analysis of customer from Germany"), 
                                   box(plotlyOutput("demographics_de"), width = 6),
                                   box(plotlyOutput("demographics_de2"), width = 6),
                                   box(plotlyOutput("demographics_de3"), width = 6),
                                   box(plotlyOutput("demographics_de4"), width = 6),
                                   box(plotlyOutput("demographics_de5"), width = 6),
                          )))),
        
      ))),
    
    #############Tab: Product  #################
    tabItem (tabName = "product",  fluidRow(
      # Source Page Header
      titlePanel("Product Analysis"),
      tabsetPanel(
        tabPanel("Overall",  br(),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   h3("Popular products (excluding poker)"),
                                   plotOutput("Product", width = "100%"),
                                   
                          )))),
    
        tabPanel("Best & Worst Performing Applications", br(),
                   mainPanel("",
                   fluidRow(
                   splitLayout(cellWidths = c("50%", "50%")),
#                   selectInput("Country", "Select a country",
#                               c("Germany","Turkey","Poland","Spain","Greece","France","Denmark","Austria","Italy","Switzerland"),
#                               selected ="Germany"),
                                      box(sliderInput("top_apps", "Select number of apps to compare",
                               min=1, max=20,value=5, step=2),
                               br(),
                   plotlyOutput("plt_top_apps"))
                   ,
                   
                   box(sliderInput("bottom_apps", "Select number of apps to compare",
                               min=1, max=20,value=5, step=2),
                       br(),
                   plotlyOutput("plt_bottom_apps")))
                   
                   
                       
                 ),
                 mainPanel(
                   column(width=11,
                          fluidRow(height = 200,
                                   
                                   
                          ))))                  
      ))),

############ Tab: gambleing #################     
   
    tabItem (tabName = "gambleing",
             h1("Gambling - Winners Analysis"),
             plotOutput("gambleing", width = "100%")
             
    ),

#############Tab: distribution/playplots  #################
tabItem (tabName = "distribution",  fluidRow(
  # Source Page Header
  titlePanel("Play area for custom ploting"),
  h4("Does not work when deployed :("),
  tabsetPanel(
    tabPanel("Hist",  br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 200,
                               selectInput("var", "Select a Variable to plot",
                                           names(final_base_table[ , unlist(lapply(final_base_table, is.numeric))]),
                                           selected ="txn_cnt"),
                               plotOutput("distribution", width = "100%")
                               
                      )))),
    tabPanel("More Plots",  br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 200,
                               box(selectInput("var_x", "Select a Variable to plot (X)",
                                           names(final_base_table[ , unlist(lapply(final_base_table, is.numeric))]),
                                           selected ="txn_cnt")),
                               box(selectInput("var_y", "Select a Variable to plot (Y)",
                                           names(final_base_table[ , unlist(lapply(final_base_table, is.numeric))]),
                                           selected ="amount")),
                               plotOutput("playplots", width = "100%")
                               
                      ))))
  ))),

############tab: customer profiling##########
tabItem (tabName = "customers",  fluidRow(
  # Source Page Header
  titlePanel("Play area for custom ploting"),
  tabsetPanel(
    tabPanel("Segments by Loyalty Scores",  br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 200,
                               box(h4("Segments by Loyalty Scores"),
                               plotlyOutput("plt_loyalty", width = "100%")),
                               box(h4("Segments by Clustering Scores"),
                               plotlyOutput("plt_clustering", width = "100%"))
                               
                      )))),
    tabPanel("Segments by Clustering",  br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 200,
                               selectInput("ncluster", "Select number of clusters",
                                               c(2,3,4,5,6,7,8,9,10),
                                               selected =5),
                               
                               plotlyOutput("plt_clustering_cstm", width = "100%")
                               
                      )))),
    tabPanel("Customer Languages",  br(),
             mainPanel(
               column(width=11,
                      fluidRow(height = 200,
                               plotlyOutput("plt_language", width = "100%")
                      ))))
    
  ))),


#############################################
tabItem (tabName = "conclusion",
         fluidRow(
           img(src='conc.png', align = "left", height = 80),
           br(),br(),br(),br(),
           box(
           br(),
           h3("The most relevant insights from a business perspective that our analysis brought to the forefront are as follow:"),
           br(),
           h4(tags$div(
            tags$ul(tags$li("We have a small population of loyalists with a mid-range betting frequency, who seldomly win big, but bet a lot of money, making them our most profitable gamblers.")
                          )),
           h4(tags$div(
           tags$ul(tags$li("We have large population of passive or casual betters with low-frequency betting, whom are winning at a larger rate substantially than our loyalists")
          ))),
           h4(tags$div(
           tags$ul(tags$li("RFM is weakly correlated with profit for Bwin, and gamblers with a mid-range RFM value are our
            most profitable segment of gamblers.")))),
          h4(tags$div(
            tags$ul(tags$li("The majority of the customer base is from Germany or from Europe, hence we need to focus more on the customers from other regions and analyze why they don't use our products")))),
          h4(tags$div(
            tags$ul(tags$li("We can do a further analysis of the customer segments if we get more data points such as customer Age, Income etc."))))

           )
           , width = 12) ))

    
  ))# end of tab item and body      
shinyUI(fluidPage(
                  dashboardPage(header, sidebar, body)
))      

