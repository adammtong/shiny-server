## -===================================================================================================- ##
## -========================================== LIBRARIES ==============================================- ##
## -===================================================================================================- ##
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(highcharter)
## -===================================================================================================- ##
## -========================================== DEFINE UI ==============================================- ##
## -===================================================================================================- ##

## DEFINE HEADER
header <- dashboardHeader(
  title = "Worldwide COVID-19 proliferation"
)

## DEFINE SIDEBAR
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tabs",
    menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard"), startExpanded = TRUE,
      menuSubItem("Global Overview", tabName = "overview", icon = icon("globe")),
      menuSubItem("Country Level", tabName = "country", icon = icon("binoculars"))),
    menuItem("Modelling", tabName = "Modelling", icon = icon("info-circle")),
    menuItem("About", tabName = "About", icon = icon("info-circle"))
  )
)

## DEFINE BODY
body <- dashboardBody(
  
  ## select dashboardtheme
  shinyDashboardThemes(
    theme = "blue_gradient"
  ),
  
  tabItems(
    tabItem(tabName = "overview",
            fluidRow(
              tags$script(src = "https://code.highcharts.com/mapdata/custom/world-robinson-highres.js"),
              withMathJax(),
              column(width =12,
                       htmlOutput('wiki_summary'),
                     p(class = "text-info",
                       paste(
                             "All data is sourced from the Johns Hopkins University Center for Systems Science and Engineering via https://github.com/CSSEGISandData/COVID-19/")
                     )
              )
              ),
            fluidRow(
              column(width =12,
                     h3(textOutput("overview_title"), align = "left")
              )
              ),
            fluidRow(
              column(width = 12,
              valueBoxOutput("days_since_first_confirmed_value_box", width = 3),
              valueBoxOutput("confirmed_value_box", width = 3),
              valueBoxOutput("deaths_value_box", width = 3),
              valueBoxOutput("recovered_value_box", width = 3)
              )
              ),
            fluidRow(
              column(width = 8,
                            selectInput("sorting_var", "Select category", choices = c("Confirmed","Deaths","Recovered"), selected = "Confirmed"))
            ),
            fluidRow(
              column(width = 12,
                     h5("Please click on a country to get see the evolution of COVID-19 over time for that particular country."))
            ),
            fluidRow(
              column(width = 8,
                     box(width = NULL, solidHeader = TRUE,
                         highchartOutput("map_hc")
                     ),
              ),
              column(width = 4,
                     tabBox(
                       id = "top10",
                       width = NULL,
                       tabPanel("Top 10 cumulative total",
                                textOutput("date_text_tab1"),
                                DT::dataTableOutput("top10_total")),
                       tabPanel("Top 10 day-on-day increase",
                                textOutput("date_text_tab2"),
                                DT::dataTableOutput("top10_change"))
                       )
                     )
              )
          ), #end overview tab
    tabItem(tabName = "country",
            fluidRow(
              column(width =8,
                     h3(textOutput("country_overview_title"), align = "left")
              ),
              
            ),
            fluidRow(
              column(width = 8,
                     selectInput("country", "Select your country", choices = NULL)
                     )
              ),
              
              fluidRow(
                column(width = 12,
                     valueBoxOutput("country_days_since_first_confirmed_value_box", width = 3),
                     valueBoxOutput("country_confirmed_value_box", width = 3),
                     valueBoxOutput("country_deaths_value_box", width = 3),
                     valueBoxOutput("country_recovered_value_box", width = 3)
                ),
                
              ),
              fluidRow(
                column(width = 8,
                     box(width = NULL,
                         highchartOutput("tsPlot"),
                         highchartOutput("tsDiffPlot")
                     )
                ),
                column(width = 4,
                       box(width = NULL, status = "warning",
                           h3("Latest coronavirus news"),
                           DT::dataTableOutput("news_df"),
                           p(
                             class = "text-muted",
                             paste("powered by NewsAPI.org"))
                           )
                       )
                )
            ), ## end country tab
    tabItem(tabName = "Modelling",
            h2("Using the modelling dashboard"),
          tags$br(),
            paste0("This page provides an interface to fiddle with theparameters of the SIR model in order to see ",
                   "how they affect the predicted infection level of Australia in the future. I may add all other countries in the future ",
                   "depending on the availability and stability of the data source."),
          tags$br(),
          tags$br(),
            tags$div("The initialised values for the parameters are the optimal parameters based on the dates selected to the left.", 
                   " Once changed, the new predictions will be replotted along with the new value of \\(R_0\\)."),
          tags$br(),
          actionLink("model_info", "Click here for specification of the SIR model"),
          tags$br(),
          tags$br(),
          HTML(paste0(p(class = 'text-muted',
                   paste0("Note the range starts in late February for Australia as there is a significant period of time in ",
                  "early-mid February where there no new cases occured, before ramping up again.")))),
            
            tags$br(),
            fluidRow(
              column(width = 4,
                     box(width = NULL, solidHeader = TRUE,
                         dateRangeInput("SIR_daterange", 
                                        label = h4(HTML("<i class='glyphicon glyphicon-calendar'></i> Select date range of data used to fit model")), 
                                        start = '2020-02-25', end= Sys.Date()-3, 
                                        min = '2020-01-22', max = Sys.Date()), 
                         sliderInput("beta", "Beta - infection rate:",
                                     min = 0, max = 1,
                                     value = 0),#0.5657528),
                         sliderInput("gamma", "Gamma - recovery rate:",
                                     min = 0, max = 1,
                                     value = 0),#0.4342468),,
                         checkboxInput("check_fit", label = "Inspect SIR model fit", value = FALSE),
                     )
              ),
              column(width = 8,
                     box(width = NULL, solidHeader = TRUE,
                         highchartOutput("SIR_model"))
                     )
              )
          ), #end modelling tab
    tabItem(tabName = "About",
            h2("About"),
            h4("Created by Adam Ong"),
            paste0("This app was built to provide a central dashboard to browse high-level infection numbers by country for the COVID-19 virus. ",
            "I was sick of not having worldwide numbers in one spot,  parcticularly as I was keen on tracking the trajectory of the infection rate ",
            "across several countries."),
            h3("Terms of use"),
            HTML(paste0("The following is taken from the GitHub repo which hosts the source data: https://github.com/CSSEGISandData/COVID-19/", br(),br())),
            tags$em("This GitHub repo and its contents herein, including all data, mapping, and analysis, copyright 2020 Johns Hopkins University
            all rights reserved, is provided to the public strictly for educational and academic research purposes. The Website relies upon publicly
            available data from multiple sources, that do not always agree. The Johns Hopkins University hereby disclaims any and all representations
            and warranties with respect to the Website, including accuracy, fitness for use, and merchantability. Reliance on the Website for medical
            guidance or use of the Website in commerce is strictly prohibited.")
    ) # end About Tab
  ) # end tabItems
) # end dashboardBody

## put it all together
dashboardPage(
  header,
  sidebar,
  body
)


