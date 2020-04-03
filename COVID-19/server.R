## -===================================================================================================- ##
## -========================================== LIBRARIES ==============================================- ##
## -===================================================================================================- ##
library(dplyr)
library(readr)
library(rvest)
library(leaflet)
library(highcharter)
library(tidyr)
library(stringr)
library(DT)
library(httr)
library(jsonlite)
library(TTR)
library(deSolve)
library(lubridate)
source('auxiliary_functions.R')

## -========================================================================================================- ##
## -=========================================== DEFINE CONSTANTS ===========================================- ##
## -========================================================================================================- ##

## set data path
local_data_path <- 'data/'
local_confirmed <- paste0(local_data_path,'confirmed.csv')
local_recovered <- paste0(local_data_path,'recovered.csv')
local_deaths <-  paste0(local_data_path,'deaths.csv')


source_data_path <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'

## news api details 
news_api_key <- read_csv(paste0(local_data_path,"access_tokens.csv")) %>% 
  filter(service == 'news_api') %>% pull(token)
news_api_path <- "http://newsapi.org/v2/everything?"
github_api_path <- "https://api.github.com/repos/CSSEGISandData/COVID-19/contents/csse_covid_19_data/csse_covid_19_time_series"

## wikipedia scraping details
wiki_path <- "https://en.wikipedia.org/wiki/Coronavirus_disease_2019"
wiki_summary_val <- wiki_path %>%
  read_html() %>%
  html_nodes('p') %>%
  .[3] %>%
  html_text()

## -===================================================================================================- ##
## -=========================================== IMPORT DATA ===========================================- ##
## -===================================================================================================- ##

## see if source data repo has been updated - use GitHub v3 rest API
## GitHub details of data currently being used
current_github_details <- read.csv(paste0(local_data_path,'github_details.csv'), stringsAsFactors = FALSE) 
## send GET request
request <- GET(url = github_api_path)

## response from request
response <- content(request, as = "text", encoding = "UTF-8")

## transform JSON response to dataframe
github_details <- fromJSON(response, flatten = TRUE) %>% 
  filter(name %in% c('time_series_covid19_confirmed_global.csv',
                     'time_series_covid19_deaths_global.csv',
                     'time_series_covid19_recovered_global.csv')) %>% 
  select(name,sha) 

## check if hashes for all of the files are not equal or if local files don't exists - if so, fetch new data
if(all.equal(current_github_details,github_details) != TRUE | !all(file.exists(local_confirmed,local_recovered, local_deaths))){
  print('data has changed - fetching latest')
   confirmed_csv <- read_csv(paste0(source_data_path,"time_series_covid19_confirmed_global.csv"), 
                            col_names = TRUE) %>% mutate(type = "Confirmed")
   
  recovered_csv <- read_csv(paste0(source_data_path,"time_series_covid19_recovered_global.csv"),
                            col_names = TRUE) %>% mutate(type = "Recovered")
  
  deaths_csv <- read_csv(paste0(source_data_path,"time_series_covid19_deaths_global.csv"), 
                         col_names = TRUE) %>% mutate(type = "Deaths")
  
  ## update github_details.csv
  github_details %>% write.csv(paste0(local_data_path,'github_details.csv'), row.names = FALSE)
  
  ## update data on disk
  confirmed_csv %>% write.csv(local_confirmed, row.names = FALSE)
  recovered_csv %>% write.csv(local_recovered, row.names = FALSE)
  deaths_csv %>% write.csv(local_deaths, row.names = FALSE)
  
} else {
  print('data is unchanged - loading from disk')
  confirmed_csv <- read_csv(paste0(local_data_path,'confirmed.csv'), col_names = TRUE)
  recovered_csv <- read_csv(paste0(local_data_path,'recovered.csv'), col_names = TRUE)
  deaths_csv <- read_csv(paste0(local_data_path,'deaths.csv'), col_names = TRUE)
}

## combine data
ts_all_data = list(confirmed = confirmed_csv,
                   recovered = recovered_csv,
                   deaths = deaths_csv)

## -================================== PREPROCESSING FOR TIME SERIES ===================================- ##
## combine and give nice names
ts_all_data_flattened <- do.call(rbind, ts_all_data)
rownames(ts_all_data_flattened) <- NULL
colnames(ts_all_data_flattened) <- make.names(colnames(ts_all_data_flattened))

## data wrangling
## summary by country and time
ts_data_flattened_clean <- ts_all_data_flattened %>% 
  select(-Province.State) %>% 
  pivot_longer(cols = starts_with("X")) %>% 
  mutate(name = str_extract(name,"[0-9]+.[0-9]+.[0-9]+") %>% as.Date('%m.%d.%y')) %>% 
  group_by(Country.Region) %>% 
  mutate(Lat = mean(Lat, na.rm = TRUE),
         Long = mean(Long, na.rm = TRUE)) %>% 
  group_by(Country.Region, type, name) %>% 
  summarise(value = sum(value, na.rm = TRUE),
            Lat = mean(Lat, na.rm = TRUE),
            Long = mean(Long, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = type, values_from = value) %>% 
  rename(Date = name) %>% 
  arrange(Country.Region, Date) %>% 
  ungroup()

## rename counrties
ts_data_flattened_clean <- ts_data_flattened_clean %>% 
  mutate(Country.Region = case_when(Country.Region == "US" ~ "United States of America",
                                    Country.Region == "South Korea" ~ "Korea, South",
                                    TRUE ~ Country.Region))

## get max date
max_date <- ts_data_flattened_clean %>% summarise(maxdate = max(Date)) %>% pull(maxdate)

## get global summary over time
ts_data_flattened_clean_global <- ts_data_flattened_clean %>% 
  group_by(Date) %>%  
  summarise(Confirmed = sum(Confirmed, na.rm = TRUE), 
            Deaths = sum(Deaths, na.rm = TRUE), 
            Recovered = sum(Recovered, na.rm = TRUE)) %>% 
  mutate(Country.Region = "Global", Lat = NA, Long = NA) %>% 
  select(Country.Region, Date, Lat, Long, Confirmed, Deaths, Recovered)

##combine datasets
ts_all_data_flattened_clean <- rbind(ts_data_flattened_clean_global,ts_data_flattened_clean) %>% 
  group_by(Country.Region) %>% 
  arrange(Country.Region) %>% 
  mutate(Confirmed.Delta = Confirmed - lag(Confirmed),
         Recovered.Delta = Recovered - lag(Recovered),
         Deaths.Delta = Deaths - lag(Deaths),
         Confirmed.Delta.Percentage = case_when(Confirmed - Confirmed.Delta != 0 ~ Confirmed.Delta / (Confirmed - Confirmed.Delta),
                                                TRUE ~ NA_real_),
         Recovered.Delta.Percentage = case_when(Recovered - Recovered.Delta != 0 ~ Recovered.Delta / (Recovered - Recovered.Delta),
                                                TRUE ~ NA_real_),
         Deaths.Delta.Percentage = case_when(Deaths - Deaths.Delta != 0 ~ Deaths.Delta / (Deaths - Deaths.Delta),
                                             TRUE ~ NA_real_),
         Confirmed.Delta.Percentage.EMA = EMA_dplyr(Confirmed.Delta.Percentage, n = 5),
         Recovered.Delta.Percentage.EMA = EMA_dplyr(Recovered.Delta.Percentage, n = 5),
         Deaths.Delta.Percentage.EMA = EMA_dplyr(Deaths.Delta.Percentage, n = 5))

## set countries for dropdown
countries <- ts_all_data_flattened_clean %>% 
  filter(Country.Region != 'Global') %>% 
  select(Country.Region) %>% 
  unique() %>% 
  pull() 
  

## -========================================================================================================- ##
## -================================================== server ==============================================- ##
## -========================================================================================================- ##
server <- function(input, output, session) {
    
  ## update country selection choices
  updateSelectInput(session,"country",choices=c('Global',countries))
  
  ## update date sliders in modelling section
  updateDateRangeInput(session, "SIR_daterange", 
                       label = NULL, 
                       start = NULL,
                       end = max_date, 
                       min = NULL, 
                       max = max_date)
  
  ## get vals for value box - global summary
  global_summary_vals <- ts_all_data_flattened_clean %>% 
    filter(Date == max(Date, na.rm = TRUE) & Country.Region == 'Global') %>% 
    select(-Lat, -Long)
  
  ## get date of first confirmed - Global
  global_first_confirmed_vals <- ts_all_data_flattened_clean %>% 
    filter(Confirmed !=0) %>% 
    filter(row_number() == 1) %>% 
    filter(Country.Region == 'Global') 
  
  ## -===================================================================================================- ##
  ## -====================================== DEFINE REACTIVES  ==========================================- ##
  ## -===================================================================================================- ##
  
  country_summary_vals_reactive <- reactive({
    ts_all_data_flattened_clean %>% 
      filter(Date == max(Date, na.rm = TRUE) & Country.Region == input$country)
  })
  
  ## get date of first confirmed - all coutries
  country_first_confirmed_vals_reactive <- reactive({
    ts_all_data_flattened_clean %>% 
      filter(Confirmed !=0) %>% 
      filter(row_number() == 1) %>% 
      filter(Country.Region == input$country)
  })
  
  ts_filtered_reactive <- reactive(ts_all_data_flattened_clean %>% 
                                     filter(Country.Region == input$country))
  

  ## top 10 total countries
  top_10_countries_total_reactive <- reactive({
    ts_all_data_flattened_clean %>% 
      filter(Date == max(Date, na.rm = TRUE) & Country.Region != "Global") %>% 
      select(Country.Region, Confirmed, Deaths, Recovered) %>% 
      arrange(desc(!!sym(input$sorting_var))) %>% 
      filter(row_number() <= 10) %>%
      datatable(
               colnames = c('Country','Confirmed','Deaths','Recovered')
              ,rownames = TRUE
              ,fillContainer = FALSE
              ,options = list(columnDefs = list(list(className = 'dt-head-center dt-center', targets = "_all"))
                              ,dom = 't'
                              ,ordering = T
                              ,pageLength = 10)
              ,escape = FALSE) %>% 
      formatCurrency(2:4,currency = "", interval = 3, mark = ",", digits = 0)

  })
  
  ## top 10 change countries
  top_10_countries_change_reactive <- reactive({
    ts_all_data_flattened_clean %>% 
      filter(Date == max(Date, na.rm = TRUE) & Country.Region != "Global") %>% 
      select(Country.Region, Confirmed.Delta, Deaths.Delta, Recovered.Delta) %>% 
      arrange(desc(!!sym(paste0(input$sorting_var,".Delta")))) %>% 
      filter(row_number() <= 10) %>%   
      datatable(
        colnames = c('Country','Confirmed increase','Deaths increase', 'Recovered increase"')
        ,rownames = T
        ,fillContainer = FALSE
        ,options = list(columnDefs = list(list(className = 'dt-head-center dt-center', targets = "_all"))
                        ,dom = 't'
                        ,ordering = T
                        ,pageLength = 10)
        ,escape = FALSE) %>%
      formatCurrency(2:4,currency = "", interval = 3, mark = ",", digits = 0)
  })
  
  ## choropleth reactive
  choropleth_reactive <- reactive({
    map_values <- ts_all_data_flattened_clean %>%
      filter(Date == max(Date, na.rm = TRUE) & Country.Region != "Global") %>% 
      mutate(Confirmed = case_when(Confirmed <= 0 ~ 0.001, TRUE ~ as.numeric(Confirmed)),
             Deaths = case_when(Deaths <= 0 ~ 0.001, TRUE ~ as.numeric(Deaths)),
             Recovered = case_when(Recovered <= 0 ~ 0.001, TRUE ~ as.numeric(Recovered))
             )
    
    #js click function
    clickFunction <- JS("function(event) {Shiny.onInputChange('clicked', event.point.name);}")
    
    map <- hcmap("custom/world-robinson-highres",
          data = map_values,
          value = input$sorting_var,
          download_map_data = FALSE,
          joinBy = c("name","Country.Region"),
          name = paste0("Number of ", tolower(input$sorting_var)),
          dataLabels = list(enabled = TRUE, format = '{point.name}'),
          borderColor = "#FAFAFA", borderWidth = 0.1,
          # tooltip = list(valueDecimals = 0)) %>%
          tooltip = list(pointFormatter = JS("function() {
                              return  this.name + ': '  +
                              this.value.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, \"$1,\") ; 
                                             }")
          )) %>%
      hc_plotOptions(series = list(events = list(click = clickFunction))) %>%
      hc_title(text = paste0("Total count of ", input$sorting_var," as at ", format(max_date,'%d-%b-%y'))) %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_colorAxis(type = "logarithmic") 

    map

  })

 ## news reactive
  news_reactive <- reactive({
    qry <- list(qInTitle = paste0("coronavirus", ifelse(input$country == "Global", "", paste0(" AND ",input$country))),
                # from=Sys.Date(),
                # category = "health",
                sortBy='popularity',
                language = 'en',
                apiKey=news_api_key)
    ## send GET request
    request <- GET(url = news_api_path, 
                   query = qry)
    
    ## response from request
    response <- content(request, as = "text", encoding = "UTF-8")
    
    ## transform JSON response to dataframe
    df_news <- fromJSON(response, flatten = TRUE) 
    
    ## handle if no news is returned
    if(df_news$totalResults != 0 ){
      df_news_output <- df_news %>% 
        data.frame() %>% 
        select(articles.source.name, articles.publishedAt, articles.title, articles.url, articles.urlToImage) %>% 
        mutate(articles.urlToImage = paste0('<a href="',articles.url,'" target="_blank">',
                                            '<img src="',
                                            articles.urlToImage,
                                            '" height="50"></img></a>')) %>% 
        select(articles.urlToImage,articles.title) %>% 
        group_by(articles.title) %>% 
        filter(row_number() == 1) %>% 
        ungroup() %>% 
        filter(row_number() <= 10) 
    } else {
      
      df_news_output <- data.frame(articles.urlToImage = NA,articles.title = "No news articles available")
    }
    
    df_news_output_dt <- df_news_output %>% 
      datatable( colnames = c('','')
                 ,rownames = FALSE
                 ,fillContainer = FALSE
                 ,options = list(columnDefs = list(list(className = 'dt-head-left dt-left', targets = "_all"))
                                 ,dom = 't'
                                 ,ordering = F
                                 ,pageLength = 10)
                 ,escape = FALSE)
  })
  
  ## -========================================== MODELLING REACTIVES ====================================- ##
  
  modelling_data_reactive <- reactive({
    sir_start_date <- input$SIR_daterange[1]
    sir_cut_off_date = input$SIR_daterange[2]
    
    ## define modelling dataset - it updates slider input for optimal beta/gamma vals as as well
    ## as returns the reactive dataset and associated parameters
    modelling_data <- ts_all_data_flattened_clean %>% 
      ungroup() %>% 
      filter(Country.Region == 'Australia') %>% 
      filter(Date>=sir_start_date & Date <= sir_cut_off_date) %>% 
      arrange(Date)
    
    ## get number of infected
    Infected <- modelling_data %>% 
      # mutate(Confirmed = Confirmed - lag(Confirmed, default = min(Confirmed) )) %>%
      pull(Confirmed) 
    
    print(Infected)
    
    ## get initial conditions for Recovered
    R_init <- modelling_data %>% filter(row_number() == 1) %>% pull(Recovered)
    ## get initial conditions for Infected
    I_init <- modelling_data %>% filter(row_number() == 1) %>% pull(Confirmed)
    
    # Create an incrementing Day vector the same length as cases vector
    Day <- 1:(length(Infected))
    
    # now specify initial values for S, I and R
    ## initial population for Australia
    N <- 25421386
    init <- c(S = N - I_init-R_init, I = I_init, R = R_init)
    
    ## Define time vector to define how far into the future forecasts go
    t <- 1:120
    
    ## find optimal beta / gamma by minimising RSS of observed infected and predicted infected incidence rates
    ## use logistic to satisfy constraints that beta/gamma are in [0,1]
    RSS <- function(parameters) {
      names(parameters) <- c("beta", "gamma")
      out <- ode(y = init, times = Day, func = SIR, N=N, parms = logistic(parameters)) %>% data.frame()
      fit <- out[, 3]
      # fit <- out %>% mutate(I_diff = I - lag(I, default = min(I))) %>% pull(I_diff)
      sum((Infected - fit)^2)
    }
    
    ## get optimal values
    Opt <- optim(c(0.5, 0.5),
                 RSS,
                 method = "L-BFGS-B"
    )
    
    ## check convergence
    print(Opt$message)
    print(Opt$par)
    Opt_par <- setNames(logistic(Opt$par), c("beta", "gamma"))
    
    ## update slider input with optimal value of params
    updateSliderInput(session, 'beta', min = 0, max = 1, value = Opt_par[[1]])
    updateSliderInput(session, 'gamma', min = 0, max = 1, value = Opt_par[[2]])
    
    ## return required data
    all_data <- list(modelling_data = modelling_data,
         Day = Day,
         init = init,
         N = N,
         t = t,
         sir_start_date = sir_start_date,
         sir_cut_off_date = sir_cut_off_date)
    
    all_data
    
    
  })
  
  ## prepare SIR model for plotting - reactive
  SIR_model_reactive <- reactive({
    ## get dates from shiny input
    N <-modelling_data_reactive()$N
    init <- modelling_data_reactive()$init
    t <- modelling_data_reactive()$t
    sir_start_date <- ymd(modelling_data_reactive()$sir_start_date)
    modelling_data <- modelling_data_reactive()$modelling_data
    
    # #get the fitted values from our SIR model
    params <- setNames(c(input$beta, input$gamma), c("beta", "gamma"))
  
    ## solve ODE with selected parameters
    fitted_cumulative_incidence <- ode(y = init, 
                                       times = t, 
                                       func = SIR, 
                                       N = N,
                                       parms = params) %>% 
      data.frame()
    
    fitted_cumulative_incidence <- fitted_cumulative_incidence %>%
      mutate(Date = sir_start_date + lubridate::days(t - 1)) %>%
      left_join(modelling_data  %>%
                  select(Date, Confirmed, Country.Region),
                by = "Date")

  })
  
  ## -===================================================================================================- ##
  ## -============================================= EVENTS ==============================================- ##
  ## -===================================================================================================- ##
  
  ## go to country specific details when country is selected from choropleth
  observeEvent(input$clicked, {
    print(paste0("You clicked on series ", input$clicked,".")) 
    updateTabItems(session, "tabs", selected = "country")
    updateSelectInput(session,"country",choices=c('Global',countries), selected = input$clicked)
  })
  
  ## model popup with SIR specification
  observeEvent(input$model_info, {
    showModal(modalDialog(
      title = "Model specification",
      h3("Modelling - SIR model"),
      paste0("The Susceptible-Infected-Recovered model is a compartmental model in which the population is divided into three 'compartments':"),
      tags$ul(
        tags$li("Susceptible (S) - the part of the population that can be infected at the current point in time"),
        tags$li("Infected (I) - the part of the population which are infected at the current point in time"),
        tags$li("Recovered (R) - the part of the population which have recovered from infection at the current point in time. Recovered means they can no longer transmit the disease; they have been isolated/immune from the disease or have died.")
      ),
      tags$br(),
      paste0("The dynamics of the three compartments are governed a system of three ordinary differential equations (ODE)."),
      tags$br(),
      tags$br(),
      # section below allows in-line LaTeX via $ in mathjax.
      tags$ol(
        tags$li(withMathJax("\\(\\frac{dS}{dt}  = - \\frac{\\beta I S}{N}\\)"),tags$div(tags$br())),
        tags$li(tags$div("\\(\\frac{dI}{dt} =  \\frac{\\beta I S}{N} - \\gamma I\\)"),tags$br()),
        tags$li(tags$div("\\(\\frac{dR}{dt} = \\gamma I \\)"),tags$br())
      ),
      tags$div("where \\(\\beta\\) is the infection rate and \\(\\gamma\\) is the recovery rate. We can then define the basic reproduction number as"),
      tags$div("$$ R_0 = \\frac{\\beta}{\\gamma} $$"),
      paste0("This ratio is derived as the expected number of new infections (these new infections are sometimes called secondary infections) from a single infection in a population where all subjects are susceptible."),
      
      h4("Some comments"),
      tags$ul(
        tags$li(tags$div("\\(\\beta\\) is the infection rate - \\(\\beta I S\\) represents the number of susceptible individuals that become infected per unit time.")),
        tags$li(tags$div("\\(\\gamma\\) is the recovery rate rate - \\(\\gamma I\\) represents the number of individuals that recover per unit time.")),
        tags$li(tags$div("\\(\\gamma^{-1}\\) can be interpreted as the average duration of time an individual remains infected.")),
        tags$li("The sum of all three equations is 0; meaning that at any point in time, the sum of S, I and R - the population size - is a constant."),
        tags$li("These models assume continuous time transition rates are constant; there are other classes of stochastic models which do not make these assumptions. I'll probably write a blog post at some point in the future exploring some stochastic epidemiology models."),
        tags$li("The model obioviously does not take into account any external actions from the government such as enforcing social isolation/mandatory quarantine periods - which would violate the aforementioned constant transition rates.")
      ),
      easyClose = TRUE
    ))
  })
  
  ## -===================================================================================================- ##
  ## -============================================= OUTPUTS =============================================- ##
  ## -===================================================================================================- ##

  ## wiki summary
  output$wiki_summary <- renderUI({
      tags$blockquote(
        p(class = 'text-muted',
          style = "font-size:14px",
                   wiki_summary_val %>%  str_replace_all("\\[[0-9]*\\]","")
          ),
        tags$footer(
          tags$a(paste0("Wikipedia (as at ", format(Sys.Date(),'%b-%d-%Y'),")"),href=wiki_path)
                    )
        )
      })
  
  ## overview title
  output$overview_title <- renderText({
    paste0("COVID-19 global summary as at ", format(max_date,'%d-%b-%y'))
  })
  
  ## country specific title
  output$country_overview_title <- renderText({
    paste0("COVID-19 summary for ", input$country, " as at ", format(max_date,'%d-%b-%y'))
  })
  ## global summary value boxes - number of confirmed
  output$confirmed_value_box <- renderValueBox({
    value_box_data(global_summary_vals, "Confirmed")
  })
  
  ## global summary value boxes - number of deaths
  output$deaths_value_box <- renderValueBox({
    value_box_data(global_summary_vals, "Deaths")
  })
  
  ## global summary value boxes - number of recovered
  output$recovered_value_box <- renderValueBox({
    value_box_data(global_summary_vals, "Recovered")
  })
  
  ## global summary value boxes - number of recovered
  output$days_since_first_confirmed_value_box <- renderValueBox({
    value_box_first_confirmed_data(global_first_confirmed_vals, max_date)
  })

  ## country level summary value boxes - number of confirmed
  output$country_confirmed_value_box <- renderValueBox({
    value_box_data(country_summary_vals_reactive(), "Confirmed")
  })
  
  ## country levelvalue boxes - number of deaths
  output$country_deaths_value_box <- renderValueBox({
    value_box_data(country_summary_vals_reactive(), "Deaths")
  })
  
  ## country levelvalue boxes - number of recovered
  output$country_recovered_value_box <- renderValueBox({
    value_box_data(country_summary_vals_reactive(), "Recovered")
  })
  
  ## country level value boxes - number of recovered
  output$country_days_since_first_confirmed_value_box <- renderValueBox({
    value_box_first_confirmed_data(country_first_confirmed_vals_reactive(), max_date)
  })
  
  ## highcharter map
  output$map_hc <- renderHighchart({
    choropleth_reactive()
  })
  
  ## cummulative totals plot
  output$tsPlot <- renderHighchart({
    ds <- list(
      list(data = ts_filtered_reactive()$Deaths, name = 'Deaths', color = 'black'),
      list(data = ts_filtered_reactive()$Recovered, name = 'Recovered', color = '#009900'),
      list(data = ts_filtered_reactive()$Confirmed, name = 'Confirmed', color = '#76EE00')
    )
    
    highchart() %>% 
      hc_chart(type = "column") %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(categories = ts_all_data_flattened_clean$Date %>% unique()) %>% 
      hc_add_series_list(ds) %>% 
      hc_chart(zoomType = "x") %>% 
      hc_title(text = paste0("Cumulative Coronavirus cases for ", input$country)) %>% 
      hc_tooltip(formatter = JS("function() {
                              

                              return '<b>'+ this.series.name +'</b><br/>'+
                              this.x +': '+ this.y.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, \"$1,\") ; 
}")
      )
  })
  
  ## day on day difference plots
  output$tsDiffPlot <- renderHighchart({
    ds <- list(
      list(data = ts_filtered_reactive()$Deaths.Delta, name = 'Deaths', type = "column", yAxis = 0, mapping = hcaes(X1), color = 'black'),
      list(data = ts_filtered_reactive()$Recovered.Delta, name = 'Recovered', type = "column", yAxis = 0, mapping = hcaes(X1), color = '#009900'),
      list(data = ts_filtered_reactive()$Confirmed.Delta, name = 'Confirmed', type = "column", yAxis = 0, mapping = hcaes(X1), color = '#76EE00'),
      list(data = 100*ts_filtered_reactive()$Deaths.Delta.Percentage.EMA, name = 'Day-on-day % change in deaths (moving average)', type = "line", dashStyle = "shortdash", yAxis = 1, mapping = hcaes(X1), color = 'black'),
      list(data = 100*ts_filtered_reactive()$Recovered.Delta.Percentage.EMA, name = 'Day-on-day %Recovered (5 day EMA)', type = "line", dashStyle = "shortdash", yAxis = 1, mapping = hcaes(X1), color = '#009900'),
      list(data = 100*ts_filtered_reactive()$Confirmed.Delta.Percentage.EMA, name = 'Day-on-day % change in confirmed (moving average)', type = "line", dashStyle = "shortdash", yAxis = 1, mapping = hcaes(X1), color = '#7800ee')
    )
    
    highchart() %>% 
      hc_add_series_list(ds) %>% 
      hc_yAxis_multiples(list(title = list(text = "# Entities"),min = 0,opposite=FALSE,gridLineColor = 'transparent'),
                         list(title = list(text = ""),opposite=TRUE,min=0, labels = list(format = "{value}%"))) %>%
      hc_plotOptions(column = list(stacking = "normal")) %>%
      hc_xAxis(categories = ts_filtered_reactive()$Date %>% unique()) %>% 
      hc_chart(zoomType = "x") %>% 
      hc_title(text = paste0("Daily change in Coronavirus cases for ", input$country)) %>% 
      hc_plotOptions(line = list(marker = list(
        enabled = FALSE))) %>% 
      hc_tooltip(formatter = JS("function() {
                              
                              if(this.series.yAxis.options.index == 0){
                              return '<b>'+ this.series.name +'</b><br/>'+
                              this.x +': '+ this.y.toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, \"$1,\") ; 
                              // to disable the tooltip at a point return false 
                              }else {
                              return '<b>'+ this.series.name +'</b><br/>'+
                              this.x +': '+ this.y.toFixed(2) + '%';
                              }   
                                }")
                 )
  })
  
  ## sir model fit plot
  output$SIR_model <- renderHighchart({
    if(input$check_fit){
     start_date <- modelling_data_reactive()$sir_start_date
     end_date <- modelling_data_reactive()$sir_cut_off_date
     
    } else{
      start_date <- SIR_model_reactive()$Date %>% min
      end_date <- SIR_model_reactive()$Date %>% max
    }
    
    data <- SIR_model_reactive() %>% 
      filter(Date >= start_date & Date <= end_date) %>% 
      unique()
    
    ds <- list(
      list(data = data$I, name = 'Infected (Predicted)', color = 'red'),
      list(data = data$Confirmed, name = 'Infected (Observed)', color = 'orange')
    )
    
    highchart() %>% 
      hc_chart(type = "line") %>%
      hc_xAxis(categories = format(data$Date, '%d-%b-%Y'), 
               tickInterval = 30,
               plotLines = list(
                 list(label = list(text = "Infected peak"),
                      color = "black",
                      width = 1,
                      dashStyle = 'shortdash',
                      value = ifelse(input$check_fit,which(data$I == max(data$I)),which(data$I == max(data$I))-1)))) %>% 
      hc_add_series_list(ds) %>% 
      hc_chart(zoomType = "x") %>% 
      hc_title(text = paste0("SIR model predictions for COVID-19 infections in ",  "Australia", " with R0: ", round(input$beta/input$gamma,4))) %>%
      hc_tooltip(formatter = JS("function() {
                              return '<b>'+ this.series.name +'</b><br/>'+
                              this.x +': '+ this.y.toFixed(0).toString().replace(/(\\d)(?=(\\d{3})+(?!\\d))/g, \"$1,\") ; 
                                }"
                                )
                 )
})
  
  ## date text
  output$date_text_tab1 <- renderText(paste0("Data as of: ",as.Date(max_date)))
  output$date_text_tab2 <- renderText(paste0("Data as of: ",as.Date(max_date)))
  ## top 10 total table
  output$top10_total <- DT::renderDataTable(top_10_countries_total_reactive())
  
  ## top 10 change table
  output$top10_change <- DT::renderDataTable(top_10_countries_change_reactive())
  
  ## news output
  output$news_df <- DT::renderDataTable(news_reactive())
  
} # end server

