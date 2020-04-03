## function for value box information on confirmed/deaths 
value_box_data <- function(df, type){
  type_str = type
  type <- sym(type)
  type.delta <- sym(paste0(type,".Delta"))
  type.delta.percentage <- sym(paste0(type,".Delta.Percentage"))
  
  type_val <- df %>% pull(!!type)
  type_val_delta <- df %>% pull(!!type.delta)
  type_val_delta_percentage  <- df %>% pull(!!type.delta.percentage)
  
  if(!is.na(type_val_delta_percentage)){
    subtitle <- HTML(paste0("Number of ",tolower(type_str), br(), 
                            formatC(type_val_delta,digits=0, format = "f", big.mark = ","),
                                    " (", paste0(round(100*type_val_delta_percentage,2),"%"),")"
                           ," increase day on day.")) 
    
    color <- if(type == "Recovered"){
      if (type_val_delta_percentage >= 0.20 ) "green" else if (type_val_delta_percentage >= 0.10) "yellow" else "red"
    } else {
      if (type_val_delta_percentage >= 0.15 ) "red" else if(type_val_delta_percentage >= 0.05) "yellow" else "green"
    }
    
  } else {
    subtitle <- HTML(paste0("Number of ",tolower(type_str), br(),"&nbsp"))
    color <- "yellow"
  }

  valueBox(
    value = formatC(type_val, digits = 0, format = "f", big.mark = ","),
    subtitle = subtitle,
    icon = icon("area-chart"),
    color = color
  )
}

## function for value box information for 'days since first confirmed'
value_box_first_confirmed_data <- function(df, max_date){
  
  first_confirmed_date <- df %>% pull(Date) %>% as.Date()
  days_since_first_confirmed <- difftime(max_date, first_confirmed_date , units = c("days"))
  
  valueBox(
    value = formatC(days_since_first_confirmed, digits = 0, format = "f", big.mark = ","),
    subtitle = HTML(paste0("days since first confirmed case",br(), "&nbsp")),
    icon = icon("clock"),
    color = "red"
  )
}

## EMA function with in built error handling for use in dplyr pipeline
EMA_dplyr <- function(ts,n,...){
  if(length(ts) <= n | sum(!is.na(ts)) <= n ) {
    return(rep(NA,length(ts)))
  } else{
    
    tryCatch(EMA(ts,n,...), 
             error = function(e) { 
               rep(NA,length(ts)) })
    
  }
}


## -============================ MODELLING FUNCTIONS ============================- ##
## SIR function for Susceptible-Infected-Recovered ode model
SIR <- function(time, state, parameters, N=N) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * I * S/N
    dI <- beta * I * S/N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}
  
logistic <- function(x){
  0.5+0.5*tanh(x/2)
}

## RSS of SIR model
# RSS <- function(parameters, init, Day) {
#   names(parameters) <- c("beta", "gamma")
#   out <- ode(y = init, times = Day, func = SIR, N=N, parms = parameters)
#   fit <- out[, 3]
#   sum((Infected - fit)^2)
# }

  