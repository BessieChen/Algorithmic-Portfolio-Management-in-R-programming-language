##############################
# This is a shiny app for simulating rolling portfolio 
# optimization strategies, which produces an interactive 
# dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

## Below is the setup code that runs once when the shiny app is started

# load packages
library(shiny)
library(dygraphs)
library(rutils)

# Model and data setup
# source the model function
source("C:/Develop/R/lecture_slides/scripts/roll_portf.R")
max_eigen <- 2
sym_bols <- colnames(rutils::env_etf$re_turns)
sym_bols <- sym_bols[!(sym_bols=="VXX")]
n_weights <- NROW(sym_bols)
re_turns <- rutils::env_etf$re_turns[, sym_bols]
re_turns <- zoo::na.locf(re_turns)
re_turns <- na.omit(re_turns)
risk_free <- 0.03/260
ex_cess <- re_turns - risk_free
# calculate equal weight portfolio
equal_portf <- cumsum(re_turns %*% rep(1/sqrt(NCOL(re_turns)), NCOL(re_turns)))

# Define end_points
end_points <- rutils::calc_endpoints(re_turns, inter_val="months")
end_points <- end_points[end_points>50]
len_gth <- NROW(end_points)

# End setup code


## Create elements of the user interface
inter_face <- shiny::fluidPage(
  titlePanel("Rolling Portfolio Optimization Strategy for 19 ETFs"),
  
  # create single row with two slider inputs
  fluidRow(
    # input look_back interval
    column(width=5, sliderInput("look_back", label="lookback interval (months):",
                                min=6, max=30, value=12, step=1)),
    # input the shrinkage intensity
    column(width=5, sliderInput("al_pha", label="shrinkage intensity alpha:",
                                min=0.01, max=0.99, value=0.1, step=0.05))
  ),  # end fluidRow
  
  # create output plot panel
  mainPanel(dygraphOutput("dygraph"), width=12)
)  # end fluidPage interface


## Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  # re-calculate the data and rerun the model
  da_ta <- reactive({
    # get model parameters from input argument
    look_back <- input$look_back
    al_pha <- input$al_pha
    # define start_points
    start_points <- c(rep_len(1, look_back-1), end_points[1:(len_gth-look_back+1)])
    # rerun the model
    strat_rets <- cbind(
      roll_portf_r(ex_cess, re_turns, start_points, end_points, al_pha, max_eigen), 
      equal_portf)  # end cbind
    colnames(strat_rets) <- c("strategy", "equal weight")
    strat_rets
  })  # end reactive code
  
  # return the dygraph plot to output argument
  output$dygraph <- renderDygraph({
    dygraph(da_ta(), main="Rolling Portfolio Optimization Strategy") %>%
      dySeries("strategy", label="strategy", strokeWidth=1, color=c("blue", "red"))
  })  # end output plot
  
})  # end server code

## Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
