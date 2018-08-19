##############################
# This is an example of creating a dynamic shiny app
# which produces an interactive dygraphs plot.
# Just press the "Run App" button on upper right of this panel.
##############################

library(shiny)
library(dygraphs)
library(rutils)


# Define elements of the UI user interface
inter_face <- shiny::shinyUI(fluidPage(
  
  titlePanel("VTI prices"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("lamb_da", label="lambda:",
                  min=0.01, max=0.5, value=0.2, step=0.05),
      numericInput("wid_th", label="wid_th:", min=10, max=201, value=51)
    ),
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
))  # end shinyUI interface


# Define the server code
ser_ver <- shiny::shinyServer(function(input, output) {

  # source the model function
  source("C:/Develop/R/lecture_slides/scripts/ewma_model.R")
  
  # Calculate the data for plotting
  da_ta <- reactive({
    # get model parameters from input
    lamb_da <- input$lamb_da
    wid_th <- input$wid_th
    # calculate close prices
    cl_ose <- quantmod::Cl(rutils::env_etf$VTI["2008/2009"])
    # calculate EWMA prices
    weight_s <- exp(-lamb_da*(1:wid_th))
    weight_s <- weight_s/sum(weight_s)
    ew_ma <- filter(cl_ose, filter=weight_s, sides=1)
    ew_ma[1:(wid_th-1)] <- ew_ma[wid_th]
    ew_ma <- xts(cbind(cl_ose, ew_ma), order.by=index(cl_ose))
    colnames(ew_ma) <- c("VTI", "VTI_EWMA")
    ew_ma  # return data for plotting
  })  # end reactive data
  
  # Define the output plot
  output$dygraph <- renderDygraph({
    dygraph(da_ta(), main="VTI prices") %>%
      dySeries("VTI", label="VTI", strokeWidth=1.5, color=c("red", "blue"))
  })  # end output plot
  
})

# Return a Shiny app object
shiny::shinyApp(ui=inter_face, server=ser_ver)
