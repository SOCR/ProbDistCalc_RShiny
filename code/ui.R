#SOCR Probability Distribution Calculator
#Version 0.4
#Updated July 19th 2020 by Jared (Tianyi) Chai at the University of Michigan -SOCR

# This is a SOCR Interactive Graphical Probability Distribution Calculator
# You can run the application by clicking
# the 'Run App' button above.

# ----------------------- Ui.R ----------------------- #
# For frontend user interface
library(xml2)
library(shiny)
library(plotly)
library(stringr)
library("Rlab")
library("shinyWidgets")
shinyUI(
  fluidPage(
    withMathJax(),
    # ----------------------- Output: Title ----------------------- #
    titlePanel("SOCR Probability Distribution Calculator"),
    sidebarPanel(
      # ----------------------- Input: Specifying Distrtibution Type ----------------------- #
      selectInput("Distribution","Please select distribution type",
                  choices=distributions),
      # ----------------------- Input: Specifying Function Type ----------------------- #
      selectInput("FunctionType","Please select distribution functin type",
                  choices=c("","PDF/PMF","CDF/CMF")),
      # ----------------------- Input: Parameter Inputs ----------------------- #
        # ----------------------- Input: Bernoulli Distribution ----------------------- #
      conditionalPanel(condition = paste("input.Distribution == '",distributions[4],"'",sep = ""),
                       textInput("BernProb",paste("Please input Probability for ",distributions[4]," : ",sep = ""),1)),
        # ----------------------- Input: Expopnential Distribution ----------------------- #
      conditionalPanel(condition = paste("input.Distribution == '",distributions[23],"'",sep = ""),
                       textInput("ExpLambda",paste("Please input lambda for ",distributions[23]," : ",sep = ""),1)),
        # ----------------------- Input: LogNormal Distribution ----------------------- #
      conditionalPanel(condition = paste("input.Distribution == '",distributions[46],"'",sep = ""),
                       textInput("LogNormMean",paste("Please input mean for ",distributions[46]," : ",sep = ""),0),
                       textInput("LogNormSD",paste("Please input standard deviation for ",distributions[46]," : ",sep = ""),1)),
        # ----------------------- Input: Normal Distribution ----------------------- #
      conditionalPanel(condition = paste("input.Distribution == '",distributions[57],"'",sep = ""),
                       textInput("NormMean",paste("Please input mean for ",distributions[57]," : ",sep = ""),0),
                       textInput("NormSD",paste("Please input standard deviation for ",distributions[57]," : ",sep = ""),1)),
        # ----------------------- Input: Poisson Distribution ----------------------- #
      conditionalPanel(condition = paste("input.Distribution == '",distributions[61],"'",sep = ""),
                       textInput("PoiLambda",paste("Please input lambda for ",distributions[61]," : ",sep = ""),1)),
      # ----------------------- Output: Metadata Output ----------------------- #
      uiOutput("MetaData"),tags$style(type="text/css", "#MetaData {white-space: pre-wrap;}")
      
    ),
    mainPanel(
      # ----------------------- Input: Switch between Slider and Manual Inputs for Ranges ----------------------- #
      switchInput(inputId = "numericalValues", value = FALSE, onLabel = "Manual", offLabel = "Slider"),
      # ----------------------- Input: Slider Input for x-Range ----------------------- #
      conditionalPanel(condition = "input.numericalValues == 0",
                       sliderInput("plotrange", "X Range:",
                                   min = -1000, max = 1000,
                                   value = c(0,10),
                                   step = 0.01,
                                   width='100%')
      ),
      # ----------------------- Input: Numerical Inputs for x-Range ----------------------- #
      conditionalPanel(condition = "input.numericalValues == 1",
                       fluidRow(column(3,numericInput(inputId="plotrangeNumMin", label="X-range Min:", value = 0)),
                                column(3,numericInput(inputId="plotrangeNumMax", label="X-range Max:", value = 10)))
      ),
      # ----------------------- Output: Main Plot Output ----------------------- #
      plotlyOutput("myPlot"),
      # ----------------------- Input: Slider Input for Lower and Upper Bounds for Probability Calculation ----------------------- #
      conditionalPanel(condition = "input.numericalValues == 0",
                              sliderInput("probrange", "Probability Range:",
                                            min = -10, max = 10,
                                            value = c(0,1),
                                            step = 0.01,
                                            width='100%')
      ),
      # ----------------------- Input: Numerical Input for Lower and Upper Bounds for Probability Calculation ----------------------- #
      conditionalPanel(condition = "input.numericalValues == 1",
                       fluidRow(column(3,numericInput(inputId="probrangeNumMin", label="Prob. Lower Bound:", value = 0)),
                                column(3,numericInput(inputId="probrangeNumMax", label="Prob. Upper Bound:", value = 0)))
      ),
      # ----------------------- Ouput: Calculated Probability ----------------------- #
      textOutput("probability"),
      # ----------------------- Output: SOCR Footer ----------------------- #
      tags$footer(
        div(shinyUI(bootstrapPage(div(
          # include The SOCR footer HTML
          includeHTML("SOCR_footer_tracker.html")
        )))),
        div(
          "Version: V.0.1",
          align = 'center'
        ),
      #HTML("<img class='statcounter' src='https://c.statcounter.com/5714596/0/038e9ac4/0/' alt='Web Analytics' border='0' align='center'>")
      )
    )
  )
)