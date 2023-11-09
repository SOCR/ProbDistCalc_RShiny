# SOCR Probability Distribution Calculator
# Version 0.9
# Updated March 20th 2022 by Shihang Li and Yongxiang Zhao at the University of Michigan -SOCR
# Orginally created by Jared(Tianyi) Chai

# This is a SOCR Interactive Graphical Probability Distribution Calculator
# You can run the application by clicking
# the 'Run App' button above.

# ----------------------- Ui.R ----------------------- #
# For frontend user interface
source("server.R")
library(xml2)
library(shiny)
library(plotly)
library(stringr)
library(HypergeoMat)
library("Rlab")
library("shinyWidgets")

# Function to generate conditional panels
generateParameterlPanel <- function(distributionInfo) {
  conditionalPanel(
    condition = paste("input.Distribution == '", nameToFullName(distributionInfo$name), "'", sep = ""),
    lapply(seq_along(distributionInfo$inputNames), function(i) {
      textInput(
        distributionInfo$inputNames[i],
        paste("Please input ", distributionInfo$labels[i], " for ", distributionInfo$name, " : ", sep = ""),
        distributionInfo$defaultValues[i]
      )
    })
  )
}

generateCalculatorSidePanel <- function() {
  panel(
    selectInput("Distribution", "Please select distribution type",
      choices = distributions,
      selected = distributions[57]
    ),
    selectInput("FunctionType", "Please select distribution function type",
      choices = c("", "PDF/PMF", "CDF/CMF")
    ),
    lapply(distributionInfoList, generateParameterlPanel)
  )
}


generateModelerSidePanel <- function() {
  panel(
    selectInput("outcome",
      label = h3("Outcome (y)"),
      choices = unique(namedListOfFeatures()), selected = namedListOfFeatures()[6]
    ),
    selectInput("indepvar",
      label = h3("Explanatory variable (x)"),
      choices = unique(namedListOfFeatures()), selected = namedListOfFeatures()[4]
    ),
    actionButton("fitParams", "Fit Parameters from Data"),
  )
}

generateHelpPanel <- function() {
  panel(
    actionButton("vh.readme", "ReadMe/Help"),
    actionButton("toggleButton", "Show/Hide Distribution Metadata"),
    conditionalPanel(
      condition = "input.toggleButton % 2 == 1",
      uiOutput("MetaData"),
      tags$style(type = "text/css", "#MetaData {white-space: pre-wrap;}")
    )
  )
}




generateSideBarPanel <- function() {
  sidebarPanel(
    tabsetPanel(
      id = "CalcModelerTabsetPanel",
      type = "tabs",
      tabPanel("Calculator", generateCalculatorSidePanel()),
      tabPanel("Modeler", generateModelerSidePanel())
    ),
    generateHelpPanel()
  )
}

generateMainPlotPanel <- function() {
  # ----------------------- Input: Switch between Slider and Manual Inputs for Ranges and SD function ----------------------- #
  panel(
    fluidRow(
      column(2, switchInput(inputId = "numericalValues", value = FALSE, onLabel = "Manual", offLabel = "Slider")),
      column(5, div(
        textInput("SDNum", paste("Standard deviation from mean (0 to adjust freely)", sep = ""), 0)
      )),
      column(5, panel(div(htmlOutput("currentParameters"))))
    ),
    # ----------------------- Input: Slider Input for x-Range ----------------------- #
    conditionalPanel(
      condition = "input.numericalValues == 0",
      div(align = "right", sliderInput("plotrange", "X Range:",
        min = -1000, max = 1000,
        value = c(-10, 10),
        step = 0.01,
        width = "96%"
      ))
    ),
    # ----------------------- Input: Numerical Inputs for x-Range ----------------------- #
    conditionalPanel(
      condition = "input.numericalValues == 1",
      fluidRow(
        column(3, numericInput(inputId = "plotrangeNumMin", label = "X-range Min:", value = -10)),
        column(3, numericInput(inputId = "plotrangeNumMax", label = "X-range Max:", value = 10))
      )
    ),
    # ----------------------- Output: Main Plot Output ----------------------- #
    div(plotlyOutput("myPlot", height = "400"), aligh = "left"),
    # ----------------------- Output: Implementing Text ----------------------- #
    textOutput("Implementing"),
    # ----------------------- Input: Slider Input for Lower and Upper Bounds for Probability Calculation ----------------------- #
    conditionalPanel(
      condition = "input.numericalValues == 0",
      div(align = "right", sliderInput("probrange", "Probability Range:",
        min = -10, max = 10,
        value = c(-1, 1),
        step = 0.01,
        width = "96%"
      ))
    ),
    # ----------------------- Input: Numerical Input for Lower and Upper Bounds for Probability Calculation ----------------------- #
    conditionalPanel(
      condition = "input.numericalValues == 1",
      fluidRow(
        column(3, numericInput(inputId = "probrangeNumMin", label = "Prob. Lower Bound:", value = -1)),
        column(3, numericInput(inputId = "probrangeNumMax", label = "Prob. Upper Bound:", value = 1))
      )
    ),
    # ----------------------- Ouput: Calculated Probability ----------------------- #
    textOutput("probability")
  )
}

generateDatasetPanel <- function() {
  panel(
    h3(textOutput("caption")),
    tabsetPanel(
      type = "tabs",
      tabPanel("Data", DT::dataTableOutput("tbl")), # Data as datatable
      tabPanel(
        "Scatterplot", plotlyOutput("scatterplot"),
        br(), hr(),
        withMathJax(
          paste0("Least Squares Linear Model Estimates"),
          br(),
          paste0(
            "Slope: \\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i=1} x_i y_i \\big) - n \\bar{x}
                                    \\bar{y}}{\\sum^n_{i=1} (x_i- \\bar{x})^2} \\) =",
            "\\(\\dfrac{ n \\big(\\sum^n_{i=1} x_i y_i \\big) -
                                    \\big(\\sum^n_{i=1} x_i \\big) \\big(\\sum^n_{i=1} y_i \\big) }
                                    {n \\sum^n_{i=1} x_i^2 - \\big(\\sum^n_{i=1} x_i \\big)^2} \\)"
          ),
          br(),
          paste0("Intercept: \\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} \\) "),
          br(),
          paste0("Prediction: \\( \\hat{y} = \\hat{\\beta}_0 + \\hat{\\beta}_1 x \\) ")
        )
      ), # Scatter Plot,
      tabPanel("Model Summary", verbatimTextOutput("summary")) # Regression output
    )
  )
}

shinyUI(
  fluidPage(
    tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             top: calc(40%);
             left: calc(40%);
             font-size: 30px;
             }
             .highlight-input {
             background-color: yellow;
             }
             ")
      ),
      tags$script(
        HTML(
          "Shiny.addCustomMessageHandler('highlightTextInput', function(inputId) {
            var duration = 500; // 0.5 seconds
            var inputElement = $('#' + inputId);
            var originalColor = inputElement.css('background-color');
            inputElement.addClass('highlight-input');
            setTimeout(function() {
              inputElement.removeClass('highlight-input');
            }, duration);
          });"
        )
      )
    ),
    withMathJax(),
    # ----------------------- Output: Title ----------------------- #
    titlePanel("SOCR Probability Distribution Calculator/Modeler"),
    # ----------------------- Output: Sidebar Panel ----------------------- #
    generateSideBarPanel(),
    # ----------------------- Output: Main Panel ----------------------- #
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot", generateMainPlotPanel()),
        tabPanel("Dataset", generateDatasetPanel())
      ),
      # ----------------------- Output: SOCR Footer ----------------------- #
      tags$footer(
        div(shinyUI(bootstrapPage(div(
          # include The SOCR footer HTML
          includeHTML("SOCR_footer_tracker.html")
        )))),
        div(
          "Version: V.0.8",
          align = "center"
        ),
        # HTML("<img class='statcounter' src='https://c.statcounter.com/5714596/0/038e9ac4/0/' alt='Web Analytics' border='0' align='center'>")
      )
    )
  )
)
