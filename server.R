#SOCR Probability Distribution Calculator
#Version 0.9
#Updated March 20th 2022 by Shihang Li and Yongxiang Zhao at the University of Michigan -SOCR
#Orginally created by Jared(Tianyi) Chai

# This is a SOCR Interactive Graphical Probability Distribution Calculator
# You can run the application by clicking
# the 'Run App' button above.

# ----------------------- Server.R ----------------------- #
# For backend calculations
library(xml2)
library(shinyjs)
library(flexsurv)
library(vcdExtra)
library(evd)
library(DescTools)
library(shiny)
library(triangle)
library(plotly)
library(stringr)
library(VGAM)
library(BayesTools)
library(extraDistr)
library(statmod)
library(truncnorm)
library(tolerance)
library(chi)
library(Rlab)
library(shinyWidgets)
library(circular)
library(mnormt)

plotlyFunctions <- list.files("plotlyFunctions", full.names = TRUE)
for (file in plotlyFunctions){
  source(file)
}
source("renderMainPlot.R")
source("renderProbability.R")

shinyServer(
  function(input, output, session){
    # ----------------------- Update Distribution Type and Function Type according to URL handle ----------------------- #
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[['d']])) {
        updateSelectInput(session, "Distribution", selected = distributions[as.numeric(query[['d']])])
        updateSelectInput(session, "FunctionType", selected = 'PDF/PMF')
        if (!is.null(query[['t']])) {
          updateSelectInput(session, "FunctionType", selected = query[['t']])
        }
      }
      # ----------------------- Update Range of Probability Calculation according to Range of X ----------------------- #
      updateSliderInput(session, 
                        "probrange", 
                        value = 0,
                        min = input$plotrange[1], 
                        max = input$plotrange[2], 
                        step = 0.01)
      updateNumericInput(session,
                      "probrangeNumMin",
                      value = 0,
                      min = input$plotrangeNumMin,
                      max = input$plotrangeNumMax)
      updateNumericInput(session,
                      "probrangeNumMax",
                      value = 0,
                      min = input$plotrangeNumMin,
                      max = input$plotrangeNumMax)
      })
    # ----------------------- HelpMe ----------------------- #
    observeEvent(input$vh.readme, {
      showModal(modalDialog(
        title = "Help / ReadMe",
        HTML('<div>
             <font size="3"><font color="blue"><b>SOCR Interactive Probability Distribution Calculator [Version: V.0.8]</b></font></font> 
             The SOCR RShiny probability distribution calculators provide interactive vizualizations of probability densities, 
             mass functions, and cumulative distributions, e.g., bivariate normal distribution.
             <br /><br />
            <b> Acknowledgments </b>
             <br /><br />
            This work is supported in part by NIH grants P20 NR015331, UL1TR002240, P30 DK089503, UL1TR002240, 
            and NSF grants 1916425, 1734853, 1636840, 1416953, 0716055 and 1023115. Students, trainees, scholars, 
            and researchers from SOCR, BDDS, MIDAS, MICHR, and the broad R-statistical computing community have contributed ideas, 
            code, and support.
             <div align="center">
             <font size="3"><b>Developers</b><br /></font></div>
             <font size="2">Jared (Tianyi) Chai (<b>chtianyi@umich.edu</b>)
             <font size="2">Shihang Li (<b>shihangl@umich.edu</b>)
             <font size="2">Yongxiang Zhao (<b>zyxleo@umich.edu</b>),
             Ivo Dinov (<b>dinov@med.umich.edu</b>).</font>
             <br /><br />
             '),
        
        easyClose = TRUE
      ))
    })
    # ----------------------- Render Metadata Information from xml Database ----------------------- #
    output$MetaData <- renderPrint({
      distType <- input$Distribution
      distType = tolower(str_replace_all(distType, "[^[:alnum:]]", ""))
      counter = 0
      for(i in 1:xml_len){
        j = 1
        while(distributions_meta[[j,i*2-1]] == "name"){
          if(tolower(str_replace_all(distributions_meta[[1,i*2]], "[^[:alnum:]]", "")) == distType){
            counter = i
            break
          }
          else{
            j= j+1
          }
        }
      }
      outputstring = ""
      if (counter != 0){
        row = 1
        while(distributions_meta[[row,counter*2-1]] != "" && row < xml_wid){
          outputstring = paste(outputstring,"<b>",distributions_meta[[row,counter*2-1]],":</b> ",distributions_meta[[row,counter*2]],"\n", sep = "")
          row = row+1
        }
      }
      withMathJax(helpText(HTML(outputstring)))
    })
    # ----------------------- Render Main Plot ----------------------- #
    renderMainPlot(input, output, session)
    # ----------------------- Render Implementing Message ----------------------- #
    output$Implementing <- renderText({
      if(input$Distribution %in% distToImpl){
        paste("The ", input$Distribution, " is still being implemented.", sep="")
      }
    })
    # ----------------------- Calculate and Render Probability ----------------------- #
    renderProbability(input, output, session)
  }
)