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

plotlyFunctions <- list.files("plotlyFunctions", full.names = TRUE)
for (file in plotlyFunctions){
  source(file)
}

# update_slider<-function(input,output,session,mean,stand_dev, SDNum,old_SD,plotrange2,numericalValues){
#     if(plotrange2 != mean + SDNum*stand_dev && numericalValues == 0){
#       if(SDNum > 0){
#         updateSliderInput(
#           session,
#           "plotrange",
#           label = NULL,
#           value = c(mean - SDNum*stand_dev,mean + SDNum*stand_dev),
#           min = mean - SDNum*stand_dev,
#           max = mean + SDNum*stand_dev,
#           step = NULL,
#           timeFormat = NULL,
#           timezone = NULL
#         )
#         #plotrange[2] = as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD)
#         #plotrange[1] = as.numeric(input$NormMean) - as.numeric(input$SDNum)*as.numeric(input$NormSD)
#       } else{
#         updateSliderInput(
#           session,
#           "plotrange",
#           label = NULL,
#           value = NULL,
#           min = -1000,
#           max = 1000,
#           step = NULL,
#           timeFormat = NULL,
#           timezone = NULL
#         )
#       }
#     }
#     if(old_SD != SDNum && numericalValues == 0 && SDNum > 0){
#       updateSliderInput(
#         session,
#         "plotrange",
#         label = NULL,
#         value = c(mean - SDNum*stand_dev,mean + SDNum*stand_dev),
#         min = mean - SDNum*stand_dev,
#         max = mean + SDNum*stand_dev,
#         step = NULL,
#         timeFormat = NULL,
#         timezone = NULL
#       )
#       #plotrange[2] = as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD)
#       #plotrange[1] = as.numeric(input$NormMean) - as.numeric(input$SDNum)*as.numeric(input$NormSD)
#     } else if(numericalValues == 0 && SDNum <= 0){
#       updateSliderInput(
#         session,
#         "plotrange",
#         label = NULL,
#         value = NULL,
#         min = -1000,
#         max = 1000,
#         step = NULL,
#         timeFormat = NULL,
#         timezone = NULL
#       )
#     }
# }
shinyServer(
  function(input, output,session){
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
    # ----------------------- Render Metadata Information from XML Database ----------------------- #
    output$MetaData <- renderPrint({
      distType <- input$Distribution
      distType = tolower(str_replace_all(distType, "[^[:alnum:]]", ""))
      counter = 0
      for(i in 1:XML_len){
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
        while(distributions_meta[[row,counter*2-1]] != "" && row < XML_wid){
          outputstring = paste(outputstring,"<b>",distributions_meta[[row,counter*2-1]],":</b> ",distributions_meta[[row,counter*2]],"\n", sep = "")
          row = row+1
        }
      }
      withMathJax(helpText(HTML(outputstring)))
    })
    # ----------------------- Render Main Plot ----------------------- #
    output$myPlot <- renderPlotly({
      distType <- input$Distribution
      plotrange = c(0,0)
      probrange = c(0,0)
      old_SD = 0
      if(input$Distribution %in% SDS){
        
      } else{
        updateSliderInput(
          session,
          "plotrange",
          label = NULL,
          value = NULL,
          min = -1000,
          max = 1000,
          step = NULL,
          timeFormat = NULL,
          timezone = NULL
        )
      }
      if(input$numericalValues == FALSE){
        plotrange[1] = input$plotrange[1]
        plotrange[2] = input$plotrange[2]
        probrange[1] = input$probrange[1]
        probrange[2] = input$probrange[2]
      }
      else{
        plotrange[1] = input$plotrangeNumMin
        plotrange[2] = input$plotrangeNumMax
        probrange[1] = input$probrangeNumMin
        probrange[2] = input$probrangeNumMax
      }
      # ----------------------- Continuous: ArcSine Distribution ----------------------- #
      if (distType == distributions[2]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f2 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f2 <- darcsine(xseq,as.numeric(input$ArcSineA),as.numeric(input$ArcSineB))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f2 <- parcsine(xseq,as.numeric(input$ArcSineA),as.numeric(input$ArcSineB))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f2,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f2
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = parcsine(as.numeric(probrange[2]),as.numeric(input$ArcSineA),as.numeric(input$ArcSineB))-parcsine(as.numeric(probrange[1]),as.numeric(input$ArcSineA),as.numeric(input$ArcSineB))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[2],' - ',graphtype,sep = ""),
                              autosize = TRUE,
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f2),max(f2))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Benford Distribution ----------------------- #
      else if(distType == distributions[3]){
        xseq<-seq(0,9,1)
        f3 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f3 <- dBenf(xseq, as.numeric(input$Benfn))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f3 <- pBenf(xseq, as.numeric(input$Benfn))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f3,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f3,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[3],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f3),max(f3)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Bernoulli Distribution ----------------------- #
      else if(distType == distributions[4]){
        plotlyBernoulliDistribution(plotrange, input, distType, probrange)
      }
      # ----------------------- Continuous: Beta Distribution ----------------------- #
      else if(distType == distributions[5]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f5 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f5 <- dbeta(xseq,as.numeric(input$BetaAlpha),as.numeric(input$BetaBeta))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f5 <- pbeta(xseq,as.numeric(input$BetaAlpha),as.numeric(input$BetaBeta))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f5,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f5
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pbeta(as.numeric(probrange[2]),as.numeric(input$BetaAlpha),as.numeric(input$BetaBeta))-pbeta(as.numeric(probrange[1]),as.numeric(input$BetaAlpha),as.numeric(input$BetaBeta))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[5],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f5),max(f5))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Beta (Generalized) Distribution ----------------------- #
      else if(distType == distributions[6]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f6 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f6 <- dgenbeta(xseq,as.numeric(input$BetaGenA),as.numeric(input$BetaGenB),as.numeric(input$BetaGenC),as.numeric(input$BetaGenP))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f6 <- pgenbeta(xseq,as.numeric(input$BetaGenA),as.numeric(input$BetaGenB),as.numeric(input$BetaGenC),as.numeric(input$BetaGenP))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f6,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f6
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pgenbeta(as.numeric(probrange[2]),as.numeric(input$BetaGenA),as.numeric(input$BetaGenB),as.numeric(input$BetaGenC),as.numeric(input$BetaGenP)) - pgenbeta(as.numeric(probrange[1]),as.numeric(input$BetaGenA),as.numeric(input$BetaGenB),as.numeric(input$BetaGenC),as.numeric(input$BetaGenP))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[6],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f6),max(f6))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Beta-Binomial Distribution ----------------------- #
      else if(distType == distributions[7]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f7 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f7 <- dbbinom(xseq, as.numeric(input$BetaBinomN),as.numeric(input$BetaBinomU), as.numeric(input$BetaBinomV))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f7 <- pbbinom(xseq, as.numeric(input$BetaBinomN),as.numeric(input$BetaBinomU), as.numeric(input$BetaBinomV))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f7,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f7,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[7],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f7),max(f7)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Beta Distribution ----------------------- #
      else if(distType == distributions[8]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f8 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f8 <- dbinom(xseq, as.numeric(input$BinomN), as.numeric(input$BinomP))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f8 <- pbinom(xseq,as.numeric(input$BinomN), as.numeric(input$BinomP))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f8,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f8,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[8],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f8),max(f8)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Cauchy Distribution ----------------------- #
      else if(distType == distributions[11]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f11 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f11 <- dcauchy(xseq,as.numeric(input$CauchyX0),as.numeric(input$CauchyGamma))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f11 <- pcauchy(xseq,as.numeric(input$CauchyX0),as.numeric(input$CauchyGamma))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f11,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f11
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pcauchy(as.numeric(probrange[2]),as.numeric(input$CauchyX0),as.numeric(input$CauchyGamma))-pcauchy(as.numeric(probrange[1]),as.numeric(input$CauchyX0),as.numeric(input$CauchyGamma))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[11],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f11),max(f11))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Chi Distribution ----------------------- #
      else if(distType == distributions[12]){
        k = as.numeric(input$ChiK)
        mean = sqrt(2)*gamma((k+1)/2)/gamma(k/2)
        standard_dev = sqrt(k-mean**2)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f12 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f12 <- dchi(xseq,as.numeric(input$ChiK))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f12 <- pchi(xseq,as.numeric(input$ChiK))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f12,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f12
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pchi(as.numeric(probrange[2]),as.numeric(input$ChiK))-pchi(as.numeric(probrange[1]),as.numeric(input$ChiK))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[12],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f12),max(f12))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Chi Square Distribution ----------------------- #
      else if(distType == distributions[13]){
        k = as.numeric(input$Chi2n)
        mean = k
        standard_dev = sqrt(2*k)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f13 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f13 <- dchisq(xseq, as.numeric(input$Chi2n))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f13 <- pchisq(xseq, as.numeric(input$Chi2n))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f13,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f13
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pchisq(as.numeric(probrange[2]),as.numeric(input$Chi2n))-pchisq(as.numeric(probrange[1]),as.numeric(input$Chi2n))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[13],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f13),max(f13))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Chi Square Non Central Distribution ----------------------- #
      else if(distType == distributions[14]){
        k = as.numeric(input$Chi2NCn)
        lemda = as.numeric(input$Chi2NCNCP)
        mean = k + lemda
        standard_dev = sqrt(2*(k+2*lemda))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f14 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f14 <- dchisq(xseq, as.numeric(input$Chi2NCn), as.numeric(input$Chi2NCNCP))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f14 <- pchisq(xseq, as.numeric(input$Chi2NCn), as.numeric(input$Chi2NCNCP))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f14,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f14
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pchisq(as.numeric(probrange[2]),as.numeric(input$Chi2NCn), as.numeric(input$Chi2NCNCP))-pchisq(as.numeric(probrange[1]),as.numeric(input$Chi2NCn), as.numeric(input$Chi2NCNCP))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[14],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f14),max(f14))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Continuous Uniform Distribution ----------------------- #
      else if(distType == distributions[16]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f16 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f16 <- dunif(xseq, as.numeric(input$UnifMin), as.numeric(input$UnifMax))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f16 <- punif(xseq, as.numeric(input$UnifMin), as.numeric(input$UnifMax))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f16,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f16
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = punif(as.numeric(probrange[2]),as.numeric(input$UnifMin), as.numeric(input$UnifMax))-punif(as.numeric(probrange[1]),as.numeric(input$UnifMin), as.numeric(input$UnifMax))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[16],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f16),max(f16))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Discrete ArcSine Distribution ----------------------- #
      else if(distType == distributions[19]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f19 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f19 <- darcsine(xseq,as.numeric(input$DisArcSineA),as.numeric(input$DisArcSineB))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f19 <- parcsine(xseq,as.numeric(input$DisArcSineA),as.numeric(input$DisArcSineB))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f19,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f19,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[19],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f19),max(f19)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Discrete Uniform Distribution ----------------------- #
      else if(distType == distributions[20]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f20 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f20 <- dunif(xseq, as.numeric(input$DisUnifMin),as.numeric(input$DisUnifMax))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f20 <- punif(xseq, as.numeric(input$DisUnifMin),as.numeric(input$DisUnifMax))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f20,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f20,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[20],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f20),max(f20)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Exponential Distribution ----------------------- #
      else if(distType == distributions[23]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f23 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f23 <- Rlab::dexp(xseq, as.numeric(input$ExpLambda))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f23 <- Rlab::pexp(xseq, as.numeric(input$ExpLambda))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f23,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f23
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = Rlab::pexp(as.numeric(probrange[2]),as.numeric(input$ExpLambda))-Rlab::pexp(as.numeric(probrange[1]),as.numeric(input$ExpLambda))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[23],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f23),max(f23))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: F Distribution ----------------------- #
      else if(distType == distributions[25]){
        d2 =  as.numeric(input$FdTwo)
        d1 = as.numeric(input$FdOne)
        if(d2 > 4){
        mean = d2/(d2-2)
        standard_dev = sqrt(2*d2**2*(d1+d2-2)/(d1*(d2-2)**2*(d2-4)))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] =  mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        } else{
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f25 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f25 <- df(xseq, as.numeric(input$FdOne),as.numeric(input$FdTwo))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f25 <- pf(xseq, as.numeric(input$FdOne),as.numeric(input$FdTwo))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f25,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f25
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pf(as.numeric(probrange[2]),as.numeric(input$FdOne),as.numeric(input$FdTwo))-pf(as.numeric(probrange[1]),as.numeric(input$FdOne),as.numeric(input$FdTwo))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[25],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f25),max(f25))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Gamma Distribution ----------------------- #
      else if(distType == distributions[27]){
        mean = as.numeric(input$GammaA)/as.numeric(input$GammaB)
        standard_dev = sqrt(mean/as.numeric(input$GammaB))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f27 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f27 <- Rlab::dgamma(xseq, shape = as.numeric(input$GammaA), rate = as.numeric(input$GammaB))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f27 <- Rlab::pgamma(xseq, shape = as.numeric(input$GammaA), rate = as.numeric(input$GammaB))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f27,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f27
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = Rlab::pgamma(as.numeric(probrange[2]), shape = as.numeric(input$GammaA), rate = as.numeric(input$GammaB)) - Rlab::pgamma(as.numeric(probrange[1]), shape = as.numeric(input$GammaA), rate = as.numeric(input$GammaB))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[27],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f27),max(f27))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: GEV Distribution ------------------------------#
      ## Miu = locationn
      ##Sig = scale
      ## Epsilon = shape
      ##dgev(x, loc=0, scale=1, shape=0, log = FALSE)
      ##pgev(q, loc=0, scale=1, shape=0, lower.tail = TRUE) 

      
      else if(distType == distributions[29]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f29 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f29<-dgev(xseq, as.numeric(input$GEVMiu), as.numeric(input$GEVSigma), as.numeric(input$GEVEpsilon) )
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f29<-pgev(xseq, as.numeric(input$GEVMiu), as.numeric(input$GEVSigma), as.numeric(input$GEVEpsilon) )
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f29,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f29,
                       ##text = c(rep( ppois(round(as.numeric(probrange[2]),0),as.numeric(input$PoiLambda))-ppois(round(as.numeric(probrange[1]),0)-1,as.numeric(input$PoiLambda)),xsize)),
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[29],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f29),max(f29)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      
      # ----------------------- Discrete: Geometric Distribution ----------------------- #
      else if(distType == distributions[30]){
        p = as.numeric(input$GeomProb)
        mean = 1/p
        standard_dev = sqrt((1-p)/p**2)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(-1,mean + as.numeric(input$SDNum)*standard_dev),
              min = -1,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = -1
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(-1,mean + as.numeric(input$SDNum)*standard_dev),
            min = -1,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = -1
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f30 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f30 <- dgeom(xseq, as.numeric(input$GeomProb))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f30 <- pgeom(xseq, as.numeric(input$GeomProb))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f30,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f30,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[30],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f30),max(f30)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Gompertz Distribution ----------------------- #
      else if(distType == distributions[32]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f32 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f32 <- dgompertz(xseq,as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f32 <- pgompertz(xseq,as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f32,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f32
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob =  pgompertz(as.numeric(plotrange[2]),as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))-pgompertz(as.numeric(plotrange[1]),as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[33],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f32),max(f32))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Gumbel Distribution ----------------------- #
      else if(distType == distributions[33]){
        Beta = as.numeric(input$Gumbel_Beta)
        U = as.numeric(input$Gumbel_U)
        mean = U+Beta*0.57721
        standard_dev = sqrt(pi**2/6*Beta**2)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(mean - as.numeric(input$SDNum)*standard_dev,mean + as.numeric(input$SDNum)*standard_dev),
              min = mean - as.numeric(input$SDNum)*standard_dev,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = mean - as.numeric(input$SDNum)*standard_dev
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(mean - as.numeric(input$SDNum)*standard_dev,mean + as.numeric(input$SDNum)*standard_dev),
            min = mean - as.numeric(input$SDNum)*standard_dev,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = mean - as.numeric(input$SDNum)*standard_dev
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f33 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f33 <- dgumbel(xseq,as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f33 <- pgumbel(xseq,as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f33,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f33
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pgumbel(as.numeric(probrange[2]),as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))-pgumbel(as.numeric(probrange[1]),as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[33],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f33),max(f33))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Half Normal Distribution ----------------------- #
      else if(distType == distributions[34]){
        sig = as.numeric(input$HNorm)
        mean = sig*sqrt(2/pi)
        standard_dev = sqrt(sig**2*(1-2/pi))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f34 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f34 <- dhnorm(xseq,as.numeric(input$HNorm))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f34 <- phnorm(xseq,as.numeric(input$HNorm))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f34,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f34
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = phnorm(as.numeric(probrange[2]),as.numeric(input$HNorm))-phnorm(as.numeric(probrange[1]),as.numeric(input$HNorm))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[34],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f34),max(f34))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Hyper Geometric Distribution ----------------------- #
      else if(distType == distributions[35]){
        M = as.numeric(input$HyperM)
        n = as.numeric(input$HyperK)
        N = as.numeric(input$HyperN)
        mean = M*n/N
        standard_dev = sqrt(n*M/N*(N-M)/N*(N-n)/(N-1))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(-1,mean + as.numeric(input$SDNum)*standard_dev),
              min = -1,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = -1
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(-1,mean + as.numeric(input$SDNum)*standard_dev),
            min = -1,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = -1
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f35 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f35 <- dhyper(xseq, as.numeric(input$HyperM),as.numeric(input$HyperN)-as.numeric(input$HyperM),as.numeric(input$HyperK))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f35 <- phyper(xseq, as.numeric(input$HyperM),as.numeric(input$HyperN)-as.numeric(input$HyperM),as.numeric(input$HyperK))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f35,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f35,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[35],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f35),max(f35)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Inverse Gamma Distribution ----------------------- #
      else if(distType == distributions[37]){
        a = as.numeric(input$InvGammaA)
        b = as.numeric(input$InvGammaB)
        if(a>2){
        mean = b/(a-1)
        standard_dev = sqrt(b**2/((a-1)**2*(a-2)))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f37 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f37<-dinvgamma(xseq, as.numeric(input$InvGammaA), 1/as.numeric(input$InvGammaB))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f37<-pinvgamma(xseq, as.numeric(input$InvGammaA), 1/as.numeric(input$InvGammaB))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f37,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f37
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pinvgamma(as.numeric(probrange[2]), as.numeric(input$InvGammaA), 1/as.numeric(input$InvGammaB))-pinvgamma(as.numeric(probrange[1]), as.numeric(input$InvGammaA), 1/as.numeric(input$InvGammaB))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[37],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f37),max(f37))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Inverse Gaussian(Wald) Distribution ----------------------- #
      else if(distType == distributions[38]){
        u = as.numeric(input$InvGausM)
        L = as.numeric(input$InvGausL)
        mean = u
        standard_dev = sqrt(u**3/L)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f38 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f38<-dinvgauss(xseq, as.numeric(input$InvGausM), as.numeric(input$InvGausL))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f38<-pinvgauss(xseq, as.numeric(input$InvGausM), as.numeric(input$InvGausL))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f38,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f38
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pinvgauss(as.numeric(probrange[2]), as.numeric(input$InvGausM), as.numeric(input$InvGausL))-pinvgauss(as.numeric(probrange[1]), as.numeric(input$InvGausM), as.numeric(input$InvGausL))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[38],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f38),max(f38))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Laplace Distribution ----------------------- #
      else if(distType == distributions[42]){
        mean = as.numeric(input$LapMu)
        standard_dev = sqrt(2*as.numeric(input$LapSig)**2)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(mean - as.numeric(input$SDNum)*standard_dev,mean + as.numeric(input$SDNum)*standard_dev),
              min = mean - as.numeric(input$SDNum)*standard_dev,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = mean - as.numeric(input$SDNum)*standard_dev
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(mean - as.numeric(input$SDNum)*standard_dev,mean + as.numeric(input$SDNum)*standard_dev),
            min = mean - as.numeric(input$SDNum)*standard_dev,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = mean - as.numeric(input$SDNum)*standard_dev
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f42 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f42<-dlaplace(xseq, as.numeric(input$LapMu), as.numeric(input$LapSig))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f42<-plaplace(xseq, as.numeric(input$LapMu), as.numeric(input$LapSig))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f42,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f42
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = plaplace(as.numeric(probrange[2]), as.numeric(input$LapMu), as.numeric(input$LapSig))-plaplace(as.numeric(probrange[1]), as.numeric(input$LapMu), as.numeric(input$LapSig))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[42],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f42),max(f42))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Logarithmic Series Distribution ----------------------- #
      else if(distType == distributions[43]){
        p = as.numeric(input$LogP)
        mean = 1/-(log(1-p,exp(1)))*p/(1-p)
        standard_dev = sqrt(mean/(1-p)*(1-p/-(log(1-p,exp(1)))))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 1,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 1
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 1,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 1
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(round(max(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f43 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f43<-dlogseries(xseq, as.numeric(input$LogP))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f43<-plogseries(xseq, as.numeric(input$LogP))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f43,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f43,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[43],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f43),max(f43)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Logistic Distribution ----------------------- #
      else if(distType == distributions[44]){
        a = as.numeric(input$LogiA)
        b = as.numeric(input$LogiB)
        mean = a
        standard_dev = sqrt(pi**2*b**2/3)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(mean - as.numeric(input$SDNum)*standard_dev,mean + as.numeric(input$SDNum)*standard_dev),
              min = mean - as.numeric(input$SDNum)*standard_dev,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = mean - as.numeric(input$SDNum)*standard_dev
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(mean - as.numeric(input$SDNum)*standard_dev,mean + as.numeric(input$SDNum)*standard_dev),
            min = mean - as.numeric(input$SDNum)*standard_dev,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = mean - as.numeric(input$SDNum)*standard_dev
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f44 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f44<-dlogis(xseq, as.numeric(input$LogiA), as.numeric(input$LogiB))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f44<-plogis(xseq, as.numeric(input$LogiA), as.numeric(input$LogiB))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f44,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f44
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = plogis(as.numeric(probrange[2]),as.numeric(input$LogiA),as.numeric(input$LogiB))-plogis(as.numeric(probrange[1]),as.numeric(input$LogiA),as.numeric(input$LogiB))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[44],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f44),max(f44))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      #-----------------------Continuous: Logistic-Exponential Distribution ----------------------- #
      else if(distType == distributions[45]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f45 <- vector()
        graphtype<-""
        Beta = as.numeric(input$LogEx_B)
        Alpha = as.numeric(input$LogEx_A)
        if(input$FunctionType == "PDF/PMF"){
          for(index in 1:length(xseq)){
            if( xseq[index] > 0){
              f45<-append(f45, (Alpha * Beta * ((exp(Alpha * xseq[index])  - 1) **(Beta-1))  * exp(Alpha * xseq[index]))
                          / ((1 + (exp(Alpha * xseq[index])  - 1) **Beta   )**2)    )
            } else{
              f45<-append(f45,0)
            }
          }
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          for(index in 1:length(xseq)){
            if(xseq[index] > 0){
              #(exp(Alpha * xseq[index] ) - 1) ** Beta)/(1 + ( exp(Alpha * (xseq[index] ) - 1) ** Beta))
              f45<-append(f45,  (exp(Alpha*xseq[index]) - 1)**Beta/(1+(exp(Alpha*xseq[index]) - 1)**Beta)   )
            } else{
              f45<-append(f45,0)
            }
          }
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f45,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f45
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = 0
          
          if(as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0){
            prob = 0
          } else if(as.numeric(probrange[1]) <= 0){
            prob = ( (exp(Alpha * (as.numeric(probrange[2])) ) - 1) ** Beta)/(1 + (exp(Alpha * (as.numeric(probrange[2]))) - 1 )** Beta)
          } else if(as.numeric(probrange[1]) > 0 && as.numeric(probrange[2]) > 0 ){
            prob =( (exp(Alpha * (as.numeric(probrange[2])) ) - 1) ** Beta)/(1 + (exp(Alpha * (as.numeric(probrange[2]))) - 1 )** Beta)
            - ( (exp(Alpha * (as.numeric(probrange[1])) ) - 1) ** Beta)/(1 + (exp(Alpha * (as.numeric(probrange[1]))) - 1 )** Beta)
          }
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[45],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f45),max(f45))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: LogNormal Distribution ----------------------- #
      else if(distType == distributions[46]){
        u = as.numeric(input$LogNormMean)
        sig = as.numeric(input$LogNormSD)
        mean = exp((u+(sig**2/2)))
        standard_dev = sqrt((exp(sig**2) - 1)*exp(2*u+sig**2))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f46 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f46<-dlnorm(xseq, as.numeric(input$LogNormMean), as.numeric(input$LogNormSD))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f46<-plnorm(xseq, as.numeric(input$LogNormMean), as.numeric(input$LogNormSD))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f46,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f46
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = plnorm(as.numeric(probrange[2]),as.numeric(input$LogNormMean),as.numeric(input$LogNormSD))-plnorm(as.numeric(probrange[1]),as.numeric(input$LogNormMean),as.numeric(input$LogNormSD))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[46],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f46),max(f46))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Lomax Distribution ----------------------- #
      else if(distType == distributions[47]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f47 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f47<-dlomax(xseq, as.numeric(input$LomaxLamda), as.numeric(input$LomaxKappa))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f47<-plomax(xseq, as.numeric(input$LomaxLamda), as.numeric(input$LomaxKappa))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f47,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f47
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = plomax(as.numeric(probrange[2]), as.numeric(input$LomaxLamda), as.numeric(input$LomaxKappa))-plomax(as.numeric(probrange[1]), as.numeric(input$LomaxLamda), as.numeric(input$LomaxKappa))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[47],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f47),max(f47))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Maxwell Distribution ----------------------- #
       else if(distType == distributions[49]){
         a = as.numeric(input$MaxwellA)
         mean = 2*a*sqrt(2/pi)
         standard_dev = sqrt(a**2*(3*pi-8)/pi)
         if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
           if(input$SDNum > 0){
             updateSliderInput(
               session,
               "plotrange",
               label = NULL,
               value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
               min = 0,
               max = mean + as.numeric(input$SDNum)*standard_dev,
               step = NULL,
               timeFormat = NULL,
               timezone = NULL
             )
             old_SD = input$SDNum
             plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
             plotrange[1] = 0
           } else{
             updateSliderInput(
               session,
               "plotrange",
               label = NULL,
               value = NULL,
               min = -1000,
               max = 1000,
               step = NULL,
               timeFormat = NULL,
               timezone = NULL
             )
             old_SD = input$SDNum
           }
         }
         if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
           updateSliderInput(
             session,
             "plotrange",
             label = NULL,
             value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
             min = 0,
             max = mean + as.numeric(input$SDNum)*standard_dev,
             step = NULL,
             timeFormat = NULL,
             timezone = NULL
           )
           old_SD = input$SDNum
           plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
           plotrange[1] = 0
         } else if(input$numericalValues == 0 && input$SDNum <= 0){
           updateSliderInput(
             session,
             "plotrange",
             label = NULL,
             value = NULL,
             min = -1000,
             max = 1000,
             step = NULL,
             timeFormat = NULL,
             timezone = NULL
           )
           old_SD = input$SDNum
         }
         xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
         f49 <- 0
         graphtype<-""
         if(input$FunctionType == "PDF/PMF"){
           f49<-dmaxwell(xseq, as.numeric(input$MaxwellA))
           graphtype<-"PDF"
         }
         else if(input$FunctionType == "CDF/CMF"){
           f49<-pmaxwell(xseq, as.numeric(input$MaxwellA))
           graphtype<-"CDF"
         }
         else{
           graphtype<-""
         }
         if(graphtype != ""){
           fig<-plot_ly(x = xseq,
                        y = f49,
                        name = distType,
                        type = 'scatter',
                        mode='lines',
                        hoverinfo = 'xy'
           )
           xsize = length(xseq)
           newy = f49
           for (index in 1:xsize){
             if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
               newy[index] = NA
             }
           }
           prob = pmaxwell(as.numeric(probrange[2]), as.numeric(input$MaxwellA))-pmaxwell(as.numeric(probrange[1]), as.numeric(input$MaxwellA))
           fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
           fig<-fig %>% plotly::layout(title = paste(distributions[49],' - ',graphtype,sep = ""),
                               hovermode  = 'x',
                               hoverlabel = list(
                                 namelength = 100
                               ),
                               yaxis = list(fixedrange = TRUE,
                                            zeroline = TRUE,
                                            range = c(min(f49),max(f49))
                               ),
                               xaxis=list(showticklabels=TRUE,
                                          zeroline = TRUE,
                                          showline=TRUE,
                                          showgrid=TRUE,
                                          linecolor='rgb(204, 204, 204)',
                                          linewidth=2,
                                          mirror=TRUE,
                                          fixedrange = TRUE,
                                        range = c(plotrange[1],plotrange[2])
                               ),
                               showlegend = FALSE
           )
           fig<-fig %>% config(editable=FALSE)
           fig
         }
       }
      # ----------------------- Continuous: Minimax Distribution ----------------------- #
      else if(distType == distributions[50]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f50 <- vector()
        graphtype<-""
        Beta = as.numeric(input$Mini_B)
        Gamma = as.numeric(input$Mini_V)
        if(input$FunctionType == "PDF/PMF"){
          for(index in 1:length(xseq)){
            if(xseq[index] < 1 && xseq[index] > 0){
              f50<-append(f50, Beta*Gamma*(xseq[index])**(Beta - 1) *(1-(xseq[index])**Beta)**(Gamma - 1))
            } else{
              f50<-append(f50,0)
            }
          }
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          for(index in 1:length(xseq)){
            if(xseq[index] < 1 && xseq[index] > 0){
              f50<-append(f50, 1- (1-(xseq[index])**Beta)**(Gamma))
            } else{
              f50<-append(f50,0)
            }
          }
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f50,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f50
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = 0
          if(as.numeric(probrange[2]) >= 1 && as.numeric(probrange[1]) <= 0){
            prob = 1
          } else if(as.numeric(probrange[2]) >= 1 && as.numeric(probrange[1]) >= 1){
            prob = 0
          } else if(as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0){
            prob = 0
          } 
          else if(as.numeric(probrange[2]) >= 1){
            prob = 1  - (1- (1-(as.numeric(probrange[1]))**Beta)**(Gamma))
          } else if(as.numeric(probrange[1]) <= 0){
            prob = 1- (1-(as.numeric(probrange[2]))**Beta)**(Gamma)
          } else{
            prob = 1- (1-(as.numeric(probrange[2]))**Beta)**(Gamma) - (1- (1-(as.numeric(probrange[1]))**Beta)**(Gamma))
          }
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[50],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f50),max(f50))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Muth Distribution ----------------------- #
      else if(distType == distributions[53]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f53 <- vector()
        graphtype<-""
        K = as.numeric(input$MuthKappa)
        if(input$FunctionType == "PDF/PMF"){
          for(index in 1:length(xseq)){
            if(xseq[index] >0){
              f53<-append(f53, (exp(K*xseq[index]) - K)*exp(-(exp(K*xseq[index]))/K + K*xseq[index] + 1/K))
            } else{
              f53<-append(f53,0)
            }
          }
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          for(index in 1:length(xseq)){
            if(xseq[index] > 0){
              f53<-append(f53, 1- exp(-(exp(K*xseq[index]))/K + K*xseq[index] + 1/K))
            } else{
              f53<-append(f53,0)
            }
          }
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f53,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f53
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = 0
          if(as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0){
            prob = 0
          } else if(as.numeric(probrange[2]) <= 0){
            prob = 0
          } else if(as.numeric(probrange[1]) <= 0){
            prob = 1- exp(-(exp(K*as.numeric(probrange[2])))/K + K*as.numeric(probrange[2]) + 1/K)
          } else{
            prob = (1- exp(-(exp(K*as.numeric(probrange[2])))/K + K*as.numeric(probrange[2]) + 1/K)) - (1- exp(-(exp(K*as.numeric(probrange[1])))/K + K*as.numeric(probrange[1]) + 1/K))
          }
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[53],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f53),max(f53))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Negative Binomial Distribution ----------------------- #
      else if(distType == distributions[54]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f54 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f54<-dnbinom(xseq, as.numeric(input$NegBiR),as.numeric(input$NegBiP))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f54<-pnbinom(xseq, as.numeric(input$NegBiR),as.numeric(input$NegBiP))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f54,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f54,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[54],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f54),max(f54)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Negative HyperGeometric Distribution ----------------------- #
      else if(distType == distributions[55]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f55 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f55<-dnhyper(xseq, as.numeric(input$NegHyperK)-as.numeric(input$NegHyperN),as.numeric(input$NegHyperN),as.numeric(input$NegHyperR))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f55<-pnhyper(xseq, as.numeric(input$NegHyperK)-as.numeric(input$NegHyperN),as.numeric(input$NegHyperN),as.numeric(input$NegHyperR))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f55,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f55,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[55],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f55),max(f55)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Normal Distribution ----------------------- #
      else if(distType == distributions[57]){
        if(plotrange[2] != as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD) && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(as.numeric(input$NormMean) - as.numeric(input$SDNum)*as.numeric(input$NormSD),as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD)),
              min = as.numeric(input$NormMean) - as.numeric(input$SDNum)*as.numeric(input$NormSD),
              max = as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD),
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD)
            plotrange[1] = as.numeric(input$NormMean) - as.numeric(input$SDNum)*as.numeric(input$NormSD)
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
         if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(as.numeric(input$NormMean) - as.numeric(input$SDNum)*as.numeric(input$NormSD),as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD)),
            min = as.numeric(input$NormMean) - as.numeric(input$SDNum)*as.numeric(input$NormSD),
            max = as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD),
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
           old_SD = input$SDNum
            plotrange[2] = as.numeric(input$NormMean) + as.numeric(input$SDNum)*as.numeric(input$NormSD)
            plotrange[1] = as.numeric(input$NormMean) - as.numeric(input$SDNum)*as.numeric(input$NormSD)
          } else if(input$numericalValues == 0 && input$SDNum <= 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f57 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f57<-dnorm(xseq, as.numeric(input$NormMean), as.numeric(input$NormSD))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f57<-pnorm(xseq, as.numeric(input$NormMean), as.numeric(input$NormSD))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f57,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f57
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pnorm(as.numeric(probrange[2]),as.numeric(input$NormMean),as.numeric(input$NormSD))-pnorm(as.numeric(probrange[1]),as.numeric(input$NormMean),as.numeric(input$NormSD))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[57],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f57),max(f57))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Normal Truncated Distribution ----------------------- #
      else if(distType == distributions[58]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f58 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f58<-dtruncnorm(xseq, as.numeric(input$TruncNormMin),as.numeric(input$TruncNormMax),as.numeric(input$TruncNormMean), as.numeric(input$TruncNormSD))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f58<-ptruncnorm(xseq, as.numeric(input$TruncNormMin),as.numeric(input$TruncNormMax),as.numeric(input$TruncNormMean), as.numeric(input$TruncNormSD))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f58,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f58
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = ptruncnorm(as.numeric(probrange[2]),as.numeric(input$TruncNormMin),as.numeric(input$TruncNormMax),as.numeric(input$TruncNormMean),as.numeric(input$TruncNormSD))-ptruncnorm(as.numeric(probrange[1]),as.numeric(input$TruncNormMin),as.numeric(input$TruncNormMax),as.numeric(input$TruncNormMean),as.numeric(input$TruncNormSD))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[58],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f58),max(f58))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Pareto Distribution ----------------------- #
      else if(distType == distributions[59]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f59 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f59<-dpareto(xseq, as.numeric(input$ParetoA), as.numeric(input$ParetoB))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f59<-ppareto(xseq, as.numeric(input$ParetoA), as.numeric(input$ParetoB))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f59,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f59
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = ppareto(as.numeric(probrange[2]), as.numeric(input$ParetoA), as.numeric(input$ParetoB))-ppareto(as.numeric(probrange[1]), as.numeric(input$ParetoA), as.numeric(input$ParetoB))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[59],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f59),max(f59))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Point Mass Distribution ----------------------- #
      else if(distType == distributions[60]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f60 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f60<-dpoint(xseq, as.numeric(input$PMD_Location))
          f60[as.numeric(input$PMD_Location) + abs(round(plotrange[1])) + 1]<-1
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f60<-ppoint(xseq, as.numeric(input$PMD_Location))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f60,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f60,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[60],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f60),max(f60)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Poisson Distribution ----------------------- #
      else if(distType == distributions[61]){
        mean = as.numeric(input$PoiLambda)
        standard_dev = sqrt(mean)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(-1,mean + as.numeric(input$SDNum)*standard_dev),
              min = -1,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = -1
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(-1,mean + as.numeric(input$SDNum)*standard_dev),
            min = -1,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = -1
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f61 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f61<-dpois(xseq, as.numeric(input$PoiLambda))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f61<-ppois(xseq, as.numeric(input$PoiLambda))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f61,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f61,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                                             ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[61],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f61),max(f61)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Rayleigh Distribution ----------------------- #
      else if(distType == distributions[64]){
        sig = as.numeric(input$RayleighSigma)
        mean = sig*sqrt(pi/2)
        standard_dev = sqrt((4-pi)/2*sig**2)
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f64 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f64 <- drayleigh(xseq, as.numeric(input$RayleighSigma))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f64 <- prayleigh(xseq, as.numeric(input$RayleighSigma))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f64,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f64
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = prayleigh(as.numeric(probrange[2]), as.numeric(input$RayleighSigma))-prayleigh(as.numeric(probrange[1]), as.numeric(input$RayleighSigma))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[64],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f64),max(f64))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------- Continuous: Rice Distribution ------------------#
      
      else if(distType == distributions[65]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f65 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f65<-drice(xseq, as.numeric(input$RiceSigma), as.numeric(input$RiceVee))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f65<-price(xseq, as.numeric(input$RiceSigma), as.numeric(input$RiceVee))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f65,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f65
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = price(as.numeric(probrange[2]),as.numeric(input$RiceSigma),as.numeric(input$RiceVee))-price(as.numeric(probrange[1]),as.numeric(input$RiceSigma),as.numeric(input$RiceVee))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[65],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f65),max(f65))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=FALSE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: T Distribution ----------------------- #
      else if(distType == distributions[66]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f66 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f66 <- dt(xseq, as.numeric(input$Tdof))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f66 <- pt(xseq, as.numeric(input$Tdof))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f66,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f66
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pt(as.numeric(probrange[2]),as.numeric(input$Tdof))-pt(as.numeric(probrange[1]),as.numeric(input$Tdof))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[66],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f66),max(f66))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: T Non Central Distribution ----------------------- #
      else if(distType == distributions[67]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f67 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f67 <- dt(xseq, as.numeric(input$TNCdof),as.numeric(input$TNCNCP))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f67 <- pt(xseq, as.numeric(input$TNCdof),as.numeric(input$TNCNCP))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f67,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f67
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = pt(as.numeric(probrange[2]),as.numeric(input$TNCdof),as.numeric(input$TNCNCP))-pt(as.numeric(probrange[1]),as.numeric(input$TNCdof),as.numeric(input$TNCNCP))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[67],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f67),max(f67))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Triangular Distribution ----------------------- #
      else if(distType == distributions[68]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f68 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f68 <- dtriangular(xseq, as.numeric(input$Triangular_A),as.numeric(input$Triangular_B), as.numeric(input$Triangular_C))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f68 <- ptriangular(xseq, as.numeric(input$Triangular_A),as.numeric(input$Triangular_B),as.numeric(input$Triangular_C))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f68,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          print(fig)
          xsize = length(xseq)
          newy = f68
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = ptriangular(as.numeric(probrange[2]),as.numeric(input$Triangular_A),as.numeric(input$Triangular_B))-ptriangular(as.numeric(probrange[1]),as.numeric(input$Triangular_A),as.numeric(input$Triangular_B))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[68],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f68),max(f68))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: U-quadratic distribution ----------------------- #
      else if(distType == distributions[70]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f70 <- vector()
        graphtype<-""
        W = as.numeric(input$UQ_W)
        C = as.numeric(input$UQ_C)
        if(input$FunctionType == "PDF/PMF"){
          for(index in 1:length(xseq)){
            if(C-W <= xseq[index] && xseq[index] <= W+C){
            f70<-append(f70, 3/(2*W)*((xseq[index]-C)/W)**2)
            }  else{
              f70<-append(f70,0)
            }
          }
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          for(index in 1:length(xseq)){
            if(C-W <= xseq[index] && xseq[index] <= W+C){
            f70<-append(f70, 0.5*(1+((xseq[index]-C)/W)**3))
            } else{
              f70<-append(f70,0)
            }
          }
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f70,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f70
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = 0
          if(as.numeric(probrange[2]) >= W+C && as.numeric(probrange[1] <= C-W)){
            prob = 1
          } else if(as.numeric(probrange[2]) <= C-W && as.numeric(probrange[1] <= C-W)){
            prob = 0
          }
          else if(as.numeric(probrange[2]) >= W+C){
            prob = 1 - 0.5*(1+((as.numeric(probrange[1]) - C)/W)**3)
          }else if(as.numeric(probrange[1] <= C-W)){
            prob = 0.5*(1+((as.numeric(probrange[2]) - C)/W)**3)
          } 
          else{
            prob = 0.5*(1+((as.numeric(probrange[2]) - C)/W)**3) - 0.5*(1+((as.numeric(probrange[1]) - C)/W)**3)
          }
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[70],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f70),max(f70))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Continuous: Weibull Distribution ----------------------- #
      else if(distType == distributions[74]){
        k = as.numeric(input$WeibullK)
        l = as.numeric(input$WeibullLambda)
        mean = l*gamma(1+1/k)
        standard_dev = sqrt(l**2*(gamma(1+2/k)-(gamma(1+1/k))**2))
        if(plotrange[2] != mean + as.numeric(input$SDNum)*standard_dev && input$numericalValues == 0){
          if(input$SDNum > 0){
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
              min = 0,
              max = mean + as.numeric(input$SDNum)*standard_dev,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
            plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
            plotrange[1] = 0
          } else{
            updateSliderInput(
              session,
              "plotrange",
              label = NULL,
              value = NULL,
              min = -1000,
              max = 1000,
              step = NULL,
              timeFormat = NULL,
              timezone = NULL
            )
            old_SD = input$SDNum
          }
        }
        if(old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = c(0,mean + as.numeric(input$SDNum)*standard_dev),
            min = 0,
            max = mean + as.numeric(input$SDNum)*standard_dev,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
          plotrange[2] = mean + as.numeric(input$SDNum)*standard_dev
          plotrange[1] = 0
        } else if(input$numericalValues == 0 && input$SDNum <= 0){
          updateSliderInput(
            session,
            "plotrange",
            label = NULL,
            value = NULL,
            min = -1000,
            max = 1000,
            step = NULL,
            timeFormat = NULL,
            timezone = NULL
          )
          old_SD = input$SDNum
        }
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f74 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f74<-Rlab::dweibull(xseq, as.numeric(input$WeibullK), as.numeric(input$WeibullLambda))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f74<-Rlab::pweibull(xseq, as.numeric(input$WeibullK), as.numeric(input$WeibullLambda))
          graphtype<-"CDF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          fig<-plot_ly(x = xseq,
                       y = f74,
                       name = distType,
                       type = 'scatter',
                       mode='lines',
                       hoverinfo = 'xy'
          )
          xsize = length(xseq)
          newy = f74
          for (index in 1:xsize){
            if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
              newy[index] = NA
            }
          }
          prob = Rlab::pweibull(as.numeric(probrange[2]),as.numeric(input$WeibullK),as.numeric(input$WeibullLambda))-Rlab::pweibull(as.numeric(probrange[1]),as.numeric(input$WeibullK),as.numeric(input$WeibullLambda))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% plotly::layout(title = paste(distributions[74],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f74),max(f74))
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
      # ----------------------- Discrete: Zipf-Mandelbrot Distribution ----------------------- #
      else if(distType == distributions[75]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f75 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f75 <- dzipfman(xseq, as.numeric(input$Zipf_s), as.numeric(input$Zipf_q), as.numeric(input$Zipf_N))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f75 <- pzipfman(xseq, as.numeric(input$Zipf_s), as.numeric(input$Zipf_q), as.numeric(input$Zipf_N))
          graphtype<-"CMF"
        }
        else{
          graphtype<-""
        }
        if(graphtype != ""){
          xsize = length(xseq)
          colors = c(rep('rgb(31, 119, 180)', xsize))
          for (index in 1:xsize){
            if (xseq[index] >= round(probrange[1],0) && xseq[index] <= round(probrange[2],0)){
              colors[index] = 'rgb(255, 127, 14)'
            }
          }
          fig<-plot_ly(x = xseq,
                       y = f75,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = f75,
                       hovertemplate = paste('<br><b>Prob. </b>: %{y}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% plotly::layout(title = paste(distributions[75],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f75),max(f75)),
                                           type = 'linear'
                              ),
                              xaxis=list(showticklabels=TRUE,
                                         title = "* All x values rounded to nearest integers",
                                         zeroline = TRUE,
                                         showline=TRUE,
                                         showgrid=TRUE,
                                         linecolor='rgb(204, 204, 204)',
                                         linewidth=2,
                                         mirror=TRUE,
                                         fixedrange = TRUE,
                                         range = c(plotrange[1],plotrange[2])
                              ),
                              showlegend = FALSE
          )
          fig<-fig %>% config(editable=FALSE)
          fig
        }
      }
    })
    # ----------------------- Render Implementing Message ----------------------- #
    output$Implementing <- renderText({
      if(input$Distribution %in% DistImplement){
        paste("The ", input$Distribution, " is still being implemented.", sep="")
      }
    })
    # ----------------------- Calculate and Render Probability ----------------------- #
    output$probability <- renderText({
      distType <- input$Distribution
      plotrange = c(0,0)
      probrange = c(0,0)
      if(input$numericalValues == FALSE){
        plotrange[1] = input$plotrange[1]
        plotrange[2] = input$plotrange[2]
        probrange[1] = input$probrange[1]
        probrange[2] = input$probrange[2]
      }
      else{
        plotrange[1] = input$plotrangeNumMin
        plotrange[2] = input$plotrangeNumMax
        probrange[1] = input$probrangeNumMin
        probrange[2] = input$probrangeNumMax
      }
      # ----------------------- Continuous: ArcSine Distribution ----------------------- #
      if(distType == distributions[2]){
        prob = parcsine(as.numeric(probrange[2]),as.numeric(input$ArcSineA),as.numeric(input$ArcSineB))-parcsine(as.numeric(probrange[1]),as.numeric(input$ArcSineA),as.numeric(input$ArcSineB))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Benford Distribution ----------------------- #
      else if(distType == distributions[3]){
        prob = pBenf(round(as.numeric(probrange[2]),0),as.numeric(input$Benfn))-pBenf(round(as.numeric(probrange[1]),0)-1,as.numeric(input$Benfn))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Bernoulli Distribution ----------------------- #
      else if(distType == distributions[4]){
        prob = pbern(round(as.numeric(probrange[2]),0),as.numeric(input$BernProb))-pbern(round(as.numeric(probrange[1]),0)-1,as.numeric(input$BernProb))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Beta Distribution ----------------------- #
      else if(distType == distributions[5]){
        prob = pbeta(as.numeric(probrange[2]),as.numeric(input$BetaAlpha),as.numeric(input$BetaBeta))-pbeta(as.numeric(probrange[1]),as.numeric(input$BetaAlpha),as.numeric(input$BetaBeta))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Beta(Generalized) Distribution ----------------------- #
      else if(distType == distributions[6]){
        prob = pgenbeta(as.numeric(probrange[2]),as.numeric(input$BetaGenA),as.numeric(input$BetaGenB),as.numeric(input$BetaGenC),as.numeric(input$BetaGenP)) - pgenbeta(as.numeric(probrange[1]),as.numeric(input$BetaGenA),as.numeric(input$BetaGenB),as.numeric(input$BetaGenC),as.numeric(input$BetaGenP))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Beta Binomial Distribution ----------------------- #
      else if(distType == distributions[7]){
        prob = pbbinom(round(as.numeric(probrange[2]),0),as.numeric(input$BetaBinomN),as.numeric(input$BetaBinomU), as.numeric(input$BetaBinomV))-pbbinom(round(as.numeric(probrange[1]),0)-1,as.numeric(input$BetaBinomN),as.numeric(input$BetaBinomU), as.numeric(input$BetaBinomV))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Binomial Distribution ----------------------- #
      else if(distType == distributions[8]){
        prob = pbinom(round(as.numeric(probrange[2]),0),as.numeric(input$BinomN), as.numeric(input$BinomP))-pbinom(round(as.numeric(probrange[1]),0)-1,as.numeric(input$BinomN), as.numeric(input$BinomP))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Cauchy Distribution ----------------------- #
      else if(distType == distributions[11]){
        prob = pcauchy(as.numeric(probrange[2]),as.numeric(input$CauchyX0),as.numeric(input$CauchyGamma))-pcauchy(as.numeric(probrange[1]),as.numeric(input$CauchyX0),as.numeric(input$CauchyGamma))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Chi Distribution ----------------------- #
      else if(distType == distributions[12]){
        prob = pchi(as.numeric(probrange[2]),as.numeric(input$ChiK))-pchi(as.numeric(probrange[1]),as.numeric(input$ChiK))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Chi Square Distribution ----------------------- #
      else if(distType == distributions[13]){
        prob = pchisq(as.numeric(probrange[2]),as.numeric(input$Chi2n))-pchisq(as.numeric(probrange[1]),as.numeric(input$Chi2n))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Chi Square Non Central Distribution ----------------------- #
      else if(distType == distributions[14]){
        prob = pt(as.numeric(probrange[2]),as.numeric(input$TNCdof),as.numeric(input$TNCNCP))-pt(as.numeric(probrange[1]),as.numeric(input$TNCdof),as.numeric(input$TNCNCP))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Continuous Uniform Distribution ----------------------- #
      else if(distType == distributions[16]){
        prob = punif(as.numeric(probrange[2]),as.numeric(input$UnifMin), as.numeric(input$UnifMax))-punif(as.numeric(probrange[1]),as.numeric(input$UnifMin), as.numeric(input$UnifMax))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Discrete ArcSine Distribution ----------------------- #
      else if(distType == distributions[19]){
        prob = parcsine(round(as.numeric(probrange[2]),0),as.numeric(input$DisArcSineA),as.numeric(input$DisArcSineB))-parcsine(round(as.numeric(probrange[1]),0)-1,as.numeric(input$DisArcSineA),as.numeric(input$DisArcSineB))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Discrete Uniform Distribution ----------------------- #
      else if(distType == distributions[20]){
        prob = punif(round(as.numeric(probrange[2]),0),as.numeric(input$DisUnifMin),as.numeric(input$DisUnifMax))-punif(round(as.numeric(probrange[1]),0)-1,as.numeric(input$DisUnifMin),as.numeric(input$DisUnifMax))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Exponential Distribution ----------------------- #
      else if(distType == distributions[23]){
        prob = Rlab::pexp(as.numeric(probrange[2]),as.numeric(input$ExpLambda))-Rlab::pexp(as.numeric(probrange[1]),as.numeric(input$ExpLambda))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: F Distribution ----------------------- #
      else if(distType == distributions[25]){
        prob = pf(as.numeric(probrange[2]),as.numeric(input$FdOne),as.numeric(input$FdTwo))-pf(as.numeric(probrange[1]),as.numeric(input$FdOne),as.numeric(input$FdTwo))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Gamma Distribution ----------------------- #
      else if(distType == distributions[27]){
        prob = Rlab::pgamma(as.numeric(probrange[2]), shape = as.numeric(input$GammaA), rate = as.numeric(input$GammaB)) - Rlab::pgamma(as.numeric(probrange[1]), shape = as.numeric(input$GammaA), rate = as.numeric(input$GammaB))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Geometric Distribution ----------------------- #
      else if(distType == distributions[30]){
        prob = pgeom(round(as.numeric(probrange[2]),0),as.numeric(input$GeomProb))-pgeom(round(as.numeric(probrange[1]),0)-1,as.numeric(input$GeomProb))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Gompertz Distribution ----------------------- #
      else if(distType == distributions[32]){
        prob = pgompertz(as.numeric(plotrange[2]),as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))-pgompertz(as.numeric(plotrange[1]),as.numeric(input$Gompertz_N), as.numeric(input$Gompertz_B))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Gumbel Distribution ----------------------- #
      else if(distType == distributions[33]){
        prob = pgumbel(as.numeric(probrange[2]),as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))-pgumbel(as.numeric(probrange[1]),as.numeric(input$Gumbel_U), as.numeric(input$Gumbel_Beta))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Half Normal Distribution ----------------------- #
      else if(distType == distributions[34]){
        prob = phnorm(as.numeric(probrange[2]),as.numeric(input$HNorm))-phnorm(as.numeric(probrange[1]),as.numeric(input$HNorm))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Hyper Geometric Distribution ----------------------- #
      else if(distType == distributions[35]){
        prob = phyper(round(as.numeric(probrange[2]),0),as.numeric(input$HyperM),as.numeric(input$HyperN)-as.numeric(input$HyperM),as.numeric(input$HyperK))-phyper(round(as.numeric(probrange[1]),0)-1,as.numeric(input$HyperM),as.numeric(input$HyperN)-as.numeric(input$HyperM),as.numeric(input$HyperK))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Inverse Gamma Distribution ----------------------- #
      else if(distType == distributions[37]){
        prob = pinvgamma(as.numeric(probrange[2]), as.numeric(input$InvGammaA), 1/as.numeric(input$InvGammaB))-pinvgamma(as.numeric(probrange[1]), as.numeric(input$InvGammaA), 1/as.numeric(input$InvGammaB))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Inverse Gaussian(Wald) Distribution ----------------------- #
      else if(distType == distributions[38]){
        prob = pinvgauss(as.numeric(probrange[2]), as.numeric(input$InvGausM), as.numeric(input$InvGausL))-pinvgauss(as.numeric(probrange[1]), as.numeric(input$InvGausM), as.numeric(input$InvGausL))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Laplace Distribution ----------------------- #
      else if(distType == distributions[42]){
        prob = plaplace(as.numeric(probrange[2]), as.numeric(input$LapMu), as.numeric(input$LapSig))-plaplace(as.numeric(probrange[1]), as.numeric(input$LapMu), as.numeric(input$LapSig))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Logarithmic Series Distribution ----------------------- #
      else if(distType == distributions[43]){
        prob = 0
        for(i in (round(as.numeric(probrange[1])): round(as.numeric(probrange[2])))){
          prob = prob + dlogseries(i, as.numeric(input$LogP))
        }
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Logistic Distribution ----------------------- #
      else if(distType == distributions[44]){
        prob = plogis(as.numeric(probrange[2]),as.numeric(input$LogiA),as.numeric(input$LogiB))-plogis(as.numeric(probrange[1]),as.numeric(input$LogiA),as.numeric(input$LogiB))
        paste("Prob. = ",prob,sep="")
      }
      #-----------------------Continuous: Logistic-Exponential Distribution ----------------------- #
      else if(distType == distributions[45]){
        prob = 0
        Beta = as.numeric(input$LogEx_B)
        Alpha = as.numeric(input$LogEx_A)
        if(as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0){
          prob = 0
        } else if(as.numeric(probrange[1]) <= 0){
          prob = ( (exp(Alpha * (as.numeric(probrange[2])) ) - 1) ** Beta)/(1 + (exp(Alpha * (as.numeric(probrange[2]))) - 1 )** Beta)
        } else if(as.numeric(probrange[1]) > 0 && as.numeric(probrange[2]) > 0 ){
          prob =( (exp(Alpha * (as.numeric(probrange[2])) ) - 1) ** Beta)/(1 + (exp(Alpha * (as.numeric(probrange[2]))) - 1 )** Beta)
          - ( (exp(Alpha * (as.numeric(probrange[1]))) - 1) ** Beta)/(1 + (exp(Alpha * (as.numeric(probrange[1]))) - 1 )** Beta)
        }
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: LogNormal Distribution ----------------------- #
      else if(distType == distributions[46]){
        prob = plnorm(as.numeric(probrange[2]),as.numeric(input$LogNormMean),as.numeric(input$LogNormSD))-plnorm(as.numeric(probrange[1]),as.numeric(input$LogNormMean),as.numeric(input$LogNormSD))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Lomax Distribution ----------------------- #
      else if(distType == distributions[47]){
        prob = plomax(as.numeric(probrange[2]), as.numeric(input$LomaxLamda), as.numeric(input$LomaxKappa))-plomax(as.numeric(probrange[1]), as.numeric(input$LomaxLamda), as.numeric(input$LomaxKappa))
        paste("Prob. = ",prob,sep="")
      }
       # ----------------------- Continuous: Maxwell Distribution ----------------------- #
       else if(distType == distributions[49]){
         prob = pmaxwell(as.numeric(probrange[2]), as.numeric(input$MaxwellA))-pmaxwell(as.numeric(probrange[1]), as.numeric(input$MaxwellA))
         paste("Prob. = ",prob,sep="")
       }
      # ----------------------- Continuous: Minimax Distribution ----------------------- #
      else if(distType == distributions[50]){
        prob = 0
        Beta = as.numeric(input$Mini_B)
        Gamma = as.numeric(input$Mini_V)
        if(as.numeric(probrange[2]) >= 1 && as.numeric(probrange[1]) <= 0){
          prob = 1
        } else if(as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0){
          prob = 0
        } else if(as.numeric(probrange[2]) >= 1 && as.numeric(probrange[1]) >= 1){
          prob = 1
        }
        else if(as.numeric(probrange[2]) >= 1){
          prob = 1  - (1- (1-(as.numeric(probrange[1]))**Beta)**(Gamma))
        } else if(as.numeric(probrange[1]) <= 0){
          prob = 1- (1-(as.numeric(probrange[2]))**Beta)**(Gamma)
        } else{
          prob = 1- (1-(as.numeric(probrange[2]))**Beta)**(Gamma) - (1- (1-(as.numeric(probrange[1]))**Beta)**(Gamma))
        }
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Muth Distribution ----------------------- #
      else if(distType == distributions[53]){
        K = as.numeric(input$MuthKappa)
        prob = 0
        if(as.numeric(probrange[2]) <= 0 && as.numeric(probrange[1]) <= 0){
          prob = 0
        } else if(as.numeric(probrange[2]) <= 0){
          prob = 0
        } else if(as.numeric(probrange[1]) <= 0){
          prob = 1- exp(-(exp(K*as.numeric(probrange[2])))/K + K*as.numeric(probrange[2]) + 1/K)
        } else{
          prob = (1- exp(-(exp(K*as.numeric(probrange[2])))/K + K*as.numeric(probrange[2]) + 1/K)) - (1- exp(-(exp(K*as.numeric(probrange[1])))/K + K*as.numeric(probrange[1]) + 1/K))
        }
        
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Negatvie Binomial Distribution ----------------------- #
      else if(distType == distributions[54]){
        prob = pnbinom(round(as.numeric(probrange[2]),0),as.numeric(input$NegBiR),as.numeric(input$NegBiP))-pnbinom(round(as.numeric(probrange[1]),0)-1,as.numeric(input$NegBiR),as.numeric(input$NegBiP))

        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Negatvie HyperGeometric Distribution ----------------------- #
      else if(distType == distributions[55]){
        prob =pnhyper(round(as.numeric(probrange[2]),0),as.numeric(input$NegHyperK)-as.numeric(input$NegHyperN),as.numeric(input$NegHyperN),as.numeric(input$NegHyperR))-pnhyper(round(as.numeric(probrange[1]),0)-1,as.numeric(input$NegHyperK)-as.numeric(input$NegHyperN),as.numeric(input$NegHyperN),as.numeric(input$NegHyperR))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Normal Distribution ----------------------- #
      else if(distType == distributions[57]){
        prob = pnorm(as.numeric(probrange[2]),as.numeric(input$NormMean),as.numeric(input$NormSD))-pnorm(as.numeric(probrange[1]),as.numeric(input$NormMean),as.numeric(input$NormSD))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Normal Truncated Distribution ----------------------- #
      else if(distType == distributions[58]){
        prob = ptruncnorm(as.numeric(probrange[2]),as.numeric(input$TruncNormMin),as.numeric(input$TruncNormMax),as.numeric(input$TruncNormMean),as.numeric(input$TruncNormSD))-ptruncnorm(as.numeric(probrange[1]),as.numeric(input$TruncNormMin),as.numeric(input$TruncNormMax),as.numeric(input$TruncNormMean),as.numeric(input$TruncNormSD))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Pareto Distribution ----------------------- #
      else if(distType == distributions[59]){
        prob = ppareto(as.numeric(probrange[2]), as.numeric(input$ParetoA), as.numeric(input$ParetoB))-ppareto(as.numeric(probrange[1]), as.numeric(input$ParetoA), as.numeric(input$ParetoB))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Poisson Distribution ----------------------- #
      else if(distType == distributions[61]){
        prob = ppois(round(as.numeric(probrange[2]),0),as.numeric(input$PoiLambda))-ppois(round(as.numeric(probrange[1]),0)-1,as.numeric(input$PoiLambda))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Rayleigh Distribution ----------------------- #
      else if(distType == distributions[64]){
        prob = prayleigh(as.numeric(probrange[2]), as.numeric(input$RayleighSigma))-prayleigh(as.numeric(probrange[1]), as.numeric(input$RayleighSigma))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Rice Distribution -------------------- #
      else if(distType == distributions[65]){
        prob = price(as.numeric(probrange[2]),as.numeric(input$RiceSigma),as.numeric(input$RiceVee))-Rlab::pweibull(as.numeric(probrange[1]),as.numeric(input$RiceSigma),as.numeric(input$RiceVee))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: T Distribution ----------------------- #
      else if(distType == distributions[66]){
        prob = pt(as.numeric(probrange[2]),as.numeric(input$Tdof))-pt(as.numeric(probrange[1]),as.numeric(input$Tdof))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: T Non Central Distribution ----------------------- #
      else if(distType == distributions[67]){
        prob = pt(as.numeric(probrange[2]),as.numeric(input$TNCdof),as.numeric(input$TNCNCP))-pt(as.numeric(probrange[1]),as.numeric(input$TNCdof),as.numeric(input$TNCNCP))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Triangular Distribution ----------------------- #
      else if(distType == distributions[68]){
        prob = ptriangular(as.numeric(probrange[2]),as.numeric(input$Triangular_A),as.numeric(input$Triangular_B),as.numeric(input$Triangular_C))-ptriangular(as.numeric(probrange[1]),as.numeric(input$Triangular_A),as.numeric(input$Triangular_B),as.numeric(input$Triangular_C))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: U-quadratic distribution ----------------------- #
      else if(distType == distributions[70]){
        prob = 0
        W = as.numeric(input$UQ_W)
        C = as.numeric(input$UQ_C)
        if(as.numeric(probrange[2]) >= W+C && as.numeric(probrange[1] <= C-W)){
          prob = 1
        }
        else if(as.numeric(probrange[2]) <= C-W && as.numeric(probrange[1] <= C-W)){
          prob = 0
        }
        else if(as.numeric(probrange[2]) >= W+C){
          prob = 1 - 0.5*(1+((as.numeric(probrange[1]) - C)/W)**3)
        }else if(as.numeric(probrange[1] <= C-W)){
          prob = 0.5*(1+((as.numeric(probrange[2]) - C)/W)**3)
        } 
        else{
          prob = 0.5*(1+((as.numeric(probrange[2]) - C)/W)**3) - 0.5*(1+((as.numeric(probrange[1]) - C)/W)**3)
        }
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Weibull Distribution ----------------------- #
      else if(distType == distributions[74]){
        prob = Rlab::pweibull(as.numeric(probrange[2]),as.numeric(input$WeibullK),as.numeric(input$WeibullLambda))-Rlab::pweibull(as.numeric(probrange[1]),as.numeric(input$WeibullK),as.numeric(input$WeibullLambda))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Zipf-Mandelbrot Distribution  -------------------- #
      else if(distType == distributions[75]){
        prob = pzipfman(round(as.numeric(probrange[2]),0),as.numeric(input$Zipf_s), as.numeric(input$Zipf_q), as.numeric(input$Zipf_N))-pzipfman(round(as.numeric(probrange[1]),0),as.numeric(input$Zipf_s), as.numeric(input$Zipf_q), as.numeric(input$Zipf_N))
        paste("Prob. = ",prob,sep="")
      }
    })
  }
)