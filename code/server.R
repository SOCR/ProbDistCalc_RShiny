#SOCR Probability Distribution Calculator
#Version 0.4
#Updated July 19th 2020 by Jared (Tianyi) Chai at the University of Michigan -SOCR

# This is a SOCR Interactive Graphical Probability Distribution Calculator
# You can run the application by clicking
# the 'Run App' button above.

# ----------------------- Server.R ----------------------- #
# For backend calculations
library(xml2)
library(shiny)
library(plotly)
library(stringr)
library("Rlab")
library("shinyWidgets")

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
      # ----------------------- Discrete: Bernoulli Distribution ----------------------- #
      if(distType == distributions[4]){
        xseq<-seq(round(min(0,as.numeric(plotrange[1])),0),round(max(as.numeric(plotrange[2]),10),0),1)
        f4 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f4 <- dbern(xseq, as.numeric(input$BernProb))
          graphtype<-"PMF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f4 <- pbern(xseq, as.numeric(input$BernProb))
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
                       y = f4,
                       name = distType,
                       type = 'bar',
                       marker = list(color = colors),
                       text = c(rep(pbern(round(as.numeric(probrange[2]),0),as.numeric(input$BernProb))-pbern(round(as.numeric(probrange[1]),0)-1,as.numeric(input$BernProb)),xsize)),
                       hovertemplate = paste('<br><b>Prob. </b>: %{text}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                       ),
          )
          fig<-fig %>% layout(title = paste(distributions[4],' - ',graphtype,sep = ""),
                              hovermode  = 'x',
                              hoverlabel = list(
                                namelength = 100
                              ),
                              yaxis = list(fixedrange = TRUE,
                                           zeroline = TRUE,
                                           range = c(min(f4),max(f4)),
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
          f23 <- dexp(xseq, as.numeric(input$ExpLambda))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f23 <- pexp(xseq, as.numeric(input$ExpLambda))
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
          prob = pexp(as.numeric(probrange[2]),as.numeric(input$ExpLambda))-pexp(as.numeric(probrange[1]),as.numeric(input$ExpLambda))
          fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
          fig<-fig %>% layout(title = paste(distributions[23],' - ',graphtype,sep = ""),
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
      # ----------------------- Continuous: LogNormal Distribution ----------------------- #
      else if(distType == distributions[46]){
        xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
        f46 <- 0
        graphtype<-""
        if(input$FunctionType == "PDF/PMF"){
          f46<-dlnorm(xseq, as.numeric(input$LogNormMean), as.numeric(input$LogNormSD))
          graphtype<-"PDF"
        }
        else if(input$FunctionType == "CDF/CMF"){
          f46<-pnorm(xseq, as.numeric(input$LogNormMean), as.numeric(input$LogNormSD))
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
          fig<-fig %>% layout(title = paste(distributions[46],' - ',graphtype,sep = ""),
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
      # ----------------------- Continuous: Normal Distribution ----------------------- #
      else if(distType == distributions[57]){
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
          fig<-fig %>% layout(title = paste(distributions[57],' - ',graphtype,sep = ""),
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
      # ----------------------- Discrete: Poisson Distribution ----------------------- #
      else if(distType == distributions[61]){
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
                       text = c(rep(ppois(round(as.numeric(probrange[2]),0),as.numeric(input$PoiLambda))-ppois(round(as.numeric(probrange[1]),0)-1,as.numeric(input$PoiLambda)),xsize)),
                       hovertemplate = paste('<br><b>Prob. </b>: %{text}</br>',
                                             '<b>X</b>: %{x}',
                                             '<b>Y</b>: %{y}'
                                             ),
          )
          fig<-fig %>% layout(title = paste(distributions[61],' - ',graphtype,sep = ""),
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
      # ----------------------- Discrete: Bernoulli Distribution ----------------------- #
      if(distType == distributions[4]){
        prob = pbern(round(as.numeric(probrange[2]),0),as.numeric(input$BernProb))-pbern(round(as.numeric(probrange[1]),0)-1,as.numeric(input$BernProb))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Exponential Distribution ----------------------- #
      else if(distType == distributions[23]){
        prob = pexp(as.numeric(probrange[2]),as.numeric(input$ExpLambda))-pexp(as.numeric(probrange[1]),as.numeric(input$ExpLambda))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: LogNormal Distribution ----------------------- #
      else if(distType == distributions[46]){
        prob = plnorm(as.numeric(probrange[2]),as.numeric(input$LogNormMean),as.numeric(input$LogNormSD))-plnorm(as.numeric(probrange[1]),as.numeric(input$LogNormMean),as.numeric(input$LogNormSD))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Continuous: Normal Distribution ----------------------- #
      else if(distType == distributions[57]){
        prob = pnorm(as.numeric(probrange[2]),as.numeric(input$NormMean),as.numeric(input$NormSD))-pnorm(as.numeric(probrange[1]),as.numeric(input$NormMean),as.numeric(input$NormSD))
        paste("Prob. = ",prob,sep="")
      }
      # ----------------------- Discrete: Poisson Distribution ----------------------- #
      else if(distType == distributions[61]){
        prob = ppois(round(as.numeric(probrange[2]),0),as.numeric(input$PoiLambda))-ppois(round(as.numeric(probrange[1]),0)-1,as.numeric(input$PoiLambda))
        paste("Prob. = ",prob,sep="")
      }
    })
  }
)