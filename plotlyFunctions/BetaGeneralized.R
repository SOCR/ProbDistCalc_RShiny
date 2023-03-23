plotlyBetaGeneralizedDistribution <- function(plotrange, input, distType, probrange) {
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
