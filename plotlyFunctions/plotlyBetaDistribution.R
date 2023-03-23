plotlyBetaDistribution <- function(plotrange, input, distType, probrange){
  xseq<-seq(min(0,as.numeric(plotrange[1])),max(as.numeric(plotrange[2]),10),0.01)
  f24 <- 0
  graphtype<-""
  if(input$FunctionType == "PDF/PMF"){
    f24 <- dbeta(xseq, as.numeric(input$BetaAlpha), as.numeric(input$BetaBeta))
    graphtype<-"PDF"
  }
  else if(input$FunctionType == "CDF/CMF"){
    f24 <- pbeta(xseq, as.numeric(input$BetaAlpha), as.numeric(input$BetaBeta))
    graphtype<-"CDF"
  }
  else{
    graphtype<-""
  }
  if(graphtype != ""){
    fig<-plot_ly(x = xseq,
                  y = f24,
                  name = distType,
                  type = 'scatter',
                  mode='lines',
                  hoverinfo = 'xy'
    )
    print(fig)
    xsize = length(xseq)
    newy = f24
    for (index in 1:xsize){
      if (xseq[index] < probrange[1] || xseq[index] > probrange[2]){
        newy[index] = NA
      }
    }
    prob = pbeta(as.numeric(probrange[2]),as.numeric(input$BetaAlpha),as.numeric(input$BetaBeta))-pbeta(as.numeric(probrange[1]),as.numeric(input$BetaAlpha),as.numeric(input$BetaBeta))
    fig <- fig %>% add_trace(x=xseq, y = newy, name = paste("Probability = ",prob, sep = ""), hoverinfo = 'name', fill = 'tozeroy',fillcolor = 'rgba(255, 212, 96, 0.5)')
    fig<-fig %>% layout(title = paste(distributions[24],' - ',graphtype,sep = ""),
                        hovermode  = 'x',
                        hoverlabel = list(
                          namelength = 100
                        ),
                        yaxis = list(fixedrange = TRUE,
                                      zeroline = TRUE,
                                      range = c(min(f24),max(f24))
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
