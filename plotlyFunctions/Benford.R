plotlyBenfordDistribution <- function(plotrange, input, distType, probrange) {
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
