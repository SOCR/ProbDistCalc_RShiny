
plotlyLogNormalDistribution <- function(plotrange, input, distType, probrange){
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