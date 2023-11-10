fitNormal <- function(dataset) {
    fitDistModel <- fitdist(dataset, "norm")
    return(fitDistModel)
}

#plotlyNormalDistribution <- function(plotrange, input, distType, probrange, session) {
plotlyNormalDistribution <- function(plotrange, input, distType, probrange, session, dataset) {
    old_SD <- 0
    if (plotrange[2] != as.numeric(input$NormMean) + as.numeric(input$SDNum) * as.numeric(input$NormSD) &&
        input$numericalValues == 0) {
        if (input$SDNum > 0) {
            updateSliderInput(session, "plotrange", label = NULL, value = c(as.numeric(input$NormMean) -
                as.numeric(input$SDNum) * as.numeric(input$NormSD), as.numeric(input$NormMean) +
                as.numeric(input$SDNum) * as.numeric(input$NormSD)), min = as.numeric(input$NormMean) -
                as.numeric(input$SDNum) * as.numeric(input$NormSD), max = as.numeric(input$NormMean) +
                as.numeric(input$SDNum) * as.numeric(input$NormSD), step = NULL,
                timeFormat = NULL, timezone = NULL)
            old_SD = input$SDNum
            plotrange[2] = as.numeric(input$NormMean) + as.numeric(input$SDNum) *
                as.numeric(input$NormSD)
            plotrange[1] = as.numeric(input$NormMean) - as.numeric(input$SDNum) *
                as.numeric(input$NormSD)
        } else {
            updateSliderInput(session, "plotrange", label = NULL, value = NULL, min = -1000,
                max = 1000, step = NULL, timeFormat = NULL, timezone = NULL)
            old_SD = input$SDNum
        }
    }
    if (old_SD != input$SDNum && input$numericalValues == 0 && input$SDNum > 0) {
        updateSliderInput(session, "plotrange", label = NULL, value = c(as.numeric(input$NormMean) -
            as.numeric(input$SDNum) * as.numeric(input$NormSD), as.numeric(input$NormMean) +
            as.numeric(input$SDNum) * as.numeric(input$NormSD)), min = as.numeric(input$NormMean) -
            as.numeric(input$SDNum) * as.numeric(input$NormSD), max = as.numeric(input$NormMean) +
            as.numeric(input$SDNum) * as.numeric(input$NormSD), step = NULL, timeFormat = NULL,
            timezone = NULL)
        old_SD = input$SDNum
        plotrange[2] = as.numeric(input$NormMean) + as.numeric(input$SDNum) * as.numeric(input$NormSD)
        plotrange[1] = as.numeric(input$NormMean) - as.numeric(input$SDNum) * as.numeric(input$NormSD)
    } else if (input$numericalValues == 0 && input$SDNum <= 0) {
        updateSliderInput(session, "plotrange", label = NULL, value = NULL, min = -1000,
            max = 1000, step = NULL, timeFormat = NULL, timezone = NULL)
        old_SD = input$SDNum
    }

    xseq <- seq(min(0, as.numeric(plotrange[1])), max(as.numeric(plotrange[2]), 10),
        0.01)
    f57 <- 0
    graphtype <- ""
    if (input$FunctionType == "PDF/PMF") {
        f57 <- dnorm(xseq, as.numeric(input$NormMean), as.numeric(input$NormSD))
        graphtype <- "PDF"
    } else if (input$FunctionType == "CDF/CMF") {
        f57 <- pnorm(xseq, as.numeric(input$NormMean), as.numeric(input$NormSD))
        graphtype <- "CDF"
    } else {
        graphtype <- ""
    }
    if (graphtype != "") {
        # fig <- plot_ly(x = xseq, y = f57, name = distType, type = "scatter", mode = "lines",
        #     hoverinfo = "xy")
        # xsize = length(xseq)
        # newy = f57
        # for (index in 1:xsize) {
        #     if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
        #         newy[index] = NA
        #     }
        # }
        # prob = pnorm(as.numeric(probrange[2]), as.numeric(input$NormMean), as.numeric(input$NormSD)) -
        #     pnorm(as.numeric(probrange[1]), as.numeric(input$NormMean), as.numeric(input$NormSD))
        # fig <- fig %>%
        #     add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
        #         hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
        # fig <- fig %>%
        #     plotly::layout(title = paste(distributions[57], " - ", graphtype, sep = ""),
        #         hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
        #           zeroline = TRUE, range = c(min(f57), max(f57))), xaxis = list(showticklabels = TRUE,
        #           zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
        #           linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
        #             plotrange[2])), showlegend = FALSE)
        # fig <- plot_ly(data = dataset, x = ~unlist(dataset[, input$outcome]), type = "histogram",
        #                            histnorm = "probability", name = "Data Histogram", opacity = 0.5)
      
# Create the first plot
fig1 <- plot_ly(x = xseq, y = f57, name = distType, type = "scatter", mode = "lines",
                hoverinfo = "xy")
xsize <- length(xseq)
newy <- f57
for (index in 1:xsize) {
    if (xseq[index] < probrange[1] || xseq[index] > probrange[2]) {
        newy[index] <- NA
    }
}
prob <- pnorm(as.numeric(probrange[2]), as.numeric(input$NormMean), as.numeric(input$NormSD)) -
        pnorm(as.numeric(probrange[1]), as.numeric(input$NormMean), as.numeric(input$NormSD))
fig1 <- fig1 %>%
    add_trace(x = xseq, y = newy, name = paste("Probability = ", prob, sep = ""),
              hoverinfo = "name", fill = "tozeroy", fillcolor = "rgba(255, 212, 96, 0.5)")
fig1 <- fig1 %>%
    plotly::layout(title = paste(distributions[57], " - ", graphtype, sep = ""),
                    hovermode = "x", hoverlabel = list(namelength = 100), yaxis = list(fixedrange = TRUE,
                    zeroline = TRUE, range = c(min(f57), max(f57))), xaxis = list(showticklabels = TRUE,
                    zeroline = TRUE, showline = TRUE, showgrid = TRUE, linecolor = "rgb(204, 204, 204)",
                    linewidth = 2, mirror = TRUE, fixedrange = TRUE, range = c(plotrange[1],
                    plotrange[2])), showlegend = FALSE)


#Case 1 Closest I can make
# counts <- table(dataset[, input$outcome])
# percentages <- prop.table(counts)  # Calculate proportions (between 0 and 1)
# 
# # Calculate min and max values
# min_val <- min(dataset[, input$outcome])
# max_val <- max(dataset[, input$outcome])
# 
# # Calculate the bin width
# bin_width <- (max_val - min_val) / 10
# 
# # Create a data frame with the calculated proportions
#  hist_data <- data.frame(x = as.numeric(names(counts)), y = as.numeric(percentages) )
# 
# 
#  fig1 <- fig1 %>%
#    add_trace(data = hist_data, x = ~x, y = ~y, type = "bar",
#              name = "Data Histogram", opacity = 0.3)


# Case 2 histogram trace works but doesnt work after adding to fig1
# histogram_trace <- plot_ly(data = dataset, x = ~dataset[, input$outcome], type = "histogram",
#                            histnorm = "probability",
#                            name = "Histogram Layer",
#                            size = 0.4)
# print(histogram_trace)
# 
# fig1 <- fig1 %>%
#   add_histogram(histogram_trace)


# Case 3 Error Recycle_column?
# # Add the histogram trace to fig1
# fig1 <- fig1 %>%
#   add_histogram(data = dataset, x = ~dataset[,input$outcome], type = "histogram",
#                  histnorm = "probability", name = "Histogram Layer",
#                  size = 0.4)


# fig1

#best i can do
histogram_trace <- plot_ly(data = dataset, x = ~dataset[, input$outcome], type = "histogram",
                                                       histnorm = "probability",
                                                       name = "Histogram Layer",
                                                       size = 0.4)
# Combine both plots into a single subplot
combined_fig <- subplot(fig1, histogram_trace, nrows = 2)

combined_fig

    }
}

# Define the histogram trace using the provided code
# histogram_trace <- plot_ly(data = dataset, x = ~unlist(dataset[, input$outcome]), type = "bar",
#                            histnorm = "probability", name = "Data Histogram")
# 
# print(histogram_trace)

# bar_data <- data.frame(x = names(dataset[, input$outcome]), y = dataset[, input$outcome])
# 
# # Create a bar plot using Plotly
# bar_plot <- plot_ly(data = bar_data, x = ~x, type = "bar", histnorm = "probability", name = "Data Histogram")
# 
# print(bar_plot)

# Get the dynamic column name from input$outcome
# column_name <- input$outcome
# 
# # Extract the specified column
# column_data <- dataset[, column_name]
# 
# # Calculate the frequency of each unique value in the specified column
# frequency <- table(column_data)
# 
# # Calculate the total number of data points
# total_data <- length(column_data)
# 
# # Calculate the ratio of frequency to total_data
# ratio <- frequency / total_data
# 
# # Create a new dataframe with the unique values and their frequency ratio
# new_df <- data.frame(Value = as.numeric(names(ratio)), Ratio = ratio)
# 
# # Print the new dataframe
# print(new_df)




# hist_data <- dataset %>%
#   dplyr::pull(input$outcome) %>%
#   hist()

# fig1 <- fig1 %>%
#   add_trace(x = hist_data$breaks, 
#             y = hist_data$counts,
#             type = "bar",
#             name = "Histogram Layer",
#             histnorm = "probability",
#             marker = list(size = 0.4))

# hist_data <- list(dataset[, input$outcome])
# print(hist_data)
