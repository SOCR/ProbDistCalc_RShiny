# SOCR Probability Distribution Calculator
# Version 0.9
# Updated October 6th 2023 by Joonseop Kim at the University of Michigan -SOCR
# Orginally created by Jared(Tianyi) Chai

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
library(ExtDist)
library(VaRES)
source("renderMainPlot.R")
source("renderProbability.R")

shinyServer(
  function(input, output, session) {
    # ----------------------- Update Distribution Type and Function Type according to URL handle ----------------------- #
    observe({
      query <- parseQueryString(session$clientData$url_search)
      if (!is.null(query[["d"]])) {
        updateSelectInput(session, "Distribution", selected = distributions[as.numeric(query[["d"]])])
        updateSelectInput(session, "FunctionType", selected = "PDF/PMF")
        if (!is.null(query[["t"]])) {
          updateSelectInput(session, "FunctionType", selected = query[["t"]])
        }
      }
      # ----------------------- Update Range of Probability Calculation according to Range of X ----------------------- #
      updateSliderInput(session,
        "probrange",
        value = 0,
        min = input$plotrange[1],
        max = input$plotrange[2],
        step = 0.01
      )
      updateNumericInput(session,
        "probrangeNumMin",
        value = 0,
        min = input$plotrangeNumMin,
        max = input$plotrangeNumMax
      )
      updateNumericInput(session,
        "probrangeNumMax",
        value = 0,
        min = input$plotrangeNumMin,
        max = input$plotrangeNumMax
      )
    })
    # ----------------------- HelpMe ----------------------- #
    observeEvent(input$fitParams, {
      distributionInfo <- distributionInfoList[[input$Distribution]]
      if (is.null(dataset)) {
        showNotification("Dataset is not specified.", type = "error", duration = 2)
      } else if (is.null((distributionInfo$fitFunc))) {
        showNotification("Fitting this distribution is not supported yet.", type = "error", duration = 2)
      } else {
        fit_result <- distributionInfo$fitFunc(dataset[, input$outcome])
        for (i in 1:length(fit_result$estimate)) {
          inputName <- distributionInfo$inputNames[[i]]
          updateTextInput(session, inputName, value = fit_result$estimate[[i]])
          session$sendCustomMessage("highlightTextInput", inputName)
        }
        output$fitStatus <- renderText({
          # FIXME: This is not working
          # paste("Fitted mean: ", fitted_mean, " Fitted standard deviation: ", fitted_sd)
        })
      }
    })

    observeEvent(input$vh.readme, {
      showModal(modalDialog(
        title = "Help / ReadMe",
        HTML("<div>
             <font size=\"3\"><font color=\"blue\"><b>SOCR Interactive Probability Distribution Calculator [Version: V.0.8]</b></font></font>
             The SOCR RShiny probability distribution calculators provide interactive vizualizations of probability densities,
             mass functions, and cumulative distributions, e.g., bivariate normal distribution.
             <br /><br />
            <b> Acknowledgments </b>
             <br /><br />
            This work is supported in part by NIH grants P20 NR015331, UL1TR002240, P30 DK089503, UL1TR002240,
            and NSF grants 1916425, 1734853, 1636840, 1416953, 0716055 and 1023115. Students, trainees, scholars,
            and researchers from SOCR, BDDS, MIDAS, MICHR, and the broad R-statistical computing community have contributed ideas,
            code, and support.
             <div align=\"center\">
             <font size=\"3\"><b>Developers</b><br /></font></div>
             <font size=\"2\">Jared (Tianyi) Chai (<b>chtianyi@umich.edu</b>)
             <font size=\"2\">Shihang Li (<b>shihangl@umich.edu</b>)
             <font size=\"2\">Yongxiang Zhao (<b>zyxleo@umich.edu</b>),
             Ivo Dinov (<b>dinov@med.umich.edu</b>).</font>
             <br /><br />
             "),
        easyClose = TRUE
      ))
    })
    # ----------------------- Render Metadata Information from xml Database ----------------------- #
    output$MetaData <- renderPrint({
      distType <- input$Distribution
      distType <- tolower(str_replace_all(distType, "[^[:alnum:]]", ""))
      counter <- 0
      for (i in 1:xml_len) {
        j <- 1
        while (distributions_meta[[j, i * 2 - 1]] == "name") {
          if (tolower(str_replace_all(distributions_meta[[1, i * 2]], "[^[:alnum:]]", "")) == distType) {
            counter <- i
            break
          } else {
            j <- j + 1
          }
        }
      }
      outputstring <- ""
      if (counter != 0) {
        row <- 1
        while (distributions_meta[[row, counter * 2 - 1]] != "" && row < xml_wid) {
          outputstring <- paste(outputstring, "<b>", distributions_meta[[row, counter * 2 - 1]], ":</b> ", distributions_meta[[row, counter * 2]], "\n", sep = "")
          row <- row + 1
        }
      }
      withMathJax(helpText(HTML(outputstring)))
    })
    # ----------------------- Render Main Plot ----------------------- #
    renderMainPlot(input, output, session)
    # ----------------------- Render Implementing Message ----------------------- #
    output$Implementing <- renderText({
      if (input$Distribution %in% distToImpl) {
        paste("The ", input$Distribution, " is still being implemented.", sep = "")
      }
    })
    # ----------------------- Calculate and Render Probability ----------------------- #
    renderProbability(input, output, session)


    # Imputation of categorical variables using Mode
    getmode <- function(v) {
      v <- v[nchar(as.character(v)) > 0]
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }



    # Data output
    output$tbl <- DT::renderDataTable({
      DT::datatable(dataset, options = list(lengthChange = FALSE))
    })



    # Regression output
    output$summary <- renderPrint({
      fit <- lm(unlist(dataset[, input$outcome]) ~ unlist(dataset[, input$indepvar]))
      names(fit$coefficients) <- c("Intercept", input$var2)
      fitCurrent <- fit
      summary(fit)
    })

    # Scatterplot output
    output$scatterplot <- renderPlotly({
      plot_ly(
        x = ~ unlist(dataset[, input$indepvar]), y = ~ unlist(dataset[, input$outcome]),
        type = "scatter", mode = "markers", name = "Data"
      ) %>%
        add_lines(
          x = ~ unlist(dataset[, input$indepvar]),
          y = ~ (lm(unlist(dataset[, input$outcome]) ~ unlist(dataset[, input$indepvar]))$fitted.values),
          mode = "lines", name = "Linear Model"
        ) %>%
        add_lines(
          x = ~ lowess(unlist(dataset[, input$indepvar]), unlist(dataset[, input$outcome]))$x,
          y = ~ lowess(unlist(dataset[, input$indepvar]), unlist(dataset[, input$outcome]))$y,
          mode = "lines", name = "LOESS"
        ) %>%
        add_markers(
          x = mean(unlist(dataset[, input$indepvar])), y = mean(unlist(dataset[, input$outcome])),
          name = "Center Point", marker = list(size = 20, color = "green", line = list(color = "yellow", width = 2))
        ) %>%
        layout(
          title = paste0(
            "lm(", input$outcome, " ~ ", input$indepvar,
            "), Cor(", input$indepvar, ",", input$outcome, ") = ",
            round(cor(unlist(dataset[, input$indepvar]), unlist(dataset[, input$outcome])), 3)
          ),
          xaxis = list(title = input$indepvar), yaxis = list(title = input$outcome)
        )
    })
  }
)
