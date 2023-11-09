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
library(shinyjs)
shinyjs::useShinyjs()
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

    observeEvent(input$fitParams, {
      updateTextInput(session, "FunctionType", value = "PDF/PMF")
      distributionInfo <- distributionInfoList[[input$Distribution]]
      if (is.null(dataset)) {
        showNotification("Dataset is not specified.", type = "error", duration = 2)
      } else if (is.null((distributionInfo$fitFunc))) {
        showNotification("Fitting this distribution is not supported yet.", type = "error", duration = 2)
      } else {
        fit_result <- distributionInfo$fitFunc(dataset[, input$outcome])
        for (i in 1:length(fit_result$estimate)) {
          inputName <- distributionInfo$inputNames[[i]]
          fitted_parameter <- round(fit_result$estimate[[i]], digits = 4)
          updateTextInput(session, inputName, value = fitted_parameter)
          session$sendCustomMessage("highlightTextInput", inputName)
        }
      }
    })

    observe({
      if (input$numericalValues == 0 && input$Distribution %in% distWithSD) {
        shinyjs::enable("SDNum")
        # FIXME: This is not working
        # shinyjs::toggle("SDNumColumn", condition = TRUE)
      } else {
        shinyjs::disable("SDNum")
        # FIXME: This is not working
        # shinyjs::toggle("SDNumColumn", condition = FALSE)
      }
    })

    # Generate text to display current parameters and their values
    output$currentParameters <- renderUI(
      HTML({
        distributionInfo <- distributionInfoList[[input$Distribution]]
        paramValues <- lapply(seq_along(distributionInfo$labels), function(i) {
          label <- distributionInfo$labels[[i]]
          inputName <- distributionInfo$inputNames[[i]]
          paste("<i>", label, "</i>: <b>", input[[inputName]], "</b>")
        })
        # Combine the distribution name and parameter values into a single string
        paste("Current Parameters:<br />", paste(paramValues, collapse = "<br />"))
      })
    )

    observeEvent(input$CalcModelerTabsetPanel, {
      if (input$CalcModelerTabsetPanel == "Modeler") {
        updateTextInput(session, "FunctionType", value = "")
      }
    })

    # Reactive function to read uploaded file and update dataset
    observeEvent(input$file, {
      req(input$file)
      dataset <<- read.csv(input$file$datapath)
      # Update choices for selectInput widgets
      updateSelectInput(session, "outcome", choices = namedListOfFeatures(), selected = NULL)
      updateSelectInput(session, "indepvar", choices = namedListOfFeatures(), selected = NULL)
    })

    # ----------------------- HelpMe ----------------------- #
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
             <font size=\"3\"><b>Developers</b><br /></font>
             <font size=\"2\">Jared (Tianyi) Chai (<b>chtianyi@umich.edu</b>)<br />
             <font size=\"2\">Shihang Li (<b>shihangl@umich.edu</b>)<br />
             <font size=\"2\">Yongxiang Zhao (<b>zyxleo@umich.edu</b>)<br />
             <font size=\"2\">Bole Li (<b>boleli@umich.edu</b>)<br />
             Ivo Dinov (<b>dinov@med.umich.edu</b>).</font><br />
             </div>
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



    # Reactive function to read uploaded file and update dataset
    dataset_reactive <- reactive({
      req(input$file)
      read.csv(input$file$datapath)
    })

    # Render the DataTable dynamically based on the reactive dataset
    output$tbl <- DT::renderDataTable({
      # Use isolate to prevent invalidation of the reactive expression on initial render
        DT::datatable(dataset_reactive(), options = list(lengthChange = FALSE))
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
