# SOCR Probability Distribution Calculator
# Version 0.9
# Updated March 20th 2022 by Shihang Li and Yongxiang Zhao at the University of Michigan -SOCR
# Orginally created by Jared(Tianyi) Chai

# This is a SOCR Interactive Graphical Probability Distribution Calculator
# You can run the application by clicking
# the 'Run App' button above.

# ----------------------- Ui.R ----------------------- #
# For frontend user interface
source("server.R")
library(xml2)
library(shiny)
library(plotly)
library(stringr)
library(HypergeoMat)
library("Rlab")
library("shinyWidgets")



shinyUI(
  fluidPage(
    withMathJax(),
    # ----------------------- Output: Title ----------------------- #
    titlePanel("SOCR Probability Distribution Calculator"),
    sidebarPanel(
      ## added
      selectInput("outcome",
        label = h3("Outcome (y)"),
        choices = unique(namedListOfFeatures()), selected = namedListOfFeatures()[6]
      ),
      selectInput("indepvar",
        label = h3("Explanatory variable (x)"),
        choices = unique(namedListOfFeatures()), selected = namedListOfFeatures()[4]
      ),

      # add end

      # ----------------------- Input: Specifying Distrtibution Type ----------------------- #
      selectInput("Distribution", "Please select distribution type",
        choices = distributions,
        selected = distributions[57]
      ),
      # ----------------------- Input: Specifying Function Type ----------------------- #
      selectInput("FunctionType", "Please select distribution function type",
        choices = c("", "PDF/PMF", "CDF/CMF")
      ),
      switchInput(inputId = "NeedFit", value = FALSE, onLabel = "Modeler", offLabel = "Calculator"),
      # ----------------------- Input: Parameter Inputs ----------------------- #
      # ----------------------- Input: ArcSine Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[2], "'", sep = ""),
        textInput("ArcSineA", paste("Please input A for ", distributions[2], " : ", sep = ""), 1),
        textInput("ArcSineB", paste("Please input B for ", distributions[2], " : ", sep = ""), 2)
      ),
      # ----------------------- Input: Benford Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[3], "'", sep = ""),
        textInput("Benfn", paste("Please input number of leading digits for ", distributions[3], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Bernoulli Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[4], "'", sep = ""),
        textInput("BernProb", paste("Please input Probability for ", distributions[4], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Beta Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[5], "'", sep = ""),
        textInput("BetaAlpha", paste("Please input Alpha for ", distributions[5], " : ", sep = ""), 0),
        textInput("BetaBeta", paste("Please input Beta for ", distributions[5], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Beta(Generalized) Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[6], "'", sep = ""),
        textInput("BetaGenA", paste("Please input shape parameter A for ", distributions[6], " : ", sep = ""), 3),
        textInput("BetaGenB", paste("Please input shape parameter B for ", distributions[6], " : ", sep = ""), 2),
        textInput("BetaGenC", paste("Please input shape parameter C for ", distributions[6], " : ", sep = ""), 1),
        textInput("BetaGenP", paste("Please input scale parameter P for ", distributions[6], " : ", sep = ""), 5)
      ),
      # ----------------------- Input: Beta-Binomial Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[7], "'", sep = ""),
        textInput("BetaBinomN", paste("Please input n for ", distributions[7], " : ", sep = ""), 2),
        textInput("BetaBinomU", paste("Please input alpha for ", distributions[7], " : ", sep = ""), 3),
        textInput("BetaBinomV", paste("Please input beta for ", distributions[7], " : ", sep = ""), 0.2)
      ),
      # ----------------------- Input: Binomial Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[8], "'", sep = ""),
        textInput("BinomP", paste("Please input Probability for ", distributions[8], " : ", sep = ""), 0.5),
        textInput("BinomN", paste("Please input n for ", distributions[8], " : ", sep = ""), 10)
      ),
      # ----------------------- Input: Bivariate Normal Distribution (3D) ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[10], "'", sep = ""),
        textInput("BivaM1", paste("Please input mean of the first random variable for ", distributions[10], " : ", sep = ""), 0),
        textInput("BivaV1", paste("Please input variance of the first random variable for ", distributions[10], " : ", sep = ""), 2),
        textInput("BivaM2", paste("Please input mean for the second random variable for ", distributions[10], " : ", sep = ""), 0),
        textInput("BivaV2", paste("Please input variance of the second random variable for ", distributions[10], " : ", sep = ""), 2),
        textInput("BivaCov", paste("Please input covariance of the two random variables for ", distributions[10], " : ", sep = ""), -1)
      ),
      # ----------------------- Input: Cauchy Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[11], "'", sep = ""),
        textInput("CauchyX0", paste("Please input x0 for ", distributions[11], " : ", sep = ""), 1),
        textInput("CauchyGamma", paste("Please input Gamma for ", distributions[11], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Chi Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[12], "'", sep = ""),
        textInput("ChiK", paste("Please input k for ", distributions[12], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Chi Square Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[13], "'", sep = ""),
        textInput("Chi2n", paste("Please input Probability for ", distributions[13], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Chi Square Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[14], "'", sep = ""),
        textInput("Chi2NCn", paste("Please input Probability for ", distributions[14], " : ", sep = ""), 1),
        textInput("Chi2NCNCP", paste("Please input non-centrality paramter for ", distributions[14], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Circle Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '",distributions[15],"'",sep = ""),
        textInput("CircleRadius",paste("Please input radius for ",distributions[15]," : ",sep = ""),2)
      ),
      # ----------------------- Input: Continuous Uniform Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[16], "'", sep = ""),
        textInput("UnifMin", paste("Please input Min. for ", distributions[16], " : ", sep = ""), 0),
        textInput("UnifMax", paste("Please input Max. for ", distributions[16], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Discrete ArcSine Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[19], "'", sep = ""),
        textInput("DisArcSineA", paste("Please input A for ", distributions[19], " : ", sep = ""), 1),
        textInput("DisArcSineB", paste("Please input B for ", distributions[19], " : ", sep = ""), 30)
      ),
      # ----------------------- Input: Discrete Uniform Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[20], "'", sep = ""),
        textInput("DisUnifMin", paste("Please input Min. for ", distributions[20], " : ", sep = ""), 0),
        textInput("DisUnifMax", paste("Please input Max. for ", distributions[20], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Erlang Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[21], "'", sep = ""),
        textInput("ErlangScale", paste("Please input scale for ", distributions[21], " : ", sep = ""), 1),
        textInput("ErlangShape", paste("Please input shape for ", distributions[21], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Error Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[22], "'", sep = ""),
        textInput("ErrorLocation", paste("Please input location for ", distributions[22], " : ", sep = ""), 0),
        textInput("ErrorScale", paste("Please input scale for ", distributions[22], " : ", sep = ""), 1),
        textInput("ErrorShape", paste("Please input shape for ", distributions[22], " : ", sep = ""), 2)
      ),
      # ----------------------- Input: Expopnential Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[23], "'", sep = ""),
        textInput("ExpLambda", paste("Please input lambda for ", distributions[23], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: F Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[25], "'", sep = ""),
        textInput("FdOne", paste("Please input d1 for ", distributions[25], " : ", sep = ""), 1),
        textInput("FdTwo", paste("Please input d2 for ", distributions[25], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Fisher-Tippett Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[26], "'", sep = ""),
        textInput("GEVMiu", paste("Please input miu for ", distributions[26], " : ", sep = ""), 1),
        textInput("GEVSigma", paste("Please input sigma for ", distributions[26], " : ", sep = ""), 1),
        textInput("GEVEpsilon", paste("Please input Epsilon for ", distributions[26], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Gamma Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[27], "'", sep = ""),
        textInput("GammaA", paste("Please input alpha for ", distributions[27], " : ", sep = ""), 1),
        textInput("GammaB", paste("Please input beta for ", distributions[27], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: General Cauchy Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[28], "'", sep = ""),
        textInput("GeneralCauchyAlpha", paste("Please input alpha for ", distributions[28], " : ", sep = ""), 1),
        textInput("GeneralCauchyBeta", paste("Please input beta for ", distributions[28], " : ", sep = ""), 1),
      ),
      # ----------------------- Input: Generalized Extreme Value (GEV) Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[29], "'", sep = ""),
        textInput("GEVMiu", paste("Please input miu for ", distributions[29], " : ", sep = ""), 1),
        textInput("GEVSigma", paste("Please input sigma for ", distributions[29], " : ", sep = ""), 1),
        textInput("GEVEpsilon", paste("Please input Epsilon for ", distributions[29], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Geometric Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[30], "'", sep = ""),
        textInput("GeomProb", paste("Please input Probability for ", distributions[30], " : ", sep = ""), 0.5)
      ),
      # ----------------------- Input: Gilbrats Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[31], "'", sep = ""),
        textInput("GilbratsMu", paste("Please input mu for ", distributions[31], " : ", sep = ""), 0),
        textInput("GilbratsSigma", paste("Please input sigma for ", distributions[31], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Gompertz Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[32], "'", sep = ""),
        textInput("Gompertz_N", paste("Please input shape eta for ", distributions[32], " : ", sep = ""), 2),
        textInput("Gompertz_B", paste("Please input scale Beta for ", distributions[32], " : ", sep = ""), 3),
      ),
      # ----------------------- Input: Gumbel Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[33], "'", sep = ""),
        textInput("Gumbel_U", paste("Please input location u for ", distributions[33], " : ", sep = ""), 2),
        textInput("Gumbel_Beta", paste("Please input scale Beta for ", distributions[33], " : ", sep = ""), 3),
      ),
      # ----------------------- Input: Half Normal Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[34], "'", sep = ""),
        textInput("HNorm", paste("Please input sigma for ", distributions[34], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Hyper Geometric Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[35], "'", sep = ""),
        textInput("HyperM", paste("Please input m for ", distributions[35], " : ", sep = ""), 50),
        textInput("HyperN", paste("Please input N for ", distributions[35], " : ", sep = ""), 500),
        textInput("HyperK", paste("Please input n for ", distributions[35], " : ", sep = ""), 100)
      ),
      # ----------------------- Input: Hyperbolic-Secant Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[36], "'", sep = ""),
        textInput("HSmu", paste("Please input mu for ", distributions[36], " : ", sep = ""), 0),
        textInput("HSsigma", paste("Please input sigma for ", distributions[36], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Inverse-Gamma Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[37], "'", sep = ""),
        textInput("InvGammaA", paste("Please input alpha for ", distributions[37], " : ", sep = ""), 1),
        textInput("InvGammaB", paste("Please input beta for ", distributions[37], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Inverse-Gaussian(Wald) Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[38], "'", sep = ""),
        textInput("InvGausM", paste("Please input mu for ", distributions[38], " : ", sep = ""), 1),
        textInput("InvGausL", paste("Please input lambda for ", distributions[38], " : ", sep = ""), 0.2)
      ),
      # ----------------------- Input: Johnson SB (Bounded) Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[39], "'", sep = ""),
        textInput("JohnSBgamma", paste("Please input gamma for ", distributions[39], " : ", sep = ""), -0.5),
        textInput("JohnSBdelta", paste("Please input delta for ", distributions[39], " : ", sep = ""), 2),
        textInput("JohnSBxi", paste("Please input xi for ", distributions[39], " : ", sep = ""), -0.5),
        textInput("JohnSBlambda", paste("Please input lambda for ", distributions[39], " : ", sep = ""), 2)
      ),
      # ----------------------- Input: Johnson SU (Unbounded) Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[40], "'", sep = ""),
        textInput("JohnSUgamma", paste("Please input gamma for ", distributions[40], " : ", sep = ""), -0.5),
        textInput("JohnSUdelta", paste("Please input delta for ", distributions[40], " : ", sep = ""), 2),
        textInput("JohnSUxi", paste("Please input xi for ", distributions[40], " : ", sep = ""), -0.5),
        textInput("JohnSUlambda", paste("Please input lambda for ", distributions[40], " : ", sep = ""), 2)
      ),
      # ----------------------- Input: Laplace Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[42], "'", sep = ""),
        textInput("LapMu", paste("Please input mu for ", distributions[42], " : ", sep = ""), 0),
        textInput("LapSig", paste("Please input sigma for ", distributions[42], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Logarithmic-series Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[43], "'", sep = ""),
        textInput("LogP", paste("Please input P for ", distributions[43], " : ", sep = ""), 0.5),
      ),
      # ----------------------- Input: Logistic Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[44], "'", sep = ""),
        textInput("LogiA", paste("Please input a for ", distributions[44], " : ", sep = ""), 0),
        textInput("LogiB", paste("Please input b for ", distributions[44], " : ", sep = ""), 1)
      ),
      #-------------------------- Input: Logistic Exponential Distribution ----------------------#
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[45], "'", sep = ""),
        textInput("LogEx_A", paste("Please input a for ", distributions[45], " : ", sep = ""), 5),
        textInput("LogEx_B", paste("Please input b for ", distributions[45], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: LogNormal Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[46], "'", sep = ""),
        textInput("LogNormMean", paste("Please input mean for ", distributions[46], " : ", sep = ""), 0),
        textInput("LogNormSD", paste("Please input standard deviation for ", distributions[46], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Lomax Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[47], "'", sep = ""),
        textInput("LomaxLamda", paste("Please input Lambda for ", distributions[47], " : ", sep = ""), 1),
        textInput("LomaxKappa", paste("Please input Kappa for ", distributions[47], " : ", sep = ""), 2)
      ),
      # ----------------------- Input: Matching Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[48], "'", sep = ""),
        textInput("MatchParam", paste("Please input parameter for ", distributions[48], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Maxwell Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[49], "'", sep = ""),
        textInput("MaxwellA", paste("Please input a for ", distributions[49], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Minimax Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[50], "'", sep = ""),
        textInput("Mini_B", paste("Please input Beta for ", distributions[50], " : ", sep = ""), 1),
        textInput("Mini_V", paste("Please input Gama for ", distributions[50], " : ", sep = ""), 2)
      ),
      # ----------------------- Input: Muth Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[53], "'", sep = ""),
        textInput("MuthKappa", paste("Please input parameter Kappa for ", distributions[53], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Negative Binomial Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[54], "'", sep = ""),
        textInput("NegBiR", paste("Please input r for ", distributions[54], " : ", sep = ""), 1),
        textInput("NegBiP", paste("Please input p for ", distributions[54], " : ", sep = ""), 0.5)
      ),
      # ----------------------- Input: Negative HyperGeometric Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[55], "'", sep = ""),
        textInput("NegHyperK", paste("Please input W for ", distributions[55], " : ", sep = ""), 45),
        textInput("NegHyperN", paste("Please input B for ", distributions[55], " : ", sep = ""), 30),
        textInput("NegHyperR", paste("Please input b for ", distributions[55], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Normal Distribution ----------------------- #
      conditionalPanel(
        condition = paste("!input.NeedFit && input.Distribution == '", distributions[57], "'", sep = ""),
        textInput("NormMean", paste("Please input mean for ", distributions[57], " : ", sep = ""), 0),
        textInput("NormSD", paste("Please input standard deviation for ", distributions[57], " : ", sep = ""), 1)
      ),
      conditionalPanel(
        condition = paste("input.NeedFit && input.Distribution == '", distributions[57], "'", sep = ""),
        actionButton("fitNormalParams", "Fit Parameters from Data"),
        textOutput("fitStatus")
      ),
      # ----------------------- Input: Normal Truncated Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[58], "'", sep = ""),
        textInput("TruncNormMean", paste("Please input mean for ", distributions[58], " : ", sep = ""), 0),
        textInput("TruncNormSD", paste("Please input standard deviation for ", distributions[58], " : ", sep = ""), 3),
        textInput("TruncNormMin", paste("Please input Min for ", distributions[58], " : ", sep = ""), -5),
        textInput("TruncNormMax", paste("Please input Max for ", distributions[58], " : ", sep = ""), 5),
      ),
      # ----------------------- Input: Pareto Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[59], "'", sep = ""),
        textInput("ParetoA", paste("Please input a for ", distributions[59], " : ", sep = ""), 1),
        textInput("ParetoB", paste("Please input b for ", distributions[59], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Point Mass Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[60], "'", sep = ""),
        textInput("PMD_Location", paste("Please input location for ", distributions[60], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Poisson Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[61], "'", sep = ""),
        textInput("PoiLambda", paste("Please input lambda for ", distributions[61], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Poker Dice Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[62], "'", sep = "")
      ),
      # ----------------------- Input: Power Function Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[63], "'", sep = ""),
        textInput("PowerAlpha", paste("Please input alpha for ", distributions[63], " : ", sep = ""), 3),
        textInput("PowerBeta", paste("Please input beta for ", distributions[63], " : ", sep = ""), 4)
      ),
      # ----------------------- Input: Rayleigh Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[64], "'", sep = ""),
        textInput("RayleighSigma", paste("Please input sigma for ", distributions[64], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Rice Distribution ------------------------ #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[65], "'", sep = ""),
        textInput("RiceVee", paste("Please input VEE for ", distributions[65], " : ", sep = ""), 1),
        textInput("RiceSigma", paste("Please input Sigma for ", distributions[65], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: T Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == \"", distributions[66], "\"", sep = ""),
        textInput("Tdof", paste("Please input degree of freedom for ", distributions[66], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: T Non Central Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == \"", distributions[67], "\"", sep = ""),
        textInput("TNCdof", paste("Please input degree of freedom for ", distributions[67], " : ", sep = ""), 1),
        textInput("TNCNCP", paste("Please input non-centrality parameter for ", distributions[67], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Triangular Distribution ----------------------- #s
      conditionalPanel(
        condition = paste("input.Distribution == \"", distributions[68], "\"", sep = ""),
        textInput("Triangular_A", paste("Please input parameter a for ", distributions[68], " : ", sep = ""), 1),
        textInput("Triangular_B", paste("Please input parameter b for ", distributions[68], " : ", sep = ""), 10),
        textInput("Triangular_C", paste("Please input parameter c for ", distributions[68], " : ", sep = ""), 4),
      ),
      # ----------------------- Input: Two-Sided Power Distribution ----------------------- #s
      conditionalPanel(
        condition = paste("input.Distribution == \"", distributions[69], "\"", sep = ""),
        textInput("TSPowerLeft", paste("Please input left for ", distributions[69], " : ", sep = ""), -5),
        textInput("TSPowerRight", paste("Please input right for ", distributions[69], " : ", sep = ""), 5),
        textInput("TSPowerMed", paste("Please input med for ", distributions[69], " : ", sep = ""), 0),
        textInput("TSPowerPower", paste("Please input power for ", distributions[69], " : ", sep = ""), 3)
      ),
      # ----------------------- Input: U-quadratic distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == \"", distributions[70], "\"", sep = ""),
        textInput("UQ_C", paste("Please input parameter c for ", distributions[70], " : ", sep = ""), 1),
        textInput("UQ_W", paste("Please input parameter w for ", distributions[70], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Von Mises Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == \"", distributions[71], "\"", sep = ""),
        textInput("vonMisesMu", paste("Please input parameter mu for ", distributions[71], " : ", sep = ""), 0),
        textInput("vonMisesKappa", paste("Please input parameter kappa for ", distributions[71], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Weibull Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[74], "'", sep = ""),
        textInput("WeibullLambda", paste("Please input lambda for ", distributions[74], " : ", sep = ""), 1),
        textInput("WeibullK", paste("Please input k for ", distributions[74], " : ", sep = ""), 1)
      ),
      # ----------------------- Input: Zipf-Mandelbrot Distribution ----------------------- #
      conditionalPanel(
        condition = paste("input.Distribution == '", distributions[75], "'", sep = ""),
        textInput("Zipf_q", paste("Please input shape parameter q for ", distributions[75], " : ", sep = ""), 1),
        textInput("Zipf_N", paste("Please input shape parameter N for ", distributions[75], " : ", sep = ""), 1),
        textInput("Zipf_s", paste("Please input shape parameter s for ", distributions[75], " : ", sep = ""), 1)
      ),

      # ----------------------- Input: Helpme ----------------------- #
      actionButton("vh.readme", "ReadMe/Help"),
      # ----------------------- Output: Metadata Output ----------------------- #
      uiOutput("MetaData"), tags$style(type = "text/css", "#MetaData {white-space: pre-wrap;}")
    ),
    mainPanel(
      h3(textOutput("caption")),
      tabsetPanel(
        type = "tabs",
        tabPanel("Data", DT::dataTableOutput("tbl")), # Data as datatable
        tabPanel(
          "Scatterplot", plotlyOutput("scatterplot"),
          br(), hr(),
          withMathJax(
            paste0("Least Squares Linear Model Estimates"),
            br(),
            paste0(
              "Slope: \\(\\hat{\\beta}_1 = \\dfrac{\\big(\\sum^n_{i=1} x_i y_i \\big) - n \\bar{x}
                                    \\bar{y}}{\\sum^n_{i=1} (x_i- \\bar{x})^2} \\) =",
              "\\(\\dfrac{ n \\big(\\sum^n_{i=1} x_i y_i \\big) -
                                    \\big(\\sum^n_{i=1} x_i \\big) \\big(\\sum^n_{i=1} y_i \\big) }
                                    {n \\sum^n_{i=1} x_i^2 - \\big(\\sum^n_{i=1} x_i \\big)^2} \\)"
            ),
            br(),
            paste0("Intercept: \\(\\hat{\\beta}_0 = \\bar{y} - \\hat{\\beta}_1 \\bar{x} \\) "),
            br(),
            paste0("Prediction: \\( \\hat{y} = \\hat{\\beta}_0 + \\hat{\\beta}_1 x \\) ")
          )
        ), # Scatter Plot,
        tabPanel("Model Summary", verbatimTextOutput("summary")) # Regression output
      ),

      # ----------------------- Input: Switch between Slider and Manual Inputs for Ranges and SD function ----------------------- #
      fluidRow(
        column(2, switchInput(inputId = "numericalValues", value = FALSE, onLabel = "Manual", offLabel = "Slider")),
        column(10, div(align = "left", conditionalPanel(
          condition = "input.numericalValues == 0 && (input.Distribution == 'Normal Distribution'
                                                              || input.Distribution == 'Poisson Distribution'
                                                              || input.Distribution == 'Rayleigh Distribution'
                                                              || input.Distribution == 'Weibull Distribution'
                                                              || input.Distribution == 'Maxwell Distribution'
                                                              || input.Distribution == 'Log-Normal Distribution'
                                                              || input.Distribution == 'Logistic Distribution'
                                                              || input.Distribution == 'Logarithmic-Series Distribution'
                                                              || input.Distribution == 'Laplace Distribution'
                                                              || input.Distribution == 'Inverse Gaussian (Wald) Distribution'
                                                              || input.Distribution == 'Inverse-Gamma Distribution'
                                                              || input.Distribution == 'Hyper Geometric Distribution'
                                                              || input.Distribution == 'Half-Normal Distribution'
                                                              || input.Distribution == 'Gumbel Distribution'
                                                              || input.Distribution == 'Gamma Distribution'
                                                              || input.Distribution == 'Geometric Distribution'
                                                              || input.Distribution == '(Non-Central) Chi-Squre Distribution'
                                                              || input.Distribution == 'Chi-Square Distribution'
                                                              || input.Distribution == 'Chi Distribution'
                                                              || input.Distribution == 'Fisher F Distribution')",
          textInput("SDNum", paste("Standard deviations from mean (0 to adjust freely, many are still implementing : )", sep = ""), 0)
        )))
      ),

      # ----------------------- Input: Slider Input for x-Range ----------------------- #
      conditionalPanel(
        condition = "input.numericalValues == 0",
        div(align = "right", sliderInput("plotrange", "X Range:",
          min = -1000, max = 1000,
          value = c(-10, 10),
          step = 0.01,
          width = "96%"
        ))
      ),
      # ----------------------- Input: Numerical Inputs for x-Range ----------------------- #
      conditionalPanel(
        condition = "input.numericalValues == 1",
        fluidRow(
          column(3, numericInput(inputId = "plotrangeNumMin", label = "X-range Min:", value = -10)),
          column(3, numericInput(inputId = "plotrangeNumMax", label = "X-range Max:", value = 10))
        )
      ),
      # ----------------------- Output: Main Plot Output ----------------------- #
      div(plotlyOutput("myPlot", height = "400"), aligh = "left"),
      # ----------------------- Output: Implementing Text ----------------------- #
      textOutput("Implementing"),
      # ----------------------- Input: Slider Input for Lower and Upper Bounds for Probability Calculation ----------------------- #
      conditionalPanel(
        condition = "input.numericalValues == 0",
        div(align = "right", sliderInput("probrange", "Probability Range:",
          min = -10, max = 10,
          value = c(-1, 1),
          step = 0.01,
          width = "96%"
        ))
      ),
      # ----------------------- Input: Numerical Input for Lower and Upper Bounds for Probability Calculation ----------------------- #
      conditionalPanel(
        condition = "input.numericalValues == 1",
        fluidRow(
          column(3, numericInput(inputId = "probrangeNumMin", label = "Prob. Lower Bound:", value = -1)),
          column(3, numericInput(inputId = "probrangeNumMax", label = "Prob. Upper Bound:", value = 1))
        )
      ),
      # ----------------------- Ouput: Calculated Probability ----------------------- #
      textOutput("probability"),
      # ----------------------- Output: SOCR Footer ----------------------- #
      tags$footer(
        div(shinyUI(bootstrapPage(div(
          # include The SOCR footer HTML
          includeHTML("SOCR_footer_tracker.html")
        )))),
        div(
          "Version: V.0.8",
          align = "center"
        ),
        # HTML("<img class='statcounter' src='https://c.statcounter.com/5714596/0/038e9ac4/0/' alt='Web Analytics' border='0' align='center'>")
      )
    )
  )
)
