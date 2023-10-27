# SOCR Probability Distribution Calculator
# Version 0.9
# Updated March 20th 2022 by Shihang Li and Yongxiang Zhao at the University of Michigan -SOCR
# Orginally created by Jared(Tianyi) Chai


# This is a SOCR Interactive Graphical Probability Distribution Calculator
# You can run the application by clicking
# the 'Run App' button above.

# ----------------------- Global.R ----------------------- #
# For Retrieving and Storing Global Variables
library(xml2)
library(shiny)
library(plotly)
library(stringr)
library("Rlab")
library("shinyWidgets")

# ----------------------- Parse and Store xml Databse of Metadata from Distributome Project ----------------------- #
xml_data <- read_xml(
  "https://socr.umich.edu/docs/uploads/2020/Distributome.xml",
  encoding = "UTF-8", as_html = FALSE,
  options = c("RECOVER", "NOERROR", "NOBLANKS")
)
xml_rootnode <- xml_root(xml_data)
xml_distributions <- xml_children(xml_child(xml_rootnode))
xml_len <- length(xml_distributions)
xml_wid <- 0
xml_tags <- c()
for (i in 1:xml_len) {
  xml_i <- xml_contents(xml_distributions[[i]])
  width <- length(xml_i)
  if (width > xml_wid) {
    xml_wid <- width
    xml_tags <- xml_name(xml_i)
  }
}
distributions_meta <- matrix("", xml_wid, xml_len * 2)
for (i in 1:xml_len) {
  xml_i <- xml_contents(xml_distributions[[i]])
  width <- length(xml_i)
  for (j in 1:width) {
    distributions_meta[[j, i * 2 - 1]] <- xml_name(xml_i[j])
    distributions_meta[[j, i * 2]] <- xml_text(xml_i[j])
  }
}
# ----------------------- Distribubtions to be Implemented ----------------------- #
distToImpl <- c(
  "Anderson-Darling Distribution",
  "Birthday Distribution",
  "Coupon Distribution",
  "Die Distribution",
  "Erlang Distribution",
  "Error Distribution",
  "Finite Distribution",
  "General Cauchy Distribution",
  "Gilbrats Distribution",
  "Kolmogorov Distribution",
  "Matching Distribution",
  "Mixture Distribution",
  "Multinomial Distribution",
  "Negative-Multinomial Distribution",
  "Poker-Dice Distribution",
  "Two-Sided Power Distribution",
  "WalkMax Distribution",
  "WalkPosition Distribution"
)
#------------------------ List of Distributions with SD function -----------------------------#
distWithSD <- c(
  "Chi Distribution",
  "Chi-Square Distribution",
  "(Non-Central) Chi-Squre Distribution",
  "Fisher F Distribution",
  "Gamma Distribution",
  "Geometric Distribution",
  "Gumbel Distribution",
  "Half-Normal Distribution",
  "Hyper Geometric Distribution",
  "Inverse-Gamma Distribution",
  "Inverse Gaussian (Wald) Distribution",
  "Laplace Distribution",
  "Logarithmic-Series Distribution",
  "Logistic Distribution",
  "Log-Normal Distribution",
  "Maxwell Distribution",
  "Normal Distribution",
  "Poisson Distribution",
  "Rayleigh Distribution",
  "Weibull Distribution"
)
# ----------------------- Complete List of All Probability Distributions ----------------------- #
distributions <- c(
  "Anderson-Darling Distribution",
  "ArcSine Distribution",
  "Benford Distribution",
  "Bernoulli Distribution",
  "Beta Distribution",
  "Beta (Generalized) Distribution",
  "Beta-Binomial Distribution",
  "Binomial Distribution",
  "Birthday Distribution",
  "(3D) Bivariate Normal Distribution",
  "Cauchy Distribution",
  "Chi Distribution",
  "Chi-Square Distribution",
  "(Non-Central) Chi-Squre Distribution",
  "Circle Distribution",
  "Continuous Uniform Distribution",
  "Coupon Distribution",
  "Die Distribution",
  "Discrete ArcSine Distribution",
  "Discrete Uniform Distribution",
  "Erlang Distribution",
  "Error Distribution",
  "Exponential Distribution",
  "Finite Distribution",
  "Fisher F Distribution",
  "Fisher-Tippett Distribution",
  "Gamma Distribution",
  "General Cauchy Distribution",
  "Generalized Extreme Value (GEV) Distribution",
  "Geometric Distribution",
  "Gilbrats Distribution",
  "Gompertz Distribution",
  "Gumbel Distribution",
  "Half-Normal Distribution",
  "Hyper Geometric Distribution",
  "Hyperbolic-Secant Distribution",
  "Inverse-Gamma Distribution",
  "Inverse Gaussian (Wald) Distribution",
  "Johnson SB (Bounded) Distribution",
  "Johnson SU (Unbounded) Distribution",
  "Kolmogorov Distribution",
  "Laplace Distribution",
  "Logarithmic-Series Distribution",
  "Logistic Distribution",
  "Logistic-Exponential Distribution",
  "Log-Normal Distribution",
  "Lomax Distribution",
  "Matching Distribution",
  "Maxwell Distribution",
  "Minimax Distribution",
  "Mixture Distribution",
  "Multinomial Distribution",
  "Muth Distribution",
  "Negative-Binomial Distribution",
  "Negative-HyperGeometric Distribution",
  "Negative-Multinomial Distribution",
  "Normal Distribution",
  "Normal Truncated Distribution",
  "Pareto Distribution",
  "Point-Mass Distribution",
  "Poisson Distribution",
  "Poker-Dice Distribution",
  "Power-Function Distribution",
  "Rayleigh Distribution",
  "Rice (Rician) Distribution",
  "Student's T Distribution",
  "Student's T Non-Central Distribution",
  "Triangle Distribution",
  "Two-Sided Power Distribution",
  "U-Quadratic Distribution",
  "Von Mises Distribution",
  "WalkMax Distribution",
  "WalkPosition Distribution",
  "Weibull Distribution",
  "Zipf-Manelbrot Distribution"
)

# index of each distribution
AndersonDarling <- 1
ArcSine <- 2
Benford <- 3
Bernoulli <- 4
Beta <- 5
BetaGeneralized <- 6
BetaBinomial <- 7
Binomial <- 8
Birthday <- 9
BivariateNormal3D <- 10
Cauchy <- 11
Chi <- 12
ChiSquare <- 13
ChiSqureNonCentral <- 14
Circle <- 15
ContinuousUniform <- 16
Coupon <- 17
Die <- 18
DiscreteArcSine <- 19
DiscreteUniform <- 20
Erlang <- 21
Error <- 22
Exponential <- 23
Finite <- 24
FisherF <- 25
FisherTippett <- 26
Gamma <- 27
GeneralCauchy <- 28
GeneralizedExtremeValueGEV <- 29
Geometric <- 30
Gilbrats <- 31
Gompertz <- 32
Gumbel <- 33
HalfNormal <- 34
HyperGeometric <- 35
HyperbolicSecant <- 36
InverseGamma <- 37
InverseGaussianWald <- 38
JohnsonSBBounded <- 39
JohnsonSUUnbounded <- 40
Kolmogorov <- 41
Laplace <- 42
LogarithmicSeries <- 43
Logistic <- 44
LogisticExponential <- 45
LogNormal <- 46
Lomax <- 47
Matching <- 48
Maxwell <- 49
Minimax <- 50
Mixture <- 51
Multinomial <- 52
Muth <- 53
NegativeBinomial <- 54
NegativeHyperGeometric <- 55
NegativeMultinomial <- 56
Normal <- 57
NormalTruncated <- 58
Pareto <- 59
PointMass <- 60
Poisson <- 61
PokerDice <- 62
PowerFunction <- 63
Rayleigh <- 64
RiceRician <- 65
StudentsT <- 66
StudentsTNonCentral <- 67
Triangle <- 68
TwoSidedPower <- 69
UQuadratic <- 70
VonMises <- 71
WalkMax <- 72
WalkPosition <- 73
Weibull <- 74
ZipfManelbrot <- 75
