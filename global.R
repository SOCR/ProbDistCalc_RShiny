#SOCR Probability Distribution Calculator
#Version 0.9
#Updated March 20th 2022 by Shihang Li and Yongxiang Zhao at the University of Michigan -SOCR
#Orginally created by Jared(Tianyi) Chai


# This is a SOCR Interactive Graphical Probability Distribution Calculator
# You can run the application by clicking
# the 'Run App' button above.

# ----------------------- Global.R ----------------------- #
#For Retrieving and Storing Global Variables
library(xml2)
library(shiny)
library(plotly)
library(stringr)
library("Rlab")
library("shinyWidgets")

# ----------------------- Parse and Store XML Databse of Metadata from Distributome Project ----------------------- #
XML_data <- read_xml("https://socr.umich.edu/docs/uploads/2020/Distributome.xml",encoding = "UTF-8",as_html=FALSE,options = c("RECOVER", "NOERROR", "NOBLANKS"))
XML_rootnode <- xml_root(XML_data)
XML_distributions <- xml_children(xml_child(XML_rootnode))
XML_len <- length(XML_distributions)
XML_wid = 0
XML_tags <- c()
for (i in 1:XML_len){
  XML_i <-xml_contents(XML_distributions[[i]])
  width <- length(XML_i)
  if(width > XML_wid){
    XML_wid = width
    XML_tags = xml_name(XML_i)
  }
}
distributions_meta <- matrix("",XML_wid,XML_len*2)
for (i in 1:XML_len){
  XML_i <-xml_contents(XML_distributions[[i]])
  width <- length(XML_i)
  for (j in 1:width){
    distributions_meta[[j,i*2-1]]<-xml_name(XML_i[j])
    distributions_meta[[j,i*2]]<-xml_text(XML_i[j])
  }
}
# ----------------------- Distribubtions to be Implemented ----------------------- #
DistImplement <- c("Anderson-Darling Distribution",
                   "Birthday Distribution",
                   "(3D) Bivariate Normal Distribution",
                   "Circle Distribution",
                   "Coupon Distribution",
                   "Die Distribution",
                   "Erlang Distribution",
                   "Error Distribution",
                   "Finite Distribution",
                   "Fisher-Tippett Distribution",
                   "General Cauchy Distribution",
                   "Gilbrats Distribution",
                   "Hyperbolic-Secant Distribution",
                   "Johnson SB (Bounded) Distribution",
                   "Johnson SU (Unbounded) Distribution",
                   "Kolmogorov Distribution",
                   "Matching Distribution",
                   "Mixture Distribution",
                   "Multinomial Distribution",
                   "Negative-Multinomial Distribution",
                   "Poker-Dice Distribution",
                   "Power-Function Distribution",
                   "Two-Sided Power Distribution",
                   "Von Mises Distribution",
                   "WalkMax Distribution",
                   "WalkPosition Distribution")
#------------------------ List of Distributions with SD function -----------------------------#
SDS <- c(
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
distributions <- c("Anderson-Darling Distribution",
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
                   "Zipf-Manelbrot Distribution")
