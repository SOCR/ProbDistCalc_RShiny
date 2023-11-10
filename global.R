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
library(fitdistrplus)
library("Rlab")
library("shinyWidgets")

plotlyFunctions <- list.files("plotlyFunctions", full.names = TRUE)
for (file in plotlyFunctions) {
  source(file)
}
source("fitFunctions.R")
source("dataset.R")
source("distributionInfo.R")

# ----------------------- Discrete: Anderson Darling Distribution ----------------------- #    
n_AD <- 0.0

# ----------------------- Discrete: Birthday Distribution ----------------------- #
popSize_Birt <- 0
sampleSize_Birt <- 0
prob_birt <- array(0, c(0, 0))

# ----------------------- Parse and Store xml Databse of Metadata from Distributome Project ----------------------- #
xml_data <- read_xml("Distributome.xml", encoding = "UTF-8")
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

distributionInfoList <- loadDistributionInfo("distribution_info.yaml")

distToImpl <- names(Filter(function(x) !x$hasImplementation, distributionInfoList))
distWithSD <- names(Filter(function(x) x$isWithSD, distributionInfoList))
distributions <- names(distributionInfoList)
nameToFullName <- function(name) {
  for (distributionInfo in distributionInfoList) {
    if (distributionInfo$name == name) {
      return(distributions[distributionInfo$id])
    }
  }
}
