# Libraries --------------------------------------------------------------

library(bslib)
library(shinyWidgets)
library(shinyFeedback)
library(dplyr)
library(shinyjs)
library(ggiraph)
library(ggplot2)
library(duckdb)


# Read in Data -----------------------------------------------------------

sim_df <- readRDS("imports/portfolio_sims.rds")

file.sources <- list.files(path = "modules/", full.names = TRUE)
sapply(file.sources, source)
