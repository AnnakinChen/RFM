library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinythemes)
library(readxl)
library(ggplot2)
library(DT)



user_base <- dplyr::tibble(
  user = c("Skywalker"),
  password = c("May the force be with you"),
  permissions = c("admin"),
  name = c("User One")
)
options(shiny.maxRequestSize = 50*1024^2)
source('ui.R')
source('server.R')

shinyApp(ui = ui, server = server)






















