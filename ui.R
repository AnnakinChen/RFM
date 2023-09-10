library(shiny)
library(shinydashboard)
library(shinyauthr)
library(dplyr)
library(shinythemes)
library(readxl)
library(ggplot2)
library(DT)

options(shiny.maxRequestSize = 50*1024^2)
user_base <- dplyr::tibble(
  user = c("Skywalker"),
  password = c("May the force be with you"),
  permissions = c("admin"),
  name = c("User One")
)

ui = fluidPage(
  theme = shinytheme('united'),
  # themeSelector(),
  # add logout button UI
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
  # add login panel UI function
  shinyauthr::loginUI(id = "login"),
  title = 'RFM模型客户分类',
  br(),
  br(),
  fluidRow(
    column(
      #offset = 1,
      width = 2,
      uiOutput('u1'),
      uiOutput('u2'),
      uiOutput('u3'),
      uiOutput('u4')
    ),
    column(
      width = 2,
      uiOutput('u5'),
      uiOutput('u6'),
      uiOutput('u7'),
      uiOutput('u8')
    ),
    column(
      width = 8,
      dataTableOutput('table1')
    )
  ),
  br(),
  br(),
  fluidRow(
    column(
      width = 4,
      dataTableOutput('table2')
    ),
    column(
      width = 5,
      plotOutput('plot1')
    ),
    column(
      width = 3,
      plotOutput('plot2')
    )
  ),
  br(),
  br(),
  fluidRow(
    column(
      width = 2,
      uiOutput('u9'),
      uiOutput('u10')
    ),
    column(
      width = 7,
      dataTableOutput('table3')
    ),
    column(
      width = 3,
      verbatimTextOutput('p1')
    )
  )
)









