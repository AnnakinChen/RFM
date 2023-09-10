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
server = function(input,output,session){
  options(shiny.maxRequestSize = 50*1024^2) 
  # Reactive value to track if the user is logged in
  user_authenticated <- shiny::reactiveVal(FALSE)
  # call login module supplying data frame,
  # user and password cols and reactive trigger
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_base,
    user_col = user,
    pwd_col = password,
    log_out = reactive(logout_init())
  )
  # call the logout module with reactive trigger to hide/show
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  # Observe when the user is authenticated
  observe({
    if (credentials()$user_auth) {
      user_authenticated(TRUE)
    } else {
      user_authenticated(FALSE)
    }
  })
  observe({
    req(input$f1)
    updateSelectInput(session,'c1',choices = names(data()))
    updateSelectInput(session,'c2',choices = names(data()))
    updateSelectInput(session,'c3',choices = names(data()))
  })
  output$u1 <- renderUI({
    if (user_authenticated()) {
      fileInput('f1','Upload a file',accept = '.xlsx')
    } else {
      NULL
    }
  })
  data = reactive({
    req(input$f1)
    read_excel(input$f1$datapath)
  })
  output$u2 <- renderUI({
    if (user_authenticated()) {
      selectInput('c1','用户id',choices = NULL)
    } else {
      NULL
    }
  })
  output$u3 <- renderUI({
    if (user_authenticated()) {
      selectInput('c2','下单时间',choices = NULL)
    } else {
      NULL
    }
  })
  output$u4 <- renderUI({
    if (user_authenticated()) {
      selectInput('c3','订单金额',choices = NULL)
    } else {
      NULL
    }
  })
  output$u5 <- renderUI({
    if (user_authenticated()) {
      textInput('t1','R得分分位点',value = '0,0.2,0.4,0.6,0.8,1')
    } else {
      NULL
    }
  })
  output$u6 <- renderUI({
    if (user_authenticated()) {
      textInput('t2','F得分分位点',value = '0,0.2,0.4,0.6,0.8,1')
    } else {
      NULL
    }
  })
  output$u7 <- renderUI({
    if (user_authenticated()) {
      textInput('t3','M得分分位点',value = '0,0.2,0.4,0.6,0.8,1')
    } else {
      NULL
    }
  })
  output$u8 <- renderUI({
    if (user_authenticated()) {
      actionButton('a1','submit')
    } else {
      NULL
    }
  })
  observeEvent(
    input$a1,
    {
      data = as.data.frame(data())
      data[[input$c2]] = as.Date(data[[input$c2]])
      window_date = as.Date(max(data[[input$c2]]))+1
      diff_days = function(start,end){
        return(as.numeric(difftime(start,end,units = 'days')))
      }
      data$date_diff = sapply(data[[input$c2]],FUN = diff_days,start = window_date)
      df = data %>%
        group_by(.data[[input$c1]]) %>%
        summarise(R = min(.data[['date_diff']]),F = n(),M = sum(.data[[input$c3]]))
      q1 = as.numeric(unlist(strsplit(input$t1,',')))
      q2 = as.numeric(unlist(strsplit(input$t2,',')))
      q3 = as.numeric(unlist(strsplit(input$t3,',')))
      df$R_Score = as.numeric(cut(-df$R,breaks = quantile(-df$R,probs = q1),include.lowest = T,labels = seq(1:(length(q1)-1))))
      df$F_Score = as.numeric(cut(df$F,breaks = quantile(df$F,probs = q2),include.lowest = T,labels = seq(1:(length(q2)-1))))
      df$M_Score = as.numeric(cut(df$M,breaks = quantile(df$M,probs = q3),include.lowest = T,labels = seq(1:(length(q3)-1))))
      df$R_Score1 = ifelse(df$R_Score>=mean(df$R_Score),'1','0')
      df$F_Score1 = ifelse(df$F_Score>=mean(df$F_Score),'1','0')
      df$M_Score1 = ifelse(df$M_Score>=mean(df$M_Score),'1','0')
      df$type = apply(df[c("R_Score1","F_Score1","M_Score1")],1,paste,collapse = '')
      output$table1=renderDT(
        datatable(
          df,
          extensions = 'Buttons',
          options = list(
            columnDefs = list(list(className='dt-center',targets=1:length(colnames(df)))),
            scrollX = T,
            dom='lfrtiBp',
            lengthMenu=c(10,50,100),
            buttons = list(
              list(
                extend='csv',
                text='csv',
                className='dt-button',
                titleAttr='Export as CSV',
                filename='result'
              )
            )
          )
        )
      )
      df1 = df %>%
        group_by(type) %>%
        summarise(count = n())
      df1$precent = round(df1$count/sum(df1$count),3)
      output$table2=renderDT(
        datatable(
          df1,
          extensions = 'Buttons',
          options = list(
            columnDefs = list(list(className='dt-center',targets=1:length(colnames(df1)))),
            scrollX = T,
            dom='lfrtiB',
            lengthMenu=c(10),
            buttons = list(
              list(
                extend='excel',
                text='excel',
                className='dt-button',
                titleAttr='Export as excel',
                filename='result'
              )
            )
          )
        )
      )
      output$plot1 = renderPlot({
        ggplot(df1,aes(x = reorder(type,count,decreasing = T),y = count,color = type,fill = type,alpha = 0.2))+
          geom_bar(stat = 'identity')+
          geom_text(aes(label = count,family = "serif",vjust = -0.5),size = 4.3)+
          labs(x = 'type')
      })
      output$plot2 = renderPlot({
        ggplot(df1,aes(x = '',y = count,fill = type))+
          geom_bar(stat = 'identity',position = position_stack(reverse = T))+
          geom_text(aes(y = cumsum(count)-0.5*count,label = paste(precent*100,"%")))+
          coord_polar(theta = "y",start = 0)+
          theme_void()
      })
    }
  )
  
  output$u9 <- renderUI({
    if (user_authenticated()) {
      textInput('t4','聚类簇数',value = 8)
    } else {
      NULL
    }
  })
  output$u10 <- renderUI({
    if (user_authenticated()) {
      actionButton('a2','submit')
    } else {
      NULL
    }
  })
  
  observeEvent(
    input$a2,
    {
      k = as.numeric(input$t4)
      data = as.data.frame(data())
      data[[input$c2]] = as.Date(data[[input$c2]])
      window_date = as.Date(max(data[[input$c2]]))+1
      diff_days = function(start,end){
        return(as.numeric(difftime(start,end,units = 'days')))
      }
      data$date_diff = sapply(data[[input$c2]],FUN = diff_days,start = window_date)
      df = data %>%
        group_by(.data[[input$c1]]) %>%
        summarise(R = min(.data[['date_diff']]),F = n(),M = sum(.data[[input$c3]]))
      q1 = as.numeric(unlist(strsplit(input$t1,',')))
      q2 = as.numeric(unlist(strsplit(input$t2,',')))
      q3 = as.numeric(unlist(strsplit(input$t3,',')))
      df$R_Score = as.numeric(cut(-df$R,breaks = quantile(-df$R,probs = q1),include.lowest = T,labels = seq(1:(length(q1)-1))))
      df$F_Score = as.numeric(cut(df$F,breaks = quantile(df$F,probs = q2),include.lowest = T,labels = seq(1:(length(q2)-1))))
      df$M_Score = as.numeric(cut(df$M,breaks = quantile(df$M,probs = q3),include.lowest = T,labels = seq(1:(length(q3)-1))))
      res = kmeans(df[c("R_Score","F_Score","M_Score")],k)
      df$cluster = res$cluster
      output$table3 = renderDT(
        datatable(
          df,
          extensions = 'Buttons',
          options = list(
            columnDefs = list(list(className='dt-center',targets=1:length(colnames(df)))),
            scrollX = T,
            dom='lBfrtip',
            lengthMenu=c(10,20,50),
            buttons = list(
              list(
                extend='csv',
                text='csv',
                className='dt-button',
                titleAttr='Export as csv',
                filename='kmeans_result'
              )
            )
          )
        )
      )
      df1 = df %>%
        group_by(cluster) %>%
        summarise(count = n())
      df1$precent = round(df1$count/sum(df1$count),3)
      result = c(list(res$centers),list(as.matrix(df1)))
      names(result) = c("center","summary")
      output$p1 = renderPrint({
        result
      })
    }
  )
}
































