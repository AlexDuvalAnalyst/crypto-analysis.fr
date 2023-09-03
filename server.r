library(shiny)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(fontawesome)
library(lubridate)
library(tidyverse)
library(plotly)
library(scales)
library(gridExtra)

top_crypto <- setNames(read.csv("data/top_crypto.csv"),c("id","name","symbol","rank"))


server <- function(input, output, session) {
  
  ls <- seq(1,20,1)
  for(i in ls){
    name <- paste("crypto",i,sep = "")
    eval(parse(text = paste(
      "observeEvent(
        input$",name,",{
        observe(hide('wrapper'))
        observe(shinyjs::show('cryp",i,"'))
      })",sep = ""
    )))
    for(j in ls[ls != i]){
      eval(parse(text = paste(
        "observeEvent(
        input$",name,",{
        observe(shinyjs::hide('cryp",j,"'))
      })",sep = ""
      )))
    }
    eval(parse(text = paste(
      "observeEvent(input$exit",i,",{
        observe(shinyjs::hide('cryp",i,"'))
        observe(shinyjs::show('wrapper'))
      })",sep = ""
    )))
  }
  
  observeEvent(input$inf,{
    shinyjs::toggle("toggle-info")
  })
  observeEvent(input$exinf,{
    shinyjs::toggle("toggle-info")
  })
  
  
  top_crypto <- setNames(read.csv("data/top_crypto.csv"),c("id","name","symbol","rank","logo"))
  top_crypto_1 <- top_crypto[top_crypto$rank <= 10,]
  
  dt <- read.csv("data/data.csv")
  dt$Date <- as_datetime(dt$Date)
  dtmois <- read.csv("data/data_mois.csv")
  dtmois$Date <- as_datetime(dtmois$Date)
  dt6mois <- read.csv("data/data_6mois.csv")
  dt6mois$Date <- as_datetime(dt6mois$Date)
  
  
  for(i in c(1:10)){
    id <- paste("name",i,sep="")
    nom_crypto <- top_crypto_1[top_crypto_1$rank == i,]$name
    date_max <- max(dtmois[dtmois$id ==  top_crypto_1[top_crypto_1$rank == i,]$id,]$Date, na.rm = T)
    date_max <- format(as.Date(as_datetime(date_max)), format = "%d/%m/%Y")
    
    val <- dtmois[(dtmois$Date == max(dtmois[dtmois$id ==  top_crypto_1[top_crypto_1$rank == i,]$id,]$Date, na.rm = T))
                  & dtmois$id ==  top_crypto_1[top_crypto_1$rank == i,]$id,]$Mean
    logo <- top_crypto[top_crypto$id == top_crypto_1[top_crypto_1$rank == i,]$id,]$logo
    
    trend1mois <- dtmois[dtmois$id == top_crypto_1[top_crypto_1$rank == i,]$id,]
    t1m <- round(((trend1mois[trend1mois$Date == max(trend1mois$Date, na.rm = T),]$Mean - trend1mois[trend1mois$Date == min(trend1mois$Date, na.rm = T),]$Mean)/
                    trend1mois[trend1mois$Date == min(trend1mois$Date, na.rm = T),]$Mean)*100,2)
    if(t1m > 0.1){
      t1micon <- "fa-solid fa-arrow-trend-up fa-beat"
      col1m = "#118c00"
      sign <- "+"
    }else{
      if(t1m < 0) {
        t1micon <- "fa-solid fa-arrow-trend-down fa-beat"
        col1m = "#ff3b48"
        sign <- "-"
      }else{
        t1micon <- "fa-solid fa-equals fa-beat"
        col1m = "#49afe3"
        sign <- ""
      }
    }
    trend6mois <- dt6mois[dt6mois$id == top_crypto_1[top_crypto_1$rank == i,]$id,]
    t6m <- round(((trend6mois[trend6mois$Date == max(trend6mois$Date, na.rm = T),]$Mean - trend6mois[trend6mois$Date == min(trend6mois$Date, na.rm = T),]$Mean)/
                    trend6mois[trend6mois$Date == min(trend6mois$Date, na.rm = T),]$Mean)*100,2)
    if(t6m > 0.1){
      t6micon <- "fa-solid fa-arrow-trend-up fa-beat"
      col6m = "#118c00"
      sign <- "+"
    }else{
      if(t6m < 0){
        t6micon <- "fa-solid fa-arrow-trend-down fa-beat"
        col6m = "#ff3b48"
        sign <- "-"
      }else{
        t6micon <- "fa-solid fa-equals fa-beat"
        col6m = "#49afe3"
        sign <- ""
      }
    }
    trend1an <- dt[dt$id == top_crypto_1[top_crypto_1$rank == i,]$id,]
    t1a <- round(((trend1an[trend1an$Date == max(trend1an$Date, na.rm = T),]$Mean - trend1an[trend1an$Date == min(trend1an$Date, na.rm = T),]$Mean)/
                    trend1an[trend1an$Date == min(trend1an$Date, na.rm = T),]$Mean)*100,2)
    if(t1a > 0.1){
      t1aicon <- "fa-solid fa-arrow-trend-up fa-beat"
      col1a = "#118c00"
      sign <- "+"
    }else{
      if(t1a < 0){
        t1aicon <- "fa-solid fa-arrow-trend-down fa-beat"
        col1a = "#ff3b48"
        sign <- "-"
      }else{
        t1aicon <- "fa-solid fa-equals fa-beat"
        col1a = "#49afe3"
        sign <- ""
      }
    }
    eval(parse(text = paste(
      "output$",id," <- renderUI({
        tags$div(class = 'info_crypto',tags$p(class = 'nom_crypto','",nom_crypto,"'),
        tags$img(width = '30px',height = '30px', class = 'logo',src = '",logo,"'),
        tags$p(class = 'val_crypto','",paste(round(val,0),"$"),"'),
        tags$p(class = 'date_crypto','",date_max,"'),
        tags$div(class = 'ic1', tags$i(class = '",t1micon,"', style = 'font-size:1.3em;background:rgba(255, 255, 255,0.8);border-radius:35px;padding:10px;
        color:",col1m,";opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;')),
        tags$p(class = 't1m', '",paste(sign,abs(t1m),'%'),"'),
        tags$div(class = 'ic2', tags$i(class = '",t6micon,"', style = 'font-size:1.3em;background:rgba(255, 255, 255,0.8);border-radius:35px;padding:10px;
        color:",col6m,";opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;')),
        tags$p(class = 't6m', '",paste(sign,abs(t6m),'%'),"'),
        tags$div(class = 'ic3', tags$i(class = '",t1aicon,"', style = 'font-size:1.3em;background:rgba(255, 255, 255,0.8);border-radius:35px;padding:10px;
        color:",col1a,";opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;')),
        tags$p(class = 't1a', '",paste(sign,abs(t1a),'%'),"'))
      })",sep = ""
    )))
  }
  
  for(i in c(1:10)){
    k <- 0
    for (j in c(((i*3)-2):(i*3))){
      k <- k+1
      eval(parse(text = paste(
        "output$p",j," <- renderPlotly({fplot",k,"(top_crypto_1[top_crypto_1$rank == ",i,",]$id, dtmois, dt6mois, dt)})",sep = ""
      )))
    }
  }
  
  dthist <- read.csv("data/data_hist.csv")
  
  for (i in c(1:20)){
    
    id <- top_crypto[top_crypto$rank == i,]$id
    eval(parse(text = paste(
      
      "output$graph",i," <- renderPlotly({
        if(length(dthist[dthist$id == ",id,",]$Mean) > 10){
          bplot(",id,",dthist)
        }
      })
      
      output$graphbp",i," <- renderPlotly({
       if(length(dthist[dthist$id == ",id,",]$Mean) > 10){
          boxp(",id,",dthist)
       }
      })
     output$trendtitle",i," <- renderUI(
      if(length(dthist[dthist$id == ",id,",]$Mean) > 10){
          trendtitle(",id,",dthist)
       }
      )
     output$trendicon",i," <- renderUI(
      if(length(dthist[dthist$id == ",id,",]$Mean) > 10){
          trendlogo(",id,",dthist)
       }
      )
  ",sep = ""
    )))
  }
  
  
  
  bplot <- function(id_cryp, dthisto) {
    name <- top_crypto[top_crypto$id == id_cryp,]$name
    
    bt <- dthist[dthist$id == id_cryp,]
    bt$Date <- as.Date(bt$Date)
    
    
    p <- ggplot(bt, aes(x = Date, y = Mean))+
      geom_area(size = 1, alpha = 0.08, fill = "#520606") +
      geom_line(size = 1, color = "#ff4f4f",alpha = 0.7) +
      geom_smooth(method = "loess", span = 0.18, size = 0.8, color = "green")+
      scale_x_date(date_labels="%Y/%m",
                   breaks = seq(min(bt$Date,na.rm = T),
                                max(bt$Date, na.rm = T),length.out = 8))+
      scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
      labs(y = "Weely Mean $", title = paste("Historical",name,"price $"))+
      theme_light()+
      theme(plot.title = element_text(color = "white", hjust = 0.5, vjust = -7.5, size = 18,face = "bold"),
            axis.title.x = element_blank(),
            axis.title = element_text(size = 15, color = "white", face = "bold"),
            axis.text.x = element_text(size = 11,color = "white", face = "bold"),
            axis.text.y = element_text(size = 12,color = "white", face = "bold"),
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill = 'transparent', color = 'transparent'),
            panel.grid.major = element_blank())
    ggplotly(p)
  }
  
  boxp <- function(id_cryp, dthisto){
    
    name <- top_crypto[top_crypto$id == id_cryp,]$name
    
    br <- dthist[dthist$id == id_cryp,]
    
    p <- ggplot(br, aes(x = as.factor(substr(Date,6,7)), y = Mean)) +
      geom_boxplot(aes(fill = as.factor(substr(Date,6,7))), colour = "white", outlier.shape = NA, alpha = 0.7) +
      scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3)) +
      scale_x_discrete(labels = c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
      labs(y = "Price $", title = paste(name," value each month of the year",sep = ""), color = "") +
      theme_light() +
      theme(plot.title = element_text(color = "white", hjust = 0.5, vjust = -7.5, size = 12,face = "bold"),
            axis.title.x = element_blank(),
            axis.title = element_text(size = 12, color = "white", face = "bold"),
            axis.text = element_text(size = 10,color = "white", face = "bold"),
            legend.position = "none",
            panel.background = element_rect(fill='transparent'),
            plot.background = element_rect(fill = 'transparent', color = 'transparent'),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    ggplotly(p)
  }
  
  trendtitle <- function(id_cryp, dthist){
    
    name <- top_crypto[top_crypto$id == id_cryp,]$name
    
    br <- dthist[dthist$id == id_cryp,]
    
    dtt <- tail(br,2)
    v2 <- dtt[2,6]
    v1 <- dtt[1,6]
    
    val <- round(((v2-v1)/v1)*100,1)
    if(val > 0 ){
      sign <- "+"
    }else{
      if(val < 0){
        sign <- "-"
      }else{
        sign <- ""
      }
    }
    text_return <- tags$p(class = 'trendtitle', paste('Weekly trend : ',val,'$',sep = ''))
    return(text_return)
  }
  
  trendlogo <- function(id_cryp, dthist){
    
    name <- top_crypto[top_crypto$id == id_cryp,]$name
    
    br <- dthist[dthist$id == id_cryp,]
    
    dtt <- tail(br,2)
    v2 <- dtt[2,6]
    v1 <- dtt[1,6]
    
    val <- round(((v2-v1)/v1)*100,1)
    if(val > 0 ){
      trend_logo <- tags$i(class = "fa-solid fa-arrow-trend-up fa-beat", style = "font-size:12vw;
           background:rgba(36, 47, 57,0);color:blue;position:absolute;bottom:8%;
           left:25%;opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;")
      
    }else{
      if(val < 0){
        trend_logo <- tags$i(class = "fa-solid fa-arrow-trend-down fa-beat", style = "font-size:12vw;
           background:rgba(36, 47, 57,0);color:blue;position:absolute;bottom:8%;
           left:25%;opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;")
      }else{
        trend_logo <- tags$i(class = "", style = "font-size:12vw;
           background:rgba(36, 47, 57,0);color:blue;position:absolute;bottom:8%;
           left:25%;opacity:0.5;--fa-animation-duration: 3.8s; --fa-beat-scale: 1.08;")
      }
    }
    return(trend_logo)
  }
  
  fplot1 <- function(id_cryp, mois, mois6, annee){
    
    
    datas_mois <- mois[mois$id == id_cryp,]
    
    trend1mois <- datas_mois
    t1m <- round(((trend1mois[trend1mois$Date == max(trend1mois$Date, na.rm = T),]$Mean - trend1mois[trend1mois$Date == min(trend1mois$Date, na.rm = T),]$Mean)/
                    trend1mois[trend1mois$Date == min(trend1mois$Date, na.rm = T),]$Mean)*100,2)
    if(t1m > 0.1){
      col1m = "#1cd46e"
      colfill = "green"
    }else{
      if(t1m < 0) {
        col1m = "#ff3b48"
        colfill = "red"
      }else{
        col1m = "#49afe3"
        colfill = "skyblue"
      }
    }
    
    # 1 mois
    if(length(datas_mois$Mean) < 1){
      datas_mois <- data.frame(fake = rep("fake",30))
      datas_mois$Date <- seq(Sys.Date()-30,Sys.Date(),length.out = 30)
      datas_mois$id <- id_cryp
      datas_mois$name <- "na"
      datas_mois$min <- 0
      datas_mois$max <- 0
      datas_mois$Mean <- 0
    }else{
      p1 <- ggplot(data = datas_mois, aes(x = Date, y = Mean))+geom_point(color = "#eb3440",alpha = 0, size = 0.1)+
        geom_line(color = col1m,alpha = 0.7, size = 0.8)+
        geom_area(fill = colfill, alpha = 0.2)+
        scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
        theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill = 'transparent', color = 'transparent'),
          panel.grid = element_blank(),
          axis.text = element_text(color = "white", size = 17,face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(color = "white", hjust = 0.5, vjust = -7.5, size = 18,face = "bold"),
          #plot.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA),
          legend.margin=margin(l = -20, unit='pt'),
          legend.text = element_text(color = "white", size = 15,face = "bold"),
          legend.key.height = unit(50, "pt")
        )
    }
    try(ggplotly(p1)%>% config(displayModeBar = F))
    #ggplotly(p, width = 870, height = 140)
    #try(subplot(p1, p2,p3, nrows=1,margin = 0.09)) # la fonction subplot de plotly adapte automatiquement la taille des 3 graphiques
    # margin permet de creer de l'espace entre les differents plot
    
  }
  
  
  fplot2 <- function(id_cryp, mois, mois6, annee){
    
    
    datas_6mois <- mois6[mois6$id == id_cryp,]
    
    trend6mois <- datas_6mois
    t6m <- round(((trend6mois[trend6mois$Date == max(trend6mois$Date, na.rm = T),]$Mean - trend6mois[trend6mois$Date == min(trend6mois$Date, na.rm = T),]$Mean)/
                    trend6mois[trend6mois$Date == min(trend6mois$Date, na.rm = T),]$Mean)*100,2)
    if(t6m > 0.1){
      col6m = "#1cd46e"
      colfill = "green"
    }else{
      if(t6m < 0) {
        col6m = "#ff3b48"
        colfill = "red"
      }else{
        col6m = "#49afe3"
        colfill = "skyblue"
      }
    }
    
    if(length(datas_6mois$Mean) < 1){
      datas_6mois <- data.frame(fake = rep("fake",180))
      datas_6mois$Date <- seq(Sys.Date()-180,Sys.Date(),length.out = 180)
      datas_6mois$id <- id_cryp
      datas_6mois$name <- "na"
      datas_6mois$min <- 0
      datas_6mois$max <- 0
      datas_6mois$Mean <- 0
    }else{
      p2 <- ggplot(data = datas_6mois, aes(x = Date, y = Mean))+geom_point(color = "#eb3440",alpha = 0)+
        geom_line(color = col6m,alpha = 0.7, size = 0.8)+
        geom_area(fill = colfill, alpha = 0.2)+
        scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
        theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill = 'transparent', color = 'transparent'),
          panel.grid = element_blank(),
          axis.text = element_text(color = "white", size = 17,face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(color = "white", hjust = 0.5, vjust = -7.5, size = 18,face = "bold"),
          #plot.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA),
          legend.margin=margin(l = -20, unit='pt'),
          legend.text = element_text(color = "white", size = 15,face = "bold"),
          legend.key.height = unit(50, "pt"))
    }    
    try(ggplotly(p2)%>% config(displayModeBar = F))
    
    
    #try(subplot(p1, p2,p3, nrows=1,margin = 0.09)) # la fonction subplot de plotly adapte automatiquement la taille des 3 graphiques
    # margin permet de creer de l'espace entre les differents plot
    
  }
  
  
  fplot3 <- function(id_cryp, mois, mois6, annee){
    
    datas <- annee[annee$id == id_cryp,]
    
    trend <- datas
    tm <- round(((trend[trend$Date == max(trend$Date, na.rm = T),]$Mean - trend[trend$Date == min(trend$Date, na.rm = T),]$Mean)/
                   trend[trend$Date == min(trend$Date, na.rm = T),]$Mean)*100,2)
    if(tm > 0.1){
      colm = "#1cd46e"
      colfill = "green"
    }else{
      if(tm < 0) {
        colm = "#ff3b48"
        colfill = "red"
      }else{
        colm = "#49afe3"
        colfill = "skyblue"
      }
    }
    
    if(length(datas$Mean) < 1){
      datas <- data.frame(fake = rep("fake",365))
      datas$Date <- seq(Sys.Date()-365,Sys.Date(),length.out = 365)
      datas$id <- id_cryp
      datas$name <- "na"
      datas$min <- 0
      datas$max <- 0
      datas$Mean <- 0
    }else{
      p3 <- ggplot(data = datas, aes(x = Date, y = Mean))+geom_point(color = "#eb3440",alpha = 0)+
        geom_line(color = colm,alpha = 0.7, size = 0.8)+
        geom_area(fill = colfill, alpha = 0.2)+
        scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))+
        theme(
          panel.background = element_rect(fill='transparent'),
          plot.background = element_rect(fill = 'transparent', color = 'transparent'),
          panel.grid = element_blank(),
          axis.text = element_text(color = "white", size = 17,face = "bold"),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(color = "white", hjust = 0.5, vjust = -7.5, size = 18,face = "bold"),
          #plot.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.title = element_blank(),
          legend.background = element_rect(fill = NA),
          legend.margin=margin(l = -20, unit='pt'),
          legend.text = element_text(color = "white", size = 15,face = "bold"),
          legend.key.height = unit(50, "pt"))
    }
    try(ggplotly(p3)%>% config(displayModeBar = F))
    
    
    #try(subplot(p1, p2,p3, nrows=1,margin = 0.09)) # la fonction subplot de plotly adapte automatiquement la taille des 3 graphiques
    # margin permet de creer de l'espace entre les differents plot
    
  }
  
  
  
}

