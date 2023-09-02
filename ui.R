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

code_bouton <- ""
for(i in c(1:20)){
  
  if(i < 20){
    ajout <- paste("actionButton('crypto",i,"', '",top_crypto[top_crypto$rank == i,]$name,"', class = 'c",i,"'),",sep = "")
  }else{
    ajout <- paste("actionButton('crypto",i,"','",top_crypto[top_crypto$rank == i,]$name,"', class = 'c",i,"')",sep = "")
  }
  code_bouton <- paste(code_bouton,ajout,sep = "")
}

code_div <- ""
for(i in c(1:20)){
  
  if(i < 20){
    ajout <- paste("shinyjs::hidden(tags$div(id = 'cryp",i,"', class = 'divcrypto',
                   actionButton('exit",i,"','X',class = 'exit'),
                   div(class = 'graph',plotlyOutput('graph",i,"', height = '100%', width = '100%')),
                   div(class = 'graphbp',plotlyOutput('graphbp",i,"', height = '100%', width = '100%')),
                   div(class = 'card-text',uiOutput('trendtitle",i,"'),uiOutput('trendicon",i,"'))
                   )
                   ),",sep = "")
  }else{
    ajout <- paste("shinyjs::hidden(tags$div(id = 'cryp",i,"', class = 'divcrypto',
                   actionButton('exit",i,"','X',class = 'exit'),
                   div(class = 'graph',plotlyOutput('graph",i,"', height = '100%', width = '100%')),
                   div(class = 'graphbp',plotlyOutput('graphbp",i,"', height = '100%', width = '100%')),
                   div(class = 'card-text',uiOutput('trendtitle",i,"'),uiOutput('trendicon",i,"'))
                   ))",sep = "")
  }
  code_div <- paste(code_div,ajout,sep = "")
}

code_app <- paste("fluidPage(useShinyjs(),
      
      theme = 'main.css',
      tags$script(src = 'https://kit.fontawesome.com/54af662477.js'),
      tags$head(tags$style(HTML('
                  .shiny-output-error-validation {
                    color: red;
                    font-weight: bold;
                    position:absolute;
                    top:25vh;
                    width:80vh;
                    font-size:2em;
                    background-color:rgba(255,255,255,0.7);
                    border-radius:7px;
                    box-shadow: 0 6px 4px 0 rgba(0,0,0,0.3);
                    padding:20px;
                    z-index:3000;
                  }
                  '))),
      tags$body(
       tags$div(
        tags$div(class='wave'),
        tags$div(class='wave'),
        tags$div(class='wave'),
        ),
        tags$div(id = 'menu',class = 'menu', '',
                 tags$p(class = 'title_app', 'Crypto',br(),'analysis'),
                 tags$div(class = 'liste_crypto',
                          ",code_bouton,"
                 )
                 ),
        ",code_div,",
        tags$div(class = 'title',
                 tags$p(class = 'title_crypto','Top 10',br(),'crypto'),
                 tags$p(class = 'title_valeur','Price'),
                 tags$p(class = 'title_1mois','1 month trend'),
                 tags$p(class = 'title_6mois','6 month trend'),
                 tags$p(class = 'title_1an','1 year trend'),
                 actionButton(title = 'More informations','inf',class = 'btninfos','', icon = icon('circle-question', class='fa-solid fa-circle-question',
                                                           style = 'font-size: 1.5vw;text-shadow: 0px 5px 5px rgba(0,0,0,0.9);
                  position:absolute;top:0%;right:0%;'))),
        tags$div(id = 'wrapper',class = 'wrapper', 
        tags$div(class = 'control1',
                 tags$div(class = 'bandeau'),
                 div(plotlyOutput('p1', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p2', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p3', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name1')),
        tags$div(class = 'control2',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p4', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p5', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p6', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name2')),
        tags$div(class = 'control3',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p7', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p8', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p9', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name3')),
        tags$div(class = 'control4',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p10', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p11', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p12', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name4')),
        tags$div(class = 'control5',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p13', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p14', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p15', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name5')),
        tags$div(class = 'control6',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p16', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p17', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p18', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name6')),
        tags$div(class = 'control7',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p19', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p20', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p21', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name7')),
        tags$div(class = 'control8',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p22', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p23', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p24', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name8')),
        tags$div(class = 'control9',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p25', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p26', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p27', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name9')),
        tags$div(class = 'control10',
        tags$div(class = 'bandeau'),
                 div(plotlyOutput('p28', height = '100%', width = '100%'), class = 'g1'),
                 div(plotlyOutput('p29', height = '100%', width = '100%'), class = 'g2'),
                 div(plotlyOutput('p30', height = '100%', width = '100%'), class = 'g3'),
                 uiOutput('name10')),
        tags$div(id = 'toggle-info',class = 'card-infos',
                               actionButton('exinf', 'X',class = 'exit'),
                               tags$h1(style = 'text-align:center;font-size:2em;','Welcome to Crypto-analysis !'),
                               br(),
                               br(),
                               'Statistical analysis on the top 20 crypto currencies',
                               br(),
                               br(),
                               'Crypto-analysis is a web application that let you explore statistical analysis for the top 20 crypto currencies.',
                               br(),
                               'All data are collected from the', tags$a(href = 'https://coinmarketcap.com/','coinmarketcap api', target = '_blank'),'
                               and are renewed every day which allows you to have the most recent data.',
                               br(),
                               br(),
                               'All charts are build freely with plotly and are interactive for users.',
                               br(),
                               br(),
                               'The statistical and graphical analysis produced by this application is free to use, however
                                        no responsibility shall be taken, regarding the accuracy, adequacy, validity, reliability
                                        of any elements on this application usage. You shall use this application at your own risk and
                                        you may contact alexandre76370@hotmail.fr for more informations.'
                               
             )
        )
      )
    )
",sep = "")

tagList(eval(parse(text = code_app)))