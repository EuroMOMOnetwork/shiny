rm(list=ls())
library(readxl)
library(data.table)
library(tidyverse)
library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinyWidgets)


data <- readRDS("C:/Users/ANEH/Desktop/EuroMoMo/Data.rds")

########################################################################
# Dashboard EuroMOMO
# 03-2020
########################################################################

######
# UI #
######

#Header
header <- dashboardHeader(title = "",
                          tags$li(class = "dropdown", 
                                  style = "padding: 0px 800px 0px 0px;",
                                  tags$p(div(img(src="edEuro.gif",height=40,width=500)))))
#title=div(img(src="logo_mini_2.1.gif",height=42,width=160))


#Sidebar
sidebar <- dashboardSidebar(sidebarMenu(
  selectizeInput("country", 
                 label="Select country:",
                 choices = NULL,
                 multiple=T,
                 options = list(maxItems = 1)),
  selectizeInput("agegrp", 
                 label="Select age group:",
                 choices = NULL,
                 multiple=T,
                 options = list(maxItems = 1)),
  tags$footer("Zoom: Mark an area",tags$br(),"on graph or map.",tags$br(),"Doubleclick area to zoom in.",
tags$br(), "Doubleclick to zoom out.",align = "center", style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:100px;   /* Height of the footer */
                color: white;
              padding: 10px;
              z-index: 1000;")
)
)

#Body
body <- dashboardBody(
  tags$head(tags$style(HTML(".skin-black .main-sidebar {background-color: #999999;}
                            .skin-black .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #009999;}
                            .skin-black .main-sidebar. sidebar .sidebar-menu .active a{
                            background-color: #009999; color: #005555;}
                            .nav-tabs-custom .nav-tabs li.active {
                            border-top-color: #6633cc;}
                            "))),
  fluidRow(
    tabBox(id="tabmenu",width=12,
           tabPanel("Number graphs",icon = icon("chart-line","fa-lg"),
                    fluidRow(
                      column(width=12,
                             plotOutput("pnb", height = 650,
                                        dblclick = "MyPlot_dblclick",
                                        brush = brushOpts(
                                          id = "MyPlot_brush",
                                          resetOnNew = T)),
                             sliderTextInput("aggwk","Reporting week",
                                             choices= unique(data$reporting),
                                             selected=unique(data$reporting)[5])
                      )
                    )
           ),
           
           tabPanel("Z-score graphs",icon = icon("chart-bar","fa-lg"),
                    fluidRow(
                      column(width=12,
                             plotOutput("zsc", height = 650,
                                        dblclick = "MyPlot_dblclick",
                                        brush = brushOpts(
                                          id = "MyPlot_brush",
                                          resetOnNew = T),
                                        hover = hoverOpts(
                                          id = "nhover",clip=T)),
                             uiOutput("hover_n"),
                             sliderTextInput("aggwkz","Reporting week",
                                             choices= unique(data$reporting),
                                             selected=unique(data$reporting)[5])
                      )
                    )
           ),
           tabPanel("Bulletin",icon = icon("sticky-note","fa-lg"),
                    fluidRow(
                      column(width=12
                      )
                    )
           )
           
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "black")

##########
# Server #
##########
server <- function(input,output,session) {
  
  observe({
    updateSelectizeInput(session,"country",choices=as.list(unique(data$country)),
                         server=T,
                         selected=data$country[1])
    updateSelectizeInput(session,"agegrp",choices=as.list(unique(data$group)),
                         server=T,
                         selected=data$group[1])
  })
  
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  observeEvent(input$country,{
    dt_country <- data[data$country == input$country,]
    
    observeEvent(input$agegrp,{
      dt_c_age <- dt_country[dt_country$group == input$agegrp,]
      
      observeEvent(input$aggwk,{
        dt_aggregated <- dt_c_age[dt_c_age$reporting == input$aggwk,]
        
        dt <-   data.table::setDT(dt_aggregated)[,
                                                 .(country,reporting,group,nb, nbc, pnb,
                                                   sdm2 = pnb - 2*sqrt(Vexcess),
                                                   sd2 = pnb + 2*sqrt(Vexcess),
                                                   sd4 = pnb + 4*sqrt(Vexcess),
                                                   zscore = (nbc - pnb)/sqrt(Vexcess),
                                                   zscorem2 = -2*(nbc - pnb)/sqrt(Vexcess),
                                                   z2 = 2*(nbc - pnb)/sqrt(Vexcess),
                                                   z4 = 4*(nbc - pnb)/sqrt(Vexcess)
                                                 ), keyby = ISOweek]
        
        dt$wk = as.numeric(as.factor(dt$ISOweek))
        
        # Plot Number graph
        output$pnb <- renderPlot({
          validate(need(nrow(dt) !=0,"Data not available for this selection!"))
          ggplot(dt,aes(x= wk))+
            geom_line(aes(y = nbc, colour="darkgreen"), linetype="solid") +
            geom_line(aes(y = pnb, colour="red"), linetype="solid") +
            geom_line(aes(y = sdm2, colour="black"), linetype="dashed") +
            geom_line(aes(y = sd2, colour="black"), linetype="dashed") +
            geom_line(aes(y = sd4, colour="blue"), linetype="dashed") +
            ggtitle(paste(dt$country, dt$group)) + theme(plot.title = element_text(hjust = 0.5)) +
            scale_x_continuous(name = "ISOWeek", labels = dt[seq(min(dt$wk), max(dt$wk), by = 10),]$ISOweek, breaks = seq(min(dt$wk), max(dt$wk), by = 10)) +
            scale_y_continuous(name = "Number of deaths") +
            scale_color_identity(name = "",
                                 breaks = c("darkgreen", "red", 'black', 'blue'),
                                 labels = c("Observed", "Baseline", "2SD", '4SD'),
                                 guide = "legend") +
            labs(caption = paste('Data reported:', input$aggwk))+
            theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5),
                  panel.grid.major = element_blank(), legend.position = "bottom")+
            coord_cartesian(xlim = ranges$x, ylim= ranges$y, expand=F)
        })
      })
      observeEvent(input$aggwkz,{
        dt_aggregated <- dt_c_age[dt_c_age$reporting == input$aggwkz,]
        
        dt <-   data.table::setDT(dt_aggregated)[,
                                                 .(country,reporting,group,nb, nbc, pnb,
                                                   sdm2 = pnb - 2*sqrt(Vexcess),
                                                   sd2 = pnb + 2*sqrt(Vexcess),
                                                   sd4 = pnb + 4*sqrt(Vexcess),
                                                   zscore = (nbc - pnb)/sqrt(Vexcess),
                                                   zscorem2 = -2*(nbc - pnb)/sqrt(Vexcess),
                                                   z2 = 2*(nbc - pnb)/sqrt(Vexcess),
                                                   z4 = 4*(nbc - pnb)/sqrt(Vexcess)
                                                 ), keyby = ISOweek]
        
        dt$wk = as.numeric(as.factor(dt$ISOweek))
        
        # Plot z-score
        output$zsc <- renderPlot({
          validate(need(nrow(dt) !=0,"Data not available for this selection!"))
          ggplot(dt,aes(x= wk))+
            geom_line(aes(y = zscore), colour="darkgreen", linetype="solid") +
            geom_point(aes(y = zscore), colour="darkgreen", size=2) +
            geom_hline(yintercept = -2, linetype = "dashed", color = "black") +
            geom_hline(yintercept = 0, linetype = "solid", color = "red") +
            geom_hline(yintercept = 2, linetype = "dashed", color = "black") +
            geom_hline(yintercept = 4, linetype = "dashed", color = "blue") +
            ggtitle(paste(dt$country, dt$group)) + theme(plot.title = element_text(hjust = 0.5)) +
            scale_x_continuous(name = "ISOWeek", labels = dt[seq(min(dt$wk), max(dt$wk), by = 13),]$ISOweek, breaks = seq(min(dt$wk), max(dt$wk), by = 13)) +
            scale_y_continuous(name = "Z-score") +
            scale_color_identity(name = "",
                                 breaks = c("darkgreen", "red", 'black', 'blue'),
                                 labels = c("Observed", "Baseline", "2SD", '4SD'),
                                 guide = "legend") +
            labs(caption = paste('Data reported:', input$aggwkz))+
            theme(axis.text.x = element_text(angle=90,size=10,vjust=0.5),
                  panel.grid.major = element_blank(), legend.position = "bottom")+
            coord_cartesian(xlim = ranges$x, ylim= ranges$y, expand=F)
        })
        
        observeEvent(input$MyPlot_dblclick,{
          brush <- input$MyPlot_brush
          if(!is.null(brush)){
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
          } else{
            ranges$x <- NULL
            ranges$y <- NULL
          }
        })
        
        output$hover_n <- renderUI({
          if(!is.null(input$nhover)){
            hover <- input$nhover
            point<-  nearPoints(dt, hover, maxpoints = 1)
            
            left_px <- hover$range$left + (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left) * (hover$range$right - hover$range$left)
            top_px <- hover$range$top +(hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom) * (hover$range$bottom - hover$range$top)
            
            style <- paste0("position:absolute; z-index:100;", "left:", left_px - 200, "px; top:", top_px + 2, "px;")
            
            wellPanel(
              style = style,
              p(HTML(paste0("<b>Week: </b>", point$ISOweek,"<br><b>Z-score: </b>",point$zscore)))
            )
          } else{
          }
        })
      })
    })
  })
  
}

shinyApp(ui, server)