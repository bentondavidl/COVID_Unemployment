#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(shinycssloaders)

################
# Data Loading #
################

# Employment data from department of labor oui.doleta.gov/unemployment/claims.asp
unemployment <- fromJSON("./data/us_unemployment.txt")$r539cyState$week %>%
    select(-ReflectingWeekEnded) %>%
    mutate(
        weekEnded = as.Date(weekEnded,"%m/%d/%Y"),
        InitialClaims = parse_number(InitialClaims),
        ContinuedClaims = parse_number(ContinuedClaims),
        CoveredEmployment = parse_number(CoveredEmployment),
        InsuredUnemploymentRate=parse_number(InsuredUnemploymentRate)
    )

# COVID policy data from CoronaNet Project
policy_data <- read.csv("./data/coronanet_release_United States of America.csv") %>%
    mutate(
        date_start = as.Date(date_start,"%Y-%m-%d"),
        date_announced = as.Date(date_announced,"%Y-%m-%d"),
        date_end = as.Date(date_end,"%Y-%m-%d"),
        date_updated = as.Date(date_updated,"%Y-%m-%d")
    )

cases <- read.csv("data/time_series_covid19_confirmed_US.csv")
cases <- cbind(cases[,1:11],cases[,seq(15,681,7)]) %>%
    mutate(type = "cases")

deaths <- read.csv("data/time_series_covid19_deaths_US.csv")
deaths <- cbind(deaths[,1:11],deaths[,seq(16,681,7)]) %>%
    mutate(type = "deaths")

merged <- rbind(cases, deaths)

rm(cases)
rm(deaths)

covid <- merged %>%
    pivot_longer(12:107, "date", "X") %>%
    mutate(date=as.Date(date,"%m.%d.%y")) %>%
    group_by(type,Province_State,date) %>%
    summarize(value=sum(value)) %>%
    mutate(new_value=value-lag(value, default=0)) %>%
    ungroup(Province_State, type)


joined <- inner_join(covid, unemployment, by=c("Province_State"="stateName","date"="weekEnded"))

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="COVID-19 Policies and Unemployment"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data Description", tabName="description", icon=icon("book-open"))
      ),
      hr(),
      selectInput("state","Select State", choices=joined$Province_State, selected=NULL,multiple=F,selectize=T),
      hr(),
      selectInput("unemployment","Select Unemployment Measure", choices=c("Claims","Insured Unemployment Rate"), selected="Claims"),
      conditionalPanel("input.unemployment=='Claims'",
                       checkboxGroupInput("claimtype","Claim Types to Include",c('Initial Claims'="InitialClaims",'Continued Claims' = "ContinuedClaims"),c("InitialClaims","ContinuedClaims"))
      ),
      hr(),
      selectInput("cases","Select Case Count", choices=c("New","Total"), selected="New"),
      checkboxGroupInput("type","Reported Values",c('Cases'="cases",'Deaths' = "deaths"),c("cases","deaths"))
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName="dashboard",
          tags$head(tags$script('
                            var height = 0;
                            $(document).on("shiny:connected", function(e) {
                            height = window.innerHeight;
                            Shiny.onInputChange("height", height);
                            });
                            $(window).resize(function(e) {
                            height = window.innerHeight;
                            Shiny.onInputChange("height", height);
                            });
                            ')),
          fluidRow(
            column(width=8,
              h1("How Policies and COVID-19 Case Numbers Effect Unemployement")
            ),
            column(width=4,
              box(
                  width=NULL,
                  status="warning",
                  solidHeader = T,
                  title="Instructions",
                  "Click on the circles at the top to learn what policy was enacted at that point, you can click and drag on the graphs to select an area you want to zoom in on, double click to zoom. \n
                  Change what you are seeing in the sidebar to the left."
              )
            )
          ),
          fluidRow(
            column(width=8,
              box(width=NULL,
                plotOutput("events", click="coords", height="auto"),
                withSpinner(
                plotOutput("employment", 
                           dblclick="zoom", 
                           brush = brushOpts("brush_select",direction="x",resetOnNew = T),
                           height="auto")),
                withSpinner(
                plotOutput("covid", 
                           dblclick="zoom", 
                           brush = brushOpts("brush_select",direction="x",resetOnNew = T),
                           height="auto"))
              )
            ),
            column(width=4,
                   box(width=NULL,
                       title="Legend",
                       column(width=6, imageOutput("legend1",height="auto")),
                       column(width=6,
                              imageOutput("legend2",height="auto"),
                              imageOutput("legend3",height="auto"),
                       )
                    ),
                   box(width=NULL,
                       title="Policy Details",
                       textOutput("info")
                   )
            )
          )
        ),
      
        tabItem(tabName="description",
            h1("Where this data comes from"),
            fluidRow(
              box(width=4,
                  title="Case Data",
                  p("This data comes directly from John Hopkins University's ",a(href="https://github.com/CSSEGISandData/COVID-19","COVID-19 Data Repository"),
                    ". This dashboard specifically uses the time series cases and deaths data for the United States"),
                  p("The data from this source is displayed in the bottom chart and can be seen as new cases for the week (imputed) or cumulative cases. ",
                    "The source has daily updates, but the information shown in this visualization only represents updates from Saturdays as this was the frequency of the Unemployment Data."),
                  tags$blockquote("Dong E, Du H, Gardner L. An interactive web-based dashboard to track COVID-19 in real time. Lancet Inf Dis. 20(5):533-534. doi: 10.1016/S1473-3099(20)30120-1")
                  ),
              box(width=4,
                  title="Unemployment Data",
                  p("The labor statistics for this project come directly from the United States Department of Labor Employment & Training Administration. This particular data was downloaded from their ",
                    a(href="https://oui.doleta.gov/unemploy/claims.asp","Unemployment Insurance Weekly Claims Data"),
                    " with all states selected. Data for all of 2020 and 2021 (up until November 6, 2021 when the data was captured) is included in this dashboard."
                  ),
                  p("This source contains the following dimensions that are utilized in the dashboard:"),
                  tags$table(
                    tags$tr(
                      tags$td(pre("weekEnded")),tags$td("The Saturday after the week the data represents")
                    ),
                    tags$tr(
                      tags$td(pre("InitialClaims")),tags$td("Number of new unemployment claims for the week")
                    ),
                    tags$tr(
                      tags$td(pre("ContinuedClaims")),tags$td("Number of returning claims from a previous week")
                    ),
                    tags$tr(
                      tags$td(pre("InsuredUnemploymentRate")),tags$td("Number of people currently recieving unemployment insurance as a percentage of the work force")
                    )
                  )
              ),
              box(width=4,
                  title="Policy Decisions",
                  p("COVID-19 related policy changes are taken from the ",
                    a(href="https://www.coronanet-project.org/","CoronaNET Project"),
                    ". The specific dataset used in this project is the ",
                    a(href="https://github.com/CoronaNetDataScience/corona_tscs/blob/master/data/CoronaNet/data_country/coronanet_release/coronanet_release_United%20States%20of%20America.csv", "Release United States"),
                    " data."
                  ),
                  p("The dataset includes many dimensions around each announcement, but the few that are used in this project are: "),
                  tags$table(
                    tags$tr(
                      tags$td(pre("date_start")),tags$td("The date the policy was enacted")
                    ),
                    tags$tr(
                      tags$td(pre("description")),tags$td("A short overview of what the policy does")
                    ),
                    tags$tr(
                      tags$td(pre("dist_index_med_est")),tags$td("CoronaNet's calculated estimate of the effectiveness of the active policies in the region. You can read more in their ",
                                                                 a(href="https://osf.io/preprints/socarxiv/rn9xk","paper"),".")
                    )
                  ),
                  p("An important aknowledgement is that the data from this source is incomplete and becomes more sparce as time progresses. Some states have more complete data than others.")
              )
            )    
        )
      )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reactive data filtering
    filtered_policy <- reactive(filtered <- policy_data %>%
                                    filter(province==input$state) %>%
                                    mutate(y=1))
    unemployment_display <- reactive({
      if(input$unemployment == "Claims") {
        return(input$claimtype)
      }
      # I don't understand this metric and it doesn't visualize well so... 
      # if(input$unemployment == "Covered Employment") {
      #   return(c("CoveredEmployment"))
      # }
      if(input$unemployment == "Insured Unemployment Rate") {
        return(c("InsuredUnemploymentRate"))
      }
    })
    
    case_count <- reactive({
      if(input$cases == "New"){
        return("new_value")
      }
      if(input$cases == "Total"){
        return("value")
      }
    })
    
    # ensure state is selected
    observe({
      updateSelectInput(inputId="state",
                        selected = "Alabama"
      )})
    
    # brushing and zooming
    date_lim <- reactiveValues(x = c(min(joined$date),max(joined$date)))
    
    observeEvent(input$zoom, {
      brush <- input$brush_select
      if (!is.null(brush)) {
        date_lim$x <- as.Date(c(brush$xmin,brush$xmax), origin = "1970-01-01")
      } else {
        date_lim$x <-c(min(joined$date),max(joined$date))
      }
    })
    
    # plot heights
    plot_height <- reactive({
      shiny::validate(
        need(input$height!=0,message="Please wait for the page to load")
      )
      h <- input$height - 250
      if (h>700) {
        return(c(h*0.03,h*0.78,h*0.19,h*.15,h*0.09,h*0.07))
      } else {
        return(c(20,500,150,115,70,55))
      }
    })
    
    # legends
    # g_legend<-function(a.plot){
    #   tmp <- ggplotGrob(a.plot)
    #   leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    #   legend <- tmp$grobs[[leg]]
    #   legend
    # }
    # legends <- reactiveValues(l1 = NULL, l2 = NULL, l3 = NULL)
    
    # Plots
    output$events <- renderPlot(height=function() {plot_height()[1]},{
      
        g1 <- filtered_policy() %>%
            ggplot(aes(x=date_start,y=y,size=5,alpha=.5,color=dist_index_med_est)) + 
            geom_point() +
            coord_cartesian(xlim=date_lim$x, expand=F) + 
            guides(alpha="none", size="none", color=guide_colorbar(title="Measure Strictness")) +
            theme(
                axis.ticks = element_blank(), 
                axis.title = element_blank(),
                axis.text = element_blank(),
                plot.margin = unit(c(0,0.25,0,2.75),"cm"),
                panel.background = element_blank()
                )
        # legends$l1 <- g_legend(g1)
        g1+theme(legend.position = "none")
    })
      
    output$employment <- renderPlot(height=function() {plot_height()[2]},{
        
        # Unemployment chart
        g2 <- joined %>%
            filter(type=="cases",Province_State==input$state) %>%
            pivot_longer(unemployment_display(), names_to="claim_type", values_to="claims") %>%
            ggplot(aes(x=date, y=claims, fill=claim_type)) + 
            geom_bar(stat="identity") + 
            geom_vline(data=filtered_policy(),mapping=aes(xintercept=date_start)) + 
            coord_cartesian(xlim=date_lim$x,expand=F) + 
            scale_y_continuous(labels=function(label) sprintf('%15.2f', label)) +
            scale_fill_manual(values=c("ContinuedClaims"="#bd80cc","InitialClaims"="#798ad1","InsuredUnemploymentRate"="#ff7e6e"),
                               labels=c("Continued Claims","Initial Claims","Unemployment Rate")) +
            guides(fill=guide_legend(title="Claim Type")) +
            labs(y=input$unemployment) +
            theme(axis.ticks.x = element_blank(), 
                  axis.title.x = element_blank(), 
                  axis.text.x = element_blank(),
                  plot.margin = unit(c(0,0.25,0,0),"cm"))
        # legends$l2 <- g_legend(g2)
        g2+theme(legend.position = "none")
        
    })  
        
    output$covid <- renderPlot(height=function() {plot_height()[3]},{
        
        # Cases chart
        g3 <- joined %>%
            filter(Province_State==input$state & type %in% input$type) %>%
            ggplot(aes_string(x="date",y=case_count(),fill="type")) + 
            geom_area() +
            scale_x_date(breaks = scales::breaks_pretty(15)) +
            coord_cartesian(xlim=date_lim$x,expand=F) + 
            geom_vline(data=filtered_policy(),mapping=aes(xintercept=date_start)) +
            scale_y_continuous(labels=function(label) sprintf('%15.2f', label)) +
            scale_fill_manual(values=c("deaths"="#ff7a77","cases"="#f275aa"),
                              labels=function(label) tools::toTitleCase(label)) +
            guides(fill=guide_legend(title="Record Type")) +
            labs(y=paste(input$cases,"Case Count"), x="Date") +
            theme(plot.margin = unit(c(0,0.25,0,0),"cm"))
        # legends$l3 <- g_legend(g3)
        g3+theme(legend.position = "none")
        
    })
    
    output$info <- renderText({
        # nearPoints didn't work, so I did it myself
        event <- filtered_policy()[which.min(abs(filtered_policy()$date_start - as.Date(input$coords$x, origin="1970-01-01"))),]
        as.character(event$description)
    })
    
    # display legends
    output$legend1 <- renderImage({
      return(list(
        src="legend1.png",
        height=plot_height()[4],
        width=NULL
      ))
    }, deleteFile = F)
    output$legend2 <- renderImage({
      return(list(
        src="legend2.jpg",
        height=plot_height()[5],
        width=NULL
      ))
    }, deleteFile = F)
    output$legend3 <- renderImage({
      return(list(
        src="legend3.jpg",
        height=plot_height()[6],
        width=NULL
      ))
    }, deleteFile = F)
    

}

# Run the application 
shinyApp(ui = ui, server = server)
