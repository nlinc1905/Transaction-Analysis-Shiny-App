
library(plotly)
library(shiny)
library(shinydashboard)
library(googleway)
library(magrittr)

#Define UI for application
dashboardPage(
  dashboardHeader(title="Transaction Analysis Tool", titleWidth=300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName="overview", icon=icon("dashboard")),
      menuItem("Transaction Analysis", tabName="transactionAnalysis", icon=icon("bar-chart")),
      menuItem("Network Analysis", tabName="networkAnalysis", icon=icon("share-alt")),
      menuItem("Geospatial Analysis", tabName="geospatialAnalysis", icon=icon("globe")),
      menuItem("Data Filters", tabName="dataFilters", icon=icon("sliders"))
    )
  ),
  dashboardBody(
    #CSS for elements with unique styles
    tags$head(
      tags$style(HTML('#refreshData{background-color:lightgray}'))
    ),
    #Import files from www directory
    tags$head(tags$link(rel="stylesheet", type="text/css", href="style.css")),
    tags$script(src="d3.min.js"),
    tags$script(src="force_directed_graph.js"),
    #Page bodies by tab.  Tabs correspond to the tab names in the sidebar.
    tabItems(
      tabItem(tabName="overview",
              fluidRow(
                tabBox(tabPanel(title="Transaction Amount Over Time",
                                fluidRow(
                                  box(title="Transaction Amount Over Time", status="primary", solidHeader=T,
                                      div(style='overflow-x:scroll;overflow-y:scroll',
                                          plotlyOutput("trxnTimeLine", height=420, width=800)),
                                      width=12))),
                       tabPanel(title="Transaction Amount by Type",
                                fluidRow(
                                  box(title="Transaction Amount by Type", status="primary", solidHeader=T,
                                      div(style='overflow-x:scroll;overflow-y:scroll',
                                          plotlyOutput("trxnTypeBoxPlot", height=420, width=800)),
                                      width=12))),
                       tabPanel(title="Transaction Count by Type",
                                fluidRow(
                                  box(title="Transaction Count by Type", status="primary", solidHeader=T,
                                      div(style='overflow-x:scroll;overflow-y:scroll',
                                          plotlyOutput("trxnCtByType", height=420, width=800)),
                                      width=12))),
                       width=12)),
              fluidRow(
                box(title="Data Output", status="primary", solidHeader=T,
                    selectInput("dataset1", label="Choose a dataset:", choices=c("Transactions", "Customers"),
                                selected="Transactions", width=200),
                    downloadButton('downloadData1', 'Download CSV'),
                    br(),br(),
                    div(style='overflow-x:scroll', dataTableOutput("table1")), width=12))),
      tabItem(tabName="transactionAnalysis",
              fluidRow(
                tabBox(tabPanel(title="First Digit Test",
                                fluidRow(
                                  box(title="Benford's First Digit Test", status="primary", solidHeader=T,
                                      div(style='overflow-x:scroll;overflow-y:scroll',
                                          plotOutput("firstDigitPlot", height=420, width=800)),
                                      width=12))),
                       tabPanel(title="First Two Digits Test",
                                fluidRow(
                                  box(title="Benford's First Two Digits Test", status="primary", solidHeader=T,
                                      div(style='overflow-x:scroll;overflow-y:scroll',
                                          plotOutput("firstTwoDigitPlot", height=420, width=800)),
                                      width=12))),
                       tabPanel(title="Transaction Amount Frequency Distribution",
                                fluidRow(
                                  box(title="Transaction Amount Frequency Distribution", status="primary", solidHeader=T,
                                      div(style='overflow-x:scroll;overflow-y:scroll',
                                          plotlyOutput("trxnAmtDist", height=420, width=800)),
                                      width=12))),
                       tabPanel(title="Anomaly Detection",
                                fluidRow(
                                  box(title="Anomalous Transaction Amounts", status="primary", solidHeader=T,
                                      div(style='overflow-x:scroll;overflow-y:scroll',
                                          plotOutput("dbcPlot", height=420, width=800)),
                                      width=12))),
                       width=12)),
              fluidRow(
                box(title="Data Output", status="primary", solidHeader=T,
                    selectInput("dataset2", label="Choose a dataset:", 
                                choices=c("Transactions", 
                                          "Outlier Transactions Only", 
                                          "First Digit Results", 
                                          "First Two Digits Results"),
                                selected="Transactions", width=200),
                    downloadButton('downloadData2', 'Download CSV'),
                    br(),br(),
                    div(style='overflow-x:scroll', dataTableOutput("table2")), width=12))),
      tabItem(tabName="networkAnalysis",
              fluidRow(
                box(title="Network Graph", status="primary", solidHeader=T,
                    #Create the div that will hold the d3 network graph SVG
                    tags$div(id="network_graph"),
                    width=12)),
              fluidRow(
                p("Double-click to focus on a single node's first degree connections.", style="margin-left:2em"),
                p("Slide the end date limit to see how links changed over time.", style="margin-left:2em"),
                p("Node size depends on betweeennes centrality.  Link size is larger for customers who transact together frequently.", style="margin-left:2em"))),
      tabItem(tabName="geospatialAnalysis",
              textInput("googleMapsAPIKey", label="Enter your Google Maps API Key:"),
              fluidRow(
                box(title="Map", status="primary", solidHeader=T,
                    google_mapOutput("gmap", height=600, width=800),
                    width=12))),
      tabItem(tabName="dataFilters",
              box(title="Data Filters", status="primary", solidHeader=T,
                  textInput("cust_name_filter", label="Customer Name"),
                  actionButton("filterByCust", label="Filter by Customer")),
              p("Specify one or more filters."),
              p("Note that specifying a short transaction date range may impair the effectiveness of anomaly detection."),
              br(),
              actionButton("refreshData", label="Remove All Filters")
      )
    )
  )
  
  
)