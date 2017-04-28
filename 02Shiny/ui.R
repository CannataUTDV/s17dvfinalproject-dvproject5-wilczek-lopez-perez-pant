#ui.R
require(shiny)
require(shinydashboard)

dashboardPage(
  dashboardHeader(
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Age", tabName = "barchart1", icon = icon("dashboard")),
      menuItem("Race", tabName = "barchart2", icon = icon("dashboard")),
      menuItem("Income", tabName = "barchart3", icon = icon("dashboard"))
    )
  ),
  dashboardBody(    
    tabItems(
      # Begin Barchart1 tab content.
      tabItem(tabName = "barchart1",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb1", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         actionButton(inputId = "click1",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data1")
                ),
                tabPanel("Barchart", plotOutput("plot1", height=1500))
              )
      ),
      # End Barchart1 tab content.
      # Begin Barchart2 tab content.
      tabItem(tabName = "barchart2",
        tabsetPanel(
          tabPanel("Data",  
                   radioButtons("rb2", "Get data from:",
                                c("SQL" = "SQL"), inline=T),
                   actionButton(inputId = "click2",  label = "To get data, click here"),
                   hr(), # Add space after button.
                   DT::dataTableOutput("data2")
          ),
          tabPanel("Barchart", plotOutput("plot2", height=1500))
        )
      ),
      # End Barchart2 tab content.
      # Begin Barchart3 tab content.
      tabItem(tabName = "barchart3",
              tabsetPanel(
                tabPanel("Data",  
                         radioButtons("rb3", "Get data from:",
                                      c("SQL" = "SQL"), inline=T),
                         actionButton(inputId = "click3",  label = "To get data, click here"),
                         hr(), # Add space after button.
                         DT::dataTableOutput("data3")
                ),
                tabPanel("Barchart", plotOutput("plot3", height=1500))
              )
      )
      # End Barchart3 tab content.
    )
  )
)

