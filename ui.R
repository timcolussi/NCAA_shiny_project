

library(shinydashboard)

shinyUI(
  dashboardPage(
    skin = "red",
    dashboardHeader(title = "NCAA March Madness"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon('eye')),
        menuItem("Overall", tabName = "overall", icon = icon('globe-americas')),
        menuItem("Conference Subset", tabName = "subset", icon = icon('chart-bar')),
        menuItem("Power Conferences", tabName = "power_conf", icon = icon('megaport'))
      )
    ),
    dashboardBody(
      tags$head(
        tags$style(
          HTML(
            '.main-header .logo {
                                font-family: "Century Gothic", Century, "Century Gothic", Century;
                                font-weight: bold;
                                font-size: 14px;
                                                    }'
          )
        )
      ),
      
      
      tabItems(
        tabItem(tabName = "overview",
                h2("Is there a relationship between Conference Tournament performance and NCAA Tournament performace?")),
        tabItem(tabName = "overall",
                fluidRow("A plot showing Conference Tournament wins and NCAA Tournament wins colored by season."),
                fluidRow(plotOutput("overall_graph", 
                                    height = 500, 
                                    width = 800))),
        tabItem(tabName = "subset",
                fluidRow("This is why I subset the data"),
                fluidRow(box(DT::dataTableOutput("subset_table"), width = 3),
                         box(plotOutput("subset_graph"), width = 9))),
        tabItem(tabName = "power_conf",
                fluidRow(box(plotOutput("power_byconf")),
                         box(plotOutput("power_bytype"))))
                
      )
    )
  )
)