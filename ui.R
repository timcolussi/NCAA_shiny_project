

library(shinydashboard)

shinyUI(
  dashboardPage(
    skin = "red",
    dashboardHeader(title = "March Madness"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon('eye')),
        menuItem("Overall", tabName = "overall", icon = icon('globe-americas')),
        menuItem("Conference Subset", tabName = "subset", icon = icon('chart-bar')),
        menuItem("Power Conferences", tabName = "power_conf", icon = icon('megaport'))
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "overview",
                "Text about data and question"),
        tabItem(tabName = "overall",
                fluidRow("A plot showing Conference Tournament wins and NCAA Tournament wins colored by season."),
                fluidRow(plotOutput("overall_graph", 
                                    height = 500, 
                                    width = 800))),
        tabItem(tabName = "subset",
                fluidRow("This is why I subset the data"),
                fluidRow(box(DT::dataTableOutput("subset_table")),
                         box(plotOutput("subset_graph")))),
        tabItem(tabName = "power_conf",
                fluidRow(box(plotOutput("power_byconf"))),
                fluidRow(box(plotOutput("power_bytype"))))
      )
    )
  )
)