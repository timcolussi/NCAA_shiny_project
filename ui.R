

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
      ),
      selectizeInput("selected",
                     "Select Conference",
                     choice)
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "overview",
                "Text about data and question"),
        tabItem(tabName = "overall",
                "scatterplot of all data"),
        tabItem(tabName = "subset",
                "graphs showing the subset conferences along with along with explanation and table"),
        tabItem(tabName = "power_conf",
                "graph showing average wins of power conferences along with dropdown menu")
      )
    )
  )
)