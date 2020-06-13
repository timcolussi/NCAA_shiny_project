

library(shinydashboard)

shinyUI(
  dashboardPage(
    skin = "red",
    dashboardHeader(title = "NCAA March Madness"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon('eye')),
        menuItem("All Conferences", tabName = "overall", icon = icon('globe-americas')),
        menuItem("Conference Subsetting", tabName = "subset", icon = icon('chart-bar')),
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
                h2("Is there a relationship between Conference Tournament performance and NCAA Tournament performace?"),
                h3("Every year in March, the NCAA holds it's March Madness basketball tournament to crown a National Champion. \n
                   Prior to this, many conferences also hold a conference tournament to name their Conference Champions. \n
                   I've often wondered \'Is there any correlation to how a team peforms in their conference tourney and how they perform in the Big Dance?\' \n
                   So I decided to dive into the data."),
                h6("Data taken from the NCAA and Google Cloud ML Kaggle competition. \n 
                   (The competition this data was taken from did not occur because the 
                   2020 NCAA Tournament was cancelled due to COVID-19)
                   Conference Tournament data only went back to 2001, so I truncated all the
                   data to 2001.")),
        tabItem(tabName = "overall",
                h3("Overall, There doesn't seem to be too much of a correlation. 
                   It could be said that only one team has ever won more than 3 games in thier Conference
                   tournament and won the National Championship. The reverse can also be said, no team that 
                   has not won at least won Conference Tournament game and won the National Championship.
                   But I want to look deeper."),
                fluidRow(plotOutput("overall_graph"))),
        tabItem(tabName = "subset",
                h4("Every Conference Tournament champion automatically makes the NCAA Tournament.
                   For a lot of conferences, this is the only team that makes it into the field.
                   These teams, also, often do not win a single game. These conferences should be
                   filtered out to really get at the underlying question. Here I averaged the number of teams
                   that make it each year for each conference and filtered out those that average less than 1.5.
                   The reasoning is that more often than not these conferences only send one team."),
                fluidRow(box(DT::dataTableOutput("subset_table"), width = 3, height = 500),
                         box(plotOutput("subset_graph"), width = 9))),
        tabItem(tabName = "power_conf",
                h4("After subsetting to conferences averaging more than 1.5, we are down to 12.
                   There does not seem to be any overwhelming trends, just a few possibly interesting
                   observations. We can subset even further to the \'The Power Conferences\'.
                   These are conferences that average sending 4 or more teams to the Dance."),
                fluidRow(box(plotOutput("power_byconf")),
                         box(plotOutput("power_bytype"))),
                h4("Though not strong, the trend seems to suggest that conference champions perform better.
                   Big East and West Coast champions average twice as many wins as other teams that make it,
                   although most of the wins from the WCC champions come from Gonzaga. AAC teams 
                   that win two conference games have a way higher average than other teams; however,
                   the conference has not been around that long so there is not enough data points to 
                   make any major conclusions. Not surprising, power conference teams overall average 
                   more wins in the NCAA Tournament."))
                
      )
    )
  )
)