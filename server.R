
shinyServer(
  function(input,output) {
    
    output$overall_graph <- renderPlot(by_season, res = 128, height = 600, width = 1000)
    
    output$subset_table <- DT::renderDataTable(conf_ave_teams_adj, rownames=FALSE)
    output$subset_graph <- renderPlot(by_larger_conf + facet_wrap(~ conf_name) + theme(legend.position = "none"),
                                      res = 128, height = 500)
    
    output$power_byconf <- renderPlot(wins_byconf_faceted + ylim(0, 3) + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
                                      res =128)
    output$power_bytype <- renderPlot(wins_by_conftype)
    
    
    
    output$totBox <- renderInfoBox({
      selection_teams <- subset(conf_info, Conference == input$selected)
      tot_teams <- selection_teams[2]
      infoBox("Total Teams", tot_teams, icon = icon("hashtag"), fill = TRUE, color = "red")
    })
   
    output$winsBox <- renderInfoBox({
      selection_wins <- subset(conf_info, Conference == input$selected)
      tot_wins <- selection_wins[3]
      infoBox("Total Wins", tot_wins, icon = icon("dribbble"), fill = TRUE, color = "orange")
    })
   
    output$champsBox <- renderInfoBox({
      selection_champs <- subset(conf_info, Conference == input$selected)
      tot_champs <- selection_champs[4]
      infoBox("Total Champions", tot_champs, icon = icon("trophy"), fill = TRUE, color = "purple")
    })
    
    
    
  }
)