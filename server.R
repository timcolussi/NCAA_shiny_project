
shinyServer(
  function(input,output) {
    
    output$overall_graph <- renderPlot(by_season, res = 128, height = 600, width = 1000)
    
    output$subset_table <- DT::renderDataTable(conf_ave_teams_adj, rownames=FALSE)
    output$subset_graph <- renderPlot(by_larger_conf + facet_wrap(~ conf_name) + theme(legend.position = "none"),
                                      res = 128, height = 600)
    
    output$power_byconf <- renderPlot(wins_byconf_faceted + ylim(0, 3) + theme(axis.text.x = element_text(angle = 45, hjust = 1)),
                                      res =128)
    output$power_bytype <- renderPlot(wins_by_conftype)
    
    
    
    
    
  }
)