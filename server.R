
shinyServer(
  function(input,output) {
    
    output$overall_graph <- renderPlot(by_season)
    
    output$subset_table <- DT::renderDataTable(conf_ave_teams_adj, rownames=FALSE)
    output$subset_graph <- renderPlot(by_larger_conf + facet_wrap(~ conf_name) + theme(legend.position = "none"))
    
    output$power_byconf <- renderPlot(wins_byconf_faceted + ylim(0, 3) + theme_bw())
    output$power_bytype <- renderPlot(wins_by_conftype)
    
    
    
    
    
  }
)