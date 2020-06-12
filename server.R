
shinyServer(
  function(input,output) {
    output$subset_table <- DT::renderDataTable(conf_ave_teams, rownames=FALSE)
    output$subset_graph <- renderPlot(by_larger_conf + facet_wrap(~ conf_name) + theme(legend.position = "none"))
    
    
    
  }
)