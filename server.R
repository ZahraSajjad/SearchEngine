


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  query_vector = eventReactive(input$search,{
    
    
    vectorize_query(input$abstract,community)    

  })
  
  observeEvent(input$search,{
    if (all(query_vector()==0)){
      shinyalert("No results found!", "Please check your words. ", type = "error")
    }
    
  })
  

  sorted_doc = reactive({
    
    search_index(query_vector(), Mydtm)
  })
  
  
  output$result = renderDataTable({
    
    if(is.null(query_vector())) return()
    if(all(query_vector() == 0)) return()
    
    DT::datatable(sorted_doc(),options = list(searching = FALSE,lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
                                              pageLength = 5))
    
  })
  
  
})
