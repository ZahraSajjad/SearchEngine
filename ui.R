ui <- fluidPage(
  useShinyalert(),
  useShinyjs(),
  tags$head(includeCSS("www/styles.css")),
    tags$head(tags$script(src = "enter_button.js")), 
  
  
    div(class = "jumbotron",style = "background-image:url(wall.jpg);
                                     background-size:cover",
      tags$h1("PubMed 2012 Search Engine",
              style="text-align:center;
                     color:white;
                     font-family:Arial;"),
      tags$p("Search For Similar Articles",
             style="text-align:center;
                    color:white")),

  fluidRow(
    column(4,offset = 1,
      textAreaInput(inputId = "abstract",label = "Write an abstract!",resize = "none",
                    height = 130),
      actionButton(inputId = "search",label = "Find"))
      ,
      
    column(5, DT::dataTableOutput("result"))
  )
)