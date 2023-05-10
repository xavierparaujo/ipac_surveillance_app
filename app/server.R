# Define server logic required to draw a histogram
function(input, output, session) {
  output$plot1 <- home_server(input)
}
