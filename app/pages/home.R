home <<- fluidRow(
  box(plotOutput("plot1", height = 250)),
  
  box(
    title = "Controls",
    sliderInput("slider", "Number of observations:", 1, 100, 50)
  )
)

home_server <<- function(input){
  
  set.seed(122)
  histdata <- rnorm(500)
  
  return(renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  }))
}