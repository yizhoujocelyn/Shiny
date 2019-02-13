county.origin<-read.csv("/Users/yizhou/Desktop/shiny/county_facts.csv",header=TRUE)
#delete the the column of useless information;
county<-county.origin[,c(2,3,9:16,23,24,33,35)]
#rownames(county)<-county$area_name
county[,8]<-county[,8]+county[,9]+county[,10]
county[,c(9,10)]<-NULL
county[,9]<-100-county[,9]
colnames(county)<-c('county','state','below18','above65','female','white','black','other','belowHS','aboveBA','income','poverty')
dataset<-county

library(shiny)

server<-function(input, output, session) {
  datasetInput <- reactive({
    switch(input$dataset,
           "CA" = CA,
           "LA" = LA,
           "cars" = cars)
  })
  
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    county[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  
}

ui<-pageWithSidebar(
  headerPanel('County k-means clustering'),
  sidebarPanel(
    # Input: Selector for choosing dataset ----
    selectInput(inputId = "dataset",
                label = "Choose a dataset:",
                choices = c("CA", "LA")),
    selectInput('xcol', 'X Variable', names(county)),
    selectInput('ycol', 'Y Variable', names(county),
                selected=names(iris)[[2]]),
    numericInput('clusters', 'Cluster count', 3,
                 min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)

shinyApp(ui,server)




