library(shiny)
library(imager)
library(jpeg)

# Define UI for application that draws a histogram
ui <- navbarPage(title="Photo tool",
                   
  tabPanel("Decolorize", 
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
           ),
           # includeCSS("www/style.css"),            
   sidebarLayout(
      sidebarPanel(
         fileInput("myFile",
                     "Please upload your beautiful photo", accept = c('image/png', 'image/jpeg')),
         uiOutput("heigh"),
         actionButton("Black", "Make picture black and white"),
         
         uiOutput("R.col"),
         uiOutput("G.col"),
         uiOutput("B.col"),
         actionButton("refresh", "Refresh")
      ),
      
      mainPanel(
        
        fluidRow( splitLayout(cellWidths = c("50%", "50%"), 
        uiOutput("myImage"), 
        uiOutput("plot.black")
        )
      ))
   )
  ),
  
  tabPanel("Decomposition of colours", 
           
           sidebarLayout(
             sidebarPanel(
               fileInput("myFile2",
                         "Please upload your beautiful photo", accept = c('image/png', 'image/jpeg')),
               uiOutput("heigh2"),
               
               actionButton("Decomposition", "Show the decomposition of colours of the photograph"),
               uiOutput("numb"),
               actionButton("refresh", "Refresh")
             ), 
             
             mainPanel (        fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                                      uiOutput("myImage2"), 
                                                      plotOutput("barplot", height = 500, width=450)
             ))
           )      
           
  )
  
  )  
  
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
  observeEvent(input$myFile, {
    inFile <- input$myFile
    if (is.null(inFile))
      return()
    
    do.call(file.remove, list(setdiff(list.files("www", full.names = TRUE), "www/style.css")))
    file.copy(inFile$datapath, file.path("www/orig.jpg") )
    
    output$heigh <- renderUI({
      
      sliderInput("height", "Select height in px", min=0, max=1000, value=500) 
      
    })
    
    output$myImage <- renderUI({
      
      img(src = "orig.jpg", width = as.integer(input$height))
      
    })
    
  })
 

observeEvent(input$refresh, {
  
  output$myImage <- renderUI({
    
    img(src = "orig.jpg", width = as.integer(input$height))
    
  })
})

  observeEvent(input$Black, {
    
    output$R.col <- renderUI({
      
      sliderInput("R", "Please select the filter for the red colour", min=0, max=1, value=0.33)
      
    })
    
    output$G.col <- renderUI({
      
      sliderInput("G", "Please select the filter for the green colour", min=0, max=1, value=0.33)
      
    })
    
    output$B.col <- renderUI({
      
      sliderInput("B", "Please select the filter for the blue colour", min=0, max=1, value=0.33)
      
    })
    
    output$plot.black <- renderUI({
      
      p <- readJPEG("www/orig.jpg")
      
      # Making the photo black and white ----------------------------------------
      
      ## We will use the luminosity algorithm 
      ## r * R + g * G + b * B
      
      greyscale <- input$R * p[ , , 1] + input$G *  p[ , , 2] + input$B * p[, , 3] 

      writeJPEG(greyscale, target = "www/bw.jpg")
      
      img(src = "bw.jpg", width = as.integer(input$height))  
      
    })
    
  })
  
  # Decomposition of colours ------------------------------------------------
  
  observeEvent(input$myFile2, {
    inFile <- input$myFile2
    if (is.null(inFile))
      return()
    
    do.call(file.remove, list(setdiff(list.files("www", full.names = TRUE), "www/style.css")))
    file.copy(inFile$datapath, file.path("www/orig.jpg") )
    
    output$heigh2 <- renderUI({
      
      sliderInput("height2", "Select height in px", min=0, max=1000, value=600) 
      
    })
    
    output$myImage2 <- renderUI({
      
      img(src = "orig.jpg", width = as.integer(input$height2))
      
    })
    
  })
  
  observe({
    output$myImage2 <- renderUI({
      
      img(src = "orig.jpg", width = as.integer(input$height2))
      
    })
  })
  
  observeEvent(input$refresh, {
    
    output$myImage2 <- renderUI({
      
      img(src = "orig.jpg", width = as.integer(input$height))
      
    })
  })
  
  
  observeEvent(input$Decomposition, {
    
    output$numb <- renderUI({
      
      sliderInput("numb.cols", "Please select the number of main colours", min=0, max=30, value=10)
      
    })
    
    output$barplot <- renderPlot({
      
      p <- readJPEG("www/orig.jpg")
      set.seed(43)
      
      pp <- cbind(as.vector(unlist(p[ , , 1])), as.vector(unlist(p[ , , 2])), as.vector(unlist(p[ , , 3]))) %>% as.data.frame
      names(pp) <- c("R", "G", "B")
      
      kk <- kmeans(pp, centers=input$numb.cols)
      pp$cluster <- kk$cluster 
      
      pp <- ddply(pp, ~cluster, function(xframe){
        
        xframe <<- xframe
        
        xframe$mean.R <- mean(xframe$R)
        xframe$mean.G <- mean(xframe$G)
        xframe$mean.B <- mean(xframe$B)
        
        xframe$col.name <- rgb(mean(xframe$R), mean(xframe$G), mean(xframe$B))
        
        return(xframe)
        
      })
      
      pp <- arrange(pp, cluster)
      
      describtive.data <- ddply(pp, ~cluster, function(xframe){
        
        xframe <<- xframe
        
        xframe$length <- dim(xframe)[1]
        
        return(xframe[nrow(xframe), c("col.name", "length")])
        
      })
      
      describtive.data <- arrange(describtive.data, length)
      
      par(mai=c(1,2,1,1))
     
      barplot(describtive.data$length, col = describtive.data$col.name,
              names.arg = describtive.data$col.name, 
              horiz = T, las=1,  xaxt='n', main="Decomposition of colours")
      
    })
    
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)

