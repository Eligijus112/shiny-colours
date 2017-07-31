library(shiny)
library(imager)
library(jpeg)
library(shinyRGL)

# UI part -----------------------------------------------------------------

ui <- navbarPage(title="Photo tool",
 
# ------------ |--- Documentation  ----------
                 
tabPanel("Documentation",
         
         tags$head(
           tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
         ),
         
         tags$iframe(style="height:600px; width:100%", src="doc/documentation.pdf"),
         
         hr(),
         tags$div(
           includeHTML("footer.html")
         )),

# ------------ |--- Removing colours  ----------
                 
  tabPanel("Decolorize",
           
           tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
           ),
                       
   sidebarLayout(
      sidebarPanel(
         fileInput("myFile",
                     "Please upload your beautiful photo", accept = c('image/png', 'image/jpeg')),
         uiOutput("heigh"),
         actionButton("Black", "Make picture black and white"),
         uiOutput("R.col"),
         uiOutput("G.col"),
         uiOutput("B.col")
      ),
      
      mainPanel(
        fluidRow( splitLayout(cellWidths = c("50%", "50%"), 
        uiOutput("myImage"), 
        uiOutput("plot.black")
        )
      ))
   ), 
   hr(),
   tags$div(
       includeHTML("footer.html")
   )
  ),
  
# ------------ |--- Decomposition of colours  ----------

  tabPanel("Decomposition of colours", 
           
           sidebarLayout(
             sidebarPanel(
               fileInput("myFile2",
                         "Please upload your beautiful photo", accept = c('image/png', 'image/jpeg')),
               uiOutput("heigh2"),
               
               actionButton("Decomposition", "Decompose the colours"),
               uiOutput("numb")
             ), 
             
             mainPanel (fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                                      uiOutput("myImage2"), 
                                                      plotOutput("barplot", height = 500, width=450)
             ))
           )
           
  ),
  hr(),
  tags$div(
    includeHTML("footer.html")
  )
  ),
  
# ------------ |--- Intensify colours  ----------

tabPanel("Intensify colours", 
         sidebarLayout(
           sidebarPanel(
             fileInput("myFile3",
                       "Please upload your beautiful photo", accept = c('image/png', 'image/jpeg')),
             uiOutput("heigh3"),
             actionButton("intense", "Intensify colours"),
             uiOutput("R.liambda"),
             uiOutput("G.liambda"),
             uiOutput("B.liambda")
           ),
           
           mainPanel (fluidRow(splitLayout(cellWidths = c("50%", "50%"), 
                                           # tableOutput("text"),
                                           # tags$head(tags$style("#text {background-color: red; }", media="screen", type="text/css")),
                                           uiOutput("myImage3"),
                                           uiOutput("plot.intense")
                                           
           ))
           ) 
             
         ),
         hr(),
         tags$div(
           includeHTML("footer.html")
         )  
)

)


# Server part -------------------------------------------------------------

server <- function(input, output) {
   
# ------------ |--- Decolorize photo (server)  ----------
  
  observeEvent(input$myFile, {
    inFile <- input$myFile
    if (is.null(inFile))
      return()
    
    do.call(file.remove, list(setdiff(list.files("www", full.names = TRUE), "www/style.css")))
    file.copy(inFile$datapath, file.path("www/orig.jpg") )
    
    output$heigh <- renderUI({
      
      sliderInput("height", "Select size in px", min=0, max=1000, value=500) 
      
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
      
      greyscale <- input$R * p[ , , 1] + input$G *  p[ , , 2] + input$B * p[, , 3] 

      writeJPEG(greyscale, target = "www/bw.jpg")
      
      img(src = "bw.jpg", width = as.integer(input$height))  
      
    })
    
  })
  
 # ------------ |--- Decomposition of colours (server)  ----------
 
  observeEvent(input$myFile2, {
    inFile <- input$myFile2
    if (is.null(inFile))
      return()
 
    do.call(file.remove, list(setdiff(list.files("www", full.names = TRUE), c("www/style.css", "www/default.jpg"))))
    file.copy(inFile$datapath, file.path("www/orig.jpg") )
    
    output$heigh2 <- renderUI({
      
      sliderInput("height2", "Select size in px", min=0, max=1000, value=500) 
      
    })
    
    output$myImage2 <- renderUI({
      
      img(src = "orig.jpg", width = as.integer(input$height2))
      
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
  
  # ------------ |--- Intensify colours  ----------
  
  observeEvent(input$myFile3, {
    inFile <- input$myFile3
    if (is.null(inFile))
      return()
    
    do.call(file.remove, list(setdiff(list.files("www", full.names = TRUE), c("www/style.css", "www/default.jpg"))))
    file.copy(inFile$datapath, file.path("www/orig.jpg") )
    
    output$heigh3 <- renderUI({
      
      sliderInput("height3", "Select size in px", min=0, max=1000, value=500) 
      
    })
    
    output$myImage3 <- renderUI({
      
      img(src = "orig.jpg", width = as.integer(input$height3))
      
    })
    
  })
  
  pic <- reactive({ 
    
    readJPEG("www/orig.jpg")
    
    })
  
  observeEvent(input$intense, {
    
    output$R.liambda <- renderUI({
      
      sliderInput("R.l", "Please select the constant for red", min=0, max=1, value=0)
      
    })
    
    output$G.liambda <- renderUI({
      
      sliderInput("G.l", "Please select the constant for green", min=0, max=1, value=0)
      
    })
    
    output$B.liambda <- renderUI({
      
      sliderInput("B.l", "Please select the constant for blue", min=0, max=1, value=0)
      
    })
    
    output$plot.intense <- renderUI({
      
      p <- readJPEG("www/orig.jpg")
      
      p[ , , 1] <- as.matrix(p[ , , 1] * (1 - input$R.l)) +  input$R.l
      p[ , , 2] <- as.matrix(p[ , , 2] * (1 - input$G.l)) +  input$G.l
      p[ , , 3] <- as.matrix(p[ , , 3] * (1 - input$B.l)) +  input$B.l
      
      writeJPEG(p, target = "www/intense.jpg")
      
      img(src = "intense.jpg", width = as.integer(input$height3))  
      
    })
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

