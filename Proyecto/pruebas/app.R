library(shiny)

ui = fluidPage(
  titlePanel("Image output"),
  
  fluidRow(
    column(4, wellPanel(
      radioButtons("picture", "Picture:",
                   c("chainring", "face"))
    )),
    column(4,
           imageOutput("image2")
    )
  )
)

library(png) # For writePNG function

function(input, output, session) {
  # image2 sends pre-rendered images
  output$image2 <- renderImage({
    if (is.null(input$picture))
      return(NULL)
    
    if (input$picture == "face") {
      return(list(
        src = "image/Inicio1.png",
        contentType = "image/png",
      ))
    } else if (input$picture == "chainring") {
      return(list(
        src = "image/Inicio2",
        filetype = "image/png",
      ))
    }
    
  }, deleteFile = FALSE)
}

shinyApp(ui, server)

# Run the application 
shinyApp(ui = ui, server = server)


