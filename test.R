library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiRenderText(output$counter),
    tuiRenderText(output$name),
    tuiRow(
      tuiInputButton("Increment", id = "inc"),
      tuiInputButton("Decrement", id = "dec"),
      tuiInputButton("Apply name", id = "applyName")
    ),
    tuiInputText(id = "inputName", placeholder = "Type a name", value = "John")
  ),
  server = function(input, output) {
    output$counter <- input$inc - input$dec
    output$inputName <- input$inputName

    tuiObserveEvent(input$applyName, runAtInit = TRUE, {
      output$name <- paste("Your name is:", input$inputName)
    })
  }
)

tuiRun(app)  # press Escape or Ctrl+Q to quit
