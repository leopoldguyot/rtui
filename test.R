library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputNumeric("counter"),
    tuiOutputText("name"),
    tuiRow(
      tuiInputButton("Increment", id = "inc"),
      tuiInputButton("Decrement", id = "dec"),
      tuiInputButton("Apply name", id = "applyName")
    ),
    tuiInputText(id = "inputName", placeholder = "Type a name", value = "John")
  ),
  server = function(input, output) {
    counter <- tuiReactive(input$inc - input$dec)

    appliedName <- tuiReactiveEvent(input$applyName, runAtInit = TRUE, {
      input$inputName
    })

    output$counter <- tuiRenderNumeric(counter())
    output$name <- tuiRenderText(paste("Your name is:", appliedName()))
  }
)

tuiRun(app)  # press Escape or Ctrl+Q to quit
