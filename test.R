library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputNumeric("counter"),
    tuiOutputText("message"),
    tuiOutputText("name"),
    tuiRow(
      tuiInputButton("Increment", id = "inc"),
      tuiInputButton("Decrement", id = "dec"),
      tuiInputButton("Apply name", id = "applyName")
    ),
    tuiInputText(id = "nameInput", value = "John")
  ),
  server = function(input, output) {
    counter <- tuiReactive(input$inc - input$dec)
    appliedName <- tuiReactiveEvent(input$applyName, runAtInit = TRUE, {
      input$nameInput
    })
    n <- tuiReactiveVal(0)
    output$message <- tuiRenderText({
      counter()
      n(n() + 1)
      currentName <- tuiIsolate(appliedName())
      paste0("counter changed while name is ", currentName, " n = ", n())
    })
    output$counter <- tuiRenderNumeric(counter(), digits = 0)
    output$name <- tuiRenderText(paste("Your name is:", appliedName()))
  }
)

tuiRun(app)  # press Escape or Ctrl+Q to quit
