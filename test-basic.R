library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiBox(
      title = "Basic counter",
      color = "cyan",
      style = "rounded",
      child = tuiColumn(
        tuiOutputNumeric("counter"),
        tuiOutputText("message", overflow = "wrap"),
        tuiRow(
          tuiInputButton("Increment", id = "inc", widthPercent = 0.5),
          tuiInputButton("Decrement", id = "dec", widthPercent = 0.5)
        ),
        tuiInputText(id = "nameInput", value = "Ada")
      )
    )
  ),
  server = function(input, output) {
    counter <- tuiReactive(input$inc - input$dec)

    output$counter <- tuiRenderNumeric(counter())
    output$message <- tuiRenderText({
      paste("Hello", input$nameInput, "- counter =", counter())
    })
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
