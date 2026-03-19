library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputNumeric("counter"),
    tuiOutputText("name"),
    tuiOutputText("inputName"),
    tuiOutputText("events"),
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
      if (identical(input$inputName, "")) "John" else input$inputName
    })

    events <- tuiReactiveVal("init")
    tuiObserveEvent(counter(), {
      currentName <- tuiIsolate(appliedName())
      events(paste("counter changed while name is", currentName))
    })
    tuiObserveEvent(appliedName(), runAtInit = TRUE, {
      events("name applied")
    })

    output$counter <- tuiRenderNumeric(counter())
    output$inputName <- tuiRenderText(input$inputName)
    output$name <- tuiRenderText(paste("Your name is:", appliedName()))
    output$events <- tuiRenderText(events())
  }
)

tuiRun(app)  # press Escape or Ctrl+Q to quit
