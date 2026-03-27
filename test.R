library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiBox(
      title = "Counter controls",
      color = "cyan",
      backgroundColor = "#101820",
      style = "rounded",
      titleStyle = "header",
      titleAlign = "center",
      margin = 1L,
      child = tuiColumn(
        tuiOutputNumeric("counter"),
        tuiOutputText("message"),
        tuiOutputText("name"),
        tuiRow(
          tuiInputButton("Increment", id = "inc", color = "red"),
          tuiInputButton("Decrement", id = "dec"),
          tuiInputButton("Apply name", id = "applyName")
        ),
        tuiInputText(id = "nameInput", value = "John", multiline = FALSE)
      )
    ),
    tuiRow(
      tuiBox(
        title = "Left",
        titleStyle = "border",
        titleAlign = "left",
        color = "green",
        style = "light",
        margin = 1L,
        child = tuiOutputText("borderLeftDemo")
      ),
      tuiBox(
        title = "Center",
        titleStyle = "border",
        titleAlign = "center",
        color = "yellow",
        style = "light",
        margin = 1L,
        child = tuiOutputText("borderCenterDemo")
      ),
      tuiBox(
        title = "Right",
        titleStyle = "border",
        titleAlign = "right",
        color = "magenta",
        style = "light",
        margin = 1L,
        child = tuiOutputText("borderRightDemo")
      )
    )
  ),
  server = function(input, output) {
    counter <- tuiReactive(input$inc - input$dec)
    appliedName <- tuiReactiveEvent(input$applyName, runAtInit = TRUE, {
      input$nameInput
    })
    n <- tuiReactiveVal(0L)

    tuiObserveEvent(counter(), {
      n(n() + 1L)
    }, runAtInit = FALSE)

    output$message <- tuiRenderText({
      counter()
      currentName <- tuiIsolate(appliedName())
      paste0("counter changed while name is ", currentName, " n = ", n())
    })
    output$counter <- tuiRenderNumeric(counter(), digits = 0)
    output$name <- tuiRenderText(paste("Your name is:", appliedName()))
    output$borderLeftDemo <- tuiRenderText('titleAlign = "left"')
    output$borderCenterDemo <- tuiRenderText('titleAlign = "center"')
    output$borderRightDemo <- tuiRenderText('titleAlign = "right"')
  }
)

tuiRun(app)  # press Escape or Ctrl+Q to quit
