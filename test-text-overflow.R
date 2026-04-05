library(rtui)

default_text <- paste(
  "This is a long text sample to demonstrate clip, wrap, and ellipsis overflow",
  "policies in tuiOutputText. Edit this value to compare behaviors."
)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputText("help", overflow = "wrap"),
    tuiInputText(id = "messageInput", value = default_text),
    tuiRow(
      tuiBox(
        title = "clip",
        color = "yellow",
        widthPercent = 1 / 3,
        child = tuiOutputText("clipOut", overflow = "clip", minHeight = 5)
      ),
      tuiBox(
        title = "wrap",
        color = "green",
        widthPercent = 1 / 3,
        child = tuiOutputText("wrapOut", overflow = "wrap", minHeight = 5)
      ),
      tuiBox(
        title = "ellipsis",
        color = "magenta",
        widthPercent = 1 / 3,
        child = tuiOutputText("ellipsisOut", overflow = "ellipsis", minHeight = 5)
      )
    )
  ),
  server = function(input, output) {
    output$help <- tuiRenderText(
      "Type in the input field to compare text overflow policies side by side."
    )
    output$clipOut <- tuiRenderText(input$messageInput)
    output$wrapOut <- tuiRenderText(input$messageInput)
    output$ellipsisOut <- tuiRenderText(input$messageInput)
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
