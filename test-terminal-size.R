library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiBox(
      title = "Terminal size reactivity",
      color = "cyan",
      margin = 1L,
      child = tuiColumn(
        tuiOutputText("terminalInfo"),
        tuiShowIf(
          tuiBox(
            title = "Compact layout",
            style = "double",
            color = "yellow",
            child = tuiOutputText("compactInfo", overflow = "wrap")
          ),
          maxTerminalWidth = 100
        ),
        tuiShowIf(
          tuiBox(
            title = "Wide layout",
            style = "double",
            color = "green",
            child = tuiOutputText("wideInfo", overflow = "wrap")
          ),
          minTerminalWidth = 101
        ),
        tuiShowIf(
          tuiBox(
            title = "Tall terminal bonus",
            color = "magenta",
            child = tuiOutputText("tallInfo", overflow = "wrap")
          ),
          minTerminalHeight = 35
        )
      )
    )
  ),
  server = function(input, output) {
    output$terminalInfo <- tuiRenderText({
      paste("Current terminal:", input$terminalWidth, "x", input$terminalHeight)
    })
    output$compactInfo <- tuiRenderText(
      "Visible only when terminal width is <= 100 columns."
    )
    output$wideInfo <- tuiRenderText(
      "Visible only when terminal width is >= 101 columns."
    )
    output$tallInfo <- tuiRenderText(
      "Visible only when terminal height is >= 35 rows."
    )
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
