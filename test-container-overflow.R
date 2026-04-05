library(rtui)

long_line <- paste(
  rep("abcdefghijklmnopqrstuvwxyz0123456789", 3),
  collapse = " "
)
long_block <- paste(rep(long_line, 7), collapse = "\n")

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputText("help", overflow = "wrap"),
    tuiRow(
      tuiBox(
        title = "visible",
        color = "green",
        widthPercent = 1 / 3,
        overflowX = "visible",
        overflowY = "visible",
        child = tuiOutputText("visibleOut", width = 90, overflow = "clip", minHeight = 5)
      ),
      tuiBox(
        title = "clip",
        color = "yellow",
        widthPercent = 1 / 3,
        overflowX = "clip",
        overflowY = "clip",
        child = tuiOutputText("clipOut", width = 90, overflow = "clip", minHeight = 5)
      ),
      tuiBox(
        title = "scroll",
        color = "magenta",
        widthPercent = 1 / 3,
        overflowX = "scroll",
        overflowY = "scroll",
        child = tuiColumn(
          tuiInputText(id = "scrollFocus1", value = "focus field 1", width = 60),
          tuiInputText(id = "scrollFocus2", value = "focus field 2", width = 60),
          tuiInputText(id = "scrollFocus3", value = "focus field 3", width = 60),
          tuiInputText(id = "scrollFocus4", value = "focus field 4", width = 60),
          tuiInputText(id = "scrollFocus5", value = "focus field 5", width = 60),
          tuiInputText(id = "scrollFocus6", value = "focus field 6", width = 60),
          tuiOutputText("scrollOut", overflow = "clip")
        )
      ),
      minHeight = 16
    )
  ),
  server = function(input, output) {
    output$help <- tuiRenderText(
      paste(
        "Left uses overflow visible and intentionally over-wide text.",
        "Middle uses overflow clip with the same over-wide text.",
        "Right is scrollable: use Tab to move focus through its fields."
      )
    )
    output$visibleOut <- tuiRenderText(long_block)
    output$clipOut <- tuiRenderText(long_block)
    output$scrollOut <- tuiRenderText(long_block)
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
