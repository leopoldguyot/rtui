library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputText("help", overflow = "wrap"),
    tuiRow(
      tuiInputButton("Add row", id = "addRows", widthPercent = 0.5),
      tuiInputButton("Remove row", id = "removeRows", widthPercent = 0.5)
    ),
    tuiRow(
      tuiBox(
        title = "clip",
        color = "yellow",
        widthPercent = 0.5,
        child = tuiOutputTable(
          "clipTable",
          overflowX = "clip",
          overflowY = "clip"
        )
      ),
      tuiBox(
        title = "scroll (recommended)",
        color = "green",
        widthPercent = 0.5,
        child = tuiOutputTable(
          "scrollTable",
          overflowX = "scroll",
          overflowY = "scroll"
        )
      ),
      minHeight = 14
    )
  ),
  server = function(input, output) {
    row_count <- tuiReactive({
      max(5L, 8L + input$addRows - input$removeRows)
    })

    table_data <- tuiReactive({
      n <- row_count()
      data.frame(
        feature = paste0("feature-", seq_len(n)),
        status = ifelse(seq_len(n) %% 2L == 0L, "active", "pending"),
        notes = paste(
          "This row has intentionally long text to demonstrate horizontal overflow.",
          "Row", seq_len(n)
        ),
        score = seq_len(n) * 7L,
        stringsAsFactors = FALSE
      )
    })

    output$help <- tuiRenderText(
      paste(
        "This demo compares table overflow policies.",
        "Left table clips overflowing content.",
        "Right table is scrollable: Tab to focus it, then Ctrl+Arrow/Page/Home/End, mouse wheel, or drag the scrollbar.",
        paste("Use Add/Remove row to change vertical overflow. Current rows:", row_count())
      )
    )
    output$clipTable <- tuiRenderTable(table_data())
    output$scrollTable <- tuiRenderTable(table_data(), rowNames = TRUE)
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
