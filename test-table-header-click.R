library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputText("help", overflow = "wrap"),
    tuiInputButton("Reset sort", id = "resetSort"),
    tuiOutputTable(
      "sortableTable",
      headerClickId = "tableHeaderClick",
      overflowX = "scroll",
      overflowY = "scroll"
    )
  ),
  server = function(input, output) {
    base_data <- tuiReactive({
      data.frame(
        feature = c("auth", "billing", "cache", "deploy", "events", "frontend"),
        status = c("active", "pending", "paused", "active", "pending", "active"),
        owner = c("core", "ops", "api", "ops", "api", "ui"),
        score = c(74L, 22L, 61L, 89L, 35L, 48L),
        stringsAsFactors = FALSE
      )
    })

    sort_state <- tuiReactiveVal(list(column = NULL, descending = FALSE))

    tuiObserveEvent(input$resetSort, {
      sort_state(list(column = NULL, descending = FALSE))
    })

    tuiObserveEvent(input$tableHeaderClick, {
      click <- input$tableHeaderClick
      column <- if (is.null(click)) NULL else click$column
      tuiReq(
        !is.null(column),
        is.character(column),
        length(column) == 1L,
        !is.na(column),
        nzchar(column)
      )

      current <- sort_state()
      same_column <- identical(current$column, column)
      sort_state(list(
        column = column,
        descending = if (same_column) !isTRUE(current$descending) else FALSE
      ))
    })

    sorted_data <- tuiReactive({
      df <- base_data()
      state <- sort_state()
      column <- state$column
      if (is.null(column) || !column %in% names(df)) {
        return(df)
      }

      order_index <- order(
        df[[column]],
        decreasing = isTRUE(state$descending),
        na.last = TRUE
      )
      df[order_index, , drop = FALSE]
    })

    output$help <- tuiRenderText({
      state <- sort_state()
      current_sort <- if (is.null(state$column)) {
        "none"
      } else if (isTRUE(state$descending)) {
        paste0(state$column, " (desc)")
      } else {
        paste0(state$column, " (asc)")
      }

      paste(
        "Header click sorting example.",
        "Click any table header with the mouse to sort by that column.",
        "Click the same header again to toggle asc/desc.",
        "Use Reset sort to clear sorting.",
        paste("Current sort:", current_sort)
      )
    })

    output$sortableTable <- tuiRenderTable(
      sorted_data(),
      rowBorder = FALSE,
      colBorder = TRUE,
      headerRowBorder = TRUE,
      headerColBorder = FALSE,
      borderStyle = "rounded",
      columnAlign = c(score = "right")
    )
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
