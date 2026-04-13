library(rtui)

interval_choices <- c("100", "250", "500", "1000")

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputText("title"),
    tuiOutputText("help", overflow = "wrap"),
    tuiRow(
      tuiInputButton("Start / Pause", id = "toggleRun", color = "green", widthPercent = 0.34),
      tuiInputButton("Reset shown ticks", id = "resetTicks", color = "yellow", widthPercent = 0.33),
      tuiInputButton("Cycle interval", id = "cycleInterval", color = "cyan", widthPercent = 0.33)
    ),
    tuiRow(
      tuiInputButton("Fast (100ms)", id = "setFast", widthPercent = 0.5),
      tuiInputButton("Slow (1000ms)", id = "setSlow", widthPercent = 0.5)
    ),
    tuiInputDropdown(
      id = "intervalChoice",
      choices = interval_choices,
      selected = "250"
    ),
    tuiOutputNumeric("rawTicks"),
    tuiOutputNumeric("shownTicks"),
    tuiOutputText("status", overflow = "wrap")
  ),
  server = function(input, output) {
    timer100 <- tuiReactiveTimer(100)
    timer250 <- tuiReactiveTimer(250)
    timer500 <- tuiReactiveTimer(500)
    timer1000 <- tuiReactiveTimer(1000)

    running <- tuiReactiveVal(TRUE)
    offset <- tuiReactiveVal(0L)
    shown_ticks <- tuiReactiveVal(0L)

    active_raw_ticks <- tuiReactive({
      switch(
        input$intervalChoice,
        "100" = timer100(),
        "250" = timer250(),
        "500" = timer500(),
        "1000" = timer1000(),
        timer250()
      )
    })

    tuiObserve({
      if (isTRUE(running())) {
        shown_ticks(active_raw_ticks() - offset())
      }
    })

    tuiObserveEvent(input$toggleRun, {
      running(!isTRUE(running()))
    })

    tuiObserveEvent(input$resetTicks, {
      offset(active_raw_ticks())
      shown_ticks(0L)
    })

    tuiObserveEvent(input$cycleInterval, {
      current <- input$intervalChoice
      idx <- match(current, interval_choices)
      if (is.na(idx)) idx <- 1L
      next_idx <- (idx %% length(interval_choices)) + 1L
      tuiUpdateDropdownInput("intervalChoice", selected = interval_choices[[next_idx]])
    })

    tuiObserveEvent(input$setFast, {
      tuiUpdateDropdownInput("intervalChoice", selected = "100")
    })

    tuiObserveEvent(input$setSlow, {
      tuiUpdateDropdownInput("intervalChoice", selected = "1000")
    })

    output$title <- tuiRenderText("tuiReactiveTimer demo")
    output$help <- tuiRenderText(
      paste(
        "rawTicks shows the selected timer's total ticks.",
        "shownTicks can be paused and reset without stopping timer registration.",
        "Fast/Slow/Cycle use tuiUpdateDropdownInput() to update the selected interval."
      )
    )

    output$rawTicks <- tuiRenderNumeric(active_raw_ticks(), digits = 0)
    output$shownTicks <- tuiRenderNumeric(shown_ticks(), digits = 0)
    output$status <- tuiRenderText({
      paste(
        "intervalMs =", input$intervalChoice,
        "| running =", if (isTRUE(running())) "TRUE" else "FALSE",
        "| shownTicks =", shown_ticks()
      )
    })
  }
)

tuiRun(app, overflow = "clip") # press Escape or Ctrl+Q to quit
