library(rtui)

app <- tuiApp(
    ui = tuiColumn(
        tuiBox(
            title = "Counter controls (size constraints demo)",
            color = "cyan",
            style = "rounded",
            titleStyle = "header",
            titleAlign = "center",
            margin = 1L,
            heightPercent = 0.65,
            minHeight = 12,
            child = tuiColumn(
                tuiOutputNumeric("counter"),
                tuiOutputText("message", overflow = "wrap"),
                tuiOutputText("name"),
                tuiRow(
                    tuiInputButton(
                        "Increment",
                        id = "inc",
                        color = "red",
                        widthPercent = 1 / 3
                    ),
                    tuiInputButton(
                        "Decrement",
                        id = "dec",
                        widthPercent = 1 / 3
                    ),
                    tuiInputButton(
                        "Apply name",
                        id = "applyName",
                        widthPercent = 1 / 3
                    )
                ),
                tuiInputCheckbox("Enable message suffix", id = "enabledSuffix", value = TRUE),
                tuiInputText(
                    id = "nameInput",
                    value = "John",
                    multiline = FALSE,
                    width = 32
                )
            )
        ),
        tuiRow(
            tuiBox(
                title = "20%",
                titleStyle = "border",
                titleAlign = "left",
                color = "green",
                style = "light",
                margin = 0L,
                widthPercent = 0.2,
                child = tuiOutputText("sizeLeftDemo", minHeight = 3)
            ),
            tuiBox(
                title = "50%",
                titleStyle = "border",
                titleAlign = "center",
                color = "yellow",
                style = "light",
                margin = 0L,
                widthPercent = 0.5,
                child = tuiOutputText("sizeCenterDemo", overflow = "ellipsis", minHeight = 3)
            ),
            tuiShowIf(
                tuiBox(
                    title = "30%",
                    titleStyle = "border",
                    titleAlign = "right",
                    color = "magenta",
                    style = "light",
                    margin = 1L,
                    widthPercent = 0.3,
                    child = tuiOutputText("sizeRightDemo", minHeight = 3)
                ),
                minTerminalWidth = 90
            ),
            overflowX = "clip",
            heightPercent = 0.35,
            minHeight = 5
        )
    ),
    server = function(input, output) {
        counter <- tuiReactive(input$inc - input$dec)
        appliedName <- tuiReactiveEvent(input$applyName, runAtInit = TRUE, {
            input$nameInput
        })
        n <- tuiReactiveVal(0L)

        tuiObserveEvent(
            counter(),
            {
                n(n() + 1L)
            },
            runAtInit = FALSE
        )

        output$message <- tuiRenderText({
            counter()
            currentName <- tuiIsolate(appliedName())
            suffix <- if (isTRUE(input$enabledSuffix)) " [checkbox: on]" else ""
            paste0("counter changed while name is ", currentName, " n = ", n(), suffix)
        })
        output$counter <- tuiRenderNumeric(counter(), digits = 0)
        output$name <- tuiRenderText(paste("Your name is:", appliedName()))
        output$sizeLeftDemo <- tuiRenderText("widthPercent = 0.2")
        output$sizeCenterDemo <- tuiRenderText("widthPercent = 0.5")
        output$sizeRightDemo <- tuiRenderText("widthPercent = 0.3")
    }
)

tuiRun(app, overflow = "scroll") # press Escape or Ctrl+Q to quit
