<!-- badges: start -->
  [![R-CMD-check](https://github.com/leopoldguyot/rtui/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/leopoldguyot/rtui/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

# rtui

A declarative Terminal User Interface (TUI) framework for R, backed by [FTXUI](https://github.com/ArthurSonzogni/FTXUI) via Rcpp.

## Installation

### Prerequisites

You need a C++17-capable compiler toolchain.

**Linux:** install your distro's standard C/C++ build tools (`gcc`/`g++`, `make`).

**macOS:** install Xcode Command Line Tools (`xcode-select --install`).

**Windows:** install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).

You also need the `Rcpp` R package:
```r
install.packages("Rcpp")
```

### Install from GitHub

```r
# install.packages("remotes")
remotes::install_github("leopoldguyot/rtui")
```

## Quick start

```r
library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputNumeric("counter"),
    tuiOutputText("message"),
    tuiOutputText("name"),
    tuiRow(
      tuiInputButton("Increment", id = "inc"),
      tuiInputButton("Decrement", id = "dec"),
      tuiInputButton("Apply name", id = "applyName")
    ),
    tuiInputCheckbox("Enabled", id = "enabled", value = TRUE),
    tuiInputText(id = "nameInput", value = "John")
  ),
  server = function(input, output) {
    counter <- tuiReactive(input$inc - input$dec)
    appliedName <- tuiReactiveEvent(input$applyName, runAtInit = TRUE, {
      input$nameInput
    })

    message <- tuiReactive({
      counter()
      currentName <- tuiIsolate(appliedName())
      enabledState <- if (isTRUE(input$enabled)) "enabled" else "disabled"
      paste0("counter changed while name is ", currentName, " [", enabledState, "]")
    })

    output$message <- tuiRenderText(message())
    output$counter <- tuiRenderNumeric(counter(), digits = 0)
    output$name <- tuiRenderText(paste("Your name is:", appliedName()))
  }
)

tuiRun(app, overflow = "clip")  # press Escape or Ctrl+Q to quit
```

## API

| Function | Description |
|---|---|
| `tuiApp(ui, server)` | Define a TUI application |
| `tuiRun(app, overflow = "clip")` | Start the app (blocking), with terminal overflow policy (`"clip"`, `"scroll"`, or `"block"`) |
| `tuiColumn(...)` | Stack components vertically |
| `tuiRow(...)` | Place components side by side |
| `tuiShowIf(child, ...)` | Conditionally show a subtree based on terminal-size breakpoints |
| `tuiBox(child, title = NULL, color = NULL, style = "rounded", titleStyle = "header", titleAlign = "left", margin = 0)` | Wrap a component in a configurable border with title/layout options |
| `tuiOutputText("id", overflow = "clip")` / `tuiOutputNumeric("id")` / `tuiOutputTable("id")` | Display `output$id` in the UI as text/numeric/table output |
| `tuiInputButton(label, id, color = NULL)` | Button that triggers a handler (optional text color) |
| `tuiInputCheckbox(label, id, value = FALSE)` | Checkbox that toggles a logical `input$id` value |
| `tuiInputText(id, placeholder, value, multiline = FALSE)` | One-line text input by default (`multiline = TRUE` allows newlines) |
| `tuiRenderText(expr)` / `tuiRenderNumeric(expr, digits = NULL)` / `tuiRenderTable(expr)` | Build renderers assigned to `output$...` |
| `tuiReactive(expr)` / `tuiReactiveVal(value)` | Define graph-tracked reactive expressions and mutable values (auto invalidation of dependents) |
| `tuiReactiveEvent(event, expr, runAtInit = FALSE)` | Event-scoped reactive expression that re-runs when its event source invalidates |
| `tuiReq(...)` | Stop current reactive/render evaluation when required values are falsy |
| `tuiIsolate(expr)` | Read reactive values without registering graph dependencies |
| `tuiObserve(expr)` | Register an observer that runs when its dependencies invalidate |
| `tuiObserveEvent(event, expr, runAtInit = FALSE)` | Register an observer that runs only when the event source invalidates |

All exported UI builders (`tuiRow`, `tuiColumn`, `tuiBox`, outputs, and inputs) accept optional sizing arguments: `width`, `height`, `minHeight`, `maxHeight`, `widthPercent`, and `heightPercent`.

`tuiRow()`, `tuiColumn()`, and `tuiBox()` also accept per-axis overflow policies with `overflowX`/`overflowY` (`"visible"`, `"clip"`, `"scroll"`). Scroll containers expose in-place scrollbars and support keyboard/mouse scrolling. During runtime, `input$terminalWidth` and `input$terminalHeight` are available as reactive inputs so server logic can react to terminal resizing.

`tuiOutputTable()` defaults to `overflowX = "scroll"` and `overflowY = "scroll"` so wide or tall data frames stay navigable inside constrained layouts.

Navigate with **Tab** / **arrow keys**, activate buttons with **Enter**.
