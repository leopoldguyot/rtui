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

**Windows:** install [Rtools](https://cran.r-project.org/bin/windows/Rtools/) and run in a modern terminal (Windows Terminal, PowerShell, or a recent cmd.exe with VT enabled).

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
    tuiOutputText("name"),
    tuiRow(
      tuiInputButton("Increment", id = "inc"),
      tuiInputButton("Decrement", id = "dec"),
      tuiInputButton("Apply name", id = "applyName")
    ),
    tuiInputText(id = "nameInput", value = "John")
  ),
  server = function(input, output) {
    counter <- tuiReactive(input$inc - input$dec)
    appliedName <- tuiReactiveEvent(input$applyName, runAtInit = TRUE, {
      input$nameInput
    })

    tuiObserveEvent(counter(), {
      currentName <- tuiIsolate(appliedName())
      message("counter changed while name is ", currentName)
    })

    output$counter <- tuiRenderNumeric(counter(), digits = 0)
    output$name <- tuiRenderText(paste("Your name is:", appliedName()))
  }
)

tuiRun(app)  # press Escape or Ctrl+Q to quit
```

## API

| Function | Description |
|---|---|
| `tuiApp(ui, server)` | Define a TUI application |
| `tuiRun(app)` | Start the app (blocking) |
| `tuiColumn(...)` | Stack components vertically |
| `tuiRow(...)` | Place components side by side |
| `tuiOutputText("id")` / `tuiOutputNumeric("id")` | Display `output$id` in the UI |
| `tuiInputButton(label, id)` | Button that triggers a handler |
| `tuiInputText(id, placeholder, value)` | Single-line text input |
| `tuiRenderText(expr)` / `tuiRenderNumeric(expr, digits = NULL)` | Build renderers assigned to `output$...` |
| `tuiReactive(expr)` / `tuiReactiveVal(value)` | Define graph-tracked reactive expressions and mutable values (auto invalidation of dependents) |
| `tuiReactiveEvent(event, expr, runAtInit = FALSE)` | Event-scoped reactive expression that re-runs when its event source invalidates |
| `tuiReq(...)` | Stop current reactive/render evaluation when required values are falsy |
| `tuiIsolate(expr)` | Read reactive values without registering graph dependencies |
| `tuiObserve(expr)` | Run code every server cycle |
| `tuiObserveEvent(event, expr, runAtInit = FALSE)` | Run code only when an input/reactive event invalidates during the cycle |

Navigate with **Tab** / **arrow keys**, activate buttons with **Enter**.
