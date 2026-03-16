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
    tuiRenderText(output$counter),
    tuiRow(
      tuiInputButton("Increment", id = "inc"),
      tuiInputButton("Decrement", id = "dec")
    )
  ),
  server = function(input, output) {
    output$counter <- input$inc - input$dec
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
| `tuiRenderText(output$someValue)` | Display an output value as text |
| `tuiInputButton(label, id)` | Button that triggers a handler |
| `tuiInputText(id, placeholder, value)` | Single-line text input |
| `tuiObserveEvent(input$someButton, expr, runAtInit = FALSE)` | Run code only when a specific input event triggers the server |

Navigate with **Tab** / **arrow keys**, activate buttons with **Enter**.
