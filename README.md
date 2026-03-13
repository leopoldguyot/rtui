# rtui

A declarative Terminal User Interface (TUI) framework for R, backed by [FTXUI](https://github.com/ArthurSonzogni/FTXUI) via Rcpp.

## Installation

### Prerequisites

You need a C++17-capable compiler and the `ncurses` development library.

**Debian / Ubuntu:**
```bash
sudo apt install libncurses-dev
```

**Fedora / RHEL:**
```bash
sudo dnf install ncurses-devel
```

**macOS (Homebrew):**
```bash
brew install ncurses
```

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

app <- tui_app(
  state = list(counter = 0L),
  ui = tui_vbox(
    tui_render_text("counter"),
    tui_hbox(
        tui_input_button("Increment", id = "inc"),
        tui_input_button("Decrement", id = "dec")
    )
  ),
  handlers = list(
    inc = function(state) {
      state$counter <- state$counter + 1L
      state
    },
    dec = function(state) {
      state$counter <- state$counter - 1L
      state
    }
  )
)

tui_run(app)  # press Escape or Ctrl+Q to quit
```

## API

| Function | Description |
|---|---|
| `tui_app(state, ui, handlers)` | Define a TUI application |
| `tui_run(app)` | Start the app (blocking) |
| `tui_vbox(...)` | Stack components vertically |
| `tui_hbox(...)` | Place components side by side |
| `tui_render_text(key)` | Display a state value as text |
| `tui_input_button(label, id)` | Button that triggers a handler |
| `tui_input_text(id, placeholder)` | Single-line text input |

Navigate with **Tab** / **arrow keys**, activate buttons with **Enter**.
