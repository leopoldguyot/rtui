# Run a TUI application

Starts the terminal event loop for an `rtuiApp`. This call is
**blocking**: it takes over the terminal until the user presses `Escape`
or `Ctrl+Q`.

## Usage

``` r
tuiRun(app, overflow = "clip", screen = "auto")
```

## Arguments

- app:

  An `rtuiApp` object created by
  [`tuiApp()`](https://leopoldguyot.github.io/rtui/reference/tuiApp.md).

- overflow:

  Overflow handling strategy when content exceeds terminal size. Use
  `"clip"` (default) to crop content to the visible terminal viewport,
  or `"scroll"` to wrap the app in a global scrollable viewport. Use
  `"block"` to temporarily block interaction and show only a warning
  message while the terminal is smaller than the app's minimum required
  size.

  This setting is global to the whole app. Per-container overflow can be
  configured independently with `overflowX` / `overflowY` on
  [`tuiRow()`](https://leopoldguyot.github.io/rtui/reference/tuiRow.md),
  [`tuiColumn()`](https://leopoldguyot.github.io/rtui/reference/tuiColumn.md),
  and
  [`tuiBox()`](https://leopoldguyot.github.io/rtui/reference/tuiBox.md).

- screen:

  Screen backend strategy. Use `"fullscreen"` for full-screen
  alternate-buffer rendering, `"terminal"` for inline terminal output
  mode, or `"auto"` (default) to choose automatically for supported
  terminal sessions.

  `tuiRun()` requires an interactive TTY for both input and output. The
  RStudio Console is not a real terminal TTY and is therefore not
  supported for running rtui apps directly.

## Value

Invisibly returns `NULL`.
