# Run a TUI application

Starts the terminal event loop for an `rtuiApp`. This call is
**blocking**: it takes over the terminal until the user presses `Escape`
or `Ctrl+Q`.

## Usage

``` r
tuiRun(app)
```

## Arguments

- app:

  An `rtuiApp` object created by
  [`tuiApp()`](https://leopoldguyot.github.io/rtui/reference/tuiApp.md).

## Value

Invisibly returns `NULL`.
