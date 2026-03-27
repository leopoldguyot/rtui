# Create a TUI application

Defines a terminal UI application from a UI tree and a server function.
The runtime runs `server(input, output)` once during initialization to
register outputs and observers, then re-evaluates only invalidated
reactive graph nodes for each input event.

## Usage

``` r
tuiApp(ui, server)
```

## Arguments

- ui:

  A UI component tree built with
  [`tuiColumn()`](https://leopoldguyot.github.io/rtui/reference/tuiColumn.md),
  [`tuiRow()`](https://leopoldguyot.github.io/rtui/reference/tuiRow.md),
  [`tuiBox()`](https://leopoldguyot.github.io/rtui/reference/tuiBox.md),
  [`tuiOutputText()`](https://leopoldguyot.github.io/rtui/reference/tuiOutputText.md),
  [`tuiOutputNumeric()`](https://leopoldguyot.github.io/rtui/reference/tuiOutputNumeric.md),
  [`tuiInputButton()`](https://leopoldguyot.github.io/rtui/reference/tuiInputButton.md),
  or
  [`tuiInputText()`](https://leopoldguyot.github.io/rtui/reference/tuiInputText.md).

- server:

  A function called as `server(input, output)`. Both `input` and
  `output` are environments:

  - `input$<id>` is updated automatically from buttons and text inputs.

  - assign rendered outputs with `output$<name> <- tuiRenderText(...)`
    or `output$<name> <- tuiRenderNumeric(...)`.

  - use
    [`tuiObserve()`](https://leopoldguyot.github.io/rtui/reference/tuiObserve.md)
    /
    [`tuiObserveEvent()`](https://leopoldguyot.github.io/rtui/reference/tuiObserveEvent.md)
    for reactive side effects.

## Value

An object of class `rtuiApp`.

## Examples

``` r
app <- tuiApp(
  ui = tuiColumn(
    tuiOutputNumeric("counter"),
    tuiInputButton("Increment", id = "inc")
  ),
  server = function(input, output) {
    output$counter <- tuiRenderNumeric(input$inc)
  }
)
```
