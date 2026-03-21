# Getting started with rtui

`rtui` is a declarative terminal UI framework for R. You define:

- a UI tree (layout + components),
- a server function (`input`, `output`),
- reactive logic for updates.

## Minimal app

``` r
library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputNumeric("counter"),
    tuiRow(
      tuiInputButton("Increment", id = "inc"),
      tuiInputButton("Decrement", id = "dec")
    )
  ),
  server = function(input, output) {
    counter <- tuiReactive(input$inc - input$dec)
    output$counter <- tuiRenderNumeric(counter())
  }
)

tuiRun(app)
```

## Reactive helpers

- `tuiReactive(expr)`: cached reactive expression.
- `tuiReactiveVal(value)`: mutable reactive state.
- `tuiReactiveEvent(event, expr, runAtInit = FALSE)`: update only on
  selected event.
- `tuiObserve(expr)`: run side effects every cycle.
- `tuiObserveEvent(event, expr)`: run side effects on specific events.
- `tuiIsolate(expr)`: read values without creating dependencies.
- `tuiReq(...)`: short-circuit computation when values are not
  available/truthy.

## Input and output model

- Button inputs are counters incremented on each activation.
- Text inputs hold the latest string value.
- Outputs are assigned with renderers:
  - `tuiRenderText(expr)`
  - `tuiRenderNumeric(expr, digits = NULL)`

## Tips

- Keep reactivity in small expressions.
- Use
  [`tuiReactiveEvent()`](https://leopoldguyot.github.io/rtui/reference/tuiReactiveEvent.md)
  for explicit user-driven updates.
- Use
  [`tuiReq()`](https://leopoldguyot.github.io/rtui/reference/tuiReq.md)
  in renderers/reactives to avoid noisy invalid states.
