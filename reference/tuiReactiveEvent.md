# Create an event-driven reactive expression

Creates a reactive object that updates only when `event` is triggered.

## Usage

``` r
tuiReactiveEvent(event, expr, runAtInit = FALSE)
```

## Arguments

- event:

  An `input$<id>` / `input[["id"]]` reference, or a reactive call/object
  from
  [`tuiReactive()`](https://leopoldguyot.github.io/rtui/reference/tuiReactive.md),
  [`tuiReactiveVal()`](https://leopoldguyot.github.io/rtui/reference/tuiReactiveVal.md),
  or `tuiReactiveEvent()`.

- expr:

  An expression evaluated when `event` triggers.

- runAtInit:

  A single logical value. If `TRUE`, evaluate once during app
  initialization.

## Value

A reactive object (function) to be called with `()`.
