# Observe selected input/reactive events

Evaluates `expr` only when `event` is triggered.

## Usage

``` r
tuiObserveEvent(event, expr, runAtInit = FALSE)
```

## Arguments

- event:

  An `input$<id>` / `input[["id"]]` reference, or a reactive call/object
  from
  [`tuiReactive()`](https://leopoldguyot.github.io/rtui/reference/tuiReactive.md),
  [`tuiReactiveVal()`](https://leopoldguyot.github.io/rtui/reference/tuiReactiveVal.md),
  or
  [`tuiReactiveEvent()`](https://leopoldguyot.github.io/rtui/reference/tuiReactiveEvent.md).

- expr:

  An expression evaluated when `event` triggers.

- runAtInit:

  A single logical value. If `TRUE`, evaluate once during app
  initialization.

## Value

Invisibly returns `NULL`.
