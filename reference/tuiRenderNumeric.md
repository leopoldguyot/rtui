# Create a numeric renderer for `output$...`

Create a numeric renderer for `output$...`

## Usage

``` r
tuiRenderNumeric(expr, digits = NULL)
```

## Arguments

- expr:

  An expression returning numeric-compatible output.

- digits:

  Optional number of digits after the decimal point.

## Value

A numeric renderer object for assignment in `output$...`.

## Details

Renderer expressions should ideally be side-effect free. If a
`tuiRender*` expression mutates reactive state (for example via
[`tuiReactiveVal()`](https://leopoldguyot.github.io/rtui/reference/tuiReactiveVal.md)),
the output can invalidate itself during its own evaluation. In that
case, `rtui` emits a warning (once per output id) and still completes
the render pass.
