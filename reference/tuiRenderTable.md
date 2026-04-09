# Create a data-frame table renderer for `output$...`

Create a data-frame table renderer for `output$...`

## Usage

``` r
tuiRenderTable(expr, rowNames = FALSE)
```

## Arguments

- expr:

  An expression returning a data frame (or tibble).

- rowNames:

  A single logical value. If `TRUE`, row names are added as the first
  column.

## Value

A table renderer object for assignment in `output$...`.

## Details

Renderer expressions should ideally be side-effect free. If a
`tuiRender*` expression mutates reactive state (for example via
[`tuiReactiveVal()`](https://leopoldguyot.github.io/rtui/reference/tuiReactiveVal.md)),
the output can invalidate itself during its own evaluation. In that
case, `rtui` emits a warning (once per output id) and still completes
the render pass.
