# Vertical box layout

Stacks child components vertically.

## Usage

``` r
tuiColumn(
  ...,
  width = NULL,
  height = NULL,
  minHeight = NULL,
  maxHeight = NULL,
  widthPercent = NULL,
  heightPercent = NULL
)
```

## Arguments

- ...:

  Child components built with layout or component functions.

- width, height:

  Optional fixed width/height in terminal cells.

- minHeight, maxHeight:

  Optional min/max height in terminal cells.

- widthPercent, heightPercent:

  Optional relative size between `0` and `1`. `widthPercent` is
  interpreted by
  [`tuiRow()`](https://leopoldguyot.github.io/rtui/reference/tuiRow.md)
  and `heightPercent` by `tuiColumn()` for strict main-axis percentages.

## Value

A `rtuiComponent` list node of type `"column"`.
