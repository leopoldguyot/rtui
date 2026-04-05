# Vertical layout container

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
  heightPercent = NULL,
  overflowX = "visible",
  overflowY = "visible"
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

- overflowX, overflowY:

  Container overflow policy for each axis. Use `"visible"` (default) to
  keep normal flow, `"clip"` to crop child drawing to the container
  bounds on that axis, or `"scroll"` to enable a scrollable viewport on
  that axis (Ctrl+Arrow, PageUp/PageDown, Home/End, mouse wheel, and
  draggable scrollbar).

## Value

A `rtuiComponent` list node of type `"column"`.
