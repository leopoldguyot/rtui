# Table output component

Displays the value of `output$<outputId>` as a table rendered from a
data frame produced by
[`tuiRenderTable()`](https://leopoldguyot.github.io/rtui/reference/tuiRenderTable.md).

## Usage

``` r
tuiOutputTable(
  outputId,
  overflowX = "scroll",
  overflowY = "scroll",
  headerClickId = NULL,
  width = NULL,
  height = NULL,
  minHeight = NULL,
  maxHeight = NULL,
  widthPercent = NULL,
  heightPercent = NULL
)
```

## Arguments

- outputId:

  A single character string naming the output id.

- overflowX, overflowY:

  Overflow policy for table clipping/scrolling on each axis. Use
  `"scroll"` (default) to make wide/tall tables navigable, `"clip"` to
  crop to viewport bounds, or `"visible"` to allow overflow.

- headerClickId:

  Optional input id for header click events. When set, clicking a table
  header cell updates `input$<headerClickId>` with a list containing
  `column` (name) and `columnIndex` (1-based position).

- width, height:

  Optional fixed width/height in terminal cells.

- minHeight, maxHeight:

  Optional min/max height in terminal cells.

- widthPercent, heightPercent:

  Optional relative size between `0` and `1`. `widthPercent` is
  interpreted by
  [`tuiRow()`](https://leopoldguyot.github.io/rtui/reference/tuiRow.md)
  and `heightPercent` by
  [`tuiColumn()`](https://leopoldguyot.github.io/rtui/reference/tuiColumn.md)
  for strict main-axis percentages.

## Value

A `rtuiComponent` list node of type `"outputTable"`.
