# Text output component

Displays the value of `output$<outputId>` as text.

## Usage

``` r
tuiOutputText(
  outputId,
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

A `rtuiComponent` list node of type `"outputText"`.
