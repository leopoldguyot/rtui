# Checkbox input component

A focusable checkbox. Toggling it updates `input$<id>` and triggers a
server update.

## Usage

``` r
tuiInputCheckbox(
  label,
  id,
  value = FALSE,
  width = NULL,
  height = NULL,
  minHeight = NULL,
  maxHeight = NULL,
  widthPercent = NULL,
  heightPercent = NULL
)
```

## Arguments

- label:

  A character string shown as the checkbox label.

- id:

  A character string used as the input key (`input$<id>`).

- value:

  A single logical value used as the initial/default checkbox value.

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

A `rtuiComponent` list node of type `"checkbox"`.
