# Dropdown input component

A focusable dropdown/select input. Selecting an item updates
`input$<id>` and triggers a server update.

## Usage

``` r
tuiInputDropdown(
  id,
  choices,
  selected = NULL,
  maxMenuHeight = NULL,
  width = NULL,
  height = NULL,
  minHeight = NULL,
  maxHeight = NULL,
  widthPercent = NULL,
  heightPercent = NULL
)

tuiDropDownInput(...)
```

## Arguments

- id:

  A character string used as the input key (`input$<id>`).

- choices:

  Non-empty character vector of selectable values.

- selected:

  Optional selected value. Must be one of `choices`. Defaults to the
  first element of `choices`.

- maxMenuHeight:

  Optional maximum visible dropdown menu height (in terminal rows)
  before enabling internal scrolling with a scrollbar.

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

- ...:

  Passed through to `tuiInputDropdown()`.

## Value

A `rtuiComponent` list node of type `"dropdown"`.
