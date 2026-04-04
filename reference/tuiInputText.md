# Text input component

A focusable text input. The typed value is stored automatically in
`input$<id>` and triggers a server update on change. The input starts
with `value`. By default (`multiline = FALSE`), Enter triggers submit
without inserting a newline. Set `multiline = TRUE` to allow newlines.

## Usage

``` r
tuiInputText(
  id,
  placeholder = "",
  value = "",
  multiline = FALSE,
  width = NULL,
  height = NULL,
  minHeight = NULL,
  maxHeight = NULL,
  widthPercent = NULL,
  heightPercent = NULL
)
```

## Arguments

- id:

  A character string used as the input key (`input$<id>`).

- placeholder:

  A character string shown when the input is empty.

- value:

  A character string used as the initial/default input value.

- multiline:

  A single logical value. If `FALSE` (default), Enter does not modify
  the text content. If `TRUE`, Enter inserts a newline.

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

A `rtuiComponent` list node of type `"input"`.
