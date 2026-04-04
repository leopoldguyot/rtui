# Button input component

A focusable button. When activated (Enter key), it increments `input$id`
and triggers a server update.

## Usage

``` r
tuiInputButton(
  label,
  id,
  color = NULL,
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

  A character string shown as the button label.

- id:

  A character string used as the input key (`input$<id>`).

- color:

  Optional button color. Supports named colors (`"default"`, `"black"`,
  `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`, `"cyan"`,
  `"graylight"`, `"graydark"`, `"redlight"`, `"greenlight"`,
  `"yellowlight"`, `"bluelight"`, `"magentalight"`, `"cyanlight"`,
  `"white"`) or a hex string like `"#RRGGBB"`.

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

A `rtuiComponent` list node of type `"button"`.
