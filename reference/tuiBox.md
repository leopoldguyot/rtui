# Box layout wrapper

Draws a border around a child component, with optional title and style.

## Usage

``` r
tuiBox(
  child,
  title = NULL,
  color = NULL,
  style = "rounded",
  titleStyle = "header",
  titleAlign = "left",
  margin = 0L,
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

- child:

  A single `rtuiComponent` to wrap.

- title:

  Optional single character string title displayed in the border. Use
  `NULL` (default) for no title.

- color:

  Optional border color. Supports named colors (`"default"`, `"black"`,
  `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`, `"cyan"`,
  `"graylight"`, `"graydark"`, `"redlight"`, `"greenlight"`,
  `"yellowlight"`, `"bluelight"`, `"magentalight"`, `"cyanlight"`,
  `"white"`) or a hex string like `"#RRGGBB"`.

- style:

  Border style. One of `"rounded"` (default), `"light"`, `"dashed"`,
  `"heavy"`, `"double"`, or `"empty"`.

- titleStyle:

  Title rendering style. One of `"header"` (default), which places the
  title inside the box as a header separated by a bar, or `"border"`,
  which places the title in the top-left border line.

- titleAlign:

  Title alignment for `"header"` mode. One of `"left"` (default),
  `"center"`, or `"right"`.

- margin:

  Integer number of spaces outside the box (default `0`).

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

- overflowX, overflowY:

  Container overflow policy for each axis. Use `"visible"` (default) to
  keep normal flow, `"clip"` to crop child drawing to the container box
  on that axis, or `"scroll"` to enable a focus-driven scrollable
  viewport on that axis.

## Value

A `rtuiComponent` list node of type `"box"`.
