# Create a data-frame table renderer for `output$...`

Create a data-frame table renderer for `output$...`

## Usage

``` r
tuiRenderTable(
  expr,
  showRowNames = FALSE,
  showHeader = TRUE,
  outerBorder = TRUE,
  rowBorder = FALSE,
  colBorder = TRUE,
  headerBorder = TRUE,
  borderStyle = "light",
  borderColor = NULL,
  headerBorderColor = NULL,
  headerBold = TRUE,
  headerColor = NULL,
  headerBgColor = NULL,
  zebraRows = FALSE,
  zebraColorOdd = NULL,
  zebraColorEven = NULL,
  cellPaddingX = 0L,
  cellPaddingY = 0L,
  cellOverflow = "clip",
  minColWidth = NULL,
  maxColWidth = NULL,
  columnAlign = NULL,
  naText = "NA"
)
```

## Arguments

- expr:

  An expression returning a data frame (or tibble).

- showRowNames:

  Whether to prepend row names as the first column.

- showHeader:

  Whether the header row should be shown.

- outerBorder:

  Whether to draw the table outer border.

- rowBorder, colBorder, headerBorder:

  Whether to draw row separators, column separators, and the header
  separator.

- borderStyle:

  Border style (`"light"`, `"dashed"`, `"heavy"`, `"double"`,
  `"rounded"`, `"empty"`).

- borderColor, headerBorderColor:

  Optional border colors.

- headerBold:

  Whether header cells are bold.

- headerColor, headerBgColor:

  Optional header text/background colors.

- zebraRows:

  Whether alternating row backgrounds are enabled.

- zebraColorOdd, zebraColorEven:

  Optional alternating row background colors.

- cellPaddingX, cellPaddingY:

  Cell horizontal/vertical padding.

- cellOverflow:

  Cell overflow policy (`"clip"`, `"wrap"`, `"ellipsis"`).

- minColWidth, maxColWidth:

  Optional global min/max column width.

- columnAlign:

  Optional per-column alignment (`"left"`, `"center"`, `"right"`), as a
  scalar, vector, or named vector/list.

- naText:

  Replacement text for `NA` values in table cells.

## Value

A table renderer object for assignment in `output$...`.

## Details

Renderer expressions should ideally be side-effect free. If a
`tuiRender*` expression mutates reactive state (for example via
[`tuiReactiveVal()`](https://leopoldguyot.github.io/rtui/reference/tuiReactiveVal.md)),
the output can invalidate itself during its own evaluation. In that
case, `rtui` emits a warning (once per output id) and still completes
the render pass.
