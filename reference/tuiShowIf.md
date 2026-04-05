# Conditionally render a child component by terminal size

Wraps a single child component and shows it only when terminal
dimensions satisfy the specified constraints. This is useful for
responsive breakpoints (for example, hiding secondary panels on narrow
terminals) while keeping a declarative static UI tree.

## Usage

``` r
tuiShowIf(
  child,
  minTerminalWidth = NULL,
  maxTerminalWidth = NULL,
  minTerminalHeight = NULL,
  maxTerminalHeight = NULL
)
```

## Arguments

- child:

  A single `rtuiComponent` to conditionally render.

- minTerminalWidth, maxTerminalWidth:

  Optional minimum/maximum terminal width (in terminal cells) required
  to show `child`.

- minTerminalHeight, maxTerminalHeight:

  Optional minimum/maximum terminal height (in terminal cells) required
  to show `child`.

## Value

A `rtuiComponent` list node of type `"showIf"`.

## Details

Conditions are evaluated against the **current terminal size** and
rechecked during rendering, so visibility updates automatically when the
terminal is resized.
