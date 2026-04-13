# Update a dropdown input value (and optional choices) from server logic

Updates the selection and/or choices of an existing
[`tuiInputDropdown()`](https://leopoldguyot.github.io/rtui/reference/tuiInputDropdown.md)
input.

## Usage

``` r
tuiUpdateDropdownInput(id, selected = NULL, choices = NULL)
```

## Arguments

- id:

  Input id passed to
  [`tuiInputDropdown()`](https://leopoldguyot.github.io/rtui/reference/tuiInputDropdown.md).

- selected:

  Optional selected value. Must match one of effective choices. If
  `NULL`, current value is kept when valid; otherwise the first choice
  is selected.

- choices:

  Optional replacement choice vector.

## Value

Invisibly returns `NULL`.
