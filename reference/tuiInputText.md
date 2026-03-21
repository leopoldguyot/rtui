# Text input component

A focusable single-line text input. The typed value is stored
automatically in `input$<id>` and triggers a server update on change.
The input starts with `value`.

## Usage

``` r
tuiInputText(id, placeholder = "", value = "")
```

## Arguments

- id:

  A character string used as the input key (`input$<id>`).

- placeholder:

  A character string shown when the input is empty.

- value:

  A character string used as the initial/default input value.

## Value

A `rtuiComponent` list node of type `"input"`.
