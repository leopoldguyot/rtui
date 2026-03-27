# Text input component

A focusable text input. The typed value is stored automatically in
`input$<id>` and triggers a server update on change. The input starts
with `value`. By default (`multiline = FALSE`), Enter triggers submit
without inserting a newline. Set `multiline = TRUE` to allow newlines.

## Usage

``` r
tuiInputText(id, placeholder = "", value = "", multiline = FALSE)
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

## Value

A `rtuiComponent` list node of type `"input"`.
