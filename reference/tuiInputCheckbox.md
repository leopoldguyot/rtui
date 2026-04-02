# Checkbox input component

A focusable checkbox. Toggling it updates `input$<id>` and triggers a
server update.

## Usage

``` r
tuiInputCheckbox(label, id, value = FALSE)
```

## Arguments

- label:

  A character string shown as the checkbox label.

- id:

  A character string used as the input key (`input$<id>`).

- value:

  A single logical value used as the initial/default checkbox value.

## Value

A `rtuiComponent` list node of type `"checkbox"`.
