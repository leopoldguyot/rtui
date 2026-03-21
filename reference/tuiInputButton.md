# Button input component

A focusable button. When activated (Enter key), it increments `input$id`
and triggers a server update.

## Usage

``` r
tuiInputButton(label, id)
```

## Arguments

- label:

  A character string shown as the button label.

- id:

  A character string used as the input key (`input$<id>`).

## Value

A `rtuiComponent` list node of type `"button"`.
