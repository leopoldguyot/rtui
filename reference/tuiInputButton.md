# Button input component

A focusable button. When activated (Enter key), it increments `input$id`
and triggers a server update.

## Usage

``` r
tuiInputButton(label, id, color = NULL)
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

## Value

A `rtuiComponent` list node of type `"button"`.
