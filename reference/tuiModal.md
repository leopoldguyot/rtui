# Modal layout wrapper

Overlays a popup component above a background child component.

## Usage

``` r
tuiModal(child, modal, show = FALSE, showInputId = NULL, closeOnEscape = TRUE)
```

## Arguments

- child:

  Base `rtuiComponent` rendered underneath the modal.

- modal:

  `rtuiComponent` rendered as the popup overlay.

- show:

  Static fallback visibility (`FALSE` by default).

- showInputId:

  Optional input id controlling modal visibility at runtime. When set,
  visibility is driven by `input$<showInputId>` (truthy values show the
  modal) and `show` is used only as a fallback when the input is absent.

- closeOnEscape:

  Whether pressing Escape should close the modal by writing `FALSE` into
  `input$<showInputId>` and triggering its event handler. Ignored when
  `showInputId` is `NULL`.

## Value

A `rtuiComponent` list node of type `"modal"`.

## Details

The modal content is centered and rendered on top of the base child
using an overlay layer. This is useful for dialogs, confirmations, or
temporary popups while keeping the underlying UI structure declarative.
