# Create a reactive expression

Creates a reactive object that re-evaluates `expr` when called during
server execution.

## Usage

``` r
tuiReactive(expr)
```

## Arguments

- expr:

  An expression producing a reactive value.

## Value

A reactive object (function) to be called with `()`.
