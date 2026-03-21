# Evaluate an expression in isolation

Evaluates `expr` without publishing reactive read-change information for
the current server cycle. This is useful when you want the current value
of a reactive object without making it act as an event trigger.

## Usage

``` r
tuiIsolate(expr)
```

## Arguments

- expr:

  An expression to evaluate.

## Value

The evaluated value of `expr`.
