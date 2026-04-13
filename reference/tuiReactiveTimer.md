# Create a timer-driven reactive value

Creates a reactive object that increments at a fixed interval.

## Usage

``` r
tuiReactiveTimer(intervalMs = 1000L)
```

## Arguments

- intervalMs:

  Timer interval in milliseconds (must be \> 0).

## Value

A reactive object (function) to be called with `()`.
