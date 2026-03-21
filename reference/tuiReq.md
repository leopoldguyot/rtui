# Require values to be available/truthy in reactive code

Stops the current reactive/render evaluation when any supplied value is
not considered available (falsy), similarly to `shiny::req()`.

## Usage

``` r
tuiReq(..., cancelOutput = FALSE)
```

## Arguments

- ...:

  Values to validate.

- cancelOutput:

  Ignored for now. Present for API compatibility.

## Value

Invisibly returns the first supplied value when all values are truthy.
