# An explained example app

This vignette builds a small app and explains each part of the reactive
flow.

## Goal

We want a terminal app with:

- a numeric counter (`inc - dec`),
- a text input for a name,
- an **Apply name** button so the displayed name only changes when
  requested.

## Full app

``` r
library(rtui)

app <- tuiApp(
  ui = tuiColumn(
    tuiOutputNumeric("counter"),
    tuiOutputText("name"),
    tuiRow(
      tuiInputButton("Increment", id = "inc"),
      tuiInputButton("Decrement", id = "dec"),
      tuiInputButton("Apply name", id = "apply")
    ),
    tuiInputText(id = "nameInput", value = "John")
  ),
  server = function(input, output) {
    counter <- tuiReactive(input$inc - input$dec)

    applied_name <- tuiReactiveEvent(input$apply, runAtInit = TRUE, {
      tuiReq(input$nameInput)
      input$nameInput
    })

    tuiObserveEvent(counter(), {
      current_name <- tuiIsolate(applied_name())
      message("Counter changed while name is ", current_name)
    })

    output$counter <- tuiRenderNumeric(counter(), digits = 0)
    output$name <- tuiRenderText(paste("Your name is:", applied_name()))
  }
)

tuiRun(app)
```

## Step-by-step explanation

### 1) UI declaration

[`tuiColumn()`](https://leopoldguyot.github.io/rtui/reference/tuiColumn.md)
stacks components vertically. Inside it:

- two outputs (`counter`, `name`),
- one horizontal row of buttons,
- one text input.

This is static structure; no logic yet.

### 2) Counter reactive

``` r
counter <- tuiReactive(input$inc - input$dec)
```

`input$inc` and `input$dec` are button counters. The reactive graph
tracks this dependency and invalidates `counter` when either button
changes.

### 3) Name application as an explicit event

``` r
applied_name <- tuiReactiveEvent(input$apply, runAtInit = TRUE, {
  tuiReq(input$nameInput)
  input$nameInput
})
```

[`tuiReactiveEvent()`](https://leopoldguyot.github.io/rtui/reference/tuiReactiveEvent.md)
means the value updates only when `input$apply` triggers. Typing into
`nameInput` alone does not change output immediately.
`tuiReq(input$nameInput)` prevents empty values from replacing the last
valid name.

### 4) Side effects with isolation

``` r
tuiObserveEvent(counter(), {
  current_name <- tuiIsolate(applied_name())
  message("Counter changed while name is ", current_name)
})
```

This observer runs when `counter()` changes. Inside it,
[`tuiIsolate()`](https://leopoldguyot.github.io/rtui/reference/tuiIsolate.md)
reads `applied_name()` without turning that read into an extra
dependency for the observer trigger.

### 5) Output renderers

``` r
output$counter <- tuiRenderNumeric(counter(), digits = 0)
output$name <- tuiRenderText(paste("Your name is:", applied_name()))
```

Renderers define what is shown in each output node. On each event cycle,
rtui resolves those renderers and updates the UI.

## Behavior summary

- Press **Increment/Decrement**: counter updates immediately.
- Type in name: input state changes, displayed name stays unchanged.
- Press **Apply name**: displayed name updates if non-empty.
- Observer messages appear only when counter changes.
