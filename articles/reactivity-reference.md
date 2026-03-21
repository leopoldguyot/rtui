# Reactivity reference

This vignette summarizes the reactivity model used by `rtui`.

## Core ideas

- Reactives are graph-tracked nodes.
- Dependencies are captured dynamically as reactive code reads
  inputs/reactives.
- Input events invalidate downstream dependents.
- Dirty nodes recompute lazily when read.

## Helper functions

### `tuiReactive(expr)`

Creates a cached reactive expression.

### `tuiReactiveVal(value)`

Mutable reactive value:

- call with no argument to read;
- call with one argument to update and invalidate dependents.

### `tuiReactiveEvent(event, expr, runAtInit = FALSE)`

Reactive value that updates only when `event` triggers. Useful for
“Apply” button semantics.

### `tuiObserve(expr)` and `tuiObserveEvent(event, expr)`

Side-effect blocks:

- observe every cycle,
- or only on selected events.

### `tuiIsolate(expr)`

Reads values without registering dependencies for the current block.

### `tuiReq(...)`

Short-circuits when values are falsy. In renderers, output resolution is
skipped for that cycle. In reactives, previous stable value is retained
when available.

## Event cycle

At each input event:

1.  input state updates,
2.  dependents of the input node are marked dirty,
3.  server code runs,
4.  outputs resolve from renderer expressions.
