.rtuiRuntimeContext <- new.env(parent = emptyenv())
.rtuiRuntimeContext$currentRuntime <- NULL

.rtuiCurrentRuntime <- function() {
  runtime <- .rtuiRuntimeContext$currentRuntime
  if (is.null(runtime)) {
    stop(
      "Reactive/render helpers must be used inside `tuiApp(..., server = ...)`."
    )
  }
  runtime
}

.rtuiExtractMemberId <- function(expr, rootName) {
  isDollarAccess <- is.call(expr) &&
    identical(expr[[1L]], as.name("$")) &&
    length(expr) == 3L &&
    is.symbol(expr[[2L]]) &&
    identical(as.character(expr[[2L]]), rootName) &&
    is.symbol(expr[[3L]])
  if (isDollarAccess) {
    return(as.character(expr[[3L]]))
  }

  isBracketAccess <- is.call(expr) &&
    identical(expr[[1L]], as.name("[[")) &&
    length(expr) == 3L &&
    is.symbol(expr[[2L]]) &&
    identical(as.character(expr[[2L]]), rootName) &&
    is.character(expr[[3L]]) &&
    length(expr[[3L]]) == 1L &&
    !is.na(expr[[3L]])
  if (isBracketAccess) {
    return(expr[[3L]])
  }

  NULL
}

.rtuiIsReactiveObject <- function(x) {
  is.function(x) && inherits(x, "rtuiReactiveObject")
}

.rtuiEvalMaybeReactive <- function(value) {
  if (.rtuiIsReactiveObject(value)) {
    return(value())
  }
  value
}

.rtuiNextReactiveId <- function(prefix) {
  runtime <- .rtuiCurrentRuntime()
  runtime$reactiveIndex <- runtime$reactiveIndex + 1L
  paste0(prefix, "_", runtime$reactiveIndex)
}

.rtuiReactiveStoreGet <- function(runtime, reactiveId) {
  if (exists(reactiveId, envir = runtime$reactiveState, inherits = FALSE)) {
    return(get(reactiveId, envir = runtime$reactiveState, inherits = FALSE))
  }
  NULL
}

.rtuiReactiveStoreSet <- function(runtime, reactiveId, value, hasValue = TRUE) {
  assign(
    reactiveId,
    list(value = value, hasValue = hasValue),
    envir = runtime$reactiveState
  )
}

.rtuiInIsolate <- function(runtime) {
  isTRUE(runtime$currentIsolateDepth > 0L)
}

.rtuiSetReactiveChanged <- function(runtime, reactiveId, changed, force = FALSE) {
  if (!isTRUE(force) && .rtuiInIsolate(runtime)) {
    return(invisible(NULL))
  }

  assign(
    reactiveId,
    isTRUE(changed),
    envir = runtime$currentReactiveChanged
  )
}

.rtuiGetReactiveChanged <- function(runtime, reactiveId) {
  if (!exists(reactiveId, envir = runtime$currentReactiveChanged, inherits = FALSE)) {
    return(FALSE)
  }
  isTRUE(get(reactiveId, envir = runtime$currentReactiveChanged, inherits = FALSE))
}

.rtuiUpdateReactiveState <- function(runtime, reactiveId, value) {
  previous <- .rtuiReactiveStoreGet(runtime, reactiveId)
  changed <- is.null(previous) || !isTRUE(previous$hasValue) || !identical(previous$value, value)
  .rtuiReactiveStoreSet(runtime, reactiveId, value = value, hasValue = TRUE)
  .rtuiSetReactiveChanged(runtime, reactiveId, changed)
  value
}

.rtuiResolveReactiveEventCall <- function(eventExpr, eventEnv) {
  if (!is.call(eventExpr) || length(eventExpr) < 1L) {
    return(NULL)
  }

  calleeExpr <- eventExpr[[1L]]
  if (!is.symbol(calleeExpr)) {
    return(NULL)
  }

  calleeName <- as.character(calleeExpr)
  if (!exists(calleeName, envir = eventEnv, inherits = TRUE)) {
    return(NULL)
  }

  eventValue <- get(calleeName, envir = eventEnv, inherits = TRUE)
  if (!.rtuiIsReactiveObject(eventValue)) {
    return(NULL)
  }

  if (length(eventExpr) > 1L) {
    stop(
      "Reactive `event` calls must not include arguments. ",
      "Use a zero-argument call like `myReactive()`."
    )
  }

  eventValue
}

.rtuiResolveEventSpec <- function(eventExpr, eventEnv) {
  inputId <- .rtuiExtractMemberId(eventExpr, "input")
  if (!is.null(inputId)) {
    return(list(type = "input", inputId = inputId))
  }

  eventValue <- .rtuiResolveReactiveEventCall(eventExpr, eventEnv)
  if (!is.null(eventValue)) {
    reactiveId <- attr(eventValue, "rtuiId", exact = TRUE)
    if (is.null(reactiveId) || !is.character(reactiveId) || length(reactiveId) != 1L) {
      stop("Reactive events must provide a valid internal reactive id.")
    }
    return(list(type = "reactive", object = eventValue, reactiveId = reactiveId))
  }

  eventValue <- eval(eventExpr, envir = eventEnv)
  if (.rtuiIsReactiveObject(eventValue)) {
    reactiveId <- attr(eventValue, "rtuiId", exact = TRUE)
    if (is.null(reactiveId) || !is.character(reactiveId) || length(reactiveId) != 1L) {
      stop("Reactive events must provide a valid internal reactive id.")
    }
    return(list(type = "reactive", object = eventValue, reactiveId = reactiveId))
  }

  stop(
    "`event` must be an `input$<id>` / `input[[\"id\"]]` reference, ",
    "or a reactive call/object created with `tuiReactive()`, ",
    "`tuiReactiveVal()`, or `tuiReactiveEvent()`."
  )
}

.rtuiShouldTriggerEvent <- function(eventSpec, runAtInit) {
  runtime <- .rtuiCurrentRuntime()
  isInitRun <- is.null(runtime$currentEventId)

  if (identical(eventSpec$type, "input")) {
    matchesInput <- !is.null(runtime$currentEventId) &&
      identical(runtime$currentEventId, eventSpec$inputId)
    return(matchesInput || (isInitRun && isTRUE(runAtInit)))
  }

  if (identical(eventSpec$type, "reactive")) {
    eventSpec$object()
    reactiveChanged <- .rtuiGetReactiveChanged(runtime, eventSpec$reactiveId)
    if (isInitRun) {
      return(isTRUE(runAtInit))
    }
    return(reactiveChanged)
  }

  FALSE
}

.rtuiResolveOutputValue <- function(value) {
  if (!inherits(value, "rtuiRenderer")) {
    return(value)
  }

  runtime <- .rtuiCurrentRuntime()
  evalEnv <- new.env(parent = value$env)
  evalEnv$input <- runtime$currentInput
  evalEnv$output <- runtime$currentOutput

  rendered <- eval(value$expr, envir = evalEnv)
  rendered <- .rtuiEvalMaybeReactive(rendered)

  if (identical(value$kind, "text")) {
    if (is.null(rendered) || length(rendered) == 0L) {
      return("")
    }
    return(as.character(rendered[[1L]]))
  }

  if (identical(value$kind, "numeric")) {
    numericValue <- suppressWarnings(as.numeric(rendered))
    if (length(numericValue) == 0L) {
      return("NA")
    }

    scalar <- numericValue[[1L]]
    if (!is.finite(scalar)) {
      return(as.character(scalar))
    }

    if (!is.null(value$digits)) {
      scalar <- round(scalar, digits = value$digits)
      return(format(
        scalar,
        nsmall = value$digits,
        trim = TRUE,
        scientific = FALSE
      ))
    }

    if (is.finite(scalar) && abs(scalar - round(scalar)) <= sqrt(.Machine$double.eps)) {
      return(as.integer(round(scalar)))
    }
    return(as.character(scalar))
  }

  value
}

#' Create a TUI application
#'
#' Defines a terminal UI application from a UI tree and a server function.
#' The runtime automatically manages `input` and `output` state, similarly to
#' Shiny's conceptual model.
#'
#' @param ui A UI component tree built with [tuiColumn()], [tuiRow()],
#'   [tuiOutputText()], [tuiOutputNumeric()], [tuiInputButton()], or
#'   [tuiInputText()].
#' @param server A function called as `server(input, output)`. Both `input`
#'   and `output` are environments:
#'   - `input$<id>` is updated automatically from buttons and text inputs.
#'   - assign rendered outputs with `output$<name> <- tuiRenderText(...)` or
#'     `output$<name> <- tuiRenderNumeric(...)`.
#'   - use [tuiObserveEvent()] / [tuiReactiveEvent()] for event-based updates.
#'
#' @return An object of class `rtuiApp`.
#'
#' @examples
#' app <- tuiApp(
#'   ui = tuiColumn(
#'     tuiOutputNumeric("counter"),
#'     tuiInputButton("Increment", id = "inc")
#'   ),
#'   server = function(input, output) {
#'     output$counter <- tuiRenderNumeric(input$inc)
#'   }
#' )
#'
#' @export
tuiApp <- function(ui, server) {
  if (!inherits(ui, "rtuiComponent")) {
    stop("`ui` must be a rtuiComponent (built with tuiColumn, tuiRow, etc.).")
  }
  if (!is.function(server)) {
    stop("`server` must be a function called as `server(input, output)`.")
  }

  meta <- .rtuiCollectUiMeta(ui)
  overlapIds <- intersect(meta$buttonIds, meta$textInputIds)
  if (length(overlapIds) > 0L) {
    stop(
      "Component ids must be unique across buttons and text inputs. Duplicates: ",
      paste(overlapIds, collapse = ", ")
    )
  }

  runtime <- new.env(parent = emptyenv())
  runtime$reactiveState <- new.env(parent = emptyenv())
  runtime$reactiveIndex <- 0L
  runtime$currentReactiveCache <- new.env(parent = emptyenv())
  runtime$currentReactiveChanged <- new.env(parent = emptyenv())
  runtime$currentEventId <- NULL
  runtime$currentInput <- NULL
  runtime$currentOutput <- NULL
  runtime$currentIsolateDepth <- 0L

  inputState <- .rtuiInitialInput(meta)
  outputState <- .rtuiInitialOutput(meta)

  state <- list(
    input = inputState,
    output = .rtuiRunServer(
      server = server,
      inputState = inputState,
      outputState = outputState,
      eventId = NULL,
      runtime = runtime
    )
  )

  handlerIds <- unique(c(meta$buttonIds, meta$textInputIds))
  handlers <- stats::setNames(vector("list", length(handlerIds)), handlerIds)
  for (id in handlerIds) {
    handlers[[id]] <- local({
      currentEventId <- id
      function(state) {
        state$output <- .rtuiRunServer(
          server = server,
          inputState = state$input,
          outputState = state$output,
          eventId = currentEventId,
          runtime = runtime
        )
        state
      }
    })
  }

  structure(
    list(state = state, ui = ui, handlers = handlers),
    class = "rtuiApp"
  )
}

.rtuiCollectUiMeta <- function(node) {
  outputIds <- character()
  buttonIds <- character()
  textInputIds <- character()
  textInputDefaults <- list()

  walk <- function(x) {
    type <- x$type

    if (type %in% c("outputText", "outputNumeric")) {
      outputIds <<- c(outputIds, x$outputId)
    }
    if (identical(type, "button")) {
      buttonIds <<- c(buttonIds, x$id)
    }
    if (identical(type, "input")) {
      textInputIds <<- c(textInputIds, x$id)
      defaultValue <- if (is.null(x$value)) "" else x$value
      textInputDefaults[[x$id]] <<- defaultValue
    }

    if (!is.null(x$children)) {
      for (child in x$children) {
        walk(child)
      }
    }
  }

  walk(node)

  textInputIds <- unique(textInputIds)

  list(
    outputIds = unique(outputIds),
    buttonIds = unique(buttonIds),
    textInputIds = textInputIds,
    textInputDefaults = textInputDefaults[textInputIds]
  )
}

.rtuiInitialInput <- function(meta) {
  ids <- c(meta$buttonIds, meta$textInputIds)
  input <- stats::setNames(vector("list", length(ids)), ids)

  for (id in meta$buttonIds) {
    input[[id]] <- 0L
  }

  textDefaults <- if (is.null(meta$textInputDefaults)) list() else meta$textInputDefaults
  for (id in meta$textInputIds) {
    input[[id]] <- if (!is.null(textDefaults[[id]])) textDefaults[[id]] else ""
  }

  input
}

.rtuiInitialOutput <- function(meta) {
  output <- stats::setNames(
    vector("list", length(meta$outputIds)),
    meta$outputIds
  )

  for (id in meta$outputIds) {
    output[[id]] <- ""
  }
  output
}

.rtuiRunServer <- function(server, inputState, outputState, eventId = NULL, runtime) {
  oldRuntime <- .rtuiRuntimeContext$currentRuntime
  .rtuiRuntimeContext$currentRuntime <- runtime
  on.exit({
    .rtuiRuntimeContext$currentRuntime <- oldRuntime
  }, add = TRUE)

  input <- list2env(inputState, parent = emptyenv())
  output <- list2env(outputState, parent = emptyenv())

  runtime$reactiveIndex <- 0L
  runtime$currentReactiveCache <- new.env(parent = emptyenv())
  runtime$currentReactiveChanged <- new.env(parent = emptyenv())
  runtime$currentEventId <- eventId
  runtime$currentInput <- input
  runtime$currentOutput <- output
  runtime$currentIsolateDepth <- 0L

  server(input, output)

  resolvedOutput <- as.list(output, all.names = TRUE)
  for (name in names(resolvedOutput)) {
    resolvedOutput[[name]] <- .rtuiResolveOutputValue(resolvedOutput[[name]])
  }

  resolvedOutput
}

#' Create a reactive expression
#'
#' Creates a reactive object that re-evaluates `expr` when called during server
#' execution.
#'
#' @param expr An expression producing a reactive value.
#'
#' @return A reactive object (function) to be called with `()`.
#'
#' @export
tuiReactive <- function(expr) {
  exprSub <- substitute(expr)
  exprEnv <- parent.frame()
  reactiveId <- .rtuiNextReactiveId("reactive")

  reactiveObject <- function() {
    runtime <- .rtuiCurrentRuntime()
    cacheKey <- paste0("reactive:", reactiveId)
    if (exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
      return(get(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE))
    }

    value <- eval(exprSub, envir = exprEnv)
    value <- .rtuiEvalMaybeReactive(value)
    value <- .rtuiUpdateReactiveState(runtime, reactiveId, value)

    assign(cacheKey, value, envir = runtime$currentReactiveCache)
    value
  }

  class(reactiveObject) <- c("rtuiReactiveObject", "rtuiReactive", class(reactiveObject))
  attr(reactiveObject, "rtuiId") <- reactiveId
  reactiveObject
}

#' Create a mutable reactive value
#'
#' Creates a reactive value container. Call without arguments to read, and with
#' one argument to update.
#'
#' @param value Initial value.
#'
#' @return A reactive value function.
#'
#' @export
tuiReactiveVal <- function(value = NULL) {
  runtime <- .rtuiCurrentRuntime()
  reactiveId <- .rtuiNextReactiveId("reactiveVal")

  if (is.null(.rtuiReactiveStoreGet(runtime, reactiveId))) {
    .rtuiReactiveStoreSet(runtime, reactiveId, value = value, hasValue = TRUE)
  }

  reactiveValue <- function(value) {
    runtime <- .rtuiCurrentRuntime()
    current <- .rtuiReactiveStoreGet(runtime, reactiveId)
    if (is.null(current)) {
      current <- list(value = NULL, hasValue = FALSE)
    }

    if (missing(value)) {
      return(current$value)
    }

    changed <- !isTRUE(current$hasValue) || !identical(current$value, value)
    .rtuiReactiveStoreSet(runtime, reactiveId, value = value, hasValue = TRUE)
    .rtuiSetReactiveChanged(runtime, reactiveId, changed, force = TRUE)
    invisible(value)
  }

  class(reactiveValue) <- c("rtuiReactiveObject", "rtuiReactiveVal", class(reactiveValue))
  attr(reactiveValue, "rtuiId") <- reactiveId
  reactiveValue
}

#' Create an event-driven reactive expression
#'
#' Creates a reactive object that updates only when `event` is triggered.
#'
#' @param event An `input$<id>` / `input[["id"]]` reference, or a reactive
#'   call/object from [tuiReactive()], [tuiReactiveVal()], or
#'   [tuiReactiveEvent()].
#' @param expr An expression evaluated when `event` triggers.
#' @param runAtInit A single logical value. If `TRUE`, evaluate once during app
#'   initialization.
#'
#' @return A reactive object (function) to be called with `()`.
#'
#' @export
tuiReactiveEvent <- function(event, expr, runAtInit = FALSE) {
  if (!is.logical(runAtInit) || length(runAtInit) != 1L || is.na(runAtInit)) {
    stop("`runAtInit` must be TRUE or FALSE.")
  }

  eventSpec <- .rtuiResolveEventSpec(substitute(event), parent.frame())
  exprSub <- substitute(expr)
  exprEnv <- parent.frame()
  reactiveId <- .rtuiNextReactiveId("reactiveEvent")

  reactiveObject <- function() {
    runtime <- .rtuiCurrentRuntime()
    cacheKey <- paste0("reactiveEvent:", reactiveId)
    if (exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
      return(get(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE))
    }

    if (.rtuiShouldTriggerEvent(eventSpec, runAtInit = runAtInit)) {
      value <- eval(exprSub, envir = exprEnv)
      value <- .rtuiEvalMaybeReactive(value)
      value <- .rtuiUpdateReactiveState(runtime, reactiveId, value)
      assign(cacheKey, value, envir = runtime$currentReactiveCache)
      return(value)
    }

    previous <- .rtuiReactiveStoreGet(runtime, reactiveId)
    .rtuiSetReactiveChanged(runtime, reactiveId, FALSE)
    value <- if (is.null(previous) || !isTRUE(previous$hasValue)) NULL else previous$value
    assign(cacheKey, value, envir = runtime$currentReactiveCache)
    value
  }

  class(reactiveObject) <- c("rtuiReactiveObject", "rtuiReactiveEvent", class(reactiveObject))
  attr(reactiveObject, "rtuiId") <- reactiveId
  reactiveObject
}

#' Observe reactive values
#'
#' Evaluates `expr` every server run.
#'
#' @param expr An expression to evaluate.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiObserve <- function(expr) {
  eval.parent(substitute(expr))
  invisible(NULL)
}

#' Evaluate an expression in isolation
#'
#' Evaluates `expr` without publishing reactive read-change information for the
#' current server cycle. This is useful when you want the current value of a
#' reactive object without making it act as an event trigger.
#'
#' @param expr An expression to evaluate.
#'
#' @return The evaluated value of `expr`.
#'
#' @export
tuiIsolate <- function(expr) {
  runtime <- .rtuiRuntimeContext$currentRuntime
  if (is.null(runtime)) {
    return(eval.parent(substitute(expr)))
  }

  runtime$currentIsolateDepth <- runtime$currentIsolateDepth + 1L
  on.exit({
    runtime$currentIsolateDepth <- max(0L, runtime$currentIsolateDepth - 1L)
  }, add = TRUE)

  eval.parent(substitute(expr))
}

#' Observe selected input/reactive events
#'
#' Evaluates `expr` only when `event` is triggered.
#'
#' @param event An `input$<id>` / `input[["id"]]` reference, or a reactive
#'   call/object from [tuiReactive()], [tuiReactiveVal()], or
#'   [tuiReactiveEvent()].
#' @param expr An expression evaluated when `event` triggers.
#' @param runAtInit A single logical value. If `TRUE`, evaluate once during app
#'   initialization.
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiObserveEvent <- function(event, expr, runAtInit = FALSE) {
  if (!is.logical(runAtInit) || length(runAtInit) != 1L || is.na(runAtInit)) {
    stop("`runAtInit` must be TRUE or FALSE.")
  }

  eventSpec <- .rtuiResolveEventSpec(substitute(event), parent.frame())
  if (.rtuiShouldTriggerEvent(eventSpec, runAtInit = runAtInit)) {
    eval.parent(substitute(expr))
  }

  invisible(NULL)
}

#' Create a text renderer for `output$...`
#'
#' @param expr An expression returning text-compatible output.
#'
#' @return A text renderer object for assignment in `output$...`.
#'
#' @export
tuiRenderText <- function(expr) {
  structure(
    list(kind = "text", expr = substitute(expr), env = parent.frame()),
    class = "rtuiRenderer"
  )
}

#' Create a numeric renderer for `output$...`
#'
#' @param expr An expression returning numeric-compatible output.
#' @param digits Optional number of digits after the decimal point.
#'
#' @return A numeric renderer object for assignment in `output$...`.
#'
#' @export
tuiRenderNumeric <- function(expr, digits = NULL) {
  if (!is.null(digits)) {
    if (
      !is.numeric(digits) ||
      length(digits) != 1L ||
      is.na(digits) ||
      digits < 0
    ) {
      stop("`digits` must be NULL or a single non-negative number.")
    }
    digits <- as.integer(digits)
  }

  structure(
    list(
      kind = "numeric",
      expr = substitute(expr),
      env = parent.frame(),
      digits = digits
    ),
    class = "rtuiRenderer"
  )
}

#' Run a TUI application
#'
#' Starts the terminal event loop for an `rtuiApp`. This call is **blocking**:
#' it takes over the terminal until the user presses `Escape` or `Ctrl+Q`.
#'
#' @param app An `rtuiApp` object created by [tuiApp()].
#'
#' @return Invisibly returns `NULL`.
#'
#' @export
tuiRun <- function(app) {
  if (!inherits(app, "rtuiApp")) {
    stop("`app` must be an `rtuiApp` created by `tuiApp()`.")
  }

  runTuiApp(app$ui, app$state, app$handlers)
  invisible(NULL)
}

#' @export
print.rtuiApp <- function(x, ...) {
  cat("<rtuiApp>\n")
  cat("  input ids: ", paste(names(x$state$input), collapse = ", "), "\n")
  cat("  output ids:", paste(names(x$state$output), collapse = ", "), "\n")
  invisible(x)
}
