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

.rtuiReqTruth <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }

  if (inherits(x, "try-error")) {
    return(FALSE)
  }

  if (is.logical(x)) {
    if (length(x) == 0L || anyNA(x)) {
      return(FALSE)
    }
    return(any(x))
  }

  if (is.character(x)) {
    if (length(x) == 0L || anyNA(x)) {
      return(FALSE)
    }
    return(any(nzchar(x)))
  }

  if (is.numeric(x) || is.integer(x) || is.complex(x)) {
    if (length(x) == 0L || anyNA(x)) {
      return(FALSE)
    }
    return(any(x != 0))
  }

  if (is.raw(x)) {
    return(length(x) > 0L)
  }

  if (inherits(x, "data.frame")) {
    return(nrow(x) > 0L)
  }

  if (is.list(x)) {
    return(length(x) > 0L)
  }

  if (is.factor(x)) {
    if (length(x) == 0L || anyNA(x)) {
      return(FALSE)
    }
    return(any(nzchar(as.character(x))))
  }

  if (length(x) == 0L) {
    return(FALSE)
  }

  TRUE
}

.rtuiReqAbort <- function(message = NULL) {
  condition <- structure(
    list(message = message),
    class = c("rtui_req_error", "error", "condition")
  )
  stop(condition)
}

.rtuiReqDefaultValue <- function() {
  structure(list(), class = "rtuiReqDefault")
}

.rtuiReqPropagation <- function(error) {
  structure(list(error = error), class = "rtuiReqPropagation")
}

.rtuiHandleReqCondition <- function(runtime, reactiveId, err, cacheKey = NULL) {
  current <- .rtuiReactiveStoreEnsure(runtime, reactiveId, hasValue = FALSE)
  if (isTRUE(current$hasValue)) {
    current$dirty <- FALSE
    assign(reactiveId, current, envir = runtime$reactiveState)
    .rtuiSetReactiveChanged(runtime, reactiveId, FALSE)
    if (!is.null(cacheKey)) {
      assign(cacheKey, current$value, envir = runtime$currentReactiveCache)
    }
    return(current$value)
  }

  .rtuiReactiveStoreMarkDirty(runtime, reactiveId, dirty = FALSE)
  .rtuiSetReactiveChanged(runtime, reactiveId, FALSE)
  .rtuiReqPropagation(err)
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

.rtuiReactiveStoreSet <- function(runtime, reactiveId, value, hasValue = TRUE, dirty = FALSE) {
  assign(
    reactiveId,
    list(value = value, hasValue = hasValue, dirty = isTRUE(dirty)),
    envir = runtime$reactiveState
  )
}

.rtuiReactiveStoreEnsure <- function(
    runtime,
    reactiveId,
    value = NULL,
    hasValue = FALSE,
    dirty = !isTRUE(hasValue)
) {
  current <- .rtuiReactiveStoreGet(runtime, reactiveId)
  if (is.null(current)) {
    .rtuiReactiveStoreSet(
      runtime,
      reactiveId,
      value = value,
      hasValue = hasValue,
      dirty = dirty
    )
    return(.rtuiReactiveStoreGet(runtime, reactiveId))
  }

  if (is.null(current$dirty)) {
    current$dirty <- !isTRUE(current$hasValue)
    assign(reactiveId, current, envir = runtime$reactiveState)
  }
  current
}

.rtuiReactiveStoreMarkDirty <- function(runtime, reactiveId, dirty = TRUE) {
  current <- .rtuiReactiveStoreEnsure(runtime, reactiveId)
  current$dirty <- isTRUE(dirty)
  assign(reactiveId, current, envir = runtime$reactiveState)
}

.rtuiReactiveCacheKey <- function(nodeType, reactiveId) {
  paste0(nodeType, ":", reactiveId)
}

.rtuiDropReactiveCache <- function(runtime, reactiveId) {
  cachePrefixes <- c("reactive", "reactiveVal", "reactiveEvent")
  for (prefix in cachePrefixes) {
    cacheKey <- .rtuiReactiveCacheKey(prefix, reactiveId)
    if (exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
      rm(list = cacheKey, envir = runtime$currentReactiveCache)
    }
  }
}

.rtuiGraphMapGet <- function(mapEnv, key) {
  if (!exists(key, envir = mapEnv, inherits = FALSE)) {
    return(character())
  }

  value <- get(key, envir = mapEnv, inherits = FALSE)
  if (is.null(value) || length(value) == 0L) {
    return(character())
  }
  unique(as.character(value))
}

.rtuiGraphMapSet <- function(mapEnv, key, values) {
  normalized <- unique(as.character(values))
  if (length(normalized) == 0L) {
    if (exists(key, envir = mapEnv, inherits = FALSE)) {
      rm(list = key, envir = mapEnv)
    }
    return(invisible(NULL))
  }

  assign(key, normalized, envir = mapEnv)
  invisible(NULL)
}

.rtuiInputNodeId <- function(inputId) {
  paste0("input:", inputId)
}

.rtuiGraphEnsureRuntime <- function(runtime) {
  if (is.null(runtime$graphDependencies) || !is.environment(runtime$graphDependencies)) {
    runtime$graphDependencies <- new.env(parent = emptyenv())
  }
  if (is.null(runtime$graphDependents) || !is.environment(runtime$graphDependents)) {
    runtime$graphDependents <- new.env(parent = emptyenv())
  }
  if (is.null(runtime$graphNodeTypes) || !is.environment(runtime$graphNodeTypes)) {
    runtime$graphNodeTypes <- new.env(parent = emptyenv())
  }
  if (is.null(runtime$currentEvalDeps) || !is.environment(runtime$currentEvalDeps)) {
    runtime$currentEvalDeps <- new.env(parent = emptyenv())
  }
  if (is.null(runtime$graphEvalStack)) {
    runtime$graphEvalStack <- character()
  }
  if (is.null(runtime$currentRunId) || length(runtime$currentRunId) == 0L) {
    runtime$currentRunId <- 0L
  }
  if (is.null(runtime$currentInputState) || !is.list(runtime$currentInputState)) {
    runtime$currentInputState <- list()
  }
  if (is.null(runtime$currentCaptureDepth) || length(runtime$currentCaptureDepth) == 0L) {
    runtime$currentCaptureDepth <- 0L
  }
}

.rtuiGraphSetNodeType <- function(runtime, nodeId, nodeType) {
  assign(nodeId, nodeType, envir = runtime$graphNodeTypes)
}

.rtuiGraphNodeType <- function(runtime, nodeId) {
  if (!exists(nodeId, envir = runtime$graphNodeTypes, inherits = FALSE)) {
    return("reactive")
  }
  get(nodeId, envir = runtime$graphNodeTypes, inherits = FALSE)
}

.rtuiGraphEnsureReactiveNode <- function(
    runtime,
    reactiveId,
    nodeType,
    defaultValue = NULL,
    hasValue = FALSE
) {
  .rtuiGraphEnsureRuntime(runtime)
  .rtuiGraphSetNodeType(runtime, reactiveId, nodeType)
  .rtuiReactiveStoreEnsure(
    runtime,
    reactiveId,
    value = defaultValue,
    hasValue = hasValue,
    dirty = !isTRUE(hasValue)
  )
}

.rtuiGraphUpdateDependencies <- function(runtime, nodeId, dependencyIds) {
  nextDeps <- unique(as.character(dependencyIds))
  previousDeps <- .rtuiGraphMapGet(runtime$graphDependencies, nodeId)

  removedDeps <- setdiff(previousDeps, nextDeps)
  addedDeps <- setdiff(nextDeps, previousDeps)

  .rtuiGraphMapSet(runtime$graphDependencies, nodeId, nextDeps)

  for (dependencyId in removedDeps) {
    dependents <- .rtuiGraphMapGet(runtime$graphDependents, dependencyId)
    .rtuiGraphMapSet(
      runtime$graphDependents,
      dependencyId,
      setdiff(dependents, nodeId)
    )
  }

  for (dependencyId in addedDeps) {
    dependents <- .rtuiGraphMapGet(runtime$graphDependents, dependencyId)
    .rtuiGraphMapSet(
      runtime$graphDependents,
      dependencyId,
      c(dependents, nodeId)
    )
  }
}

.rtuiGraphBeginEvaluation <- function(runtime, reactiveId) {
  .rtuiGraphEnsureRuntime(runtime)
  stack <- runtime$graphEvalStack
  if (reactiveId %in% stack) {
    stop("Cyclic reactive dependency detected for `", reactiveId, "`.")
  }

  runtime$graphEvalStack <- c(stack, reactiveId)
  .rtuiGraphMapSet(runtime$currentEvalDeps, reactiveId, character())
}

.rtuiInIsolate <- function(runtime) {
  isTRUE(runtime$currentIsolateDepth > 0L)
}

.rtuiCaptureSuspended <- function(runtime) {
  isTRUE(runtime$currentCaptureDepth > 0L)
}

.rtuiGraphRegisterRead <- function(runtime, dependencyId) {
  if (.rtuiInIsolate(runtime) || .rtuiCaptureSuspended(runtime)) {
    return(invisible(NULL))
  }

  .rtuiGraphEnsureRuntime(runtime)
  stack <- runtime$graphEvalStack
  if (length(stack) == 0L) {
    return(invisible(NULL))
  }

  currentNodeId <- stack[[length(stack)]]
  dependencies <- .rtuiGraphMapGet(runtime$currentEvalDeps, currentNodeId)
  if (!(dependencyId %in% dependencies)) {
    .rtuiGraphMapSet(
      runtime$currentEvalDeps,
      currentNodeId,
      c(dependencies, dependencyId)
    )
  }

  invisible(NULL)
}

.rtuiGraphEndEvaluation <- function(runtime, reactiveId, success) {
  .rtuiGraphEnsureRuntime(runtime)
  stack <- runtime$graphEvalStack

  if (length(stack) > 0L) {
    if (identical(stack[[length(stack)]], reactiveId)) {
      runtime$graphEvalStack <- stack[-length(stack)]
    } else {
      runtime$graphEvalStack <- stack[stack != reactiveId]
    }
  }

  if (!isTRUE(success)) {
    if (exists(reactiveId, envir = runtime$currentEvalDeps, inherits = FALSE)) {
      rm(list = reactiveId, envir = runtime$currentEvalDeps)
    }
    return(invisible(NULL))
  }

  dependencies <- .rtuiGraphMapGet(runtime$currentEvalDeps, reactiveId)
  if (exists(reactiveId, envir = runtime$currentEvalDeps, inherits = FALSE)) {
    rm(list = reactiveId, envir = runtime$currentEvalDeps)
  }
  .rtuiGraphUpdateDependencies(runtime, reactiveId, dependencies)
  invisible(NULL)
}

.rtuiGraphInvalidateDependents <- function(runtime, sourceId) {
  .rtuiGraphEnsureRuntime(runtime)
  queue <- c(sourceId)
  seen <- character()

  while (length(queue) > 0L) {
    currentId <- queue[[1L]]
    queue <- queue[-1L]
    if (currentId %in% seen) {
      next
    }
    seen <- c(seen, currentId)

    dependents <- .rtuiGraphMapGet(runtime$graphDependents, currentId)
    if (length(dependents) == 0L) {
      next
    }

    for (dependentId in dependents) {
      .rtuiReactiveStoreMarkDirty(runtime, dependentId, dirty = TRUE)
      .rtuiDropReactiveCache(runtime, dependentId)
    }

    queue <- c(queue, dependents)
  }

  invisible(NULL)
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
  .rtuiReactiveStoreSet(runtime, reactiveId, value = value, hasValue = TRUE, dirty = FALSE)
  .rtuiSetReactiveChanged(runtime, reactiveId, changed)
  if (isTRUE(changed)) {
    .rtuiGraphInvalidateDependents(runtime, reactiveId)
  }
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
    eventValue <- tryCatch(
      .rtuiWithCaptureEnabled(runtime, eventSpec$object()),
      rtui_req_error = function(err) {
        .rtuiReqDefaultValue()
      }
    )
    if (inherits(eventValue, "rtuiReqDefault")) {
      return(FALSE)
    }
    reactiveChanged <- .rtuiGetReactiveChanged(runtime, eventSpec$reactiveId)
    if (isInitRun) {
      return(isTRUE(runAtInit))
    }
    return(reactiveChanged)
  }

  FALSE
}

.rtuiReactiveEventDependencies <- function(runtime, eventSpec, runAtInit) {
  dependencies <- character()

  if (identical(eventSpec$type, "input")) {
    return(.rtuiInputNodeId(eventSpec$inputId))
  }

  if (!identical(eventSpec$type, "reactive")) {
    return(dependencies)
  }

  dependencies <- c(dependencies, eventSpec$reactiveId)

  if (!is.null(runtime$currentEventId) || isTRUE(runAtInit)) {
    eventDeps <- .rtuiGraphMapGet(runtime$graphDependencies, eventSpec$reactiveId)
    if (length(eventDeps) > 0L) {
      dependencies <- c(dependencies, eventDeps)
    }
  }

  unique(dependencies)
}

.rtuiResolveOutputValue <- function(value) {
  if (!inherits(value, "rtuiRenderer")) {
    return(value)
  }

  runtime <- .rtuiCurrentRuntime()
  evalEnv <- new.env(parent = value$env)
  evalEnv$input <- runtime$currentInput
  evalEnv$output <- runtime$currentOutput

  rendered <- tryCatch(
    {
      evaluated <- eval(value$expr, envir = evalEnv)
      .rtuiEvalMaybeReactive(evaluated)
    },
    rtui_req_error = function(err) {
      .rtuiReqDefaultValue()
    }
  )

  if (identical(value$kind, "text")) {
    if (inherits(rendered, "rtuiReqDefault")) {
      return("")
    }
    if (is.null(rendered) || length(rendered) == 0L) {
      return("")
    }
    return(as.character(rendered[[1L]]))
  }

  if (identical(value$kind, "numeric")) {
    if (inherits(rendered, "rtuiReqDefault")) {
      return("NA")
    }
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

.rtuiInputEnv <- function(runtime, inputState) {
  input <- new.env(parent = emptyenv())

  for (id in names(inputState)) {
    local({
      inputId <- id
      makeActiveBinding(
        inputId,
        function(value) {
          if (!missing(value)) {
            stop("`input` bindings are read-only.")
          }
          .rtuiGraphRegisterRead(runtime, .rtuiInputNodeId(inputId))
          runtime$currentInputState[[inputId]]
        },
        env = input
      )
    })
  }

  input
}

.rtuiWithCaptureSuspended <- function(runtime, expr) {
  runtime$currentCaptureDepth <- runtime$currentCaptureDepth + 1L
  on.exit({
    runtime$currentCaptureDepth <- max(0L, runtime$currentCaptureDepth - 1L)
  }, add = TRUE)
  expr
}

.rtuiWithCaptureEnabled <- function(runtime, expr) {
  previousDepth <- runtime$currentCaptureDepth
  runtime$currentCaptureDepth <- 0L
  on.exit({
    runtime$currentCaptureDepth <- previousDepth
  }, add = TRUE)
  expr
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
  runtime$currentCaptureDepth <- 0L
  runtime$currentRunId <- 0L
  runtime$currentInputState <- list()
  runtime$graphDependencies <- new.env(parent = emptyenv())
  runtime$graphDependents <- new.env(parent = emptyenv())
  runtime$graphNodeTypes <- new.env(parent = emptyenv())
  runtime$currentEvalDeps <- new.env(parent = emptyenv())
  runtime$graphEvalStack <- character()

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

  .rtuiGraphEnsureRuntime(runtime)
  runtime$currentRunId <- runtime$currentRunId + 1L
  runtime$reactiveIndex <- 0L
  runtime$currentReactiveCache <- new.env(parent = emptyenv())
  runtime$currentReactiveChanged <- new.env(parent = emptyenv())
  runtime$currentEventId <- eventId
  runtime$currentInputState <- inputState
  runtime$currentOutput <- NULL
  runtime$currentIsolateDepth <- 0L
  runtime$currentCaptureDepth <- 0L
  runtime$graphEvalStack <- character()
  runtime$currentEvalDeps <- new.env(parent = emptyenv())

  input <- .rtuiInputEnv(runtime, inputState)
  output <- list2env(outputState, parent = emptyenv())

  runtime$currentInput <- input
  runtime$currentOutput <- output

  if (!is.null(eventId)) {
    .rtuiGraphInvalidateDependents(runtime, .rtuiInputNodeId(eventId))
  }

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
    .rtuiGraphEnsureReactiveNode(runtime, reactiveId, "reactive", hasValue = FALSE)
    .rtuiGraphRegisterRead(runtime, reactiveId)

    cacheKey <- .rtuiReactiveCacheKey("reactive", reactiveId)
    if (exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
      return(get(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE))
    }

    current <- .rtuiReactiveStoreEnsure(runtime, reactiveId, hasValue = FALSE)
    if (isTRUE(current$hasValue) && !isTRUE(current$dirty)) {
      value <- current$value
      assign(cacheKey, value, envir = runtime$currentReactiveCache)
      return(value)
    }

    success <- FALSE
    .rtuiGraphBeginEvaluation(runtime, reactiveId)
    on.exit({
      .rtuiGraphEndEvaluation(runtime, reactiveId, success)
    }, add = TRUE)

    value <- tryCatch(
      eval(exprSub, envir = exprEnv),
      rtui_req_error = function(err) {
        .rtuiHandleReqCondition(runtime, reactiveId, err, cacheKey = cacheKey)
      }
    )
    if (inherits(value, "rtuiReqPropagation")) {
      success <- TRUE
      stop(value$error)
    }
    value <- .rtuiEvalMaybeReactive(value)
    if (inherits(value, "rtuiReqPropagation")) {
      success <- TRUE
      stop(value$error)
    }
    value <- .rtuiUpdateReactiveState(runtime, reactiveId, value)

    success <- TRUE
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

  .rtuiGraphEnsureReactiveNode(
    runtime,
    reactiveId,
    "reactiveVal",
    defaultValue = value,
    hasValue = TRUE
  )

  reactiveValue <- function(value) {
    runtime <- .rtuiCurrentRuntime()
    .rtuiGraphEnsureReactiveNode(runtime, reactiveId, "reactiveVal", hasValue = FALSE)
    current <- .rtuiReactiveStoreEnsure(runtime, reactiveId, hasValue = FALSE)
    cacheKey <- .rtuiReactiveCacheKey("reactiveVal", reactiveId)

    if (missing(value)) {
      .rtuiGraphRegisterRead(runtime, reactiveId)
      if (!exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
        assign(cacheKey, current$value, envir = runtime$currentReactiveCache)
      }
      return(current$value)
    }

    changed <- !isTRUE(current$hasValue) || !identical(current$value, value)
    .rtuiReactiveStoreSet(runtime, reactiveId, value = value, hasValue = TRUE, dirty = FALSE)
    .rtuiSetReactiveChanged(runtime, reactiveId, changed, force = TRUE)
    assign(cacheKey, value, envir = runtime$currentReactiveCache)
    if (isTRUE(changed)) {
      .rtuiGraphInvalidateDependents(runtime, reactiveId)
    }
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
    .rtuiGraphEnsureReactiveNode(runtime, reactiveId, "reactiveEvent", hasValue = FALSE)
    .rtuiGraphRegisterRead(runtime, reactiveId)
    dependencyIds <- .rtuiReactiveEventDependencies(runtime, eventSpec, runAtInit)
    .rtuiGraphUpdateDependencies(runtime, reactiveId, dependencyIds)

    cacheKey <- .rtuiReactiveCacheKey("reactiveEvent", reactiveId)
    if (exists(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE)) {
      return(get(cacheKey, envir = runtime$currentReactiveCache, inherits = FALSE))
    }

    current <- .rtuiReactiveStoreEnsure(runtime, reactiveId, hasValue = FALSE)
    if (!isTRUE(current$dirty) && isTRUE(current$hasValue)) {
      assign(cacheKey, current$value, envir = runtime$currentReactiveCache)
      return(current$value)
    }
    if (!isTRUE(current$dirty) && !isTRUE(current$hasValue)) {
      .rtuiSetReactiveChanged(runtime, reactiveId, FALSE)
      assign(cacheKey, NULL, envir = runtime$currentReactiveCache)
      return(NULL)
    }

    if (.rtuiShouldTriggerEvent(eventSpec, runAtInit = runAtInit)) {
      success <- FALSE
      .rtuiGraphBeginEvaluation(runtime, reactiveId)
      on.exit({
        .rtuiGraphEndEvaluation(runtime, reactiveId, success)
      }, add = TRUE)

      .rtuiGraphMapSet(runtime$currentEvalDeps, reactiveId, dependencyIds)
      value <- tryCatch(
        .rtuiWithCaptureSuspended(runtime, eval(exprSub, envir = exprEnv)),
        rtui_req_error = function(err) {
          .rtuiHandleReqCondition(runtime, reactiveId, err, cacheKey = cacheKey)
        }
      )
      if (inherits(value, "rtuiReqPropagation")) {
        success <- TRUE
        stop(value$error)
      }
      value <- .rtuiEvalMaybeReactive(value)
      if (inherits(value, "rtuiReqPropagation")) {
        success <- TRUE
        stop(value$error)
      }
      value <- .rtuiUpdateReactiveState(runtime, reactiveId, value)
      success <- TRUE
      assign(cacheKey, value, envir = runtime$currentReactiveCache)
      return(value)
    }

    previous <- .rtuiReactiveStoreGet(runtime, reactiveId)
    .rtuiReactiveStoreMarkDirty(runtime, reactiveId, dirty = FALSE)
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
  tryCatch(
    eval.parent(substitute(expr)),
    rtui_req_error = function(err) {
      invisible(NULL)
    }
  )
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
    tryCatch(
      eval.parent(substitute(expr)),
      rtui_req_error = function(err) {
        invisible(NULL)
      }
    )
  }

  invisible(NULL)
}

#' Require values to be available/truthy in reactive code
#'
#' Stops the current reactive/render evaluation when any supplied value is not
#' considered available (falsy), similarly to `shiny::req()`.
#'
#' @param ... Values to validate.
#' @param cancelOutput Ignored for now. Present for API compatibility.
#'
#' @return Invisibly returns the first supplied value when all values are truthy.
#'
#' @export
tuiReq <- function(..., cancelOutput = FALSE) {
  if (!is.logical(cancelOutput) || length(cancelOutput) != 1L || is.na(cancelOutput)) {
    stop("`cancelOutput` must be TRUE or FALSE.")
  }

  values <- list(...)
  if (length(values) == 0L) {
    .rtuiReqAbort()
  }

  for (value in values) {
    if (!isTRUE(.rtuiReqTruth(value))) {
      .rtuiReqAbort()
    }
  }

  invisible(values[[1L]])
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
