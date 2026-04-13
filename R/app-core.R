.rtuiTerminalWidthId <- "terminalWidth"
.rtuiTerminalHeightId <- "terminalHeight"
.rtuiTerminalResizeHandlerId <- ".rtui_terminal_resize"
.rtuiTimerTickId <- ".rtui_timer_tick"

#' Create a TUI application
#'
#' Defines a terminal UI application from a UI tree and a server function.
#' The runtime runs `server(input, output)` once during initialization to
#' register outputs and observers, then re-evaluates only invalidated reactive
#' graph nodes for each input event.
#'
#' @param ui A UI component tree built with [tuiColumn()], [tuiRow()],
#'   [tuiShowIf()], [tuiModal()], [tuiBox()], [tuiOutputText()],
#'   [tuiOutputNumeric()], [tuiOutputTable()], [tuiInputButton()],
#'   [tuiInputText()], [tuiInputCheckbox()], or [tuiInputDropdown()].
#' @param server A function called as `server(input, output)`. Both `input`
#'   and `output` are environments:
#'   - `input$<id>` is updated automatically from buttons, text inputs,
#'     checkboxes, dropdowns, and table header click events.
#'   - `input$terminalWidth` and `input$terminalHeight` are automatically
#'     managed read-only reactive inputs reflecting the current terminal size.
#'   - assign rendered outputs with `output$<name> <- tuiRenderText(...)` or
#'     `output$<name> <- tuiRenderNumeric(...)` /
#'     `output$<name> <- tuiRenderTable(...)`.
#'   - use [tuiObserve()] / [tuiObserveEvent()] for reactive side effects.
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
  inputIds <- c(
    meta$buttonIds,
    meta$textInputIds,
    meta$checkboxIds,
    meta$dropdownIds,
    meta$tableHeaderClickIds
  )
  duplicatedIds <- unique(inputIds[duplicated(inputIds)])
  if (length(duplicatedIds) > 0L) {
    stop(
      "Component ids must be unique across buttons, text inputs, checkboxes, dropdowns, and table header click ids. Duplicates: ",
      paste(duplicatedIds, collapse = ", ")
    )
  }
  reservedInputIds <- c(.rtuiTerminalWidthId, .rtuiTerminalHeightId, .rtuiTimerTickId)
  conflictingReservedIds <- intersect(inputIds, reservedInputIds)
  if (length(conflictingReservedIds) > 0L) {
    stop(
      "Component ids cannot use reserved runtime input ids: ",
      paste(conflictingReservedIds, collapse = ", "),
      "."
    )
  }

  runtime <- .rtuiCreateRuntime(meta$outputIds)
  runtime$currentDropdownChoices <- meta$dropdownChoices
  runtime$currentInputTypes <- c(
    stats::setNames(rep("button", length(meta$buttonIds)), meta$buttonIds),
    stats::setNames(rep("text", length(meta$textInputIds)), meta$textInputIds),
    stats::setNames(rep("checkbox", length(meta$checkboxIds)), meta$checkboxIds),
    stats::setNames(rep("dropdown", length(meta$dropdownIds)), meta$dropdownIds),
    stats::setNames(rep("tableHeaderClick", length(meta$tableHeaderClickIds)), meta$tableHeaderClickIds),
    stats::setNames(rep("runtime", 3L), c(.rtuiTerminalWidthId, .rtuiTerminalHeightId, .rtuiTimerTickId))
  )
  inputState <- .rtuiInitialInput(meta)
  outputState <- .rtuiInitialOutput(meta)
  runtime$currentInputState <- inputState
  runtime$currentOutputState <- outputState
  .rtuiInitializeOutputStore(runtime, outputState)

  input <- .rtuiInputEnv(runtime, inputState)
  output <- .rtuiOutputEnv(runtime, meta$outputIds)
  runtime$currentInput <- input
  runtime$currentOutput <- output

  .rtuiWithRuntime(runtime, {
    server(input, output)
    .rtuiFlushRuntime(runtime, eventId = NULL, forceAll = TRUE)
  })

  state <- list(
    input = runtime$currentInputState,
    output = runtime$currentOutputState,
    dropdownChoices = runtime$currentDropdownChoices,
    timersActive = length(runtime$timers) > 0L
  )

  handlerIds <- unique(c(
    meta$buttonIds,
    meta$textInputIds,
    meta$checkboxIds,
    meta$dropdownIds,
    meta$tableHeaderClickIds,
    .rtuiTimerTickId
  ))
  handlers <- stats::setNames(vector("list", length(handlerIds)), handlerIds)
  for (id in handlerIds) {
    handlers[[id]] <- local({
      currentEventId <- id
      function(state) {
        runtime$currentInputState <- state$input
        runtime$currentDropdownChoices <- if (!is.null(state$dropdownChoices)) {
          state$dropdownChoices
        } else {
          runtime$currentDropdownChoices
        }
        .rtuiWithRuntime(runtime, {
          .rtuiFlushRuntime(runtime, eventId = currentEventId, forceAll = FALSE)
        })
        state$input <- runtime$currentInputState
        state$output <- runtime$currentOutputState
        state$dropdownChoices <- runtime$currentDropdownChoices
        state$timersActive <- length(runtime$timers) > 0L
        state
      }
    })
  }

  handlers[[.rtuiTerminalResizeHandlerId]] <- function(state) {
    runtime$currentInputState <- state$input
    runtime$currentDropdownChoices <- if (!is.null(state$dropdownChoices)) {
      state$dropdownChoices
    } else {
      runtime$currentDropdownChoices
    }
    .rtuiWithRuntime(runtime, {
      .rtuiGraphInvalidateDependents(
        runtime,
        .rtuiInputNodeId(.rtuiTerminalWidthId)
      )
      .rtuiGraphInvalidateDependents(
        runtime,
        .rtuiInputNodeId(.rtuiTerminalHeightId)
      )
      .rtuiFlushRuntime(runtime, eventId = NULL, forceAll = FALSE)
    })
    state$input <- runtime$currentInputState
    state$output <- runtime$currentOutputState
    state$dropdownChoices <- runtime$currentDropdownChoices
    state$timersActive <- length(runtime$timers) > 0L
    state
  }

  handlers[[.rtuiTimerTickId]] <- function(state) {
    runtime$currentInputState <- state$input
    runtime$currentDropdownChoices <- if (!is.null(state$dropdownChoices)) {
      state$dropdownChoices
    } else {
      runtime$currentDropdownChoices
    }
    if (is.null(runtime$timers) || length(runtime$timers) == 0L) {
      state$input <- runtime$currentInputState
      state$output <- runtime$currentOutputState
      state$dropdownChoices <- runtime$currentDropdownChoices
      state$timersActive <- FALSE
      return(state)
    }
    .rtuiWithRuntime(runtime, {
      .rtuiProcessTimers(runtime)
      .rtuiFlushRuntime(runtime, eventId = .rtuiTimerTickId, forceAll = FALSE)
    })
    state$input <- runtime$currentInputState
    state$output <- runtime$currentOutputState
    state$dropdownChoices <- runtime$currentDropdownChoices
    state$timersActive <- length(runtime$timers) > 0L
    state
  }

  structure(
    list(state = state, ui = ui, handlers = handlers, server = server),
    class = "rtuiApp"
  )
}

#' Internal helper `.rtuiCollectUiMeta`.
#'
#' Traverses the UI tree to collect output/input ids and defaults.
#'
#' @param node UI node (and children) to traverse.
#'
#' @return List with output ids, button ids, text input ids, and text defaults.
#'
#' @keywords internal
#' @noRd
.rtuiCollectUiMeta <- function(node) {
  outputIds <- character()
  buttonIds <- character()
  textInputIds <- character()
  textInputDefaults <- list()
  checkboxIds <- character()
  checkboxDefaults <- list()
  dropdownIds <- character()
  dropdownDefaults <- list()
  dropdownChoices <- list()
  tableHeaderClickIds <- character()

  walk <- function(x) {
    type <- x[["type"]]

    if (type %in% c("outputText", "outputNumeric", "outputTable")) {
      outputIds <<- c(outputIds, x[["outputId"]])
    }
    if (identical(type, "outputTable") && !is.null(x[["headerClickId"]])) {
      tableHeaderClickIds <<- c(tableHeaderClickIds, x[["headerClickId"]])
    }
    if (identical(type, "button")) {
      buttonIds <<- c(buttonIds, x[["id"]])
    }
    if (identical(type, "input")) {
      inputId <- x[["id"]]
      textInputIds <<- c(textInputIds, inputId)
      defaultValue <- if (is.null(x[["value"]])) "" else x[["value"]]
      textInputDefaults[[inputId]] <<- defaultValue
    }
    if (identical(type, "checkbox")) {
      inputId <- x[["id"]]
      checkboxIds <<- c(checkboxIds, inputId)
      checkboxDefaults[[inputId]] <<- isTRUE(x[["value"]])
    }
    if (identical(type, "dropdown")) {
      inputId <- x[["id"]]
      dropdownIds <<- c(dropdownIds, inputId)
      dropdownChoices[[inputId]] <<- as.character(x[["choices"]])
      defaultValue <- if (is.null(x[["value"]])) dropdownChoices[[inputId]][[1L]] else x[["value"]]
      dropdownDefaults[[inputId]] <<- defaultValue
    }

    children <- x[["children", exact = TRUE]]
    if (!is.null(children)) {
      for (child in children) {
        walk(child)
      }
    }
    child <- x[["child", exact = TRUE]]
    if (!is.null(child)) {
      walk(child)
    }
    modal <- x[["modal", exact = TRUE]]
    if (!is.null(modal)) {
      walk(modal)
    }
  }

  walk(node)

  textInputIds <- unique(textInputIds)
  checkboxIds <- unique(checkboxIds)
  dropdownIds <- unique(dropdownIds)
  tableHeaderClickIds <- unique(tableHeaderClickIds)

  list(
    outputIds = unique(outputIds),
    buttonIds = unique(buttonIds),
    textInputIds = textInputIds,
    textInputDefaults = textInputDefaults[textInputIds],
    checkboxIds = checkboxIds,
    checkboxDefaults = checkboxDefaults[checkboxIds],
    dropdownIds = dropdownIds,
    dropdownDefaults = dropdownDefaults[dropdownIds],
    dropdownChoices = dropdownChoices[dropdownIds],
    tableHeaderClickIds = tableHeaderClickIds
  )
}

#' Internal helper `.rtuiInitialInput`.
#'
#' Builds initial input state from collected UI metadata.
#'
#' @param meta Metadata list produced by `.rtuiCollectUiMeta()`.
#'
#' @return Named list with initial values for all inputs.
#'
#' @keywords internal
#' @noRd
.rtuiInitialInput <- function(meta) {
  ids <- c(
    meta$buttonIds,
    meta$textInputIds,
    meta$checkboxIds,
    meta$dropdownIds,
    meta$tableHeaderClickIds
  )
  ids <- c(ids, .rtuiTerminalWidthId, .rtuiTerminalHeightId, .rtuiTimerTickId)
  input <- stats::setNames(vector("list", length(ids)), ids)

  for (id in meta$buttonIds) {
    input[[id]] <- 0L
  }

  textDefaults <- if (is.null(meta$textInputDefaults)) list() else meta$textInputDefaults
  for (id in meta$textInputIds) {
    input[[id]] <- if (!is.null(textDefaults[[id]])) textDefaults[[id]] else ""
  }

  checkboxDefaults <- if (is.null(meta$checkboxDefaults)) list() else meta$checkboxDefaults
  for (id in meta$checkboxIds) {
    input[[id]] <- if (!is.null(checkboxDefaults[[id]])) isTRUE(checkboxDefaults[[id]]) else FALSE
  }

  dropdownDefaults <- if (is.null(meta$dropdownDefaults)) list() else meta$dropdownDefaults
  dropdownChoices <- if (is.null(meta$dropdownChoices)) list() else meta$dropdownChoices
  for (id in meta$dropdownIds) {
    fallback <- dropdownChoices[[id]]
    fallback <- if (is.null(fallback) || length(fallback) == 0L) "" else as.character(fallback[[1L]])
    input[[id]] <- if (!is.null(dropdownDefaults[[id]])) as.character(dropdownDefaults[[id]]) else fallback
  }
  for (id in meta$tableHeaderClickIds) {
    input[id] <- list(NULL)
  }

  input[[.rtuiTerminalWidthId]] <- 0L
  input[[.rtuiTerminalHeightId]] <- 0L
  input[[.rtuiTimerTickId]] <- 0L

  input
}

#' Internal helper `.rtuiInitialOutput`.
#'
#' Builds initial output state from collected UI metadata.
#'
#' @param meta Metadata list produced by `.rtuiCollectUiMeta()`.
#'
#' @return Named list with initial values for all outputs.
#'
#' @keywords internal
#' @noRd
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

#' Internal helper `.rtuiCreateRuntime`.
#'
#' Builds the runtime environment used by one app instance.
#'
#' @param outputIds Character vector of declared output identifiers.
#'
#' @return Runtime environment initialized with scheduler fields.
#'
#' @keywords internal
#' @noRd
.rtuiCreateRuntime <- function(outputIds) {
  runtime <- new.env(parent = emptyenv())
  runtime$reactiveState <- new.env(parent = emptyenv())
  runtime$reactiveIndex <- 0L
  runtime$observerIndex <- 0L
  runtime$currentReactiveCache <- new.env(parent = emptyenv())
  runtime$currentReactiveChanged <- new.env(parent = emptyenv())
  runtime$currentEventId <- NULL
  runtime$currentInput <- NULL
  runtime$currentOutput <- NULL
  runtime$currentIsolateDepth <- 0L
  runtime$currentCaptureDepth <- 0L
  runtime$currentRunId <- 0L
  runtime$currentInputState <- list()
  runtime$currentOutputState <- list()
  runtime$graphDependencies <- new.env(parent = emptyenv())
  runtime$graphDependents <- new.env(parent = emptyenv())
  runtime$graphNodeTypes <- new.env(parent = emptyenv())
  runtime$currentEvalDeps <- new.env(parent = emptyenv())
  runtime$graphEvalStack <- character()
  runtime$outputIds <- as.character(outputIds)
  runtime$outputDefinitions <- new.env(parent = emptyenv())
  runtime$observers <- new.env(parent = emptyenv())
  runtime$observerOrder <- character()
  runtime$selfInvalidationWarnings <- new.env(parent = emptyenv())
  runtime$currentDropdownChoices <- list()
  runtime$currentInputTypes <- character()
  runtime$currentUpdatedInputIds <- character()
  runtime$timers <- list()
  runtime$timerIndex <- 0L
  runtime$timerTickMs <- 50L
  runtime
}

#' Internal helper `.rtuiWithRuntime`.
#'
#' Temporarily binds a runtime to the global runtime context.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param expr Expression to evaluate while runtime is bound.
#'
#' @return Value of `expr`.
#'
#' @keywords internal
#' @noRd
.rtuiWithRuntime <- function(runtime, expr) {
  oldRuntime <- .rtuiRuntimeContext$currentRuntime
  .rtuiRuntimeContext$currentRuntime <- runtime
  on.exit({
    .rtuiRuntimeContext$currentRuntime <- oldRuntime
  }, add = TRUE)
  expr
}

#' Internal helper `.rtuiOutputNodeId`.
#'
#' Builds the graph node id used for an output id.
#'
#' @param outputId Output identifier.
#'
#' @return A graph node id of the form `"output:<id>"`.
#'
#' @keywords internal
#' @noRd
.rtuiOutputNodeId <- function(outputId) {
  paste0("output:", outputId)
}

#' Internal helper `.rtuiInitializeOutputStore`.
#'
#' Initializes output definitions and graph nodes with default values.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param outputState Named list of output defaults.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiInitializeOutputStore <- function(runtime, outputState) {
  for (outputId in names(outputState)) {
    assign(outputId, outputState[[outputId]], envir = runtime$outputDefinitions)
    .rtuiGraphEnsureReactiveNode(
      runtime,
      .rtuiOutputNodeId(outputId),
      "output",
      defaultValue = outputState[[outputId]],
      hasValue = TRUE
    )
  }
  invisible(NULL)
}

#' Internal helper `.rtuiSetOutputDefinition`.
#'
#' Stores a renderer/raw output definition and marks it dirty.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param outputId Output identifier.
#' @param value Value assigned to `output$<id>`.
#'
#' @return Invisibly returns `value`.
#'
#' @keywords internal
#' @noRd
.rtuiSetOutputDefinition <- function(runtime, outputId, value) {
  assign(outputId, value, envir = runtime$outputDefinitions)
  if (is.null(runtime$currentOutputState[[outputId]])) {
    runtime$currentOutputState[[outputId]] <- ""
  }

  nodeId <- .rtuiOutputNodeId(outputId)
  .rtuiGraphEnsureReactiveNode(
    runtime,
    nodeId,
    "output",
    defaultValue = runtime$currentOutputState[[outputId]],
    hasValue = TRUE
  )
  .rtuiReactiveStoreMarkDirty(runtime, nodeId, dirty = TRUE)
  invisible(value)
}

#' Internal helper `.rtuiInputEnv`.
#'
#' Creates the active-binding `input` environment for server execution.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param inputState Named list containing current input values.
#'
#' @return Environment with active bindings for each input id.
#'
#' @keywords internal
#' @noRd
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

#' Internal helper `.rtuiOutputEnv`.
#'
#' Creates the active-binding `output` environment for server execution.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param outputIds Character vector of output identifiers.
#'
#' @return Environment with active bindings for each output id.
#'
#' @keywords internal
#' @noRd
.rtuiOutputEnv <- function(runtime, outputIds) {
  output <- new.env(parent = emptyenv())

  for (id in outputIds) {
    local({
      outputId <- id
      makeActiveBinding(
        outputId,
        function(value) {
          if (missing(value)) {
            current <- runtime$currentOutputState[[outputId]]
            if (is.null(current)) {
              return("")
            }
            return(current)
          }
          .rtuiSetOutputDefinition(runtime, outputId, value)
        },
        env = output
      )
    })
  }

  output
}

#' Internal helper `.rtuiRegisterObserver`.
#'
#' Registers a persistent observer node in the scheduler.
#'
#' @param exprSub Quoted observer expression.
#' @param exprEnv Environment where the expression should be evaluated.
#' @param type Observer type, either `"observe"` or `"observeEvent"`.
#' @param eventSpec Optional normalized event spec for event observers.
#' @param runAtInit Whether event observers should run at initialization.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiRegisterObserver <- function(
    exprSub,
    exprEnv,
    type = "observe",
    eventSpec = NULL,
    runAtInit = FALSE
) {
  runtime <- .rtuiCurrentRuntime()
  runtime$observerIndex <- runtime$observerIndex + 1L
  observerId <- paste0("observer_", runtime$observerIndex)

  assign(
    observerId,
    list(
      id = observerId,
      type = type,
      expr = exprSub,
      env = exprEnv,
      eventSpec = eventSpec,
      runAtInit = isTRUE(runAtInit)
    ),
    envir = runtime$observers
  )
  runtime$observerOrder <- c(runtime$observerOrder, observerId)

  .rtuiGraphEnsureReactiveNode(runtime, observerId, "observer", hasValue = TRUE)
  .rtuiReactiveStoreMarkDirty(runtime, observerId, dirty = TRUE)
  invisible(NULL)
}

#' Internal helper `.rtuiDirtyObserverIds`.
#'
#' Returns currently dirty observer ids in registration order.
#'
#' @param runtime Runtime environment created for the current app instance.
#'
#' @return Character vector of dirty observer node ids.
#'
#' @keywords internal
#' @noRd
.rtuiDirtyObserverIds <- function(runtime) {
  dirty <- character()
  for (observerId in runtime$observerOrder) {
    current <- .rtuiReactiveStoreEnsure(runtime, observerId, hasValue = TRUE, dirty = TRUE)
    if (isTRUE(current$dirty)) {
      dirty <- c(dirty, observerId)
    }
  }
  dirty
}

#' Internal helper `.rtuiDirtyOutputIds`.
#'
#' Returns currently dirty output ids in declaration order.
#'
#' @param runtime Runtime environment created for the current app instance.
#'
#' @return Character vector of dirty output ids.
#'
#' @keywords internal
#' @noRd
.rtuiDirtyOutputIds <- function(runtime) {
  dirty <- character()
  for (outputId in runtime$outputIds) {
    nodeId <- .rtuiOutputNodeId(outputId)
    current <- .rtuiReactiveStoreEnsure(
      runtime,
      nodeId,
      value = runtime$currentOutputState[[outputId]],
      hasValue = TRUE,
      dirty = TRUE
    )
    if (isTRUE(current$dirty)) {
      dirty <- c(dirty, outputId)
    }
  }
  dirty
}

#' Internal helper `.rtuiMarkAllDirty`.
#'
#' Marks all observer and output nodes dirty.
#'
#' @param runtime Runtime environment created for the current app instance.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
#' @noRd
.rtuiMarkAllDirty <- function(runtime) {
  for (observerId in runtime$observerOrder) {
    .rtuiReactiveStoreMarkDirty(runtime, observerId, dirty = TRUE)
  }
  for (outputId in runtime$outputIds) {
    .rtuiReactiveStoreMarkDirty(runtime, .rtuiOutputNodeId(outputId), dirty = TRUE)
  }
  invisible(NULL)
}

#' Internal helper `.rtuiEvaluateObserver`.
#'
#' Evaluates one dirty observer and updates its graph dependencies.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param observerId Internal identifier of an observer node.
#'
#' @return `TRUE` if evaluation happened, `FALSE` otherwise.
#'
#' @keywords internal
#' @noRd
.rtuiEvaluateObserver <- function(runtime, observerId) {
  if (!exists(observerId, envir = runtime$observers, inherits = FALSE)) {
    return(FALSE)
  }

  current <- .rtuiReactiveStoreEnsure(runtime, observerId, hasValue = TRUE, dirty = TRUE)
  if (!isTRUE(current$dirty)) {
    return(FALSE)
  }

  observer <- get(observerId, envir = runtime$observers, inherits = FALSE)

  success <- FALSE
  .rtuiGraphBeginEvaluation(runtime, observerId)
  on.exit({
    .rtuiGraphEndEvaluation(runtime, observerId, success)
  }, add = TRUE)

  shouldRun <- TRUE
  if (identical(observer$type, "observeEvent")) {
    dependencyIds <- .rtuiReactiveEventDependencies(
      runtime,
      observer$eventSpec,
      observer$runAtInit
    )
    .rtuiGraphMapSet(runtime$currentEvalDeps, observerId, dependencyIds)
    shouldRun <- .rtuiShouldTriggerEvent(
      observer$eventSpec,
      runAtInit = observer$runAtInit
    )
  }

  if (isTRUE(shouldRun)) {
    tryCatch(
      {
        if (identical(observer$type, "observeEvent")) {
          .rtuiWithCaptureSuspended(runtime, eval(observer$expr, envir = observer$env))
        } else {
          eval(observer$expr, envir = observer$env)
        }
      },
      rtui_req_error = function(err) {
        invisible(NULL)
      }
    )
  }

  .rtuiReactiveStoreMarkDirty(runtime, observerId, dirty = FALSE)
  .rtuiSetReactiveChanged(runtime, observerId, FALSE, force = TRUE)
  success <- TRUE
  TRUE
}

#' Internal helper `.rtuiEvaluateOutput`.
#'
#' Evaluates one dirty output definition and stores its resolved value.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param outputId Output identifier to evaluate.
#'
#' @return `TRUE` if evaluation happened, `FALSE` otherwise.
#'
#' @keywords internal
#' @noRd
.rtuiEvaluateOutput <- function(runtime, outputId) {
  nodeId <- .rtuiOutputNodeId(outputId)
  current <- .rtuiReactiveStoreEnsure(
    runtime,
    nodeId,
    value = runtime$currentOutputState[[outputId]],
    hasValue = TRUE,
    dirty = TRUE
  )
  if (!isTRUE(current$dirty)) {
    return(FALSE)
  }

  definition <- runtime$currentOutputState[[outputId]]
  if (exists(outputId, envir = runtime$outputDefinitions, inherits = FALSE)) {
    definition <- get(outputId, envir = runtime$outputDefinitions, inherits = FALSE)
  }

  success <- FALSE
  .rtuiGraphBeginEvaluation(runtime, nodeId)
  on.exit({
    .rtuiGraphEndEvaluation(runtime, nodeId, success)
  }, add = TRUE)

  value <- .rtuiResolveOutputValue(definition)
  previous <- if (isTRUE(current$hasValue)) current$value else runtime$currentOutputState[[outputId]]
  changed <- !isTRUE(current$hasValue) || !identical(previous, value)

  .rtuiReactiveStoreSet(runtime, nodeId, value = value, hasValue = TRUE, dirty = FALSE)
  .rtuiSetReactiveChanged(runtime, nodeId, changed, force = TRUE)
  runtime$currentOutputState[[outputId]] <- value

  success <- TRUE
  TRUE
}

#' Internal helper `.rtuiFlushRuntime`.
#'
#' Flushes dirty observer/output nodes until the graph reaches a stable state.
#'
#' @param runtime Runtime environment created for the current app instance.
#' @param eventId Optional input id that triggered this flush.
#' @param forceAll If `TRUE`, marks all observers and outputs dirty before flush.
#'
#' @return Named list with current resolved output values.
#'
#' @keywords internal
#' @noRd
.rtuiFlushRuntime <- function(runtime, eventId = NULL, forceAll = FALSE) {
  .rtuiGraphEnsureRuntime(runtime)
  runtime$currentRunId <- runtime$currentRunId + 1L
  runtime$currentReactiveCache <- new.env(parent = emptyenv())
  runtime$currentReactiveChanged <- new.env(parent = emptyenv())
  runtime$currentEventId <- eventId
  runtime$currentIsolateDepth <- 0L
  runtime$currentCaptureDepth <- 0L
  runtime$currentUpdatedInputIds <- character()
  runtime$graphEvalStack <- character()
  runtime$currentEvalDeps <- new.env(parent = emptyenv())

  if (isTRUE(forceAll)) {
    .rtuiMarkAllDirty(runtime)
  }

  if (!is.null(eventId)) {
    .rtuiGraphInvalidateDependents(runtime, .rtuiInputNodeId(eventId))
  }

  maxIterations <- 1000L
  iterations <- 0L
  repeat {
    iterations <- iterations + 1L
    if (iterations > maxIterations) {
      stop("Reactive flush exceeded 1000 iterations; possible invalidation loop.")
    }

    didWork <- FALSE

    dirtyObservers <- .rtuiDirtyObserverIds(runtime)
    if (length(dirtyObservers) > 0L) {
      for (observerId in dirtyObservers) {
        didWork <- isTRUE(.rtuiEvaluateObserver(runtime, observerId)) || didWork
      }
    }

    dirtyOutputs <- .rtuiDirtyOutputIds(runtime)
    if (length(dirtyOutputs) > 0L) {
      for (outputId in dirtyOutputs) {
        didWork <- isTRUE(.rtuiEvaluateOutput(runtime, outputId)) || didWork
      }
    }

    if (!didWork) {
      break
    }

  }

  runtime$currentOutputState
}
