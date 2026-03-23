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

#' Internal helper `.rtuiRunServer`.
#'
#' Runs one server cycle and resolves output renderer values.
#'
#' @param server Server callback executed for the current cycle.
#' @param inputState Named list containing current input values.
#' @param outputState Named list containing previous output values.
#' @param eventId Optional input id that triggered the current server run.
#' @param runtime Runtime environment created for the current app instance.
#'
#' @return Named list of resolved output values for the cycle.
#'
#' @keywords internal
#' @noRd
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
